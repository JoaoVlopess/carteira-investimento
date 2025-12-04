;;Lógica de saldo, rentabilidade. Cálculos e relatórios
(ns carteira-investimento.servicos.carteira
  (:require [carteira-investimento.dados.estado :as estado]
            [carteira-investimento.integracao.acoes :as acoes]))

(defn- ^:private processar-compra
  "Adiciona uma transação de compra como um novo lote aberto.
   Cada compra cria um novo lote com seu preço específico para controle FIFO."
  [lotes-atuais transacao]
  (let [;; Mapeia a transação para o formato de lote aberto
        novo-lote {:id-transacao (:id-transacao transacao)
                   :quantidade (:quantidade transacao)
                   :preco-custo (:valor-total transacao) ; O custo total do lote
                   :preco-unitario (:preco_unitario transacao) ; Preço original
                   :data (:data transacao)
                   :ticker (:ticker transacao)}

        ticker (:ticker transacao)
        ;; Pega a lista de lotes existentes para o ticker (pode ser nil)
        lotes-do-ticker (get lotes-atuais ticker)

        ;; Adiciona o novo lote e mantém a lista ordenada por data (FIFO)
        novos-lotes-ordenados (sort-by :data (conj lotes-do-ticker novo-lote))]

    ;; Retorna o mapa atualizado com os novos lotes para este ticker
    (assoc lotes-atuais ticker novos-lotes-ordenados)))

(defn- ^:private processar-venda
  "Consome a quantidade vendida dos lotes mais antigos (FIFO).
   Implementa o algoritmo First-In-First-Out para controle de custos."
  [lotes-atuais transacao]
  (let [ticker (:ticker transacao)
        quantidade-venda (:quantidade transacao)

        ;; Obtém a lista de lotes do ticker, já ordenada pela data (FIFO)
        lotes-abertos (get lotes-atuais ticker)

        ;; Usa reduce para consumir os lotes mais antigos primeiro
        [lotes-restantes] (reduce
                           (fn [[lotes-res quant-pendente] lote] ; acumulador: [lotes restantes, quantidade ainda a vender]
                             (if (pos? quant-pendente) ; Se ainda há quantidade para vender
                               (let [q-lote (:quantidade lote)

                                     ;; Quantidade a consumir: mínimo entre pendente e disponível no lote
                                     q-consumir (min quant-pendente q-lote)

                                     ;; Quantidade que sobra no lote após a venda
                                     q-restante-no-lote (- q-lote q-consumir)

                                     ;; Se sobrou algo no lote, cria lote atualizado com custo proporcional
                                     lote-atualizado (when (pos? q-restante-no-lote)
                                                       (let [custo-proporcional (* (:preco-unitario lote) q-restante-no-lote)]
                                                         (assoc lote
                                                                :quantidade q-restante-no-lote
                                                                :preco-custo custo-proporcional)))]

                                 [;; Adiciona o lote restante (se houver) à lista de resultados
                                  (if lote-atualizado
                                    (conj lotes-res lote-atualizado)
                                    lotes-res)
                                  ;; Reduz a quantidade pendente
                                  (- quant-pendente q-consumir)])

                               ;; Se não há mais quantidade para vender, mantém o lote intacto
                               [(conj lotes-res lote) quant-pendente]))

                           ;; Estado inicial: [lista vazia, quantidade total a vender]
                           [[] quantidade-venda]
                           lotes-abertos)

        ;; Atualiza o mapa principal: remove ticker se não há lotes, senão atualiza
        lotes-att (if (empty? lotes-restantes)
                    (dissoc lotes-atuais ticker) ; Remove o ticker se não houver mais lotes IF
                    (assoc lotes-atuais ticker lotes-restantes))] ;; retorna o mapa atualizado com os lotes restantes depois da venda ELSE

    lotes-att))

(defn reconstruir-lotes-abertos
  "Itera sobre TODAS as transações para reconstruir o estado atual da carteira.
   Usa o algoritmo FIFO (First-In-First-Out) para controle de lotes abertos."
  [todas-transacoes]
  (let [;; Garante que as transações são processadas em ordem cronológica
        transacoes-ordenadas (sort-by :data todas-transacoes)

        ;; Usa reduce para acumular o mapa de lotes abertos {ticker -> [lotes]}
        lotes-finais (reduce
                      (fn [lotes-acumulados transacao]
                        (let [tipo (:tipo transacao)]
                          (cond
                            ;; Se é compra, adiciona novo lote
                            (= tipo :COMPRA) (processar-compra lotes-acumulados transacao)
                            ;; Se é venda, consome lotes mais antigos
                            (= tipo :VENDA) (processar-venda lotes-acumulados transacao)
                            ;; Ignora tipos desconhecidos
                            :else lotes-acumulados)))
                      {} ; Estado inicial: Mapa de lotes vazio
                      transacoes-ordenadas)]

    lotes-finais))

(defn somar-quantidade-lotes
  "Função auxiliar que soma a quantidade total de ações em todos os lotes.
   Usada para dar visão rápida do estoque disponível para um ticker."
  [lotes-abertos]
  (reduce + 0.0 (map :quantidade lotes-abertos)))

(defn somar-custo-total-lotes
  "Função auxiliar que soma o custo total investido em todos os lotes.
   Considera os custos proporcionais após vendas parciais."
  [lotes-abertos]
  (reduce + 0.0 (map :preco-custo lotes-abertos)))

(defn calcular-rentabilidade-por-posicao
  "Calcula as métricas de desempenho para uma única ação.
   Soma TODOS os lotes abertos para calcular posição consolidada."
  [lotes-abertos preco-atual]
  (when-not (empty? lotes-abertos) ; Garante que há lotes para processar
    (let [;; Soma a quantidade total de todos os lotes
          quantidade-total (somar-quantidade-lotes lotes-abertos)

          ;; Soma o valor investido baseado nos lotes restantes (já com custos proporcionais)
          valor-investido (somar-custo-total-lotes lotes-abertos)

          ;; Calcula valor de mercado atual
          valor-mercado-atual (* quantidade-total preco-atual)

          ;; Calcula lucro/prejuízo líquido
          lucro-preju-liquido (- valor-mercado-atual valor-investido)]

      ;; Retorna mapa com métricas completas da posição
      {:ticker (:ticker (first lotes-abertos)) ; Pega o ticker do primeiro lote
       :quantidade quantidade-total
       :valor-investido valor-investido
       :preco-atual preco-atual
       :valor-mercado valor-mercado-atual
       :lucro-prejuizo lucro-preju-liquido})))

(defn obter-saldo-completo
  "Retorna o saldo completo da carteira de forma funcional.
   Busca preços atuais e calcula rentabilidade de todas as posições."
  []
  (let [todas-posicoes (estado/get-posicoes)]

    (if (empty? todas-posicoes)
      {:posicoes-detalhadas []
       :total-investido 0.0
       :total-mercado 0.0
       :total-lucro-prejuizo 0.0}

      (let [pares-posicoes (seq todas-posicoes)
            estado-inicial {:posicoes-detalhadas []
                            :total-investido 0.0
                            :total-mercado 0.0
                            :total-lucro-prejuizo 0.0}]

        (reduce
         (fn [acumulador [ticker lista-de-lotes]]
           ;; Busca preço ATUAL (sem data) para valor de mercado
           (let [dados-mercado (acoes/buscar-dados-acao ticker)
                 preco-atual (or (:preco-atual dados-mercado) 0.0)

                 ;; Calcula métricas usando os custos REAIS dos lotes
                 quantidade-total (somar-quantidade-lotes lista-de-lotes)
                 valor-investido-real (somar-custo-total-lotes lista-de-lotes)  ; ← CUSTO REAL
                 valor-mercado-atual (* quantidade-total preco-atual)
                 lucro-prejuizo-real (- valor-mercado-atual valor-investido-real)

                 relatorio-posicao {:ticker ticker
                                    :quantidade quantidade-total
                                    :valor-investido valor-investido-real  ; ← CUSTO REAL DOS LOTES
                                    :preco-atual preco-atual
                                    :valor-mercado valor-mercado-atual
                                    :lucro-prejuizo lucro-prejuizo-real}]

             ;; Acumula nos totais
             {:posicoes-detalhadas (conj (:posicoes-detalhadas acumulador) relatorio-posicao)
              :total-investido (+ (:total-investido acumulador) valor-investido-real)
              :total-mercado (+ (:total-mercado acumulador) valor-mercado-atual)
              :total-lucro-prejuizo (+ (:total-lucro-prejuizo acumulador) lucro-prejuizo-real)}))
         estado-inicial
         pares-posicoes)))))

(defn atualizar-estado-carteira
  "Atualiza o estado derivado da carteira (posições e saldo) após uma transação.
   Esta função é chamada sempre que há uma nova compra ou venda."
  []
  (try
    (let [;; Busca todas as transações registradas
          transacoes-carteira (estado/get-transacoes)

          ;; Reconstrói as posições usando o algoritmo FIFO
          posicoes-calculadas (reconstruir-lotes-abertos transacoes-carteira)]

      ;; 1. Salva o novo mapa de posições (estrutura de lotes)
      (estado/set-posicoes-completas posicoes-calculadas)

      ;; 2. Calcula e salva o novo saldo total
      (let [relatorio-completo (obter-saldo-completo)
            saldo-total (:total-mercado relatorio-completo)]
        (estado/set-saldo saldo-total)))

    (catch Exception e
      ;; Em caso de erro, apenas registra no log 
      (println "Erro ao atualizar carteira:" (.getMessage e)))))