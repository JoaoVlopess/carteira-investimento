;;Lógica de saldo, rentabilidade. Cálculos e relatórios
(ns carteira-investimento.servicos.carteira
  (:require [carteira-investimento.dados.estado :as estado]
            [carteira-investimento.integracao.acoes :as acoes]))

(defn- ^:private processar-compra
  "Adiciona uma transação de compra como um novo lote aberto."
  [lotes-atuais transacao]
  (let [;; Mapeia a transação para o formato de lote aberto
        novo-lote {:id-transacao (:id-transacao transacao)
                   :quantidade (:quantidade transacao)
                   :preco-custo (:valor-liquido transacao) ; O custo total do lote
                   :preco-unitario (:preco_unitario transacao) ; Preço original
                   :data (:data transacao)
                   :ticker (:ticker transacao)}

        ticker (:ticker transacao)
        ;; Concatena o novo lote à lista de lotes existentes para o ticker
        lotes-do-ticker (get lotes-atuais ticker) ; Pega a lista de lotes do ticker em questão (pode ser nil)

        ;; Adiciona o novo lote e mantém a lista ordenada por data (FIFO)
        novos-lotes-ordenados (sort-by :data (conj lotes-do-ticker novo-lote))]

    (assoc lotes-atuais ticker novos-lotes-ordenados)))

(defn- ^:private processar-venda
  "Consome a quantidade vendida dos lotes mais antigos (FIFO)."
  [lotes-atuais transacao]
  (let [ticker (:ticker transacao)
        quantidade-venda (:quantidade transacao)

        ;; Obtém a lista de lotes do ticker em questão, ordenada pela data (FIFO)
        lotes-abertos (get lotes-atuais ticker)

        ;; Usa reduce para consumir os lotes e calcular o lucro/prejuízo
        [lotes-restantes] (reduce ;; pega só o primeiro valor do array retornado pelo reduce (pois o segundo deve ser 0)
                           (fn [[lotes-res quant-pendente] lote] ;; lista restante / quantidades de ações a ser vendidas / lote a ser analisado
                             (if (pos? quant-pendente) ; Se ainda há quantidade para vender
                               (let [q-lote (:quantidade lote)

                                     ;; Quantidade a consumir do lote atual: o mínimo entre o pendente e o lote
                                     q-consumir (min quant-pendente q-lote)

                                     ;; Quantidade restante no lote após a venda
                                     q-restante-no-lote (- q-lote q-consumir)

                                     ;; O lote remanescente (se houver) - CORREÇÃO: Ajusta o custo proporcionalmente
                                     lote-atualizado (when (pos? q-restante-no-lote)
                                                       (let [custo-proporcional (* (:preco-unitario lote) q-restante-no-lote)]
                                                         (assoc lote
                                                                :quantidade q-restante-no-lote
                                                                :preco-custo custo-proporcional)))]

                                 [;; Adiciona o lote restante (ou nada) e continua
                                  (if lote-atualizado
                                    (conj lotes-res lote-atualizado)
                                    lotes-res)
                                  (- quant-pendente q-consumir) ; Reduz a quantidade pendente
                                  ])

                               ;; Se não houver mais quantidade para vender (ELSE do primeiro if), apenas retorna o lote restante
                               [lotes-res quant-pendente]))

                           ;; [Lista de lotes resultantes, Quantidade restante a ser vendida]
                           [[] quantidade-venda]
                           lotes-abertos)

        ;; Mapeia o resultado para o mapa principal de lotes
        lotes-att (if (empty? lotes-restantes)
                    (dissoc lotes-atuais ticker) ; Remove o ticker se não houver mais lotes
                    (assoc lotes-atuais ticker lotes-restantes))]

    lotes-att))

(defn reconstruir-lotes-abertos ;; usada para o estado/set-posicoes-completas percorrendo todas as transações e retornando os lotes das posições completas 
  "Itera sobre TODAS as transações
  para reconstruir o estado atual da carteira baseado em lotes abertos (FIFO)."
  [todas-transacoes]
  (let [;; Garante que as transações são processadas em ordem cronológica para o FIFO
        transacoes-ordenadas (sort-by :data todas-transacoes)

        ;; Usa reduce para acumular o mapa de lotes abertos {ticker -> [lotes]}
        lotes-finais (reduce
                      (fn [lotes-acumulados transacao]
                        (let [tipo (:tipo transacao)]
                          (cond
                            (= tipo :COMPRA) (processar-compra lotes-acumulados transacao)
                            (= tipo :VENDA) (processar-venda lotes-acumulados transacao)
                            :else lotes-acumulados))) ; Ignora tipos desconhecidos
                      {} ; Estado inicial: Mapa de lotes vazio
                      transacoes-ordenadas)]

    lotes-finais))

(defn somar-quantidade-lotes
  "Função que serve para dar ao módulo de Transações (venda) uma visão rápida do estoque total disponível para um ativo específico."
  [lotes-abertos]
  (reduce + 0.0 (map :quantidade lotes-abertos)))

(defn somar-custo-total-lotes
  "Soma o custo total de todos os lotes em uma lista de lotes."
  [lotes-abertos]
  (reduce + 0.0 (map :preco-custo lotes-abertos)))

(defn calcular-rentabilidade-por-posicao
  "Calcula as métricas de desempenho para uma única ação. 
   Agora, calcula o valor investido somando TODOS os lotes abertos."
  [lotes-abertos preco-atual]
  (when-not (empty? lotes-abertos) ; Garante que há lotes para processar
    (let [;; Pega a quantidade total somando todos os lotes
          quantidade-total (somar-quantidade-lotes lotes-abertos)

          ;; CORREÇÃO: Calcula o valor investido baseado nos lotes restantes (já com custos proporcionais)
          valor-investido (somar-custo-total-lotes lotes-abertos)

          ;; CÁLCULOS DE MERCADO E LUCRO/PREJUÍZO
          valor-mercado-atual (* quantidade-total preco-atual)
          lucro-preju-liquido (- valor-mercado-atual valor-investido)

          ;; Criação do Mapa de Posição Detalhada
          mapa-posicao {:ticker (:ticker (first lotes-abertos)) ; Pega o ticker do primeiro lote
                        :quantidade quantidade-total
                        :valor-investido valor-investido
                        :preco-atual preco-atual
                        :valor-mercado valor-mercado-atual
                        :lucro-prejuizo lucro-preju-liquido}]
      mapa-posicao)))

(defn obter-saldo-completo
  "Retorna o saldo completo da carteira de forma funcional, sem átomos locais."
  []
  (let [todas-posicoes (estado/get-posicoes)]

    (if (empty? todas-posicoes) ;;caso a carteira esteja vazia
      {:posicoes-detalhadas []
       :total-investido 0.0
       :total-mercado 0.0
       :total-lucro-prejuizo 0.0}

      (let [pares-posicoes (seq todas-posicoes)
            estado-inicial-relatorio {:posicoes-detalhadas []
                                      :total-investido 0.0
                                      :total-mercado 0.0
                                      :total-lucro-prejuizo 0.0}

            relatorio-final (reduce
                             (fn [acumulador [ticker lista-de-lotes]]
                               (try
                                 (let [;; dados-mercado agora será buscado APÓS a validação de lotes
                                       dados-mercado (acoes/buscar-dados-acao ticker)
                                       valor-atual (or (:preco-atual dados-mercado) 0.0)

                                       ;; Chamada com a lista de lotes
                                       relatorio-posicao (calcular-rentabilidade-por-posicao lista-de-lotes valor-atual)]

                                   ;; Se o relatório da posição for nil (ex: lista-de-lotes vazia), não acumula
                                   (if relatorio-posicao
                                     {:posicoes-detalhadas (conj (:posicoes-detalhadas acumulador) relatorio-posicao)
                                      :total-investido (+ (:total-investido acumulador) (:valor-investido relatorio-posicao))
                                      :total-mercado (+ (:total-mercado acumulador) (:valor-mercado relatorio-posicao))
                                      :total-lucro-prejuizo (+ (:total-lucro-prejuizo acumulador) (:lucro-prejuizo relatorio-posicao))}
                                     acumulador)) ; Se relatorio-posicao é nil, retorna o acumulador inalterado

                                 (catch Exception e
                                   (println (str "ERRO ao buscar dados para " ticker ": " (.getMessage e)))
                                   acumulador))) ; Em caso de erro, continua com o acumulador atual
                             estado-inicial-relatorio
                             pares-posicoes)]
        relatorio-final))))

(defn atualizar-estado-carteira
  "Atualiza o estado derivado da carteira (posições e saldo) após uma transação."
  []
  (try
    (let [transacoes-carteira (estado/get-transacoes)

          ;; Chama o algoritmo FIFO
          posicoes-calculadas (reconstruir-lotes-abertos transacoes-carteira)]

      ;; 1. Salva o novo mapa de posições (estrutura de lotes)
      (estado/set-posicoes-completas posicoes-calculadas)

      (let [relatorio-completo (obter-saldo-completo)
            saldo-total (:total-mercado relatorio-completo)]

        ;; 2. Salva o novo saldo
        (estado/set-saldo saldo-total)))

    (catch Exception e
      (println "ERRO ao atualizar o estado da carteira:" (.getMessage e))
      (throw e)))) ; Re-lança a exceção para que o chamador saiba que algo deu errado