;;Lógica de registro de compra/venda, validações. camada de interface para o usuário
(ns carteira-investimento.servicos.transacoes
  (:require [carteira-investimento.dados.estado :as estado]
            [carteira-investimento.integracao.acoes :as acoes]
            [carteira-investimento.servicos.carteira :as s-carteira]))

(defn calcular-taxas-automaticas
  "Calcula taxas baseadas no valor da operação (0.1% padrão)"
  [valor-bruto]
  (* valor-bruto 0.001))

(defn registrar-compra
  "Registra compra com preço atual de mercado e taxas automáticas - IMPLEMENTAÇÃO COMPLETA"
  [dados-entrada]
  (let [{:keys [ticker quantidade]} dados-entrada

        ;; BUSCA PREÇO REAL AUTOMATICAMENTE
        dados-mercado (acoes/buscar-dados-acao ticker)
        preco-atual (:preco-atual dados-mercado)

        ;; CALCULA TAXAS AUTOMATICAMENTE (0.1% do valor)
        valor-bruto (* quantidade preco-atual)
        taxas-calculadas (calcular-taxas-automaticas valor-bruto)

        ;; CÁLCULOS FINAIS
        valor-total (* quantidade preco-atual)
        valor-liquido (+ valor-total taxas-calculadas)
        id (str (java.util.UUID/randomUUID))

        ;; TRANSAÇÃO COMPLETA
        transacao {:id-transacao id
                   :tipo :COMPRA
                   :ticker ticker
                   :quantidade quantidade
                   :preco_unitario preco-atual
                   :taxas taxas-calculadas
                   :valor-total valor-total
                   :valor-liquido valor-liquido
                   :moeda "BRL"
                   :data (java.time.LocalDate/now)}]

    ;; PERSISTE E ATUALIZA ESTADO
    (estado/add-transacao transacao)
    (s-carteira/atualizar-estado-carteira)
    transacao))

(defn registrar-venda
  "Registra venda com preço atual de mercado e taxas automáticas - IMPLEMENTAÇÃO COMPLETA"
  [dados-entrada]
  (let [{:keys [ticker quantidade]} dados-entrada

        ;; BUSCA PREÇO REAL AUTOMATICAMENTE
        dados-mercado (acoes/buscar-dados-acao ticker)
        preco-atual (:preco-atual dados-mercado)

        ;; VALIDAÇÃO DE ESTOQUE (FIFO)
        lotes-abertos (estado/get-posicao-especifica ticker)
        quantidade-em-carteira (s-carteira/somar-quantidade-lotes lotes-abertos)]

    (if (> quantidade quantidade-em-carteira)
      (throw (ex-info (str "Quantidade insuficiente de ações para vender o ticker: " ticker)
                      {:ticker ticker
                       :tentativa-venda quantidade
                       :disponivel quantidade-em-carteira}))
      (let [;; CALCULA TAXAS AUTOMATICAMENTE (0.1% do valor)
            valor-bruto (* quantidade preco-atual)
            taxas-calculadas (* valor-bruto 0.001)

            ;; CÁLCULOS FINAIS
            valor-total (* quantidade preco-atual)
            valor-liquido (- valor-total taxas-calculadas) ; Venda: bruto - taxas
            id (str (java.util.UUID/randomUUID))

            ;; TRANSAÇÃO COMPLETA
            transacao {:id-transacao id
                       :tipo :VENDA
                       :ticker ticker
                       :quantidade quantidade
                       :preco_unitario preco-atual
                       :taxas taxas-calculadas
                       :valor-total valor-total
                       :valor-liquido valor-liquido
                       :moeda "BRL"
                       :data (java.time.LocalDate/now)}]

        ;; PERSISTE E ATUALIZA ESTADO
        (estado/add-transacao transacao)
        (s-carteira/atualizar-estado-carteira)
        transacao))))

(defn obter-extrato-por-periodo
  "retorna o extrato do periodo específico"
  ([data-inicio data-fim] ;; pega todos extratos do periodo especifico
   (let [transacoes-periodo (estado/get-transacoes data-inicio data-fim)]
     (sort-by :data transacoes-periodo)))

  ([data-inicio data-fim ticker] ;; pega todos extratos do periodo e ticker especifico
   (let [transacoes-periodo (estado/get-transacoes data-inicio data-fim)
         transacoes-ticker (filter #(= (:ticker %) ticker) transacoes-periodo)]
     (sort-by :data transacoes-ticker))))

(defn obter-extrato-completo
  "retorna o extrato completo até o momento em questão"
  []
  (let [todas-transacoes (estado/get-transacoes)]
    (sort-by :data todas-transacoes)))