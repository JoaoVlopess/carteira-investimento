;;Lógica de registro de compra/venda, validações. camada de interface para o usuário
(ns carteira-investimento.servicos.transacoes
  (:require [carteira-investimento.dados.estado :as estado]
            [carteira-investimento.integracao.acoes :as acoes]
            [carteira-investimento.servicos.carteira :as s-carteira]))

(defn calcular-taxas-automaticas
  "Calcula taxas baseadas no valor da operação (0.1% padrão)"
  [valor-bruto]
  (* valor-bruto 0.001))

(defn obter-quantidade-disponivel
  "Obtem quantidade disponivel de um ticker ate uma data especifica"
  ([ticker]
   (obter-quantidade-disponivel ticker (java.time.LocalDate/now)))

  ([ticker data-limite]
   (let [transacoes (estado/get-transacoes)
         transacoes-ticker (filter #(= (:ticker %) ticker) transacoes)
         ;; Filtra apenas transações até a data limite
         transacoes-ate-data (filter #(not (.isAfter (:data %) data-limite)) transacoes-ticker)]

     (println "=== DEBUG QUANTIDADE DISPONIVEL ===")
     (println "Ticker:" ticker)
     (println "Data limite:" data-limite)
     (println "Total transacoes:" (count transacoes))
     (println "Transacoes do ticker:" (count transacoes-ticker))
     (println "Transacoes ate a data:" (count transacoes-ate-data))

     (reduce (fn [total transacao]
               (println "Processando:" (:tipo transacao) (:quantidade transacao) "em" (:data transacao))
               (case (:tipo transacao)
                 :COMPRA (+ total (:quantidade transacao))
                 :VENDA (- total (:quantidade transacao))
                 total))
             0.0
             transacoes-ate-data))))

(defn registrar-compra
  "Registra compra com preço atual de mercado e taxas automáticas - IMPLEMENTAÇÃO COMPLETA"
  [dados-entrada]
  (let [{:keys [ticker quantidade data]} dados-entrada

        ;; BUSCA PREÇO REAL AUTOMATICAMENTE
        dados-mercado (acoes/buscar-dados-acao ticker data)
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
                   :data data}]

    ;; PERSISTE E ATUALIZA ESTADO
    (estado/add-transacao transacao)
    (s-carteira/atualizar-estado-carteira)
    transacao))

(defn registrar-venda
  "Registra venda com validacao de estoque historico e preço de mercado"
  [dados-entrada]
  (let [{:keys [ticker quantidade data]} dados-entrada

        ;; VALIDAÇÃO DE DATA FUTURA
        _ (when (.isAfter data (java.time.LocalDate/now))
            (throw (ex-info "Não é possível vender em data futura" {:data data})))

        ;; BUSCA PREÇO REAL PARA A DATA ESPECÍFICA
        dados-mercado (acoes/buscar-dados-acao ticker data)
        preco-atual (:preco-atual dados-mercado)

        ;; VALIDAÇÃO DE ESTOQUE ATÉ A DATA DA VENDA
        quantidade-disponivel (obter-quantidade-disponivel ticker data)]

    (println "=== DEBUG VENDA HISTORICA ===")
    (println "Ticker:" ticker)
    (println "Data da venda:" data)
    (println "Quantidade disponivel ate" data ":" quantidade-disponivel)
    (println "Quantidade a vender:" quantidade)

    (if (>= quantidade-disponivel quantidade)
      (let [;; CALCULA TAXAS AUTOMATICAMENTE (0.1% do valor)
            valor-bruto (* quantidade preco-atual)
            taxas-calculadas (calcular-taxas-automaticas valor-bruto)

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
                       :data data}]

        ;; PERSISTE E ATUALIZA ESTADO
        (estado/add-transacao transacao)
        (s-carteira/atualizar-estado-carteira)
        (println "Venda registrada com sucesso!")
        transacao)

      (do
        (println "ERRO: Quantidade insuficiente!")
        (throw (ex-info "Quantidade insuficiente para venda"
                        {:ticker ticker
                         :quantidade-solicitada quantidade
                         :quantidade-disponivel quantidade-disponivel
                         :data data}))))))

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