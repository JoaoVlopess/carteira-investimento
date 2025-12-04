;; Lógica de registro de compra/venda, validações e interface para o usuário
(ns carteira-investimento.servicos.transacoes
  (:require [carteira-investimento.dados.estado :as estado]
            [carteira-investimento.integracao.acoes :as acoes]
            [carteira-investimento.servicos.carteira :as s-carteira]))

(defn calcular-taxas-automaticas
  "Calcula taxas baseadas no valor da operação (0.1% padrão da corretora)"
  [valor-bruto]
  (* valor-bruto 0.001))

(defn obter-quantidade-disponivel
  "Calcula quantas ações estão disponíveis para venda até uma data específica.
   Usa validação temporal: só conta transações que aconteceram até a data limite."
  ([ticker]
   ;; Se não especificar data, usa hoje
   (obter-quantidade-disponivel ticker (java.time.LocalDate/now)))

  ([ticker data-limite]
   (let [transacoes (estado/get-transacoes)

         ;; Filtra apenas transações do ticker específico
         transacoes-ticker (filter #(= (:ticker %) ticker) transacoes)

         ;; VALIDAÇÃO TEMPORAL: só conta transações até a data limite (inclusive)
         ;; Isso impede vendas "no passado" de ações compradas "no futuro"
         transacoes-ate-data (filter #(not (.isAfter (:data %) data-limite))
                                     transacoes-ticker)]

     ;; Calcula saldo: COMPRA (+) / VENDA (-)
     (reduce (fn [total transacao]
               (case (:tipo transacao)
                 :COMPRA (+ total (:quantidade transacao))
                 :VENDA (- total (:quantidade transacao))
                 total))  ; Ignora outros tipos
             0.0
             transacoes-ate-data))))

(defn registrar-compra
  "Registra uma compra de ação com preço real de mercado e taxas automáticas"
  [dados-entrada]
  (let [{:keys [ticker quantidade data]} dados-entrada

        ;; BUSCA PREÇO REAL: atual se for hoje, histórico se for data passada
        dados-mercado (acoes/buscar-dados-acao ticker data)
        preco-unitario (:preco-atual dados-mercado)

        ;; CÁLCULOS FINANCEIROS
        valor-bruto (* quantidade preco-unitario)
        taxas-calculadas (calcular-taxas-automaticas valor-bruto)
        valor-total valor-bruto  ; Para compra, valor total = valor bruto
        valor-liquido (+ valor-total taxas-calculadas)  ; Compra: bruto + taxas

        ;; CRIA TRANSAÇÃO COMPLETA
        transacao {:id-transacao (str (java.util.UUID/randomUUID))
                   :tipo :COMPRA
                   :ticker ticker
                   :quantidade quantidade
                   :preco_unitario preco-unitario
                   :taxas taxas-calculadas
                   :valor-total valor-total
                   :valor-liquido valor-liquido
                   :moeda "BRL"
                   :data data}]

    ;; PERSISTE NO ESTADO E RECALCULA CARTEIRA
    (estado/add-transacao transacao)
    (s-carteira/atualizar-estado-carteira)
    transacao))

(defn registrar-venda
  "Registra uma venda de ação com validação de estoque histórico"
  [dados-entrada]
  (let [{:keys [ticker quantidade data]} dados-entrada]

    ;; VALIDAÇÃO 1: Não permite venda em data futura
    (when (.isAfter data (java.time.LocalDate/now))
      (throw (ex-info "Não é possível vender em data futura" {:data data})))

    (let [;; BUSCA PREÇO REAL para a data específica da venda
          dados-mercado (acoes/buscar-dados-acao ticker data)
          preco-unitario (:preco-atual dados-mercado)

          ;; VALIDAÇÃO 2: Verifica se tem estoque suficiente ATÉ a data da venda
          quantidade-disponivel (obter-quantidade-disponivel ticker data)]

    ;; VALIDAÇÃO DE ESTOQUE
    (if (>= quantidade-disponivel quantidade)
      ;; TEM ESTOQUE: Processa a venda
      (let [valor-bruto (* quantidade preco-unitario)
            taxas-calculadas (calcular-taxas-automaticas valor-bruto)
            valor-total valor-bruto
            valor-liquido (- valor-total taxas-calculadas)  ; Venda: bruto - taxas

            ;; CRIA TRANSAÇÃO DE VENDA
            transacao {:id-transacao (str (java.util.UUID/randomUUID))
                       :tipo :VENDA
                       :ticker ticker
                       :quantidade quantidade
                       :preco_unitario preco-unitario
                       :taxas taxas-calculadas
                       :valor-total valor-total
                       :valor-liquido valor-liquido
                       :moeda "BRL"
                       :data data}]

        ;; PERSISTE E ATUALIZA ESTADO
        (estado/add-transacao transacao)
        (s-carteira/atualizar-estado-carteira)
        transacao)

      ;; NÃO TEM ESTOQUE: Lança erro com detalhes
      (throw (ex-info "Quantidade insuficiente para venda"
                      {:ticker ticker
                       :quantidade-solicitada quantidade
                       :quantidade-disponivel quantidade-disponivel
                       :data data}))))))

(defn obter-extrato-por-periodo
  "Retorna transações de um período específico, opcionalmente filtrado por ticker"
  ([data-inicio data-fim]
   ;; Todas as transações do período
   (let [transacoes-periodo (estado/get-transacoes data-inicio data-fim)]
     (sort-by :data transacoes-periodo)))

  ([data-inicio data-fim ticker]
   ;; Transações do período filtradas por ticker específico
   (let [transacoes-periodo (estado/get-transacoes data-inicio data-fim)
         transacoes-ticker (filter #(= (:ticker %) ticker) transacoes-periodo)]
     (sort-by :data transacoes-ticker))))

(defn obter-extrato-completo
  "Retorna todas as transações da carteira, ordenadas por data"
  []
  (let [todas-transacoes (estado/get-transacoes)]
    (sort-by :data todas-transacoes)))