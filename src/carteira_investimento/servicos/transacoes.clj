;;Lógica de registro de compra/venda, validações. camada de interface para o usuário
(ns carteira-investimento.servicos.transacoes
  (:require [carteira-investimento.dados.estado :as estado]
            [carteira-investimento.integracao.acoes :as acoes]
            [carteira-investimento.servicos.carteira :as s-carteira]))

(defn registrar-compra
  "Registra a transação de compra no atom da carteira."
  [dados-brutos]
  (when (nil? dados-brutos)
    (throw (ex-info "Dados de entrada não podem ser nulos" {:dados dados-brutos :tipo :dados-nulos})))

  (let [{:keys [ticker quantidade preco_unitario taxas moeda]} (merge {:taxas 0.0} dados-brutos)

        _ (when (or (nil? ticker) (not (pos? quantidade)) (not (pos? preco_unitario)))
            (throw (ex-info "Dados de compra inválidos..." {:dados dados-brutos :tipo :dados-invalidos})))

        valor-total (* quantidade preco_unitario)
        valor-liquido (+ valor-total taxas)
        id (str (java.util.UUID/randomUUID))

        transacao {:id-transacao id
                   :tipo :COMPRA
                   :ticker ticker
                   :quantidade quantidade
                   :preco_unitario preco_unitario
                   :taxas taxas
                   :valor-total valor-total
                   :valor-liquido valor-liquido
                   :moeda moeda
                   :data (java.time.LocalDate/now)}] ; <-- Final do único bloco let

    (estado/add-transacao transacao)
    (s-carteira/atualizar-estado-carteira)
    transacao))


(defn registrar-venda
  "Registra a transação de venda no atom da carteira após validar a posse das ações (somando lotes FIFO)."
  [dados-brutos]
  (when (nil? dados-brutos)
    (throw (ex-info "Dados de entrada não podem ser nulos" {:dados dados-brutos :tipo :dados-nulos})))

  (let [;; Desestrutura o input, garantindo default de taxas
        {:keys [ticker quantidade preco_unitario taxas moeda]} (merge {:taxas 0.0} dados-brutos)

        ;;  Pega a lista de lotes e soma suas quantidades
        lotes-abertos (estado/get-posicao-especifica ticker)
        quantidade-em-carteira (s-carteira/somar-quantidade-lotes lotes-abertos)]

    ;; 1. Validação de Dados de Entrada
    (when (or (nil? ticker)
              (not (pos? quantidade))
              (not (pos? preco_unitario)))
      (throw (ex-info "Dados de venda inválidos..." {:dados dados-brutos :tipo :dados-invalidos})))

    ;; 2. Validação de Estoque 
    (when (> quantidade quantidade-em-carteira)
      (throw (ex-info (str "Quantidade insuficiente de ações para vender o ticker: " ticker)
                      {:status 409
                       :ticker ticker
                       :tentativa-venda quantidade
                       :disponivel quantidade-em-carteira})))

    ;; 3. Lógica de Criação e Persistência
    (let [valor-total (* quantidade preco_unitario)
          valor-liquido (- valor-total taxas) ; Fórmula da venda correta (bruto - taxas)
          id (str (java.util.UUID/randomUUID))
          transacao
          {:id-transacao id
           :tipo :VENDA
           :ticker ticker
           :quantidade quantidade
           :preco_unitario preco_unitario
           :taxas taxas
           :valor-total valor-total
           :valor-liquido valor-liquido
           :moeda moeda
           :data (java.time.LocalDate/now)}]

      (estado/add-transacao transacao)
      (s-carteira/atualizar-estado-carteira)
      transacao)))

(defn obter-extrato-por-periodo 
  "retorna o extrato do periodo específico"
  ([data-inicio data-fim ] ;; pega todos extratos do periodo especifico
  (let [transacoes-periodo (estado/get-transacoes data-inicio data-fim)] 
    (sort-by :data transacoes-periodo)))

  ([data-inicio data-fim ticker] ;; pega todos extratos do periodo e ticker especifico
  (let [
        transacoes-periodo (estado/get-transacoes data-inicio data-fim)
        transacoes-ticker (filter #(= (:ticker %) ticker) transacoes-periodo)]
    (sort-by :data transacoes-ticker)))
  )

(defn obter-extrato-completo 
  "retorna o extrato completo até o momento em questão"
  []
  (let [
        todas-transacoes (estado/get-transacoes)
  ]
    (sort-by :data todas-transacoes)
    )
  )
