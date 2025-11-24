;;Lógica de registro de compra/venda, validações. camada de interface para o usuário
(ns carteira-investimento.servicos.transacoes
  (:require [carteira-investimento.dados.estado :as estado]
            [carteira-investimento.integracao.acoes :as acoes]
            [carteira-investimento.servicos.carteira :as s-carteira]))

(defn registrar-compra
  "Registra a transação de compra no atom da carteira."
  [dados-brutos]
  (let [{:keys [ticker quantidade preco_unitario moeda]} dados-brutos]
    
    (when (or (nil? ticker)
              (not (pos? quantidade))      ;; Quantidade deve ser > 0
              (not (pos? preco_unitario))) ;; Preço deve ser > 0
      (throw (ex-info "Dados de compra inválidos: ticker, quantidade e preco_unitario são obrigatórios e devem ser positivos."
                      {:dados dados-brutos :tipo :dados-invalidos}))) 
    
  (let[
       valor-total (* quantidade preco_unitario) 
       id (str (java.util.UUID/randomUUID))
       transacao
       {:id-transacao id
        :tipo "COMPRA"
        :ticker ticker
        :quantidade quantidade
        :preco_unitario preco_unitario
        :valor-total valor-total
        :moeda moeda
        :data (java.time.LocalDate/now)}
  ]
    
    (estado/add-transacao transacao)

    (s-carteira/atualizar-estado-carteira) ;;fazer em carteira.clj

    transacao
  
)))

(defn registrar-venda
  "Registra a transação de venda no atom da carteira após validar a posse das ações."
  [dados-brutos]
  (let [{:keys [ticker quantidade preco_unitario moeda]} dados-brutos
        posicoes-atuais (estado/get-acoes)
        posicao-do-ativo (get posicoes-atuais ticker)
        quantidade-em-carteira (:quantidade posicao-do-ativo) ;;quantidade do ativo especifico
        ]

    (when (or (nil? ticker)
              (not (pos? quantidade))      ;; Quantidade deve ser > 0
              (not (pos? preco_unitario))) ;; Preço deve ser > 0
      (throw (ex-info "Dados de compra inválidos: ticker, quantidade e preco_unitario são obrigatórios e devem ser positivos."
                      {:dados dados-brutos :tipo :dados-invalidos})))
    
    (when (> quantidade quantidade-em-carteira)
      (throw (ex-info (str "Quantidade insuficiente de ações para vender o ticker: " ticker)
                       {:status 409 
                        :ticker ticker
                        :tentativa-venda quantidade
                        :disponivel quantidade-em-carteira})))
      

    (let [valor-total (* quantidade preco_unitario)
          id (str (java.util.UUID/randomUUID))
          transacao
          {:id-transacao id
           :tipo "VENDA"
           :ticker ticker
           :quantidade quantidade
           :preco_unitario preco_unitario
           :valor-total valor-total
           :moeda moeda
           :data (java.time.LocalDate/now)}]

      (estado/add-transacao transacao)

      (s-carteira/atualizar-estado-carteira) ;;fazer em carteira.clj

      transacao)))

(defn obter-extrato-por-periodo 
  "retorna o extrato do periodo específico"
  [data-inicio data-fim ticker])