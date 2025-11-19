;;Lógica de saldo, rentabilidade
(ns carteira-investimento.servicos.carteira
  (:require [carteira-investimento.dados.estado :as estado]
            [carteira-investimento.integracao.acoes :as acoes]
            ;; Importações Opcionais de Clojure Core (se necessário)
            [clojure.core :as c]
            [clojure.string :as s]))

(defn calcula-preco-medio
  "custo real que cliente teve para adquirir as ações que ainda possui. Envolvendo custo total e quantidade liquida remanescente após operações"
  [transacoes]
  (let [;; Inicializa o redutor com quantidade e custo zerados
         estado-inicial {:quantidade 0.0, :custo 0.0}
  
         ;; Reduz a lista de transações para calcular o estado final (custo e quantidade)
         estado-final (reduce
                       (fn [acumulador transacao]
                         (let [tipo (:tipo transacao)]
                           (if (= tipo "COMPRA")
                             ;; Lógica da Compra: Soma quantidade e soma custo líquido
                             {:quantidade (+ (:quantidade acumulador) (:quantidade transacao))
                              :custo (+ (:custo acumulador) (:valor-liquido transacao))}
  
                             ;; ELse -> Lógica da Venda: Subtrai quantidade e ajusta o custo
                             (let [custo-medio-atual (if (pos? (:quantidade acumulador))
                                                       (/ (:custo acumulador) (:quantidade acumulador))
                                                       0.0)
                                   custo-das-vendidas (* custo-medio-atual (:quantidade transacao))]
                               {:quantidade (- (:quantidade acumulador) (:quantidade transacao))
                                :custo (- (:custo acumulador) custo-das-vendidas)}))))
                       estado-inicial
                       transacoes)
  
         quantidade-final (:quantidade estado-final)
         custo-final (:custo estado-final)]
     
     (let [preco-medio-final (c/cond
                               (c/pos? quantidade-final) (c// custo-final quantidade-final)
                               (c/zero? quantidade-final) 0.0
                               :else (c/throw (c/ex-info "Erro..." {})))]
  
     {:quantidade quantidade-final
      :preco-medio preco-medio-final})))


(defn calcular-posicoes-agregadas 
  "Agrupa as transações por ticker. Para cada grupo, chama o calcular-preco-medio para montar o mapa completo"
  [transacoes]
(let [;; 1. Agrupamento eficiente por :ticker. Retorna: {"PETR4" [t1 t2], "VALE3" [t3]}
      transacoes-agrupadas (group-by :ticker transacoes)

      ;; 2. Processamento: Itera sobre as ENTRADAS (pares [ticker lista-transacoes])
      posicoes-calculadas (map (fn [[ticker lista-transacoes]]

                                 ;; Lógica de Recálculo: Obtém o mapa {:quantidade N, :preco-medio X}
                                 (let [dados-posicao (calcula-preco-medio lista-transacoes)]

                                   ;; 3. Cria a entrada que será inserida no mapa final
                                   [ticker (assoc dados-posicao :ticker ticker)]))

                               transacoes-agrupadas)]
(into {} posicoes-calculadas)))

(defn atualizar-estado-carteira
  "Atualiza o valor contido na carteira"
  [])