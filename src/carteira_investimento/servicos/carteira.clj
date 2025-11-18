;;Lógica de saldo, rentabilidade
(ns carteira-investimento.servicos.carteira
  (:require [carteira-investimento.dados.estado :as estado]
            [carteira-investimento.integracao.acoes :as acoes]
            ;; Importações Opcionais de Clojure Core (se necessário)
            [clojure.core :as c]
            [clojure.string :as s]))

(defn calcula-preco-medio 
  "custo real que cliente teve para adquirir as ações que ainda possui. Envolvendo custo total e quantidade liquida remanescente após operações"
  [transacoes[]]

  )