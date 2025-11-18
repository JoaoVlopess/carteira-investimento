;;O atom central (RF-0 Permanecer dados) e funções de acesso/mutação crua.
(ns carteira-investimento.dados.estado
  (:require [clojure.core :as c])) ;; usando para evitar conflitos de nomes do clojure e do meu escopo do projeto (c/ para funções do proprio cojure)

(def carteira
  "O atom central que armazena o estado da carteira de investimentos."
  (atom {:transacoes []
         :acoes {}
         :saldo 0.0}))

(defn add-transacao
  "Adiciona uma transação à lista de transações no atom. 
  A 'transacao' é um mapa que contém todos os dados da operação."
  [transacao]
  (c/swap! carteira update :transacoes c/conj transacao))

(defn get-acoes
  "Retorna o mapa de posições (acoes e quantidades)."
  []
  (:acoes @carteira))

(defn get-saldo
  "Retorna o valor do saldo total da carteira."
  []
  (:saldo @carteira))

(defn remove-acao
  "Remove uma ação do mapa de :ações no atom 'carteira'.
   Esta função deve ser chamada apenas se a quantidade da ação for zero."
  [ticker]
  (c/swap! carteira update :acoes c/dissoc ticker))