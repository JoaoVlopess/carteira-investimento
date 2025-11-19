;;O atom central (RF-0 Permanecer dados) e funções de acesso/mutação crua.
(ns carteira-investimento.dados.estado
  (:require [clojure.core :as c])) ;; usando para evitar conflitos de nomes do clojure e do meu escopo do projeto (c/ para funções do proprio cojure)

(def carteira
  "O atom central que armazena o estado da carteira de investimentos."
  (atom {:transacoes [] ;; mapa de dados de cada transação financeira
         :posicoes {} ;; cada posicao: quantidade, preco-medio, valor investido
         :saldo 0.0}))

(defn add-transacao
  "Adiciona uma transação à lista de transações no atom. 
  A 'transacao' é um mapa que contém todos os dados da operação."
  [transacao]
  (c/swap! carteira update :transacoes c/conj transacao))

(defn get-acoes
  "Retorna o mapa de posições (posicoes e quantidades)."
  []
  (:posicoes @carteira))

(defn get-saldo
  "Retorna o valor do saldo total da carteira."
  []
  (:saldo @carteira))

(defn remove-posicao
  "Remove uma posicao do mapa de :posicoes no atom 'carteira'.
   Esta função deve ser chamada apenas se a quantidade da ação for zero."
  [ticker]
  (c/swap! carteira update :posicoes c/dissoc ticker))

(defn set-saldo [novo-saldo]
  "Atualiza o valor do saldo da carteira"
  (c/swap! carteira c/assoc :saldo novo-saldo))

(defn get-transacoes 
  "Retorna todas as transações da carteira"
  ([]
   :transacoes @carteira)
  
  ([data-inicio data-fim]
   ;;fazer
   ) 
  )

(defn set-posicao-acao [ticker dados-posicao]
  "atualiza os valores da posição específica"
  (c/swap! carteira
           assoc-in
           [:acoes ticker]
           dados-posicao))