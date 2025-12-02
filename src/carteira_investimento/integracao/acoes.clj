;;Funções para consultar a API externa de investimentos
(ns carteira-investimento.integracao.acoes
  (:require [clj-http.client :as http] ;; usado para enviar requisições de rede.
            [clojure.set :as set] ;; Necessário para a função set/rename-keys
            [clojure.data.json :as json])) ;; Necessário para analisar o corpo da resposta JSON

(def api-token "5N4ZTjFauu9QG5R4TW1sVk")

;; Definição do mapeamento de chaves (Dicionário de tradução)
(def ^:private chave-map-brapi
  {:symbol                 :ticker
   :shortName              :nome
   :regularMarketPrice     :preco-atual
   :currency               :moeda})

(defn normalizar-dados-api
  "Recebe dados brutos da api e retorna uma padronização e organização desses dados."
  [dados-brutos]
  (let [dados-mapa (if (string? dados-brutos)
                     (json/read-str dados-brutos :key-fn keyword) ;; se for string precisa fazer a transformação para JSON e transformar as chaves em keywords no clojure
                     dados-brutos)
        dados-acao (get-in dados-mapa [:results 0])]

    (if dados-acao
      (let [dados-renomeados (set/rename-keys dados-acao chave-map-brapi)
            chaves-necessarias [:ticker :nome :preco-atual :moeda]]
        (select-keys dados-renomeados chaves-necessarias))
      {:ticker "ERRO", :nome "Sem dados", :preco-atual 0.0, :moeda "BRL"})))

(defn buscar-dados-acao
  "Busca todos os principais dados de uma ação específica na API externa."
  [ticker]
  (try
    (let [url (str "https://brapi.dev/api/quote/" ticker)
          opcoes-http {:headers {"Authorization" (str "Bearer " api-token)}
                       :as :json
                       :throw-exceptions false}
          resposta (http/get url opcoes-http)]

      (if (= 200 (:status resposta))
        (normalizar-dados-api (:body resposta))
        {:ticker ticker, :nome "Erro ao buscar", :preco-atual 0.0, :moeda "BRL"}))

    (catch Exception e
      {:ticker ticker, :nome "Erro de conexão", :preco-atual 0.0, :moeda "BRL"})))