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
                     (json/read-str dados-brutos :key-fn keyword)
                     dados-brutos) ;; Segurança extra para o caso dos dados virem como string

        dados-acao (get-in dados-mapa [:results 0])]

    (if dados-acao
      (let [dados-renomeados (set/rename-keys dados-acao chave-map-brapi) ;; renomeia as chaves de dados-acao para o dicionario criado
            chaves-necessarias [:ticker :nome :preco-atual :moeda]] ;; filtro das chaves
        (select-keys dados-renomeados chaves-necessarias)) ;; Filtra o resultado, removendo quaisquer chaves desnecessárias

      {:ticker "ERRO", :nome "Sem dados", :preco-atual 0.0, :moeda "BRL"})))


(defn buscar-dados-acao
  "Busca todos os principais dados de uma ação específica na API externa."
  [ticker]


  (if api-token
    (try
      (let [url (str "https://brapi.dev/api/quote/" ticker)]


        (let [opcoes-http {:headers {"Authorization" (str "Bearer " api-token)} ;;nclui o cabeçalho para autenticação
                           :as :json ;; Diz ao clj-http para tentar decodificar o corpo da resposta como JSON
                           :throw-exceptions false} ;; pera permitir que eu trate os erros 

              resposta (http/get url opcoes-http) ;; faz a requisição
              status (:status resposta)] ;; retorna a resposta da requisição


          (cond
            (= status 200) (let [dados-normalizados (normalizar-dados-api (:body resposta))]
                             dados-normalizados) ;; retorna os dados tratados

            (= status 404) (throw (ex-info (str "Ticker não encontrado: " ticker)
                                           {:status 404 :ticker ticker}))
            :else (throw (ex-info (str "Erro na API: " status)
                                  {:status status :body (:body resposta)})))))

      (catch java.io.IOException e
        (throw (ex-info "Erro de rede ao buscar a API" {:erro (.getMessage e)})))

      (catch Exception e
        (throw (ex-info "Erro interno na busca de dados" {:erro (.getMessage e)}))))

    (throw (ex-info "API Token de cotação não está configurado." {:config-erro true}))))