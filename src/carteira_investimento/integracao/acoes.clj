;;Funções para consultar a API externa
(ns carteira-investimento.integracao.acoes
  (:require [clj-http.client :as http]
            [clojure.set :as set]))

(def api-token "5N4ZTjFauu9QG5R4TW1sVk")
;; (def api-token (System/getenv "STOCK_API_KEY"))

;; Definição do mapeamento de chaves
(def ^:private chave-map-brapi
  {:symbol                 :ticker
   :shortName              :nome
   :regularMarketPrice     :preco-atual
   :currency               :moeda})

(defn normalizar-dados-api
  "Recebe dados brutos da api e retorna uma padronização e organização desses dados."
  [dados-brutos]
  (let [
        dados-acao (get-in dados-brutos [:results 0])

        ;; 2. Mapeamento e Renomeação: Renomeia as chaves para o padrão interno
        dados-renomeados (set/rename-keys dados-acao chave-map-brapi)

        ;; 3. Seleção: Filtra apenas as chaves que realmente importam para o sistema
        chaves-necessarias [:ticker :nome :preco-atual :moeda]]

    ;; 4. Saída: Retorna o mapa limpo
    (select-keys dados-renomeados chaves-necessarias)))


(defn buscar-dados-acao
  "Busca todos os principais dados de uma ação específica na API externa."
  [ticker]
  (if api-token 
    (try      
      (let [url (str "https://brapi.dev/api/quote/" ticker)

            ;; 1. Configura os parâmetros da requisição
            opcoes-http {:headers {"Authorization" (str "Bearer " api-token)}
                         :as :json
                         :throw-exceptions false}

            ;; 2. Faz a requisição usando o clj-http.client
            resposta (http/get url opcoes-http)

            status (:status resposta)]


        (cond
          (= status 200) (normalizar-dados-api (:body resposta))
          ;;o ex-info permite anexar um mapa de dados à exceção
          (= status 404) (throw (ex-info (str "Ticker não encontrado: " ticker)
                                         {:status 404 :ticker ticker}))
          :else (throw (ex-info (str "Erro na API: " status)
                                {:status status :body (:body resposta)}))))

      ;; 4. Bloco 'catch' 1: Erros de Rede/I/O (dentro do escopo do try)
      (catch java.io.IOException e
        (throw (ex-info "Erro de rede ao buscar a API" {:erro (.getMessage e)})))

      ;; 5. Bloco 'catch' 2: Outros Erros (dentro do escopo do try)
      (catch Exception e
        (throw (ex-info "Erro interno na busca de dados" {:erro (.getMessage e)}))))

    ;; Caso o token não esteja configurado
    (throw (ex-info "API Token de cotação não está configurado." {:config-erro true}))))
