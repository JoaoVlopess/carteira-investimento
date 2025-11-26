(ns carteira-investimento.handler
  (:require [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.util.response :as resp]
            [carteira-investimento.servicos.carteira :as s-carteira]
            [carteira-investimento.servicos.transacoes :as s-trans]
            [carteira-investimento.integracao.acoes :as i-acoes]
            [clojure.string :as s]))

;; ========================================
;; HANDLERS
;; ========================================

(defn obter-saldo-handler
  "Retorna o saldo completo da carteira"
  []
  (try
    (resp/response (s-carteira/obter-saldo-completo))
    (catch Exception e
      (-> (resp/response {:erro (.getMessage e)})
          (resp/status 500)))))

(defn consultar-acao-handler
  "Consulta dados de uma ação na API externa"
  [ticker]
  (try
    (resp/response (i-acoes/buscar-dados-acao (s/upper-case ticker)))
    (catch Exception e
      (-> (resp/response {:erro (.getMessage e)})
          (resp/status 500)))))

(defn registrar-compra-handler
  "Registra uma transação de compra"
  [request]
  (try
    (let [dados (:body request)]
      (when (nil? dados)
        (throw (ex-info "Body vazio" {:status 400})))

      (let [resultado (s-trans/registrar-compra dados)]
        (-> (resp/response {:status "success"
                            :message "Compra realizada com sucesso"
                            :ticker (:ticker resultado)
                            :quantidade (:quantidade resultado)
                            :valor-total (:valor-liquido resultado)})
            (resp/status 201))))

    (catch clojure.lang.ExceptionInfo e
      (-> (resp/response {:erro (.getMessage e)})
          (resp/status 400)))

    (catch Exception e
      (-> (resp/response {:erro (.getMessage e)})
          (resp/status 500)))))

(defn registrar-venda-handler
  "Registra uma transação de venda"
  [request]
  (try
    (let [dados (:body request)]
      (when (nil? dados)
        (throw (ex-info "Body vazio" {:status 400})))

      (let [resultado (s-trans/registrar-venda dados)]
        (-> (resp/response {:status "success"
                            :message "Venda realizada com sucesso"
                            :ticker (:ticker resultado)
                            :quantidade (:quantidade resultado)})
            (resp/status 201))))

    (catch clojure.lang.ExceptionInfo e
      (let [dados-erro (.getData e)
            status (:status dados-erro)]
        (cond
          (= status 409) (-> (resp/response {:erro (.getMessage e)})
                             (resp/status 409))
          :else (-> (resp/response {:erro (.getMessage e)})
                    (resp/status 400)))))

    (catch Exception e
      (-> (resp/response {:erro (.getMessage e)})
          (resp/status 500)))))

;; ========================================
;; ROTAS
;; ========================================

(defroutes app-routes
  ;; Rota raiz
  (GET "/" []
    (resp/response {:message "API Carteira de Investimentos"
                    :status "Funcionando!"
                    :version "1.0.0"}))

  ;; Endpoints da carteira
  (GET "/api/carteira/saldo" []
    (obter-saldo-handler))

  ;; Endpoints de ações
  (GET "/api/acoes/:ticker" [ticker]
    (consultar-acao-handler ticker))

  ;; Endpoints de transações
  (POST "/api/transacoes/compra" request
    (registrar-compra-handler request))

  (POST "/api/transacoes/venda" request
    (registrar-venda-handler request))

  ;; Rota 404
  (route/not-found
   (resp/response {:erro "Rota não encontrada"
                   :status 404})))

;; ========================================
;; APLICAÇÃO COM MIDDLEWARE
;; ========================================

(def app
  "Aplicação principal com middleware configurado para API REST"
  (-> app-routes
      (wrap-json-response)
      (wrap-json-body {:keywords? true})
      (wrap-defaults api-defaults)))