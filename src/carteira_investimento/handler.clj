(ns carteira-investimento.handler
  (:require [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.util.response :as resp]

            ;; Seus serviços (que estão perfeitos!)
            [carteira-investimento.servicos.carteira :as s-carteira]
            [carteira-investimento.servicos.transacoes :as s-trans]
            [carteira-investimento.integracao.acoes :as i-acoes]
            [clojure.string :as s]))

;; Handlers simplificados
(defn obter-saldo-handler []
  (try
    (resp/response (s-carteira/obter-saldo-completo))
    (catch Exception e
      (resp/status 500 {:erro (.getMessage e)}))))

(defn consultar-acao-handler [ticker]
  (try
    (resp/response (i-acoes/buscar-dados-acao (s/upper-case ticker)))
    (catch Exception e
      (resp/status 500 {:erro (.getMessage e)}))))

(defn registrar-compra-handler [request]
  (try
    (println "DEBUG - Request recebido:" request)
    (println "DEBUG - Body:" (:body request))

    (let [dados (:body request)]
      (when (nil? dados)
        (throw (ex-info "Body vazio" {:status 400})))

      (println "DEBUG - Dados parseados:" dados)

      (let [resultado (s-trans/registrar-compra dados)]
        (println "DEBUG - Resultado:" resultado)

        ;; CORREÇÃO: Primeiro cria a response, depois define o status
        (-> (resp/response (update resultado :data str))
            (resp/status 201))))

    (catch clojure.lang.ExceptionInfo e
      (println "DEBUG - Erro ExceptionInfo:" (.getMessage e))
      (-> (resp/response {:erro (.getMessage e)})
          (resp/status 400)))

    (catch Exception e
      (println "DEBUG - Erro geral:" (.getMessage e))
      (-> (resp/response {:erro (.getMessage e)})
          (resp/status 500)))))

(defn registrar-venda-handler [request]
  (try
    (println "DEBUG - Request venda recebido:" request)
    (println "DEBUG - Body venda:" (:body request))

    (let [dados (:body request)]
      (when (nil? dados)
        (throw (ex-info "Body vazio" {:status 400})))

      (let [resultado (s-trans/registrar-venda dados)]
        ;; CORREÇÃO: Primeiro cria a response, depois define o status
        (-> (resp/response (update resultado :data str))
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
      (println "DEBUG - Erro venda:" (.getMessage e))
      (-> (resp/response {:erro (.getMessage e)})
          (resp/status 500)))))


(defn registrar-venda-handler [request]
  (try
    (let [dados (:body request)]
      (when (nil? dados)
        (throw (ex-info "Body vazio" {:status 400})))

      (let [resultado (s-trans/registrar-venda dados)]
        ;; Simplifica a resposta para evitar problemas de serialização
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

;; Rotas
(defroutes app-routes
  (GET "/" [] "API Carteira de Investimentos - Funcionando! 🚀")

  (GET "/api/carteira/saldo" [] (obter-saldo-handler))

  (GET "/api/acoes/:ticker" [ticker] (consultar-acao-handler ticker))

  (POST "/api/transacoes/compra" request (registrar-compra-handler request))

  (POST "/api/transacoes/venda" request (registrar-venda-handler request))

  (route/not-found "Rota não encontrada"))

;; App com middleware para API (sem anti-forgery)
(def app
  (-> app-routes
      (wrap-json-response)
      (wrap-json-body {:keywords? true})
      (wrap-defaults api-defaults))) ; api-defaults não tem anti-forgery