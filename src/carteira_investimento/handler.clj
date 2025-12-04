(ns carteira-investimento.handler
  (:require [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]] 
            [ring.util.response :as resp]
            [carteira-investimento.servicos.carteira :as s-carteira]
            [carteira-investimento.servicos.transacoes :as s-trans]
            [carteira-investimento.integracao.acoes :as i-acoes]
            [clojure.string :as s]
            [ring.middleware.cors :refer [wrap-cors]]
            [cheshire.core :as json]))

(defn obter-extrato-handler
  "Retorna extrato de transações com filtros opcionais"
  [request]
  (try
    (let [query-params (:query-params request)
          data-inicio (get query-params "data-inicio")
          data-fim (get query-params "data-fim")
          ticker (get query-params "ticker")]

      (cond
        ;; Extrato por período e ticker específico
        (and data-inicio data-fim ticker)
        (let [inicio (java.time.LocalDate/parse data-inicio)
              fim (java.time.LocalDate/parse data-fim)
              extrato (s-trans/obter-extrato-por-periodo inicio fim ticker)
              extrato-json (map #(update % :data str) extrato)]
          (resp/response {:extrato extrato-json
                          :periodo {:inicio data-inicio :fim data-fim}
                          :ticker ticker
                          :total-transacoes (count extrato-json)}))

        ;; Extrato por período (todas as ações)
        (and data-inicio data-fim)
        (let [inicio (java.time.LocalDate/parse data-inicio)
              fim (java.time.LocalDate/parse data-fim)
              extrato (s-trans/obter-extrato-por-periodo inicio fim)
              extrato-json (map #(update % :data str) extrato)]
          (resp/response {:extrato extrato-json
                          :periodo {:inicio data-inicio :fim data-fim}
                          :total-transacoes (count extrato-json)}))

        ;; Extrato completo
        :else
        (let [extrato (s-trans/obter-extrato-completo)
              extrato-json (map #(update % :data str) extrato)]
          (resp/response {:extrato extrato-json
                          :total-transacoes (count extrato-json)}))))

    (catch Exception e
      (-> (resp/response {:erro (.getMessage e)})
          (resp/status 500)))))

(defn obter-acoes-populares-handler
  "Retorna lista de ações mais populares COM DADOS REAIS"
  []
  (try
    (let [tickers-populares ["PETR4" "VALE3" "ITUB4"
                             "WEGE3" "MGLU3" "RENT3" "ELET3"]

          acoes-com-dados (reduce (fn [acc ticker]
                                    (let [dados-acao (try
                                                       (let [dados (i-acoes/buscar-dados-acao ticker)]
                                                         (assoc dados :status "success"))
                                                       (catch Exception e
                                                         {:ticker ticker
                                                          :nome "Erro ao buscar"
                                                          :preco-atual 0.0
                                                          :moeda "BRL"
                                                          :status "error"
                                                          :erro (.getMessage e)}))]
                                      (conj acc dados-acao)))
                                  []
                                  tickers-populares)]

      (resp/response {:acoes-populares acoes-com-dados
                      :total (count acoes-com-dados)
                      :descricao "Ações mais negociadas na B3 com dados atuais"
                      :timestamp (str (java.time.LocalDateTime/now))}))
    (catch Exception e
      (-> (resp/response {:erro (.getMessage e)})
          (resp/status 500)))))

(defn obter-saldo-handler
  "Retorna o saldo completo da carteira"
  []
  (try
    (resp/response (s-carteira/obter-saldo-completo))
    (catch Exception e
      (-> (resp/response {:erro (.getMessage e)})
          (resp/status 500)))))

(defn consultar-acao-handler
  "Consulta dados de uma acao na API externa com suporte a data"
  ([ticker]
   (consultar-acao-handler ticker nil))

  ([ticker data]
   (try
     (if data
       (resp/response (i-acoes/buscar-dados-acao (s/upper-case ticker) data))
       (resp/response (i-acoes/buscar-dados-acao (s/upper-case ticker))))
     (catch Exception e
       (-> (resp/response {:erro (.getMessage e)})
           (resp/status 500))))))

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
  "Registra uma transação de venda com validação histórica"
  [request]
  (try
    (let [dados (:body request)]

      (when (nil? dados)
        (throw (ex-info "Body vazio" {:status 400})))

      ;;  Verificar estoque ANTES de processar
      (let [ticker (:ticker dados)
            quantidade (:quantidade dados)
            data (:data dados)
            quantidade-disponivel (s-trans/obter-quantidade-disponivel ticker data)]

        (when (< quantidade-disponivel quantidade)
          (throw (ex-info (str "Quantidade insuficiente para venda. Disponível: "
                               quantidade-disponivel ", Solicitado: " quantidade)
                          {:ticker ticker
                           :quantidade-solicitada quantidade
                           :quantidade-disponivel quantidade-disponivel
                           :data data})))

        (let [resultado (s-trans/registrar-venda dados)]

          (-> (resp/response {:status "success"
                              :message "Venda realizada com sucesso"
                              :ticker (:ticker resultado)
                              :quantidade (:quantidade resultado)
                              :valor-liquido (:valor-liquido resultado)
                              :data (str (:data dados))})
              (resp/status 201)))))

    (catch clojure.lang.ExceptionInfo e
      (-> (resp/response {:erro (.getMessage e)
                          :detalhes (.getData e)})
          (resp/status 400)))

    (catch Exception e
      (-> (resp/response {:erro (.getMessage e)})
          (resp/status 500)))))

(defroutes app-routes
  ;; Rota raiz
  (GET "/" []
    (resp/response {:message "API Carteira de Investimentos"
                    :status "Funcionando!"
                    :version "1.0.0"}))

  ;; Endpoints da carteira
  (GET "/api/carteira/saldo" []
    (obter-saldo-handler))

  (GET "/api/acoes/populares" []
    (obter-acoes-populares-handler))

  ;; Endpoints de ações 
  (GET "/api/acoes/:ticker" [ticker :as request]
    (try
      (let [data-param (get-in request [:params "data"])
            data (when (and data-param (not (s/blank? data-param)))
                   (java.time.LocalDate/parse data-param))]

        ;; Validacao de data futura
        (when (and data (.isAfter data (java.time.LocalDate/now)))
          (throw (ex-info "Data nao pode ser futura" {:data-fornecida data})))

        (consultar-acao-handler ticker data))

      (catch java.time.format.DateTimeParseException _
        (-> (resp/response {:erro "Formato de data invalido. Use YYYY-MM-DD"})
            (resp/status 400)))

      (catch clojure.lang.ExceptionInfo e
        (-> (resp/response {:erro (.getMessage e)})
            (resp/status 400)))

      (catch Exception e
        (-> (resp/response {:erro (.getMessage e)})
            (resp/status 500)))))

  ;; Endpoints de transações - CORRIGIDOS PARA SUPORTAR DATA
  (POST "/api/transacoes/compra" request
    (try
      (let [dados-json (:body request)]

        ;; Processa data (usa hoje se nao fornecida)
        (let [data-param (:data dados-json)
              data-processada (if (and data-param (not (s/blank? data-param)))
                                (java.time.LocalDate/parse data-param)
                                (java.time.LocalDate/now))]

          ;; Validacao de data futura
          (when (.isAfter data-processada (java.time.LocalDate/now))
            (throw (ex-info "Data nao pode ser futura" {:data data-processada})))

          ;; Dados completos para o handler
          (let [dados-completos (assoc dados-json :data data-processada
                                       :ticker (s/upper-case (:ticker dados-json))
                                       :quantidade (double (:quantidade dados-json)))]

            ;; Cria request modificado
            (let [request-modificado (assoc request :body dados-completos)]
              (registrar-compra-handler request-modificado)))))

      (catch java.time.format.DateTimeParseException e
        (-> (resp/response {:erro "Formato de data invalido. Use YYYY-MM-DD"})
            (resp/status 400)))

      (catch NumberFormatException e
        (-> (resp/response {:erro "Quantidade deve ser um numero valido"})
            (resp/status 400)))

      (catch clojure.lang.ExceptionInfo e
        (-> (resp/response {:erro (.getMessage e)})
            (resp/status 400)))

      (catch Exception e
        (-> (resp/response {:erro "Erro ao processar requisicao"})
            (resp/status 500)))))

  (POST "/api/transacoes/venda" request
    (try
      (let [dados-json (:body request)]

        ;; Processa data (usa hoje se nao fornecida)
        (let [data-param (:data dados-json)
              data-processada (if (and data-param (not (s/blank? data-param)))
                                (java.time.LocalDate/parse data-param)
                                (java.time.LocalDate/now))]

          ;; Validacao de data futura
          (when (.isAfter data-processada (java.time.LocalDate/now))
            (throw (ex-info "Data nao pode ser futura" {:data data-processada})))

          ;; Dados completos para o handler
          (let [dados-completos (assoc dados-json :data data-processada
                                       :ticker (s/upper-case (:ticker dados-json))
                                       :quantidade (double (:quantidade dados-json)))]

            ;; Cria request modificado
            (let [request-modificado (assoc request :body dados-completos)]
              (registrar-venda-handler request-modificado)))))

      (catch java.time.format.DateTimeParseException e
        (-> (resp/response {:erro "Formato de data invalido. Use YYYY-MM-DD"})
            (resp/status 400)))

      (catch NumberFormatException e
        (-> (resp/response {:erro "Quantidade deve ser um numero valido"})
            (resp/status 400)))

      (catch clojure.lang.ExceptionInfo e
        (-> (resp/response {:erro (.getMessage e)})
            (resp/status 400)))

      (catch Exception e
        (-> (resp/response {:erro "Erro ao processar requisicao"})
            (resp/status 500)))))

  ;; ENDPOINT DE EXTRATO - SUPORTA FILTROS
  (GET "/api/transacoes/extrato" request
    (obter-extrato-handler request))

  ;; Rota 404
  (route/not-found
   (resp/response {:erro "Rota não encontrada"
                   :status 404})))

(def app
  "Aplicação principal com middleware configurado para API REST"
  (-> app-routes
      (wrap-cors :access-control-allow-origin [#"http://localhost:3000"
                                               #"http://127\.0\.0\.1:3000"]
                 :access-control-allow-methods [:get :post :put :delete :options]
                 :access-control-allow-headers ["Content-Type" "Authorization"])
      (wrap-json-response)
      (wrap-json-body {:keywords? true})
      (ring.middleware.params/wrap-params)      
      (ring.middleware.keyword-params/wrap-keyword-params))) 