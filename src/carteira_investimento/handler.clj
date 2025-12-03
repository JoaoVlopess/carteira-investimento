(ns carteira-investimento.handler
  (:require [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.util.response :as resp]
            [carteira-investimento.servicos.carteira :as s-carteira]
            [carteira-investimento.servicos.transacoes :as s-trans]
            [carteira-investimento.integracao.acoes :as i-acoes]
            [clojure.string :as s]
            [ring.middleware.cors :refer [wrap-cors]]
            [cheshire.core :as json])) ; Adicionar para parsing JSON manual

(defn obter-extrato-handler
  "Retorna extrato de transações com filtros opcionais"
  [request]
  (try
    (let [params (:params request)
          data-inicio (get params "data_inicio")
          data-fim (get params "data_fim")
          ticker (get params "ticker")]

      (cond
        ;; Extrato por período e ticker específico
        (and data-inicio data-fim ticker)
        (let [inicio (java.time.LocalDate/parse data-inicio)
              fim (java.time.LocalDate/parse data-fim)
              extrato (s-trans/obter-extrato-por-periodo inicio fim ticker)]
          (resp/response {:extrato extrato
                          :periodo {:inicio data-inicio :fim data-fim}
                          :ticker ticker
                          :total-transacoes (count extrato)}))

        ;; Extrato por período
        (and data-inicio data-fim)
        (let [inicio (java.time.LocalDate/parse data-inicio)
              fim (java.time.LocalDate/parse data-fim)
              extrato (s-trans/obter-extrato-por-periodo inicio fim)]
          (resp/response {:extrato extrato
                          :periodo {:inicio data-inicio :fim data-fim}
                          :total-transacoes (count extrato)}))

        ;; Extrato completo
        :else
        (let [extrato (s-trans/obter-extrato-completo)
              extrato-json (map #(update % :data str) extrato)] ;;precisa transformar em string para reconhecer como json
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

          acoes-com-dados (reduce (fn [acc ticker] ;; pega as informações sobre cada ação e acumula
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
                                      (conj acc dados-acao))) ; Adiciona os dados da ação ao acumulador
                                  [] ; Acumulador inicial 
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
  [ticker & [data]]
  (try
    (if data
      (resp/response (i-acoes/buscar-dados-acao (s/upper-case ticker) data))
      (resp/response (i-acoes/buscar-dados-acao (s/upper-case ticker))))
    (catch Exception e
      (-> (resp/response {:erro (.getMessage e)})
          (resp/status 500)))))

(defn registrar-compra-handler
  "Registra uma transação de compra"
  [request]
  (try
    (println "=== DEBUG COMPRA HANDLER ===")
    (println "Request recebido - keys:" (keys request))
    (println "Body type:" (type (:body request)))
    (println "Body content:" (pr-str (:body request)))

    (let [dados (:body request)]
      (println "Dados extraídos:" (pr-str dados))

      (when (nil? dados)
        (println "ERRO: Body está vazio!")
        (throw (ex-info "Body vazio" {:status 400})))

      (println "Chamando s-trans/registrar-compra...")
      (let [resultado (s-trans/registrar-compra dados)]
        (println "Resultado obtido:" (pr-str resultado))

        (-> (resp/response {:status "success"
                            :message "Compra realizada com sucesso"
                            :ticker (:ticker resultado)
                            :quantidade (:quantidade resultado)
                            :valor-total (:valor-liquido resultado)})
            (resp/status 201))))

    (catch clojure.lang.ExceptionInfo e
      (println "ExceptionInfo capturada:")
      (println "  Mensagem:" (.getMessage e))
      (println "  Dados:" (pr-str (.getData e)))
      (.printStackTrace e)
      (-> (resp/response {:erro (.getMessage e)})
          (resp/status 400)))

    (catch Exception e
      (println "Exception geral capturada:")
      (println "  Tipo:" (type e))
      (println "  Mensagem:" (.getMessage e))
      (.printStackTrace e)
      (-> (resp/response {:erro (.getMessage e)})
          (resp/status 500)))))

(defn registrar-venda-handler
  "Registra uma transação de venda"
  [request]
  (try
    (println "=== DEBUG VENDA HANDLER ===")
    (println "Request recebido - keys:" (keys request))
    (println "Body content:" (pr-str (:body request)))

    (let [dados (:body request)]
      (println "Dados extraídos:" (pr-str dados))

      (when (nil? dados)
        (println "ERRO: Body está vazio!")
        (throw (ex-info "Body vazio" {:status 400})))

      (println "Chamando s-trans/registrar-venda...")
      (let [resultado (s-trans/registrar-venda dados)]
        (println "Resultado obtido:" (pr-str resultado))

        (-> (resp/response {:status "success"
                            :message "Venda realizada com sucesso"
                            :ticker (:ticker resultado)
                            :quantidade (:quantidade resultado)
                            :valor-liquido (:valor-liquido resultado)
                            :data (str (:data dados))})
            (resp/status 201))))

    (catch clojure.lang.ExceptionInfo e
      (println "ExceptionInfo capturada:")
      (println "  Mensagem:" (.getMessage e))
      (println "  Dados:" (pr-str (.getData e)))
      (.printStackTrace e)
      (-> (resp/response {:erro (.getMessage e)
                          :detalhes (.getData e)})
          (resp/status 400)))

    (catch Exception e
      (println "Exception geral capturada:")
      (println "  Tipo:" (type e))
      (println "  Mensagem:" (.getMessage e))
      (.printStackTrace e)
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

  ;; Endpoints de ações - CORRIGIDO PARA SUPORTAR DATA
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
      (println "=== DEBUG ROTA COMPRA ===")
      (println "Request chegou na rota!")
  
      ;; CORREÇÃO: O body já foi processado pelo wrap-json-body
      (let [dados-json (:body request)]  ; ← NÃO usar slurp aqui!
        (println "JSON já processado:" (pr-str dados-json))
  
        ;; Processa data (usa hoje se nao fornecida)
        (let [data-param (:data dados-json)
              data-processada (if (and data-param (not (s/blank? data-param)))
                                (java.time.LocalDate/parse data-param)
                                (java.time.LocalDate/now))]
  
          (println "Data processada:" data-processada)
  
          ;; Validacao de data futura
          (when (.isAfter data-processada (java.time.LocalDate/now))
            (throw (ex-info "Data nao pode ser futura" {:data data-processada})))
  
          ;; Dados completos para o handler
          (let [dados-completos (assoc dados-json :data data-processada
                                       :ticker (s/upper-case (:ticker dados-json))
                                       :quantidade (double (:quantidade dados-json)))]
  
            (println "Dados completos:" (pr-str dados-completos))
  
            ;; Cria request modificado
            (let [request-modificado (assoc request :body dados-completos)]
              (println "Chamando handler...")
              (registrar-compra-handler request-modificado)))))
  
      (catch java.time.format.DateTimeParseException e
        (println "Erro de parsing de data:" (.getMessage e))
        (-> (resp/response {:erro "Formato de data invalido. Use YYYY-MM-DD"})
            (resp/status 400)))
  
      (catch NumberFormatException e
        (println "Erro de parsing de número:" (.getMessage e))
        (-> (resp/response {:erro "Quantidade deve ser um numero valido"})
            (resp/status 400)))
  
      (catch clojure.lang.ExceptionInfo e
        (println "ExceptionInfo na rota:" (.getMessage e))
        (-> (resp/response {:erro (.getMessage e)})
            (resp/status 400)))
  
      (catch Exception e
        (println "Exception geral na rota:")
        (println "  Tipo:" (type e))
        (println "  Mensagem:" (.getMessage e))
        (.printStackTrace e)
        (-> (resp/response {:erro "Erro ao processar requisicao"})
            (resp/status 500)))))

  (POST "/api/transacoes/venda" request
    (try
      ;; CORREÇÃO: Usar (:body request) diretamente
      (let [dados-json (:body request)]  ; ← NÃO usar slurp aqui!
  
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

  (GET "/api/transacoes/extrato" request
    (obter-extrato-handler request))

  ;; Rota 404
  (route/not-found
   (resp/response {:erro "Rota não encontrada"
                   :status 404})))

(def app
  "Aplicação principal com middleware configurado para API REST"
  (-> app-routes ;;base das rotas
      (wrap-cors :access-control-allow-origin [#"http://localhost:3000"  ; porta do frontend
                                               #"http://127\.0\.0\.1:3000"] ; alternativa localhost
                 :access-control-allow-methods [:get :post :put :delete :options]
                 :access-control-allow-headers ["Content-Type" "Authorization"])
      (wrap-json-response) ;; transforma as respostas para JSON
      (wrap-json-body {:keywords? true}) ;; transforma as entradas para o Clojure
      ));; Adiciona vários middlewares úteis