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
              extrato-json (map #(update % :data str) extrato) ] ;;precisa transformar em string para reconhecer como json
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
  "Consulta dados de uma ação na API externa"
  [ticker]
  (try
    (resp/response (i-acoes/buscar-dados-acao (s/upper-case ticker))) ;; transformo o nome do ticker em maiusculo para evitar erros
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
  (GET "/api/acoes/:ticker" [ticker]
    (consultar-acao-handler ticker))

  ;; Endpoints de transações
  (POST "/api/transacoes/compra" request
    (registrar-compra-handler request))

  (POST "/api/transacoes/venda" request
    (registrar-venda-handler request))
  
    (GET "/api/transacoes/extrato" request     
    (obter-extrato-handler request))

  ;; Rota 404
  (route/not-found
   (resp/response {:erro "Rota não encontrada"
                   :status 404})))

;; ========================================
;; APLICAÇÃO COM MIDDLEWARE
;; ========================================

(def app
  "Aplicação principal com middleware configurado para API REST"
  (-> app-routes ;;base das rotas
      (wrap-json-response) ;; transforma as respostas para JSON
      (wrap-json-body {:keywords? true}) ;; transforma as entradas para o Clojure
      )) ;; Adiciona vários middlewares úteis 