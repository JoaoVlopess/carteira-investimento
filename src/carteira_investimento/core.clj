(ns carteira-investimento.core
  (:require [clj-http.client :as http]
            [cheshire.core :as json])
  (:gen-class))

(def api-local-url "http://localhost:3000")

(defn buscar-dados-acao [ticker]
  (let [url (str api-local-url "/api/acoes/" ticker)
        response (http/get url)
        dados (json/parse-string (:body response) true)]
    dados))

(defn registrar-compra [ticker quantidade]
  (let [url (str api-local-url "/api/transacoes/compra")
        dados-json (json/generate-string {:ticker ticker :quantidade quantidade})
        response (http/post url {:body dados-json
                                 :content-type "application/json"})
        resultado (json/parse-string (:body response) true)]
    resultado))

(defn registrar-venda [ticker quantidade]
  (let [url (str api-local-url "/api/transacoes/venda")
        dados-json (json/generate-string {:ticker ticker :quantidade quantidade})
        response (http/post url {:body dados-json
                                 :content-type "application/json"})
        resultado (json/parse-string (:body response) true)]
    resultado))

(defn obter-extrato []
  (let [url (str api-local-url "/api/transacoes/extrato")
        response (http/get url)
        dados (json/parse-string (:body response) true)]
    (:extrato dados)))

(defn obter-saldo []
  (let [url (str api-local-url "/api/carteira/saldo")
        response (http/get url)
        dados (json/parse-string (:body response) true)]
    dados))

(defn obter-acoes-populares []
  (let [url (str api-local-url "/api/acoes/populares")
        response (http/get url)
        dados (json/parse-string (:body response) true)]
    (:acoes-populares dados)))

(defn exibir-menu []
  (println "\n=== CARTEIRA DE INVESTIMENTOS ===")
  (println "1. Consultar acao")
  (println "2. Comprar acao")
  (println "3. Vender acao")
  (println "4. Ver extrato")
  (println "5. Ver saldo")
  (println "6. Acoes populares")
  (println "0. Sair")
  (print "Opcao: "))

(defn processar-opcao [opcao]
  (cond
    (= opcao "1") (do
                    (print "Ticker: ")
                    (flush)
                    (let [ticker (.toUpperCase (read-line))
                          dados (buscar-dados-acao ticker)]
                      (println "Ticker:" (:ticker dados))
                      (println "Nome:" (:nome dados))
                      (println "Preco: R$" (:preco-atual dados)))
                    true)

    (= opcao "2") (do
                    (print "Ticker: ")
                    (flush)
                    (let [ticker (.toUpperCase (read-line))]
                      (print "Quantidade: ")
                      (flush)
                      (let [quantidade (Double/parseDouble (read-line))
                            resultado (registrar-compra ticker quantidade)]
                        (println "Compra realizada!")
                        (println "Valor total: R$" (:valor-total resultado))))
                    true)

   (= opcao "3") (do
                   (print "Ticker: ")
                   (flush)
                   (let [ticker (.toUpperCase (read-line))]
                     (print "Quantidade: ")
                     (flush)
                     (let [quantidade (Double/parseDouble (read-line))
                           resultado (registrar-venda ticker quantidade)]
                       (println "Venda realizada!")
                       (if-let [valor (:valor-total resultado)]
                         (println "Valor total: R$" (format "%.2f" valor))
                         (println "Venda processada - valor calculado internamente"))))
                   true)

    (= opcao "4") (do
                    (let [extrato (obter-extrato)]
                      (println "\n=== EXTRATO ===")
                      (doseq [t extrato]
                        (println (:data t) "|" (:tipo t) "|" (:ticker t)
                                 "|" (:quantidade t) "| R$" (:valor-total t))))
                    true)

    (= opcao "5") (do
                    (let [saldo (obter-saldo)]
                      (println "\n=== SALDO ===")
                      (println "Total Investido: R$" (format "%.2f" (:total-investido saldo)))
                      (println "Valor Mercado: R$" (format "%.2f" (:total-mercado saldo)))
                      (println "Lucro/Prejuizo: R$" (format "%.2f" (:total-lucro-prejuizo saldo))))
                    true)

    (= opcao "6") (do
                    (let [acoes (obter-acoes-populares)]
                      (println "\n=== ACOES POPULARES ===")
                      (doseq [acao acoes]
                        (when (= (:status acao) "success")
                          (println (:ticker acao) "-" (:nome acao) "- R$" (:preco-atual acao)))))
                    true)

    (= opcao "0") false
    :else (do (println "Opcao invalida!") true)))

(defn executar-menu
  ([] (executar-menu true))
  ([continuar]
   (when continuar
     (exibir-menu)
     (flush)
     (let [opcao (read-line)]
       (recur (processar-opcao opcao))))))

(defn -main [& _]
  (println "Carteira de Investimentos - Cliente API")
  (println "API rodando em:" api-local-url)
  (executar-menu))