(ns carteira-investimento.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clj-http.client :as http-client]
            [carteira-investimento.servicos.transacoes :as s-trans]
            [carteira-investimento.servicos.carteira :as s-carteira]
            [carteira-investimento.integracao.acoes :as i-acoes])
  (:gen-class))

(defn menu []
  (println "\n--- MENU ---")
  (println "1. Consultar dados de uma AÇÃO (API Externa)")
  (println "2. Registrar COMPRA de ações")
  (println "3. Registrar VENDA de ações")
  (println "4. Exibir EXTRATO de transações")
  (println "5. Exibir SALDO da carteira")
  (println "0. Sair")

  (let [servico (try (read) (catch Exception _ nil))]
    (cond
      ;; -------------------------------------------
      (= servico 2) (do
                      (println "\n--- Registrar Compra ---")
                      (try
                        (let [compra-exemplo {:ticker "PETR4"
                                              :quantidade 100
                                              :preco_unitario 35.00
                                              :taxas 2.50
                                              :moeda "BRL"}
                              resultado (s-trans/registrar-compra compra-exemplo)]
                          (println "✅ COMPRA REGISTRADA:")
                          (println resultado)
                          (println "\nEstado da Carteira ATUALIZADO."))
                        (catch Exception e (println (str "❌ Erro: " (.getMessage e)))))
                      (recur))

      ;; -------------------------------------------
      (= servico 3) (do
                      (println "\n--- Registrar Venda ---")
                      (print "Digite o Ticker a vender (Ex: PETR4): ")
                      (let [venda-ticker (clojure.string/upper-case (read-line))
                            venda-quantidade 50
                            venda-preco 32.00
                            venda-taxas 1.50]
                        (try
                          (let [venda-exemplo {:ticker venda-ticker
                                               :quantidade venda-quantidade
                                               :preco_unitario venda-preco
                                               :taxas venda-taxas
                                               :moeda "BRL"}
                                resultado (s-trans/registrar-venda venda-exemplo)]
                            (println "✅ VENDA REGISTRADA COM SUCESSO:")
                            (println resultado)
                            (println "\nEstado da Carteira ATUALIZADO."))
                          (catch clojure.lang.ExceptionInfo e
                            (println (str "⚠️ Erro de Negócio (Status " (:status (.getData e)) "):"))
                            (println (.getMessage e)))
                          (catch Exception e (println (str "❌ Erro: " (.getMessage e)))))
                        (recur)))
      
      ;; -------------------------------------------

      
      (= servico 4) (do
                      (println "\n--- Exibir Extrato ---")
                      (print "Filtro de Ticker (Vazio para Todos): ")
                      (let [ticker-filtro (read-line)
                            data-fim (java.time.LocalDate/now)
                            data-inicio (.minusDays data-fim 365) ; Último ano
                            extrato (if (clojure.string/blank? ticker-filtro)
                                      (s-trans/obter-extrato-por-periodo data-inicio data-fim)
                                      (s-trans/obter-extrato-por-periodo data-inicio data-fim (clojure.string/upper-case ticker-filtro)))]

                        (println (str "\nEXTRATO DE TRANSAÇÕES (" data-inicio " a " data-fim "):"))
                        (if (empty? extrato)
                          (println "Nenhuma transação encontrada no período.")
                          (doseq [t extrato]
                            (println (str " - ID: " (:id-transacao t)
                                          " | Tipo: " (:tipo t)
                                          " | Ticker: " (:ticker t)
                                          " | Qtd: " (:quantidade t)
                                          " | Valor Líquido: " (:valor-liquido t))))))
                      (recur))


      ;; -------------------------------------------
      (= servico 5) (do
                      (println "\n--- Saldo Atual ---")
                      (try
                        (let [saldo (s-carteira/obter-saldo-completo)]
                          (println "💰 SALDO TOTAL (Mercado):" (:total-mercado saldo))
                          (println "💵 VALOR INVESTIDO (Custo):" (:total-investido saldo))
                          (println "📊 LUCRO/PREJUÍZO:" (:total-lucro-prejuizo saldo))
                          (println "\nPOSIÇÕES DETALHADAS:")
                          (doseq [pos (:posicoes saldo)]
                            (println (str "   " (:ticker pos) " | QNT: " (:quantidade pos)
                                          " | PM: " (:preco-medio pos) " | Atual: " (:preco-atual pos)))))
                        (catch Exception e (println (str "❌ Erro: " (.getMessage e)))))
                      (recur))

      ;; -------------------------------------------
(= servico 1) (do
                (print "Digite o Ticker (ex: PETR4): ")
                (let [ticker-input (clojure.string/upper-case (read-line))] ; Lê e coloca em maiúsculas
                  (if (clojure.string/blank? ticker-input) ; <-- VERIFICA SE ESTÁ VAZIO
                    (println "❌ Erro: O Ticker não pode ser vazio.")
                    (do
                      (println "\n--- Dados de Mercado ---")
                      (try
                        (let [dados (i-acoes/buscar-dados-acao ticker-input)]
                          (println "✅ Dados Encontrados:")
                          (clojure.pprint/pprint dados))
                        (catch Exception e (println (str "❌ Erro ao consultar API: " (.getMessage e))))))))
                (recur))

      (= servico 0) (println "Saindo...")

      :else (do (println "Opção inválida.") (recur)))))


(defn -main
  [& args]
  ;; (System/setProperty "STOCK_API_TOKEN" "SEU_TOKEN_AQUI_PARA_TESTE") ; Opcional: define o token
  (menu))