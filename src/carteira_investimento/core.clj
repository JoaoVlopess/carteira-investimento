(ns carteira-investimento.core ;;namespace dono 
  (:require	[clojure.tools.cli	:refer	[parse-opts]];;importando função de tools.cli
            [clj-http.client	:as	http-client])
  (:gen-class)) ;;gera as classes em java para a JVM

(defn menu []
  (println "Oque deseja fazer?")
  (println "1. Consultar os dados de uma ação")
  (println "2. Registrar compra de ações ")
  (println "3. Registrar venda de ações ")
  (println "4. Exibir extrato de transações por período")
  (println "5. Exibir saldo da carteira")

  (let [
        servico (read)
  ]
  (cond
    (= servico 1) (do () (recur))
    (= servico 2) (do () (recur))
    (= servico 3) (do () (recur))
    (= servico 4) (do () (recur))
    (= servico 5) (do () (recur))
    ))
  )

(defn -main ;;- significa função estatica
  [& args] ;;pega todos os arumentos passados e empacota em um unico
  (menu) 
  )
