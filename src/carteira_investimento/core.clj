(ns carteira-investimento.core
  (:gen-class))

(defn -main
  "Ponto de entrada principal da aplicação.
   Com a API REST ativa, este arquivo serve apenas como entrada formal."
  [& args]
  (println " Carteira de Investimentos - Sistema Inicializado")
  (println " API REST disponível via servidor web")
  (println " Use 'lein ring server-headless' para iniciar a API"))