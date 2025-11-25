(defproject carteira-investimento "0.1.0-SNAPSHOT"
  :description "API de Gerenciamento de Carteira de Investimentos."
  :url "https://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies [[org.clojure/clojure "1.12.2"]
                 [org.clojure/tools.cli "0.4.1"]

                 ;; Dependências Web/HTTP
                 [compojure "1.6.2"]         ; Roteador
                 [ring/ring-core "1.12.0"]   ; Padrão Ring
                 [ring/ring-json "0.5.5"]    ; Para parsing e geração de JSON
                 [ring/ring-defaults "0.4.0"] ; Middleware (para site-defaults)
                 [clj-http "3.12.3"]         ; Cliente HTTP para APIs externas
                 [org.clojure/data.json "2.4.0"] ; Para parsing (se necessário)
                 ]

  ;; Configuração de Execução
  :main ^:skip-aot carteira-investimento.core

  ;; Configuração do Servidor Web (para rodar a API via lein ring server-devel)
  :plugins [[lein-ring "0.12.6"]]
  :ring {:handler carteira-investimento-api.handler/app}

  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})