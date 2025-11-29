(defproject carteira-investimento "0.1.0-SNAPSHOT"
  :description "Sistema Completo de Carteira de Investimentos (CLI + API)"
  :url "https://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies [[org.clojure/clojure "1.12.2"]
                 [org.clojure/tools.cli "0.4.1"]

                 ;; Dependências Web/HTTP (para API)
                 [compojure "1.6.2"]
                 [ring/ring-core "1.12.0"]
                 [ring/ring-json "0.5.1"]
                 [ring/ring-defaults "0.4.0"]

                 ;; Dependências compartilhadas
                 [clj-http "3.12.3"]
                 [org.clojure/data.json "2.4.0"]
                 [ring-cors "0.1.13"]]

  ;; Configuração CLI (padrão)
  :main ^:skip-aot carteira-investimento.core

  ;; Configuração API (opcional)
  :plugins [[lein-ring "0.12.6"]]
  :ring {:handler carteira-investimento.handler/app}

  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})