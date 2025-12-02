(ns carteira-investimento.integracao.acoes
  (:require [clj-http.client :as http]
            [clojure.set :as set]
            [clojure.data.json :as json])
  (:import [java.time LocalDate]))

(def api-token "5N4ZTjFauu9QG5R4TW1sVk")

(def ^:private chave-map-brapi
  {:symbol                 :ticker
   :longName              :nome
   :regularMarketPrice     :preco-atual
   :currency               :moeda})

(def ^:private chave-map-historico
  {:open                   :preco-abertura
   :high                   :preco-maximo
   :low                    :preco-minimo
   :close                  :preco-fechamento
   :volume                 :volume})

(defn normalizar-dados-api
  "Recebe dados brutos da api e retorna uma padronizacao"
  [dados-brutos]
  (let [dados-mapa (if (string? dados-brutos)
                     (json/read-str dados-brutos :key-fn keyword)
                     dados-brutos)
        dados-acao (get-in dados-mapa [:results 0])]

    (if dados-acao
      (let [dados-renomeados (set/rename-keys dados-acao chave-map-brapi)
            chaves-necessarias [:ticker :nome :preco-atual :moeda]]
        (select-keys dados-renomeados chaves-necessarias))
      {:ticker "ERRO", :nome "Sem dados", :preco-atual 0.0, :moeda "BRL"})))

(defn encontrar-preco-para-data
  "Encontra o preço mais próximo da data desejada no histórico"
  [historico data-desejada]
  (let [timestamp-desejado (.toEpochSecond (.atStartOfDay data-desejada java.time.ZoneOffset/UTC))
        dados-ordenados (sort-by :date historico)

        ;; Encontra o dado mais próximo da data
        dado-mais-proximo (reduce (fn [melhor atual]
                                    (let [diff-melhor (Math/abs (- (:date melhor) timestamp-desejado))
                                          diff-atual (Math/abs (- (:date atual) timestamp-desejado))]
                                      (if (< diff-atual diff-melhor) atual melhor)))
                                  (first dados-ordenados)
                                  dados-ordenados)]
    dado-mais-proximo))

(defn normalizar-dados-historicos
  "Normaliza dados historicos da API"
  [dados-brutos ticker data-desejada]
  (let [dados-mapa (if (string? dados-brutos)
                     (json/read-str dados-brutos :key-fn keyword)
                     dados-brutos)
        resultado (get-in dados-mapa [:results 0])
        historico (get resultado :historicalDataPrice)
        nome (get resultado :shortName)
        moeda (get resultado :currency)

        ;; Encontra o preço para a data específica
        dado-especifico (encontrar-preco-para-data historico data-desejada)
        preco-historico (get dado-especifico :close)]

    {:ticker ticker
     :nome nome
     :preco-atual preco-historico
     :moeda moeda}))


(defn data-eh-hoje? [data]
  "Verifica se a data fornecida eh hoje"
  (let [hoje (java.time.LocalDate/now)
        data-fornecida (if (string? data)
                         (java.time.LocalDate/parse data)
                         data)]
    (.equals hoje data-fornecida)))

(defn data-eh-futura? [data]
  "Verifica se a data eh futura (nao permitida)"
  (let [hoje (java.time.LocalDate/now)
        data-fornecida (if (string? data)
                         (java.time.LocalDate/parse data)
                         data)]
    (.isAfter data-fornecida hoje)))

(defn calcular-range-para-data
  "Calcula o range necessário para obter dados de uma data específica"
  [data-desejada]
  (let [hoje (java.time.LocalDate/now)
        dias-diferenca (.between java.time.temporal.ChronoUnit/DAYS data-desejada hoje)]
    (cond
      (<= dias-diferenca 0) "1d"      ; Hoje
      (<= dias-diferenca 2) "2d"      ; Últimos 2 dias
      (<= dias-diferenca 5) "5d"      ; Últimos 5 dias
      (<= dias-diferenca 7) "7d"      ; Última semana
      (<= dias-diferenca 30) "1mo"    ; Último mês
      (<= dias-diferenca 90) "3mo"    ; Últimos 3 meses
      (<= dias-diferenca 180) "6mo"   ; Últimos 6 meses
      :else "1y")))                   ; Último ano

(defn buscar-dados-acao
  "Busca dados de uma acao - atual ou historica baseada na data"
  ([ticker]
   (buscar-dados-acao ticker (java.time.LocalDate/now)))

  ([ticker data]
   (try
     (if (data-eh-hoje? data)
       ;; Para hoje, usa API simples
       (let [url (str "https://brapi.dev/api/quote/" ticker)
             opcoes-http {:headers {"Authorization" (str "Bearer " api-token)}
                          :as :json
                          :throw-exceptions false}
             resposta (http/get url opcoes-http)]

         (if (= 200 (:status resposta))
           (normalizar-dados-api (:body resposta))
           {:ticker ticker, :nome "Erro ao buscar", :preco-atual 0.0, :moeda "BRL"}))

       ;; Para datas passadas, usa range
       (let [range (calcular-range-para-data data)
             url (str "https://brapi.dev/api/quote/" ticker "?range=" range "&interval=1d")
             opcoes-http {:headers {"Authorization" (str "Bearer " api-token)}
                          :as :json
                          :throw-exceptions false}
             resposta (http/get url opcoes-http)]

         (if (= 200 (:status resposta))
           (normalizar-dados-historicos (:body resposta) ticker data)
           {:ticker ticker, :nome "Erro ao buscar dados historicos", :preco-atual 0.0, :moeda "BRL"})))

     (catch Exception e
       {:ticker ticker, :nome "Erro de conexao", :preco-atual 0.0, :moeda "BRL"}))))