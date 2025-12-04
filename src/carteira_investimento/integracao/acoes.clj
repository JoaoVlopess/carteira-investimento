(ns carteira-investimento.integracao.acoes
  (:require [clj-http.client :as http]
            [clojure.set :as set]
            [clojure.data.json :as json])
  (:import [java.time LocalDate]))

(def api-token "5N4ZTjFauu9QG5R4TW1sVk")

;; Mapeamento para converter chaves da API BRAPI para nosso padrão
(def ^:private chave-map-brapi
  {:symbol                 :ticker
   :longName               :nome
   :regularMarketPrice     :preco-atual
   :currency               :moeda})

;; Mapeamento para dados históricos 
(def ^:private chave-map-historico
  {:open                   :preco-abertura
   :high                   :preco-maximo
   :low                    :preco-minimo
   :close                  :preco-fechamento    ; preço usado
   :volume                 :volume})

(defn normalizar-dados-api
  "Converte dados da API BRAPI para nosso formato padrão"
  [dados-brutos]
  (let [dados-mapa (if (string? dados-brutos)
                     (json/read-str dados-brutos :key-fn keyword)
                     dados-brutos)
        dados-acao (get-in dados-mapa [:results 0])]

    (if dados-acao
      (let [dados-renomeados (set/rename-keys dados-acao chave-map-brapi)
            chaves-necessarias [:ticker :nome :preco-atual :moeda]]
        (select-keys dados-renomeados chaves-necessarias))
      ;; Fallback em caso de erro
      {:ticker "ERRO", :nome "Sem dados", :preco-atual 0.0, :moeda "BRL"})))

(defn normalizar-dados-historicos
  "Processa dados históricos e extrai o preço de fechamento"
  [dados-brutos ticker]
  (let [dados-mapa (if (string? dados-brutos)
                     (json/read-str dados-brutos :key-fn keyword)
                     dados-brutos)
        resultado (get-in dados-mapa [:results 0])
        historico (get resultado :historicalDataPrice)
        nome (get resultado :longName)
        moeda (get resultado :currency)]

    (if (and historico (seq historico)) ;; seq verifica se ta vazia
      ;;  Pega o primeiro (mais recente) do histórico
      (let [primeiro-dado (first historico)
            preco-historico (:close primeiro-dado)]
        {:ticker ticker
         :nome nome
         :preco-atual preco-historico  ; ← Usa preço de fechamento
         :moeda moeda})

      ;; Fallback se não tem histórico, retorna erro
      {:ticker ticker
       :nome "Dados históricos indisponíveis"
       :preco-atual 0.0
       :moeda "BRL"})))

(defn data-eh-hoje?
  "Verifica se a data fornecida é hoje"
  [data]
  (let [hoje (java.time.LocalDate/now)
        data-fornecida (if (string? data)
                         (java.time.LocalDate/parse data)
                         data)]
    (.equals hoje data-fornecida)))

(defn buscar-dados-atuais
  "Busca dados atuais (preço em tempo real)"
  [ticker]
  (let [url (str "https://brapi.dev/api/quote/" ticker)
        opcoes-http {:headers {"Authorization" (str "Bearer " api-token)}
                     :as :json
                     :throw-exceptions false}
        resposta (http/get url opcoes-http)]

    (if (= 200 (:status resposta))
      (normalizar-dados-api (:body resposta))
      {:ticker ticker, :nome "Erro ao buscar", :preco-atual 0.0, :moeda "BRL"})))

(defn buscar-dados-historicos
  "Busca dados históricos para uma data específica"
  [ticker data]
  (let [data-str (str data)  ; Converte LocalDate para string "2025-12-02"
        ;;  Sempre usa range de 5 dias (simplificação) para garantir dados
        url (str "https://brapi.dev/api/quote/" ticker
                 "?range=5d&interval=1d&fundamental=false"
                 "&start=" data-str "&end=" data-str) ;; formação da url de busca a respeito de uma data especifica
        opcoes-http {:headers {"Authorization" (str "Bearer " api-token)}
                     :as :json
                     :throw-exceptions false}
        resposta (http/get url opcoes-http)]

    (if (= 200 (:status resposta))
      (normalizar-dados-historicos (:body resposta) ticker)
      {:ticker ticker, :nome "Erro ao buscar histórico", :preco-atual 0.0, :moeda "BRL"})))

(defn buscar-dados-acao
  "FUNÇÃO PRINCIPAL: Busca dados de ação com fallback inteligente
   - Se é hoje: busca dados atuais
   - Se é data passada: tenta histórico, se falhar usa atual
   - Sempre retorna algo válido"
  ([ticker]
   (buscar-dados-acao ticker (java.time.LocalDate/now)))

  ([ticker data]
   (try
     (if (data-eh-hoje? data)
       ;;  Data é hoje? busca dados atuais
       (buscar-dados-atuais ticker)

       ;;  Data é passada? tenta histórico com fallback
       (let [dados-historicos (buscar-dados-historicos ticker data)]
         (if (and dados-historicos
                  (> (:preco-atual dados-historicos) 0.0))
           ;; Sucesso: retorna dados históricos
           dados-historicos

           ;; Fallback: se histórico falhou, usa dados atuais
           (do
             (println "⚠️ Aviso: Dados históricos indisponíveis para" data
                      "- usando preço atual")
             (buscar-dados-atuais ticker)))))

     ;; Erro geral
     (catch Exception e
       (println "⚠️ Erro ao buscar dados:" (.getMessage e))
       {:ticker ticker
        :nome "Erro de conexão"
        :preco-atual 0.0
        :moeda "BRL"}))))