;;Lógica de saldo, rentabilidade. Cálculos e relatórios
(ns carteira-investimento.servicos.carteira
  (:require [carteira-investimento.dados.estado :as estado]
            [carteira-investimento.integracao.acoes :as acoes]
            ;; Importações Opcionais de Clojure Core (se necessário)
))

(defn calcula-preco-medio
  "custo real que cliente teve para adquirir as ações que ainda possui. Envolvendo custo total e quantidade liquida remanescente após operações
  Recebe um array de transações"
  [transacoes]
  (let [;; Inicializa o redutor com quantidade e custo zerados
         estado-inicial {:quantidade 0.0, :custo 0.0}
  
         ;; Reduz a lista de transações para calcular o estado final (custo e quantidade)
         estado-final (reduce
                       (fn [acumulador transacao]
                         (let [tipo (:tipo transacao)]
                           (if (= tipo :COMPRA)
                             ;; Lógica da Compra: Soma quantidade e soma custo líquido
                             {:quantidade (+ (:quantidade acumulador) (:quantidade transacao))
                              :custo (+ (:custo acumulador) (:valor-liquido transacao))}
  
                             ;; ELse -> Lógica da Venda: Subtrai quantidade e ajusta o custo
                             (let [custo-medio-atual (if (pos? (:quantidade acumulador))
                                                       (/ (:custo acumulador) (:quantidade acumulador))
                                                       0.0)
                                   custo-das-vendidas (* custo-medio-atual (:quantidade transacao))]
                               {:quantidade (- (:quantidade acumulador) (:quantidade transacao))
                                :custo (- (:custo acumulador) custo-das-vendidas)}))))
                       estado-inicial
                       transacoes)
  
         quantidade-final (:quantidade estado-final)
         custo-final (:custo estado-final)]
     
     (let [preco-medio-final (cond
                               (pos? quantidade-final) (/ custo-final quantidade-final)
                               (zero? quantidade-final) 0.0
                               :else (throw (ex-info "Erro..." {})))]
  
     {:quantidade quantidade-final
      :preco-medio preco-medio-final})))


(defn calcular-posicoes-agregadas 
  "Agrupa as transações por ticker. Para cada grupo, chama o calcular-preco-medio para montar o mapa completo
  Recebe um array de transações"
  [transacoes]
(let [;; 1. Agrupamento eficiente por :ticker. Retorna: {"PETR4" [t1 t2], "VALE3" [t3]}
      transacoes-agrupadas (group-by :ticker transacoes)

      ;; 2. Processamento: Itera sobre as ENTRADAS (pares [ticker lista-transacoes])
      posicoes-calculadas (map (fn [[ticker lista-transacoes]]

                                 ;; Lógica de Recálculo: Obtém o mapa {:quantidade N, :preco-medio X}
                                 (let [dados-posicao (calcula-preco-medio lista-transacoes)]

                                   ;; 3. Cria a entrada que será inserida no mapa final
                                   [ticker (assoc dados-posicao :ticker ticker)]))

                               transacoes-agrupadas)]
(into {} posicoes-calculadas))) ;; transforma [chave valor] em map. Resultado será {"ticker" {:qnt,:preco-medio,:ticker}, ...}

(defn calcular-rentabilidade-por-posicao 
  "Calcula as métricas de desempenho para uma única ação: Valor de Mercado, Valor Investido e o Lucro/Prejuízo Líquido.
  Recebe posição {ticker {:quantidade , :preco-medio , :ticker}}
  Recebe preço atual da ação específica"
  [posicao-calculada preco-atual]
  (let [
        {:keys [ticker quantidade preco-medio]} posicao-calculada
        valor-total-investido (* quantidade preco-medio)
        valor-mercado-atual (* quantidade preco-atual)
        lucro-preju-liquido (- valor-mercado-atual valor-total-investido)

        mapa-posicao {
                      :ticker ticker
                      :quantidade (:quantidade posicao-calculada)
                      :preco-medio (:preco-medio posicao-calculada)
                      :preco-atual preco-atual
                      :valor-investido valor-total-investido
                      :valor-mercado valor-mercado-atual
                      :lucro-prejuizo lucro-preju-liquido ;; - é preju | + é lucro
                      }
        ]
    mapa-posicao
    )
  
  )


(defn obter-saldo-completo
  "Retorna o saldo completo da carteira de forma funcional, sem átomos locais."
  []
  (let [todas-posicoes (estado/get-posicoes)]

    (if (empty? todas-posicoes) ;;caso a carteira esteja vazia
      {:posicoes-detalhadas []
       :total-investido 0.0
       :total-mercado 0.0
       :total-lucro-prejuizo 0.0}

      (let [pares-posicoes (seq todas-posicoes) ;;converte o mapa de posições em uma sequência de pares [[ticker1 dados1] [ticker2 dados2] ...]
            estado-inicial-relatorio {:posicoes-detalhadas [] ;; mapa de desempenho de cada posição (ticker)
                                      :total-investido 0.0 ;; valor investido total
                                      :total-mercado 0.0 ;; valor de mercado total
                                      :total-lucro-prejuizo 0.0} ;; lucro/preju total

            relatorio-final (reduce
                             (fn [acumulador [ticker dados-posicao]]
                               (try
                                 (let [dados-mercado (acoes/buscar-dados-acao ticker) ;; pega os dados atuais do mercado do ticker em questão
                                       valor-atual (or (:preco-atual dados-mercado) 0.0) ;; busca valor atual da posição
                                       relatorio-posicao (calcular-rentabilidade-por-posicao dados-posicao valor-atual)] ;; calcula a rentabilidade de cada posição do ticker em questão
                                   
                                   {:posicoes-detalhadas (conj (:posicoes-detalhadas acumulador) relatorio-posicao)
                                    :total-investido (+ (:total-investido acumulador) (:valor-investido relatorio-posicao))
                                    :total-mercado (+ (:total-mercado acumulador) (:valor-mercado relatorio-posicao))
                                    :total-lucro-prejuizo (+ (:total-lucro-prejuizo acumulador) (:lucro-prejuizo relatorio-posicao))})
                                 ;; atualiza o estado-inicial-relatorio com o valor acumulado de cada desempenho das posições
                                 

                                 (catch Exception e
                                   (println (str "ERRO ao buscar dados para " ticker ": " (.getMessage e)))
                                   acumulador)))
                             estado-inicial-relatorio
                             pares-posicoes)]
        relatorio-final))))


(defn atualizar-estado-carteira
  "Atualiza o estado derivado da carteira (posições e saldo) após uma transação."
  []
  (try
    (let [transacoes-carteira (estado/get-transacoes)] ;;pega todas as transações da carteira

      (when (empty? transacoes-carteira)
        (println "AVISO: Nenhuma transação encontrada"))

      (let [posicoes-calculadas (calcular-posicoes-agregadas transacoes-carteira)]
        (estado/set-posicoes-completas posicoes-calculadas) ;; calcula preço medio , quantidade de cada posição e atualiza na carteira

        (let [relatorio-completo (obter-saldo-completo) ;; calcula o saldo da carteira (depois de ter calculado e adicionado as posições)
              saldo-total (:total-mercado relatorio-completo)]
          (estado/set-saldo saldo-total)))) ;; atualiza saldo total da carteira

    (catch Exception e
      (println "ERRO em atualizar-estado-carteira:" (.getMessage e))
      (throw e))))