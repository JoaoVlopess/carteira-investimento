;;O atom central e funções de acesso/mutação crua. "Banco de dados"
(ns carteira-investimento.dados.estado)

(def carteira
  "O atom central que armazena o estado da carteira de investimentos."
  (atom {:transacoes [] ;; mapa de dados de cada transação financeira
         :posicoes {} ;; cada posicao/ação e o lote: {ticker -> [lote-1, lote-2, ...]} ;; cada lote tem :id-transacao, :quantidade (remanescente) e :preco-custo
         :saldo 0.0}))

(defn add-transacao
  "Adiciona uma transação à lista de transações no atom. 
  A 'transacao' é um mapa que contém todos os dados da operação."
  [transacao]
  (swap! carteira update :transacoes conj transacao))

(defn get-posicoes
  "Retorna o mapa de posições (posicoes e quantidades)."
  []
  (:posicoes @carteira))

(defn get-posicao-especifica
  "Retorna o mapa de posição para um único ticker, ou nil se não encontrado."
  [ticker]
  (get (:posicoes @carteira) ticker))

(defn get-saldo
  "Retorna o valor do saldo total da carteira."
  []
  (:saldo @carteira))

(defn remove-posicao
  "Remove uma posicao do mapa de :posicoes no atom 'carteira'.
   Esta função deve ser chamada apenas se a quantidade da ação for zero."
  [ticker]
  (swap! carteira update :posicoes dissoc ticker))

(defn set-saldo [novo-saldo]
  "Atualiza o valor do saldo da carteira"
  (swap! carteira assoc :saldo novo-saldo))

(defn get-transacoes
  "Retorna todas as transações da carteira ou filtra por período de datas"
  ([]
   ;; Retorna todas ordenadas por data
   (sort-by :data (:transacoes @carteira)))

  ([data-inicio data-fim]
   (let [transacoes (:transacoes @carteira)
         filtradas (filter (fn [transacao]
                             (let [data-transacao (:data transacao)]
                               ;; data-inicio <= data-transacao <= data-fim
                               (and (not (.isBefore data-transacao data-inicio))
                                    (not (.isAfter data-transacao data-fim)))))
                           transacoes)]
     (sort-by :data filtradas))))

(defn set-posicao-especifica [ticker dados-posicao]
  "atualiza os valores da posição específica"
  (swap! carteira
           assoc-in
           [:posicoes ticker]
           dados-posicao))

(defn set-posicoes-completas
  "Substitui o mapa de :posicoes do atom pelo novo mapa calculado."
  [novo-mapa-posicoes]
  (swap! carteira assoc :posicoes novo-mapa-posicoes))