(ns carteira-investimento.core
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.string :as str]
            [carteira-investimento.integracao.acoes :as acoes])
  (:gen-class))

(def api-local-url "http://localhost:3000")

;; ========================================
;; UTILITARIOS E FORMATACAO
;; ========================================

(defn limpar-tela []
  "Limpa a tela do terminal"
  (print "\033[2J\033[H")
  (flush))

(defn pausar []
  "Pausa e aguarda o usuario pressionar Enter"
  (println "\n>> Pressione ENTER para continuar...")
  (read-line))

(defn formatar-moeda [valor]
  "Formata valor monetario com separadores"
  (format "R$ %,.2f" valor))

(defn formatar-percentual [valor]
  "Formata percentual com 2 casas decimais"
  (format "%.2f%%" valor))

(defn exibir-linha-separadora []
  "Exibe linha decorativa"
  (println "==============================================================="))

(defn exibir-cabecalho [titulo]
  "Exibe cabecalho estilizado"
  (limpar-tela)
  (exibir-linha-separadora)
  (println (str ">>> " titulo))
  (exibir-linha-separadora))

(defn exibir-erro [mensagem]
  "Exibe mensagem de erro formatada"
  (println (str "[ERRO] " mensagem))
  (pausar))

(defn exibir-sucesso [mensagem]
  "Exibe mensagem de sucesso formatada"
  (println (str "[SUCESSO] " mensagem)))

(defn exibir-aviso [mensagem]
  "Exibe mensagem de aviso formatada"
  (println (str "[AVISO] " mensagem)))

;; ========================================
;; VALIDACOES DE ENTRADA CORRIGIDAS
;; ========================================

(defn validar-ticker [ticker]
  "Valida formato do ticker e converte para maiusculo automaticamente"
  (let [ticker-upper (.toUpperCase (str/trim ticker))]
    (cond
      (str/blank? ticker)
      {:valido false :erro "Ticker nao pode estar vazio"}

      (< (count ticker-upper) 4)
      {:valido false :erro "Ticker deve ter pelo menos 4 caracteres"}

      (> (count ticker-upper) 6)
      {:valido false :erro "Ticker deve ter no maximo 6 caracteres"}

      (not (re-matches #"[A-Z0-9]+" ticker-upper))
      {:valido false :erro "Ticker deve conter apenas letras e numeros (sem simbolos especiais)"}

      :else
      {:valido true :valor ticker-upper}))) ; Retorna ticker em maiusculo

(defn validar-quantidade [quantidade-str]
  "Valida e converte quantidade"
  (try
    (cond
      (str/blank? quantidade-str)
      {:valido false :erro "Quantidade nao pode estar vazia"}

      :else
      (let [qtd (Double/parseDouble quantidade-str)]
        (cond
          (<= qtd 0)
          {:valido false :erro "Quantidade deve ser maior que zero"}

          (> qtd 1000000)
          {:valido false :erro "Quantidade muito alta (maximo: 1.000.000)"}

          :else
          {:valido true :valor qtd})))
    (catch NumberFormatException _
      {:valido false :erro "Quantidade deve ser um numero valido"})))

(defn validar-data [data-str]
  "Valida formato de data YYYY-MM-DD"
  (try
    (cond
      (str/blank? data-str)
      {:valido true :valor (java.time.LocalDate/now)} ; Data vazia = hoje

      (not (re-matches #"\d{4}-\d{2}-\d{2}" data-str))
      {:valido false :erro "Formato deve ser YYYY-MM-DD (ex: 2025-12-01)"}

      :else
      (let [data (java.time.LocalDate/parse data-str)]
        (cond
          (.isAfter data (java.time.LocalDate/now))
          {:valido false :erro "Data nao pode ser futura"}

          (.isBefore data (.minusYears (java.time.LocalDate/now) 1))
          {:valido false :erro "Data muito antiga (maximo 1 ano atras)"}

          :else
          {:valido true :valor data})))
    (catch Exception _
      {:valido false :erro "Data invalida. Use formato YYYY-MM-DD"})))

(defn validar-confirmacao [confirmacao-str]
  "Valida confirmacao s/n"
  (let [conf (.toLowerCase (str/trim confirmacao-str))]
    (cond
      (= conf "s") {:valido true :valor "s"}
      (= conf "n") {:valido true :valor "n"}
      (str/blank? conf) {:valido true :valor "n"} ; Enter = nao
      :else {:valido false :erro "Digite 's' para SIM ou 'n' para NAO (ou apenas Enter para NAO)"})))

(defn ler-entrada-segura [prompt validador]
  "Le entrada do usuario com validacao e retry"
  (loop [tentativas 0]
    (if (>= tentativas 3)
      (do
        (exibir-erro "Muitas tentativas invalidas. Retornando ao menu.")
        nil)
      (do
        (print (str prompt " "))
        (flush)
        (let [entrada (str/trim (read-line))
              resultado (validador entrada)]
          (if (:valido resultado)
            (if (contains? resultado :valor)
              (:valor resultado)    ; Retorna valor processado
              entrada)              ; Retorna entrada original se nao tem :valor
            (do
              (println (str "[X] " (:erro resultado)))
              (println ">> Tente novamente...")
              (recur (inc tentativas)))))))))

;; ========================================
;; FUNCOES DE API COM TRATAMENTO DE ERRO
;; ========================================

(defn executar-requisicao-segura [funcao-req descricao]
  "Executa requisicao HTTP com tratamento de erro robusto"
  (try
    {:sucesso true :dados (funcao-req)}
    (catch java.net.ConnectException _
      {:sucesso false :erro "Nao foi possivel conectar ao servidor. Verifique se a API esta rodando."})
    (catch java.net.SocketTimeoutException _
      {:sucesso false :erro "Timeout na conexao. Tente novamente."})
    (catch clojure.lang.ExceptionInfo e
      (let [status (-> e ex-data :status)]
        (case status
          404 {:sucesso false :erro (str "Recurso nao encontrado: " descricao)}
          400 {:sucesso false :erro "Dados invalidos enviados para o servidor"}
          409 {:sucesso false :erro "Quantidade insuficiente de acoes para venda"}
          500 {:sucesso false :erro "Erro interno do servidor"}
          {:sucesso false :erro (str "Erro HTTP " status ": " (.getMessage e))})))
    (catch Exception e
      {:sucesso false :erro (str "Erro inesperado: " (.getMessage e))})))

(defn buscar-dados-acao
  "Busca dados de acao via integracao direta (TEMPORARIO)"
  ([ticker]
   (buscar-dados-acao ticker (java.time.LocalDate/now)))

  ([ticker data]
   (try
     ;; Chama diretamente a função de integração que sabemos que funciona
     (let [dados (acoes/buscar-dados-acao ticker data)]
       {:sucesso true :dados dados})
     (catch Exception e
       {:sucesso false :erro (str "Erro ao buscar dados: " (.getMessage e))}))))

(defn registrar-compra [ticker quantidade data]
  "Registra compra com data especifica"
  (let [resultado (executar-requisicao-segura
                   #(let [url (str api-local-url "/api/transacoes/compra")
                          dados-json (json/generate-string {:ticker ticker
                                                            :quantidade quantidade
                                                            :data (str data)})
                          response (http/post url {:body dados-json
                                                   :content-type "application/json"})
                          resultado (json/parse-string (:body response) true)]
                      resultado)
                   "registro de compra")]
    resultado))

(defn registrar-venda [ticker quantidade data]
  "Registra venda com data especifica"
  (let [resultado (executar-requisicao-segura
                   #(let [url (str api-local-url "/api/transacoes/venda")
                          dados-json (json/generate-string {:ticker ticker
                                                            :quantidade quantidade
                                                            :data (str data)})
                          response (http/post url {:body dados-json
                                                   :content-type "application/json"})
                          resultado (json/parse-string (:body response) true)]
                      resultado)
                   "registro de venda")]
    resultado))

(defn obter-extrato []
  "Obtem extrato com tratamento de erro"
  (let [resultado (executar-requisicao-segura
                   #(let [url (str api-local-url "/api/transacoes/extrato")
                          response (http/get url)
                          dados (json/parse-string (:body response) true)]
                      (:extrato dados))
                   "extrato de transacoes")]
    resultado))

(defn obter-saldo []
  "Obtem saldo com tratamento de erro"
  (let [resultado (executar-requisicao-segura
                   #(let [url (str api-local-url "/api/carteira/saldo")
                          response (http/get url)
                          dados (json/parse-string (:body response) true)]
                      dados)
                   "saldo da carteira")]
    resultado))

(defn obter-acoes-populares []
  "Obtem acoes populares com tratamento de erro"
  (let [resultado (executar-requisicao-segura
                   #(let [url (str api-local-url "/api/acoes/populares")
                          response (http/get url)
                          dados (json/parse-string (:body response) true)]
                      (:acoes-populares dados))
                   "acoes populares")]
    resultado))

;; ========================================
;; INTERFACE VISUAL APRIMORADA
;; ========================================

(defn exibir-menu-principal []
  "Exibe menu principal estilizado"
  (exibir-cabecalho "CARTEIRA DE INVESTIMENTOS - MENU PRINCIPAL")
  (println "")
  (println ">> CONSULTAS:")
  (println "   1 - Consultar Acao")
  (println "   2 - Acoes Populares")
  (println "   3 - Ver Saldo Detalhado")
  (println "   4 - Extrato Completo")
  (println "")
  (println ">> TRANSACOES:")
  (println "   5 - Comprar Acao")
  (println "   6 - Vender Acao")
  (println "")
  (println ">> SISTEMA:")
  (println "   7 - Status da API")
  (println "   0 - Sair")
  (println "")
  (exibir-linha-separadora)
  (print ">> Escolha uma opcao: "))

(defn limpar-nome-acao [nome]
  "Remove espacos extras e codigos de mercado do nome da acao"
  (when nome
    (-> nome
        str/trim
        (str/replace #"\s+(ON|PN|NM)\s*$" "")  ; Remove codigos de mercado
        (str/replace #"\s+" " ")               ; Normaliza espacos
        str/trim)))

(defn exibir-dados-acao-detalhados [dados]
  "Exibe dados da acao de forma detalhada"
  (exibir-cabecalho "INFORMACOES DA ACAO")
  (let [nome-limpo (limpar-nome-acao (:nome dados))]
    (println (str "Empresa: " nome-limpo))
    (println (str "Ticker: " (:ticker dados)))
    (println (str "Preco Atual: " (formatar-moeda (:preco-atual dados))))
    (println (str "Moeda: " (:moeda dados "BRL")))
    (println (str "Ultima Atualizacao: " (java.time.LocalDateTime/now))))
  (exibir-linha-separadora))

(defn exibir-saldo-detalhado [saldo]
  "Exibe saldo com informacoes detalhadas"
  (exibir-cabecalho "RESUMO DA CARTEIRA")

  ;; Resumo financeiro
  (println ">> RESUMO FINANCEIRO:")
  (println (str "   Total Investido: " (formatar-moeda (:total-investido saldo))))
  (println (str "   Valor de Mercado: " (formatar-moeda (:total-mercado saldo))))

  (let [lucro (:total-lucro-prejuizo saldo)]
    (if (>= lucro 0)
      (println (str "   Lucro: " (formatar-moeda lucro)))
      (println (str "   Prejuizo: " (formatar-moeda (Math/abs lucro))))))

  ;; Rentabilidade
  (when (> (:total-investido saldo) 0)
    (let [rentabilidade (* 100 (/ (:total-lucro-prejuizo saldo) (:total-investido saldo)))]
      (println (str "   Rentabilidade: " (formatar-percentual rentabilidade)))))

  (println "")

  ;; Posicoes detalhadas
  (when-let [posicoes (:posicoes-detalhadas saldo)]
    (when (seq posicoes)
      (println ">> POSICOES DETALHADAS:")
      (doseq [posicao posicoes]
        (println (str "   Ticker: " (:ticker posicao)))
        (println (str "      Quantidade: " (:quantidade posicao) " acoes"))
        (println (str "      Preco Atual: " (formatar-moeda (:preco-atual posicao))))
        (println (str "      Valor Investido: " (formatar-moeda (:valor-investido posicao))))
        (println (str "      Valor de Mercado: " (formatar-moeda (:valor-mercado posicao))))
        (let [lucro-posicao (:lucro-prejuizo posicao)]
          (if (>= lucro-posicao 0)
            (println (str "      Lucro: " (formatar-moeda lucro-posicao)))
            (println (str "      Prejuizo: " (formatar-moeda (Math/abs lucro-posicao))))))
        (println ""))))

  (exibir-linha-separadora))

(defn exibir-extrato-detalhado [extrato]
  "Exibe extrato formatado e organizado"
  (exibir-cabecalho "EXTRATO DE TRANSACOES")

  (if (empty? extrato)
    (do
      (println "Nenhuma transacao encontrada.")
      (println ">> Comece fazendo sua primeira compra!"))
    (do
      (println (str "Total de transacoes: " (count extrato)))
      (println "")

      ;; Cabecalho da tabela
      (println "+------------+--------+--------+-------------+------------------+")
      (println "|    DATA    |  TIPO  | TICKER | QUANTIDADE  |      VALOR       |")
      (println "+------------+--------+--------+-------------+------------------+")

      ;; Dados das transacoes
      (doseq [t extrato]
        (let [data (str (:data t))
              tipo (case (:tipo t)
                     :COMPRA "COMPRA"
                     :VENDA "VENDA "
                     (str (:tipo t)))
              ticker (:ticker t)
              quantidade (format "%9.1f" (:quantidade t))
              valor (format "%14s" (formatar-moeda (:valor-total t)))]
          (println (str "| " data " | " tipo " | " (format "%6s" ticker) " | " quantidade " | " valor " |"))))

      (println "+------------+--------+--------+-------------+------------------+")))

  (exibir-linha-separadora))

(defn exibir-acoes-populares [acoes]
  "Exibe acoes populares formatadas"
  (exibir-cabecalho "ACOES MAIS NEGOCIADAS - B3")

  (if (empty? acoes)
    (println "Nenhuma acao encontrada.")
    (do
      (println ">> Precos atualizados em tempo real")
      (println "")

      ;; Cabecalho
      (println "+--------+---------------------------------+------------------+")
      (println "| TICKER |            EMPRESA              |      PRECO       |")
      (println "+--------+---------------------------------+------------------+")

      ;; Dados das acoes
      (doseq [acao acoes]
        (when (= (:status acao) "success")
          (let [ticker (format "%6s" (:ticker acao))
                nome (format "%-31s" (limpar-nome-acao (:nome acao)))
                preco (format "%14s" (formatar-moeda (:preco-atual acao)))]
            (println (str "| " ticker " | " nome " | " preco " |")))))

      (println "+--------+---------------------------------+------------------+")))

  (exibir-linha-separadora))

(defn verificar-status-api []
  "Verifica se a API esta funcionando"
  (exibir-cabecalho "STATUS DA API")

  (let [resultado (executar-requisicao-segura
                   #(let [url (str api-local-url "/")
                          response (http/get url)]
                      (json/parse-string (:body response) true))
                   "status da API")]

    (if (:sucesso resultado)
      (do
        (println "[OK] API esta funcionando corretamente!")
        (println (str "URL: " api-local-url))
        (when-let [dados (:dados resultado)]
          (println (str "Mensagem: " (:message dados)))
          (println (str "Versao: " (:version dados)))))
      (do
        (println "[ERRO] API nao esta respondendo!")
        (println (str "URL testada: " api-local-url))
        (println (str "Erro: " (:erro resultado)))
        (println "")
        (println ">> SOLUCOES:")
        (println "   - Verifique se o servidor backend esta rodando")
        (println "   - Confirme se a porta 3000 esta disponivel")
        (println "   - Teste o comando: lein ring server-headless"))))

  (exibir-linha-separadora))

;; ========================================
;; PROCESSAMENTO DE OPCOES APRIMORADO
;; ========================================

(defn processar-consulta-acao []
  "Processa consulta de acao com validacao"
  (exibir-cabecalho "CONSULTAR ACAO")

  (when-let [ticker (ler-entrada-segura
                     "Digite o ticker da acao (ex: PETR4, vale3, itub4):"
                     validar-ticker)]
    (when-let [data (ler-entrada-segura
                     "Digite a data (YYYY-MM-DD) ou ENTER para hoje:"
                     validar-data)]
      (let [resultado (buscar-dados-acao ticker data)]

        (if (:sucesso resultado)
          (do
            (exibir-dados-acao-detalhados (:dados resultado))
            (when (not (= data (java.time.LocalDate/now)))
              (println (str ">> Dados historicos para: " data)))
            (pausar))
          (exibir-erro (:erro resultado)))))))

(defn processar-compra []
  "Processa compra com validacao completa"
  (exibir-cabecalho "COMPRAR ACAO")

  (when-let [ticker (ler-entrada-segura
                     "Digite o ticker da acao:"
                     validar-ticker)]
    (when-let [quantidade (ler-entrada-segura
                           "Digite a quantidade:"
                           validar-quantidade)]
      (when-let [data (ler-entrada-segura
                       "Digite a data da compra (YYYY-MM-DD) ou ENTER para hoje:"
                       validar-data)]

        (println "")
        (println (str ">> Buscando dados da acao para " data "..."))

        ;; Primeiro busca dados da acao para mostrar preco
        (let [dados-acao (buscar-dados-acao ticker data)]
          (if (:sucesso dados-acao)
            (let [acao (:dados dados-acao)
                  preco (:preco-atual acao)
                  valor-estimado (* quantidade preco)]

              (println "")
              (println ">> RESUMO DA COMPRA:")
              (println (str "   Data: " data))
              (println (str "   Acao: " (:ticker acao) " - " (limpar-nome-acao (:nome acao))))
              (println (str "   Quantidade: " quantidade " acoes"))
              (println (str "   Preco Unitario: " (formatar-moeda preco)))
              (println (str "   Valor Estimado: " (formatar-moeda valor-estimado)))
              (println "   AVISO: Valor final incluira taxas (~0.1%)")
              (when (not (= data (java.time.LocalDate/now)))
                (println "   INFO: Usando preco historico"))
              (println "")

              (when-let [confirmacao (ler-entrada-segura
                                      "Confirma a compra? (s/N):"
                                      validar-confirmacao)]
                (if (= confirmacao "s")
                  (do
                    (println "")
                    (println ">> Processando compra...")
                    (let [resultado (registrar-compra ticker quantidade data)]
                      (if (:sucesso resultado)
                        (do
                          (exibir-sucesso "Compra realizada com sucesso!")
                          (println (str "Valor total: " (formatar-moeda (:valor-total (:dados resultado)))))
                          (pausar))
                        (exibir-erro (:erro resultado)))))
                  (do
                    (exibir-aviso "Compra cancelada pelo usuario.")
                    (pausar)))))
            (exibir-erro (:erro dados-acao))))))))

(defn processar-venda []
  "Processa venda com validacao completa"
  (exibir-cabecalho "VENDER ACAO")

  (when-let [ticker (ler-entrada-segura
                     "Digite o ticker da acao:"
                     validar-ticker)]
    (when-let [quantidade (ler-entrada-segura
                           "Digite a quantidade:"
                           validar-quantidade)]
      (when-let [data (ler-entrada-segura
                       "Digite a data da venda (YYYY-MM-DD) ou ENTER para hoje:"
                       validar-data)]

        (println "")
        (println (str ">> Verificando posicao na carteira para " data "..."))

        (let [dados-acao (buscar-dados-acao ticker data)]
          (if (:sucesso dados-acao)
            (let [acao (:dados dados-acao)
                  preco (:preco-atual acao)
                  valor-estimado (* quantidade preco)]

              (println "")
              (println ">> RESUMO DA VENDA:")
              (println (str "   Data: " data))
              (println (str "   Acao: " (:ticker acao) " - " (limpar-nome-acao (:nome acao))))
              (println (str "   Quantidade: " quantidade " acoes"))
              (println (str "   Preco Unitario: " (formatar-moeda preco)))
              (println (str "   Valor Estimado: " (formatar-moeda valor-estimado)))
              (println "   AVISO: Valor final descontara taxas (~0.1%)")
              (println "   INFO: Sera aplicado algoritmo FIFO")
              (when (not (= data (java.time.LocalDate/now)))
                (println "   INFO: Usando preco historico"))
              (println "")

              (when-let [confirmacao (ler-entrada-segura
                                      "Confirma a venda? (s/N):"
                                      validar-confirmacao)]
                (if (= confirmacao "s")
                  (do
                    (println "")
                    (println ">> Processando venda...")
                    (let [resultado (registrar-venda ticker quantidade data)]
                      (if (:sucesso resultado)
                        (do
                          (exibir-sucesso "Venda realizada com sucesso!")
                          (println ">> Algoritmo FIFO aplicado automaticamente")
                          (pausar))
                        (exibir-erro (:erro resultado)))))
                  (do
                    (exibir-aviso "Venda cancelada pelo usuario.")
                    (pausar)))))
            (exibir-erro (:erro dados-acao))))))))

(defn processar-saldo []
  "Processa visualizacao de saldo"
  (println ">> Carregando dados da carteira...")
  (let [resultado (obter-saldo)]
    (if (:sucesso resultado)
      (do
        (exibir-saldo-detalhado (:dados resultado))
        (pausar))
      (exibir-erro (:erro resultado)))))

(defn processar-extrato []
  "Processa visualizacao de extrato"
  (println ">> Carregando extrato...")
  (let [resultado (obter-extrato)]
    (if (:sucesso resultado)
      (do
        (exibir-extrato-detalhado (:dados resultado))
        (pausar))
      (exibir-erro (:erro resultado)))))

(defn processar-acoes-populares []
  "Processa visualizacao de acoes populares"
  (println ">> Carregando acoes populares...")
  (let [resultado (obter-acoes-populares)]
    (if (:sucesso resultado)
      (do
        (exibir-acoes-populares (:dados resultado))
        (pausar))
      (exibir-erro (:erro resultado)))))

(defn processar-opcao [opcao]
  "Processa opcao do menu principal"
  (case opcao
    "1" (do (processar-consulta-acao) true)
    "2" (do (processar-acoes-populares) true)
    "3" (do (processar-saldo) true)
    "4" (do (processar-extrato) true)
    "5" (do (processar-compra) true)
    "6" (do (processar-venda) true)
    "7" (do (verificar-status-api) (pausar) true)
    "0" false
    (do
      (exibir-erro "Opcao invalida! Escolha um numero de 0 a 7.")
      true)))

;; ========================================
;; LOOP PRINCIPAL CORRIGIDO
;; ========================================

(defn executar-menu-seguro []
  "Executa uma iteracao do menu com tratamento de erro"
  (try
    (exibir-menu-principal)
    (flush)
    (let [opcao (str/trim (read-line))]
      (processar-opcao opcao))
    (catch Exception e
      (exibir-erro (str "Erro inesperado: " (.getMessage e)))
      true))) ; Continua o loop mesmo com erro

(defn executar-menu
  "Loop principal do sistema"
  ([] (executar-menu true))
  ([continuar]
   (when continuar
     (let [deve-continuar (executar-menu-seguro)]
       (recur deve-continuar)))))

(defn inicializar-sistema []
  "Inicializa o sistema com verificacao de conexao"
  (exibir-cabecalho "INICIALIZANDO SISTEMA")
  (println ">> Carteira de Investimentos - Cliente Profissional")
  (println (str "API: " api-local-url))
  (println "")
  (println ">> Verificando conexao com a API...")

  ;; Teste inicial de conexao
  (let [teste-api (executar-requisicao-segura
                   #(http/get api-local-url)
                   "teste de conexao")]
    (if (:sucesso teste-api)
      (do
        (println "[OK] Conexao estabelecida com sucesso!")
        (Thread/sleep 1500)
        (executar-menu))
      (do
        (println "[ERRO] Falha na conexao inicial!")
        (println (str "Erro: " (:erro teste-api)))
        (println "")
        (println ">> Verifique se o backend esta rodando e tente novamente.")
        (pausar)))))

(defn finalizar-sistema []
  "Exibe mensagem de despedida"
  (exibir-cabecalho "SISTEMA ENCERRADO")
  (println "Obrigado por usar a Carteira de Investimentos!")
  (println "Seus dados foram salvos com seguranca.")
  (println "Ate a proxima!"))

(defn -main [& _]
  "Funcao principal"
  (try
    (inicializar-sistema)
    (catch Exception e
      (exibir-erro (str "Falha critica na inicializacao: " (.getMessage e)))))

  (finalizar-sistema))