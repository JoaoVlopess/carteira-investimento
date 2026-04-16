# 💼 Sistema de Gestão de Carteira de Investimentos

## 📖 Sobre o Projeto
Este projeto é um sistema de gerenciamento de carteira de investimentos desenvolvido em **Clojure**. Ele atua como um cliente de terminal (CLI) altamente interativo e seguro que se comunica com uma API RESTful para gerenciar transações financeiras e consumir dados do mercado de ações em tempo real.

O foco principal do desenvolvimento foi aplicar conceitos sólidos de Programação Funcional, imutabilidade de dados e integração robusta de sistemas, garantindo que as operações financeiras (como compras e vendas de ativos) sejam processadas com alta confiabilidade.

## 🚀 Funcionalidades
* **Integração com Mercado Financeiro:** Consulta de preços de ações atualizados e histórico de cotações de dias anteriores.
* **Motor de Transações:** Registro de compras e vendas de ativos com aplicação automática do algoritmo **FIFO (First-In, First-Out)** para cálculo exato de lucro/prejuízo.
* **Análise de Portfólio:** Cálculo de saldo detalhado, consolidando o total investido, valor de mercado atual e percentual de rentabilidade (ROI) geral e por ativo.
* **Auditoria e Extratos:** Geração de extratos completos ou filtrados por período, garantindo a rastreabilidade de todas as movimentações.
* **Resiliência:** Sistema robusto de validação de inputs (datas, tickers, quantidades) e tratamento de exceções HTTP (Timeouts, erros 4xx e 5xx).

## 🛠️ Tecnologias Utilizadas
* **Clojure** (Linguagem principal, focada em paradigma funcional).
* **clj-http** (Cliente HTTP para comunicação com a API backend e APIs de mercado).
* **Cheshire** (Parseamento e geração de dados em JSON).
* **Java Time API** (Para manipulação precisa de datas históricas e atuais).

## ⚙️ Como Executar

1. Certifique-se de ter o [Leiningen](https://leiningen.org/) instalado.
2. Inicie o servidor backend da API (necessário para rodar na porta `3000`):
   ```bash
   lein ring server-headless
