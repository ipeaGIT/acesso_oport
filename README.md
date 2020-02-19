
<img align="right" src="ipea.png" alt="ipea" width="250">

# Projeto Acesso a Oportunidades

Este repositório contem os códigos utilizados no processamento e análise de dados do **Projeto Acesso a Oportunidades**, do Ipea. **[Para mais informações sobre a pesquisa com seus resultados, download dos dados e metodologia, visite o site do projeto](https://www.ipea.gov.br/acessooportunidades/)**.



# Organização do repositório

Os scripts em `R` utilizados neste repositório estão organizados em grupos e numeros segundo ordem de processamento.

  - `01_tratamento`: documentação do tratamento inicial feito às bases
    de dados brutas de informações socieconômicas, de uso do solo e
    transporte;
  - `02_agrupamento`: criação das unidades de espaciais de análise (hexágonos) e
    agregação espacial das variáveis de transporte e uso do solo;
  - `03_otp`: configuração das pastas e arquivos do OpenTripPlanner e cálculo de de matriz de tempo de
    viagem;
  - `04_acesso`: cálculo dos indicadores de acessiblidade;
  - `05_outputs`: organização da base de dados, mapas e gráficos para publicação.

Além dessa documentação, outros arquivos necessários para o andamento do
projeto estão divididos nas pastas:

  - `./R/fun`: funções úteis para o desenvolvimento do projeto;

Pastas referentes aos dados que não estão presentes nesse repositório
porque contém dados grande demais para a plataforma do GitHub:

  - `data-raw`: dados brutos;
  - `data`: dados tratados e organizados;
  - `otp`: arquivos utilizados na construção do *router* do
    OpenTripPlanner.

