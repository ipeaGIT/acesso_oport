
<img align="right" src="ipea.png" alt="ipea" width="250">

# Projeto Acesso a Oportunidades

Repositório com os códigos utilizados no processamento e análise de dados do **Projeto Acesso a Oportunidades**, do Ipea. 


# Sobre o projeto
 
O projeto tem como objetivos:
1. Estimar anualmente o acesso da população a oportunidades de trabalho, serviços de saúde e educação por modo de transporte nos maiores centros urbanos do país,
2. Criar uma base de dados abertos sobre as condições de acessibilidade urbana nas cidades brasileiras. 
3. Construir redes de pesquisa para utilizar esses dados em estudos comparativos e no planejamento e avaliação de políticas públicas
  
<p align="justify">
O projeto combina dados de registros administrativos, pesquisas amostrais, dados de imagens de satélite e de mapeamento colaborativo para calcular os níveis de acessibilidade em alta resolução espacial e desagregados por grupos socioeconômicos segundo nível de renda e cor/raça para as maiores cidades do Brasil.

Para mais informações sobre a pesquisa, e o relatório do estudo com resultados e metodologia, visite o site do projeto (em breve no ar).
</p>

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



## Mapa interativo Acesso a Oportunidades

Um aplicativo web em Shiny com os resultados do projeto estará disponível breve.
