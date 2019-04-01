# Projeto Acesso a Oportunidades

Repositório com os códigos utilizados no projeto do Ipea Acesso a Oportunidades.

# Sobre o projeto

Este projeto tem como objetivo analisar o número de oportunidades de emprego, educação e saúde que a população consegue acessar por meio do transporte público, a pé e de bicicleta em grandes áreas urbanas do Brasil. O projeto tem foco particular na análise das desigualdades sociais e espaciais no acesso a oportunidades em relação às políticas urbanas de transporte, habitação e uso do solo.

Combinando dados de uso do solo, Censo Demográfico e de transporte (incluindo dados do OpenStreetMap e dados de transporte público em formato GTFS), o projeto irá estimar o acesso da população à postos de trabalho formais, escolas públicas e serviços de saúde providos pelos SUS utilizando medidas de acessibilidade gravitacionais e de acesso acumulativo de oportunidades, entre outras. As analises serão realizadas em alta resolução espacial, permitindo identificar a distribuição dos serviços de transporte público em cada área urbana e apontar clusters espaciais de áreas com baixo nível de acesso a oportunidades. 

# Hierarquia organizacional

O relatório do projeto com a explicação da metodologia e o código utilizado será disponibilizado na página principal do repositório e é dividido em:

- ``01_tratamento``: documentação do tratamento inicial feito às bases de dados brutas de informações socieconômicas, de uso do solo e transporte;
- ``02_agrupamento``: criação das unidades de agregação (hexágonos) e agregação das variáveis de transporte e uso do solo;
- ``03_otp``: configuração das pastas e arquivos do OpenTripPlanner;
- ``04_matriz``: benchmark de funções de criação de matriz de tempo de viagem e aplicação;
- ``05_acessibilidade``: cálculo dos indicadores de acessiblidade.

Além dessa documentação, outros arquivos necessários para o andamento do projeto estão divididos nas pastas:

- ``analysis``: análises feitas a partir dos dados;
- ``presentations``: apresentações advindas do projeto;
- ``R``: funções úteis para o desenvolvimento do projeto;
- ``reports``: relatórios criados com RMarkdown.

Pastas referentes aos dados que não estão presentes nesse repositório porque contém dados grande demais para a plataforma do GitHub:

- ``data-raw``: dados brutos;
- ``data``: dados tratados e organizados;
- ``otp``: arquivos utilizados na construção do _router_ do OpenTripPlanner.

Futuramente esses dados serão disponibilizados através de uma API.