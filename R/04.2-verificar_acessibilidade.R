# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.4.2 Verificar acessibilidade extrema

# A verificacao eh feita pegando todos os hexagonos que tem menos que 1% de acessibilidade para o CMATT90
# p/ transporte publico. O ponto de roteamento desses hexagonos sao alocados para o hexagono mais proximo 
# que nao tenha menos que 1% de acess.
# Hexagonos que estiverem a menos de 1km das bordas serao desconsiderados

# carregar bibliotecas
source('./R/fun/setup.R')

# definir funcao
corrigir_acess_extremos <- function(sigla_muni) {
 
  # sigla_muni <- "bsb"; ano <- 2019
  
  # path in da acessibilidade
  path_in <- sprintf("../data/output_access/acess_%s_%s.rds", sigla_muni, ano)
  
  # abrir
  acess <- readr::read_rds(path_in)
  
  # trazer dados de uso do solo
  us <- readr::read_rds(sprintf("../data/hex_agregados/hex_agregado_%s_09.rds", sigla_muni)) %>%
    st_set_geometry(NULL)
  
  # Fazer teste indicador CMATT60
  acess_cma <- acess %>% filter(mode == "walk" & pico == 1) %>% select(origin, city, CMATT60, CMPPT60) 
  
  # juntar acessibilidade com dados de uso do solo
  acess_cma_us <- left_join(acess_cma, us, by = c("origin" = "id_hex"))
  
  # abrir limites
  limits <- read_rds(sprintf("../data-raw/municipios/%s/municipio_%s.rds", sigla_muni, sigla_muni)) %>%
    # transformar em uma linestring
    st_cast("LINESTRING") %>%
    st_set_crs(4326)
  
  # extrair hexagonos que tenham uma acessibilidade menor que 1%?
  acess_prob <- acess_cma_us %>% filter(CMATT90 < 0.01)
  
  # extrair somente hexagonos que estejam a pelo menos 1km de distancia das bordas do municipio
  acess_prob_dist <- acess_prob %>%
    # extrair distancia entre hexagonos e limites da cidade
    mutate(dist = st_distance(., limits) %>% as.numeric()) %>%
    # extarir distancias maior que 1000m
    filter(dist > 1000) %>%
    # transformar para pontos
    st_centroid()
  

  # alocar hexagonos problematicos para hexagono nao-problematico mais proximo ------------------
  
  # pegar hexagonos nao problematicos
  acess_nprob <- acess_cma_us %>%
    filter(origin %nin% acess_prob_dist$origin) %>%
    # transformar para pontos
    st_centroid() %>%
    # criar id
    mutate(id = 1:n()) %>%
    # transformar para lon lat
    sfc_as_cols()
  
  # pegar os pontos mais proximos nao-problematicos dos problematicos
  uui <- RANN::nn2(select(acess_nprob, lon, lat), 
                   select(acess_prob_dist %>% sfc_as_cols(), lon,lat), 1)
  
  # entao identificar quais sao esses hexagonos
  acess_prob_proximos <- acess_prob_dist %>% 
    # tirar a geometria: ela agora sera nova dos outros hexagonos!
    st_set_geometry(NULL) %>%
    mutate(id = uui$nn.idx) %>%
    # trazer o id completo do hex
    left_join(acess_nprob %>% select(id, lon, lat), by = "id") %>%
    # selecionar colunas e renomear
    select(id_hex = origin, X = lon, Y = lat)
  
  # fim: transformar de volta para sf e juntar com os nao-problematicos
  # abrir pontos roteaveis
  points <- fread(sprintf("../otp/points/points_%s_09.csv", sigla_muni)) %>%
    # selecionar so os nao problematicos
    filter(id_hex %nin% acess_prob_proximos$id_hex) %>%
    # juntar os problematicos corrigidos
    rbind(acess_prob_proximos)
  
  # teste
  mapview(points %>% to_spatial(c("X", "Y"))) + acess_prob_dist
    
  }


# ATENCAO
# ATENCAO
# ATENCAO
# ATENCAO
# ATENCAO
# ATENCAO
# PEGAR TODOS OS HEXAGONOS COM ACESSIBILIDADE MENOR QUE 5% DO CMATT90



