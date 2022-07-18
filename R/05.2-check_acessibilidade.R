# carregar bibliotecas
source('./R/fun/setup.R')

# verificar:
# - se foi calculada acessibilidade para todos os pontos (algumas cidades podem nao ter acessibilidade
#   por transporte publico para todos os pontos, pq estes nao foram roteados)
# - quantidade de pontos de acessibilidade por cidade/ano/modo
# - quantidade de NA's para cada indicador


# oepn all access
acess <- read_rds(sprintf("../../data/acesso_oport/output_base_final/%s/dados%s_AcessOport_access_tpcar_v1.0.rds", "2017", "2017")) %>%
  setDT()



# quantidade de NA
a_summary <- acess %>%
  group_by(sigla_muni, modo) %>%
  summarise(across(CMATT15:CMPP70I120, ~sum(is.na(.x))))

# quantidade de NA - TMI
a_summary_tmi <- acess %>%
  group_by(sigla_muni, modo) %>%
  summarise(across(TMIST:TMICT, ~sum(is.na(.x))))

# quantidade de hexagonos para cada cidade, modo, ano
a_summary1 <- acess %>%
  count(sigla_muni, modo)
