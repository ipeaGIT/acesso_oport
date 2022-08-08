# carregar bibliotecas
source('./R/fun/setup.R')

# verificar:
# - se foi calculada acessibilidade para todos os pontos (algumas cidades podem nao ter acessibilidade
#   por transporte publico para todos os pontos, pq estes nao foram roteados)
# - quantidade de pontos de acessibilidade por cidade/ano/modo
# - quantidade de NA's para cada indicador


# oepn all access
acess_tpcar <- rbind(
  read_rds(sprintf("../../data/acesso_oport/output_base_final/%s/dados%s_AcessOport_access_tpcar_v1.0.rds", "2017", "2017")) %>%
    setDT(),
  read_rds(sprintf("../../data/acesso_oport/output_base_final/%s/dados%s_AcessOport_access_tpcar_v1.0.rds", "2018", "2018")) %>%
    setDT(),
  # oepn all access
  read_rds(sprintf("../../data/acesso_oport/output_base_final/%s/dados%s_AcessOport_access_tpcar_v1.0.rds", "2019", "2019")) %>%
    setDT())
# oepn all access
acess_active <- rbind(
  # oepn all access
  read_rds(sprintf("../../data/acesso_oport/output_base_final/%s/dados%s_AcessOport_access_active_v1.0.rds", "2017", "2017")) %>%
    setDT(),
  read_rds(sprintf("../../data/acesso_oport/output_base_final/%s/dados%s_AcessOport_access_active_v1.0.rds", "2018", "2018")) %>%
    setDT(),
  read_rds(sprintf("../../data/acesso_oport/output_base_final/%s/dados%s_AcessOport_access_active_v1.0.rds", "2019", "2019")) %>%
    setDT())
landuse <- rbind(
  # oepn all access
  read_rds(sprintf("../../data/acesso_oport/output_base_final/%s/dados%s_AcessOport_landuse_v1.0.rds", "2017", "2017")) %>%
    setDT(),
  read_rds(sprintf("../../data/acesso_oport/output_base_final/%s/dados%s_AcessOport_landuse_v1.0.rds", "2018", "2018")) %>%
    setDT(),
  read_rds(sprintf("../../data/acesso_oport/output_base_final/%s/dados%s_AcessOport_landuse_v1.0.rds", "2019", "2019")) %>%
    setDT())


table(acess_tpcar$sigla_muni, useNA = 'always')
table(acess_tpcar$nome_muni, useNA = 'always')
table(acess_tpcar$code_muni, useNA = 'always')

table(acess_active$sigla_muni, useNA = 'always')
table(acess_active$nome_muni, useNA = 'always')
table(acess_active$code_muni, useNA = 'always')

table(landuse$sigla_muni, useNA = 'always')
table(landuse$nome_muni, useNA = 'always')
table(landuse$code_muni, useNA = 'always')

# valores minimos e maximos
check_extreme1 <- acess_tpcar %>%
  group_by(sigla_muni, ano, modo) %>%
  summarise(across(CMATT15:TMICT, min))
  

# quantidade de NA
a_summary <- acess1 %>%
  group_by(sigla_muni, ano, modo) %>%
  summarise(across(CMATT15:CMPP70I120, ~sum(is.na(.x))))

# quantidade de NA - TMI
a_summary_tmi <- acess1 %>%
  group_by(sigla_muni, ano, modo) %>%
  summarise(across(TMIST:TMICT, ~sum(is.na(.x))))
a_summary_tmi <- acess_tpcar %>%
  filter(pico == 1) %>%
  group_by(sigla_muni, ano, modo) %>%
  summarise(across(TMIST:TMICT, ~sum(is.infinite(.x))))

# quantidade de hexagonos para cada cidade, modo, ano
a_summary <- rbind(acess_tpcar %>%
                     count(sigla_muni, ano, modo),
                   acess_active %>%
                     count(sigla_muni, ano, modo)
) %>%
  pivot_wider(names_from = modo, values_from = n)
