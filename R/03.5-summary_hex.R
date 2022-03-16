# criar tabelas com sumario das variaveis para cada uma das cidades
# e salvar numa planilha
library(data.table)
library(dplyr)
library(googlesheets4)


# abrir todos os hex e juntar
hex_files <- dir("../../data/acesso_oport/hex_agregados", full.names = TRUE, recursive = TRUE)
# tirar todo mundo 2019_old
hex_files <- hex_files[!(hex_files %like% "2019_old")]
# tirar res 08
hex_files <- hex_files[!(hex_files %like% "_08_")]


# abrir hex
hex <- lapply(hex_files, readr::read_rds) %>% rbindlist() %>% select(-geometry)


hex_summary <- hex %>%
  select(id_hex, sigla_muni, ano,
         pop_total, pop_homens, pop_mulheres,
         starts_with("cor_"),
         starts_with("idade_"),
         starts_with("empregos_"),
         starts_with("saude_"),
         starts_with("edu_"),
         starts_with("mat_"),
         starts_with("cras_")) %>%
  tidyr::pivot_longer(cols = pop_total:cras_total, names_to = "var", values_to = "values")


# refactor
hex_summary$var <- factor(hex_summary$var, levels =  c("pop_total", "pop_homens", "pop_mulheres", 
                                                       unique(hex_summary$var[stringr::str_starts(hex_summary$var, "cor_")]),
                                                       unique(hex_summary$var[stringr::str_starts(hex_summary$var, "idade_")]),
                                                       unique(hex_summary$var[stringr::str_starts(hex_summary$var, "empregos_")]),
                                                       unique(hex_summary$var[stringr::str_starts(hex_summary$var, "saude_")]),
                                                       unique(hex_summary$var[stringr::str_starts(hex_summary$var, "edu_")]),
                                                       unique(hex_summary$var[stringr::str_starts(hex_summary$var, "mat_")]),
                                                       unique(hex_summary$var[stringr::str_starts(hex_summary$var, "cras_")])),
                          ordered = TRUE)

hex_summary1 <- hex_summary %>%
  mutate(values = round(values)) %>%
  group_by(sigla_muni, ano, var) %>%
  summarise(sum = sum(values, na.rm = TRUE)) %>%
  ungroup()

hex_summary1 <- hex_summary1 %>%
  tidyr::pivot_wider(names_from = ano, values_from = "sum")

# save
googlesheets4::write_sheet(data = hex_summary1,
                           ss = "https://docs.google.com/spreadsheets/d/12crCjRAm_ZknmYKn3hNBzcFTKibhaRP76lGriz8RhYQ/edit#gid=0",
                           sheet = "summary")
