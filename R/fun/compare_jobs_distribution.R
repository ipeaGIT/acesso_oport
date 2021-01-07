# sigla_munii <- 'for'; ano1 <- 2018; ano2 <- 2019

# function by city
compare_jobs_distribution <- function(sigla_munii, ano_bottom, ano_top, corte) {
  
  # open hex files
  hex_jobs_2017 <- read_rds(sprintf("../../data/acesso_oport/rais/%s/hex_agregados_teste/hex_agregados_teste_%s_%s.rds",
                                    ano_bottom, sigla_munii, ano_bottom)) %>%
    mutate(ano_jobs = 'ano1')
  
  hex_jobs_2018 <- read_rds(sprintf("../../data/acesso_oport/rais/%s/hex_agregados_teste/hex_agregados_teste_%s_%s.rds",
                                    ano_top, sigla_munii, ano_top)) %>%
    mutate(ano_jobs = 'ano2')
  
  hex_jobs <- rbind(hex_jobs_2017, hex_jobs_2018)
  hex_jobs <- select(hex_jobs, id_hex, sigla_muni, empregos_total, ano_jobs, geometry) %>% setDT()
  
  hex_jobs_wide <- pivot_wider(hex_jobs, names_from = ano_jobs, values_from = empregos_total,
                               names_prefix = "jobs_")
  
  # compare!
  hex_jobs_wide <- hex_jobs_wide %>%
    mutate(dif1_abs = jobs_ano2 - jobs_ano1)
    # mutate(dif1_log = log(jobs_2018/jobs_2017)) %>%
    # truncate
    # mutate(dif1_abs_tc = case_when(dif1_abs < -500 ~ -500,
    #                                dif1_abs > 500 ~ 500,
    #                                TRUE ~ dif1_abs)) %>%
    # mutate(dif1_log_tc = case_when(dif1_log < -1 ~ -1,
    #                                dif1_log > 1 ~ 1,
    #                                TRUE ~ dif1_log))
  
  
  hex_jobs_wide <- hex_jobs_wide %>%
    filter(!(jobs_ano1 == 0 & jobs_ano2 == 0))
  
  # filter hex with a diffrence of more than 1000 vinc
  hex_probs <- filter(hex_jobs_wide, dif1_abs > corte) %>%
    select(sigla_muni, id_hex, dif1_abs)
  
}