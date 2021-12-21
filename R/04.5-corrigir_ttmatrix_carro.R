source('./R/fun/setup.R')


# sigla_muni <- "spo"

organizar_matriz_carro <- function(sigla_muni) {
  
  # abrir matrizes de carro
  ttmatrix_carro <- fread(sprintf("../../data/acesso_oport/output_ttmatrix_ti/OD_TI_%s.csv", sigla_muni))
  
  # matriz de base para verificar colunas e variaveis
  # ttmatrix_teste <- fread("E:/data/output_ttmatrix/2017/r5/ttmatrix_2017_for_r5.csv", nrow = 100)
  
  # padronizar
  ttmatrix_carro <- ttmatrix_carro %>%
    rename(origin = 1, destination = 2, "1" = median_morning_peak, "0" = median_afternoon_offpeak) %>%
    # to long
    pivot_longer(matches("1|0"),
                 names_to = "pico",
                 values_to = "travel_time") %>%
    # identificar cidade e ano
    mutate(city = sigla_muni,
           ano = 2019,
           mode = "car") %>%
    setDT()
  
  
  
  # save
  write_rds(ttmatrix_carro, sprintf("E:/data/output_ttmatrix/car/%s/ttmatrix_car_%s_%s.rds", 
                                  "2019", "2019", sigla_muni),
            compress = "gz")
  
  
  # calcular acess
  ui <- ttmatrix_carro[pico == 1]
  ui[, opp := 1]
  acess <- ui[, .(acess = sum(opp[which(travel_time <= 30)], na.rm = TRUE)),
                    by = .(origin, ano)]

  hex <- read_rds(sprintf("../../data/acesso_oport/hex_agregados/2017/hex_agregado_%s_09_2017.rds", sigla_muni))


  acess_sf <- left_join(acess, hex %>% select(id_hex),
                        by = c("origin" = "id_hex")) %>%
    st_sf(crs = 4326)

  ggplot()+
    geom_sf(data = acess_sf, aes(fill = acess), color = NA)+
    scale_fill_viridis_c(direction = 1)+
    theme_void()
  
  ggsave(filename = sprintf("testes/acess_carro/acess_carro_%s.png", sigla_muni),
         width = 12,
         height = 10,
         units = "cm")
  
  rm(ttmatrix_carro)
  rm(ui)
  rm(acess)
  rm(acess_sf)
  
  
}


organizar_matriz_carro("for")
gc(T)
organizar_matriz_carro("spo")
organizar_matriz_carro("rio")
gc(T)
organizar_matriz_carro("cur")
organizar_matriz_carro("poa")
organizar_matriz_carro("bho")
organizar_matriz_carro("bsb")
organizar_matriz_carro("sal")
organizar_matriz_carro("man")
organizar_matriz_carro("rec")
organizar_matriz_carro("goi")
organizar_matriz_carro("bel")
organizar_matriz_carro("gua")
organizar_matriz_carro("cam")
organizar_matriz_carro("slz")
organizar_matriz_carro("sgo")
organizar_matriz_carro("mac")
organizar_matriz_carro("duq")

organizar_matriz_carro("cgr")
organizar_matriz_carro("nat")
