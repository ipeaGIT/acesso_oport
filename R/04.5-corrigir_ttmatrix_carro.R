source('./R/fun/setup.R')


# sigla_muni <- "for"
# sigla_muni <- "bsb"

organizar_matriz_carro <- function(sigla_muni) {
  
  # abrir matrizes de carro
  ttmatrix_carro <- fread(sprintf("../../data/acesso_oport/output_ttmatrix_ti/OD_TI_%s.csv", sigla_muni))
  
  # matriz de base para verificar colunas e variaveis
  # ttmatrix_teste <- fread("E:/data/output_ttmatrix/2017/r5/ttmatrix_2017_for_r5.csv", nrow = 100)
  
  # padronizar o nome das colunas
  setnames(ttmatrix_carro, c("origin", "destination", "1", "0"))
  
  # from wide to long
  ttmatrix_carro <- melt(ttmatrix_carro, 
                          # id.vars = c("1", "0"),
                          measure.vars = c("0", "1"),
                          variable.name = "pico",
                          value.name = "travel_time"
  )
  # pivot_longer(matches("1|0"),
  #              names_to = "pico",
  #              values_to = "travel_time") %>%
  
  
  # identificar cidade e ano
  ttmatrix_carro[, city := sigla_muni]
  ttmatrix_carro[, ano := 2019]
  ttmatrix_carro[, mode := "car"]
  
  # Se a origem e o destino forem o mesmo, adotar o tempo de viagem como:
  # transit / walk / car: 350s equivale ao tempo necessario para cruzar um hexagono a de pe (~1 metro/sec = ~3.6 km/h)
  # ttmatrix_carro[mode == "car", travel_time := fifelse(origin == destination, 5.8, travel_time)]
  
  
  
  # save
  fwrite(ttmatrix_carro, sprintf("E:/data/ttmatrix_fixed/%s/car/ttmatrix_fixed_car_%s_%s.csv", 
                                    "2019", "2019", sigla_muni))
  # write_rds(ttmatrix_carro, sprintf("E:/data/ttmatrix_fixed/%s/car/ttmatrix_fixed_car_%s_%s.rds", 
  #                                   "2019", "2019", sigla_muni),
  #           compress = "gz")
  
  # 
  # # calcular acess
  # ui <- ttmatrix_carro[pico == 1]
  # ui[, opp := 1]
  # acess <- ui[, .(acess = sum(opp[which(travel_time <= 30)], na.rm = TRUE)),
  #             by = .(origin, ano)]
  # 
  # hex <- read_rds(sprintf("../../data/acesso_oport/hex_agregados/2017/hex_agregado_%s_09_2017.rds", sigla_muni))
  # 
  # 
  # acess_sf <- left_join(acess, hex %>% select(id_hex),
  #                       by = c("origin" = "id_hex")) %>%
  #   st_sf(crs = 4326)
  # 
  # ggplot()+
  #   geom_sf(data = acess_sf, aes(fill = acess), color = NA)+
  #   scale_fill_viridis_c(direction = 1)+
  #   theme_void()
  # 
  # ggsave(filename = sprintf("testes/acess_carro/acess_carro_%s.png", sigla_muni),
  #        width = 12,
  #        height = 10,
  #        units = "cm")
  
  # rm(ttmatrix_carro)
  # rm(ui)
  # rm(acess)
  # rm(acess_sf)
  
  
}



plan(multiprocess, workers = 5)
# big boys first
organizar_matriz_carro("spo")
organizar_matriz_carro("bsb")
organizar_matriz_carro("rio")
organizar_matriz_carro("goi")
# others
furrr::future_walk(c("for", "cur","poa","bho",
                     "sal","man","rec","bel",
                     "gua","cam","slz","sgo","mac",
                     "duq","cgr", 'nat'),
                   organizar_matriz_carro)

