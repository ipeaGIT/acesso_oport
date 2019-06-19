#' ## Avaliar qualidade dos resultados
#' 
#' 
## ----avaliar_qualidade_otp, eval=TRUE------------------------------------

avaliar_qualidade_otp <- function(cidade) {
  
  pattern_cidade_pt <- sprintf("ttmatrix_%s_pt_08_.*.csv$", cidade)
  pattern_cidade_ative <- sprintf("ttmatrix_%s_(walk|bike)_08.csv$", cidade)
  
  files_cidade_pt <- dir("../data/output_ttmatrix", full.names = TRUE, pattern = pattern_cidade_pt)[1]
  files_cidade_ative <- dir("../data/output_ttmatrix", full.names = TRUE, pattern = pattern_cidade_ative)
  
  otp_matrix_pt <- map_dfr(files_cidade_pt, fread)
    
  # abrir os pontos
  points_file <- sprintf("../otp/points/points_%s_08.csv", cidade)
  points <- read_csv(points_file)
  
  # checar os pontos na matrix
  origem_matrix_pt <- unique(otp_matrix_pt$origin)
  destino_matrix_pt <- unique(otp_matrix_pt$destination)
  
  # quais origens e destinos ficaram fora?
  origem_fora <- setdiff(points$id_hex, origem_matrix_pt)
  destino_fora <- setdiff(points$id_hex, destino_matrix_pt)
  
  # quais pontos ficaram fora completamente? tanto a origem como o destino
  pontos_fora <- intersect(origem_fora, destino_fora)
  
  fim <- data.frame(cidade = cidade, ponto_fora = pontos_fora) %>%
    left_join(points, by = c("ponto_fora" = "id_hex")) %>%
    group_by(cidade) %>%
    mutate(n = n()) %>%
    mutate(Percentual = n/nrow(points)) %>%
    mutate(Percentual = scales::percent(Percentual))
  
}

# Aplicar funcao
qualidade_otp <- map_dfr(c("for", "bel", "cur", "por", "rio"), avaliar_qualidade_otp)

# visualilzar os pontos que ficaram fora

# para porto alegre
qualidade_otp %>%
  dplyr::filter(cidade == "por") %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  mapview() +
  read_rds("../data/hex_municipio/hex_por_08.rds") %>% mapview()


#' 
#' 
## ----tabela_qualidade_otp, eval = TRUE-----------------------------------

qualidade_otp %>%
  mutate(Cidade = c("Fortaleza", "Belo Horizonte", "Curitiba", "Porto Alegre")) %>%
  # mutate(Percentual = color_bar("red")(Percentual)) %>%
  select(Cidade, n, Percentual) %>%
  kable() %>%
  # column_spec(3, width = "3cm") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)


#' 
#' 
#' 
## ----avaliar_qualidade_acessibilidade, eval = TRUE-----------------------

# cidade <- "bel"

avaliar_qualidade_acess <- function(cidade) {
  
  pattern_cidade_pt <- sprintf("ttmatrix_%s_pt_08_.*.csv$", cidade)
  pattern_cidade_ative <- sprintf("ttmatrix_%s_(walk|bike)_08.csv$", cidade)
  
  files_cidade_pt <- dir("../data/output_ttmatrix", full.names = TRUE, pattern = pattern_cidade_pt)
  files_cidade_ative <- dir("../data/output_ttmatrix", full.names = TRUE, pattern = pattern_cidade_ative)
  
  otp_matrix_pt <- map_dfr(files_cidade_pt, fread)
  
  # abrir oportunidades com hexagonos
  dir_hex <- sprintf("../data/hex_agregados/hex_agregado_%s_%s.rds", cidade, "08")
  hexagonos_for_sf <- read_rds(dir_hex) %>%
    select(id_hex) %>%
    ungroup()
  
  matriz_for <- otp_matrix_pt %>%
    left_join(hexagonos_for_sf, by = c("origin" = "id_hex")) %>%
    select(origin, destination, travel_time) %>%
    mutate(travel_time = travel_time/60) %>%
    mutate(empregos = 1) %>%
    dplyr::filter(travel_time < 60) %>%
    group_by(origin) %>%
    summarise(empregos = sum(empregos))
  
  access_ac_for_fim <- hexagonos_for_sf %>%
    select(id_hex) %>%
    left_join(matriz_for, by = c("id_hex" = "origin"))
  
  
  access_ac_for_fim %>%
    ggplot() +
    geom_sf(aes(fill=empregos), color="gray70") +
    scale_fill_distiller( palette="Oranges", guide = "colorbar", name="Jobs\nDensity", direction = 1) +
    theme_bw() +
    theme(legend.position = "none")
  
  # mapview(access_ac_for_fim, zcol = "empregos")
  
}

avaliar_qualidade_acess("for") +
avaliar_qualidade_acess("bel") +
avaliar_qualidade_acess("por") +
avaliar_qualidade_acess("cur") +
  plot_layout(ncol = 2)



#' 
