# carregar bibliotecas
source('./R/fun/setup.R')


# 1) Identificar e corrigir hexagonos que nao foram roteados  -----

# Sao identificados hexagos que nao foram roteados pelo streetmap 

# sigla_muni <- 'spo'
# sigla_muni <- 'poa'
# sigla_muni <- 'bel'
# sigla_muni <- 'bho'
# sigla_muni <- 'nat'
# sigla_muni <- 'man'
# sigla_muni <- 'bel'
# sigla_muni <- 'spo'
# sigla_muni <- 'for'
# sigla_muni <- 'goi'
# sigla_muni <- 'rio'
# sigla_muni <- 'cgr'

corrigir_ttmatrix <- function(sigla_muni) {
  
  # status message
  message('Woking on city ', sigla_muni)
  
  # abrir matriz original
  ttmatrix_allmodes <- fread(sprintf("E:/data/output_ttmatrix/car/OD_TI_%s.csv", sigla_muni))
  ttmatrix_allmodes <- ttmatrix_allmodes %>%
    dplyr::rename(origin = 1, destination = 2) %>%
    setDT()
  
  # abrir matriz extra
  if (sigla_muni %nin%  c('cur', 'goi')) {
    
    ttmatrix_extra <- fread(sprintf("E:/data/output_ttmatrix/car/OD_TI_%s_extra.csv", sigla_muni))
    # trazer pontos com ids
    points_int <- fread(sprintf("../../git_kaue/acesso_oport/R/pontos_extras/ids/ids_%s.csv", sigla_muni))
    ttmatrix_extra[points_int, on = c("origin_hex" = "id_hex_int"),
                   c("id_hex_origin") :=
                     list(i.id_hex)]
    ttmatrix_extra[points_int, on = c("destination_hex" = "id_hex_int"),
                   c("id_hex_destination") :=
                     list(i.id_hex)]
    ttmatrix_extra$origin_hex <- NULL
    ttmatrix_extra$destination_hex <- NULL
    ttmatrix_extra <- ttmatrix_extra %>% rename(origin = id_hex_origin, destination = id_hex_destination) %>% setDT()
    
    # juntar matriz original com matriz extra
    ttmatrix_allmodes <- rbind(ttmatrix_allmodes, ttmatrix_extra)
    
  }
  
  
  # 1) initial setup --------------------------------------------------------
  
  
  # select only necessary columns
  ttmatrix_allmodes <- ttmatrix_allmodes[, .(origin, destination,
                                             median_morning_peak, median_afternoon_offpeak)]
  ttmatrix_allmodes[, city := substr(sigla_muni, 1, 3)]
  ttmatrix_allmodes[, mode := "car"]
  
  # add parking time
  ttmatrix_allmodes[origin != destination, median_morning_peak := median_morning_peak + 2]
  ttmatrix_allmodes[origin != destination, median_afternoon_offpeak :=  median_afternoon_offpeak + 2]
  
  # abrir os pontos da resolucao 09 ~~~~
  points_file <- sprintf("../../data/acesso_oport/r5/points/%s/points_%s_09_%s.csv", c(2017:2019), sigla_muni, c(2017:2019))
  # points_file <- "../../data/avaliacao_intervencoes/r5/points/points_for_09_2019.csv"
  points <- lapply(points_file, fread) %>% rbindlist() %>% distinct(id_hex, .keep_all = TRUE) %>% setDT()
  
  
  # 2) make sure we dont have too many points -------------------------------
  ttmatrix_allmodes <- ttmatrix_allmodes[origin %in% points$id_hex]
  ttmatrix_allmodes <- ttmatrix_allmodes[destination %in% points$id_hex]
  
  
  
  
  # salvar output corrigido ---------------
  
  # split
  if (sigla_muni %in% c("bsb", "goi")) {
    
    # list origins
    origins <- unique(ttmatrix_allmodes$origin)
    # divide by 3
    a <- split(origins, rep_len(1:3, length(origins)))
    # split ttmatrix
    fwrite(ttmatrix_allmodes[origin %in% a[[1]]], sprintf("E:/data/ttmatrix_fixed/car/ttmatrix_fixed_%s_%s_origin1.csv", "2019", sigla_muni))
    fwrite(ttmatrix_allmodes[origin %in% a[[2]]], sprintf("E:/data/ttmatrix_fixed/car/ttmatrix_fixed_%s_%s_origin2.csv", "2019", sigla_muni))
    fwrite(ttmatrix_allmodes[origin %in% a[[3]]], sprintf("E:/data/ttmatrix_fixed/car/ttmatrix_fixed_%s_%s_origin3.csv", "2019", sigla_muni))
    # list dests
    dests <- unique(ttmatrix_allmodes$destination)
    # divide by 3
    a <- split(dests, rep_len(1:3, length(dests)))
    # split ttmatrix
    fwrite(ttmatrix_allmodes[destination %in% a[[1]]], sprintf("E:/data/ttmatrix_fixed/car/ttmatrix_fixed_%s_%s_dest1.csv", "2019", sigla_muni))
    fwrite(ttmatrix_allmodes[destination %in% a[[2]]], sprintf("E:/data/ttmatrix_fixed/car/ttmatrix_fixed_%s_%s_dest2.csv", "2019", sigla_muni))
    fwrite(ttmatrix_allmodes[destination %in% a[[3]]], sprintf("E:/data/ttmatrix_fixed/car/ttmatrix_fixed_%s_%s_dest3.csv", "2019", sigla_muni))
    
    
  } else {
    
    setorder(ttmatrix_allmodes, origin)
    
    fwrite(ttmatrix_allmodes, sprintf("E:/data/ttmatrix_fixed/car/ttmatrix_fixed_%s_%s.csv", "2019", sigla_muni))
    
  }
  
  rm(ttmatrix_allmodes)
  gc(TRUE)
  
}



# aplicar funcao ------------------------------------------------------------------------------
walk(c("for", "cur","poa","bho",
       "sal","rec","bel",
       "gua","cam","slz","sgo",
       "mac","duq", 'nat', 'rio', 'spo'),
     corrigir_ttmatrix)
walk(c("rio","goi","bsb", "spo", "cgr","man"),
     corrigir_ttmatrix)

# corrigir_ttmatrix("rio")
corrigir_ttmatrix("spo")

# need spliting
corrigir_ttmatrix("cgr")
corrigir_ttmatrix("bsb")
corrigir_ttmatrix("goi")
