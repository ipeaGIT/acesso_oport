#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.1 Download de shape file de municipios e setores censitarios dos  municipios incluidos no projeto

# carregar bibliotecas
source('./R/fun/setup.R')


# 1. Funcao para download de shape file dos municipios e setores censitarios ------------------
download_muni_setores <- function(ano, munis = "all") {
  

  download_muni_setores_un <- function(sigla_muni) {
    
    
    # extract code muni
    code_munis <- munis_list$munis_metro[abrev_muni == sigla_muni & ano_metro == ano]$code_muni %>% 
      unlist()
    
    # criar pasta do municipios
    dir.create(sprintf("../../data-raw/municipios/%s", ano))  
    dir.create(sprintf("../../data-raw/setores_censitarios/%s", ano))  
    
    
    # Download de arquivos
    muni_sf <- purrr::map_dfr(code_munis, geobr::read_municipality, year=ano, simplified = F)

    ct_sf <- purrr::map_dfr(code_munis, geobr::read_census_tract, year=2010, simplified = F)


    # salvar municipios
    readr::write_rds(muni_sf, sprintf("../../data-raw/municipios/%s/municipio_%s_%s.rds", ano, sigla_muni, ano), compress = 'gz')
    
    # salvar setores censitarios
    readr::write_rds(ct_sf, sprintf("../../data-raw/setores_censitarios/%s/setores_%s_%s.rds", ano, sigla_muni, ano), compress = 'gz')
  }
  
  # 2. Aplica funcao ------------------------------------------
  if (munis == "all") {
    
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
    
  } else (x = munis)
  
  
  lapply(X=x, FUN=download_muni_setores_un)
  
  
}


download_muni_setores(ano = 2017)
download_muni_setores(ano = 2018)
download_muni_setores(ano = 2019)
download_muni_setores(ano = 2020)

