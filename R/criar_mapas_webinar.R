# carregar bibliotecas
source('./R/fun/setup.R')

# FIGURAS A FAZER

# 1) TMI saude de media e alta - PT  (uma cidade)- rio 

# 2) CMA empregos 60 min - PT (duas cidades) - BHe Fortaleza

# 3) CMP - Gráfico de pontos do acesso a pé  educação infantil - população negra e branca (TODAS CIDADES)

# 4) Um gráfico de pontos do acesso de bicicleta de educação média (TODAS CIDADES) - decil, 1, 10  e média

# 5) Box plot por renda - acesso a emprego de PT - 60 min


# temas

theme_for_CMA <- function(base_size) {
  
  theme_void(base_family="Roboto Condensed") %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(2,"line"),
      legend.key.height = unit(0.2,"cm"),
      legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.5)),
      plot.title = element_text(hjust = 0, vjust = 4)
      
      
    )
}

theme_for_TMI <- function(base_size) {
  
  theme_void(base_family="Roboto Condensed") %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(1,"line"),
      legend.key.height = unit(0.2,"cm"),
      legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.5)),
      plot.title = element_text(hjust = 0, vjust = 4)
      # legend.key.width=unit(0.5,"cm")
      
    )
}



# 1) TMI saude de media e alta - PT - rio  ---------------------------------------

# abrir acess rio
acess_rio <- read_rds("../data/output_access/acess_rio_2019.rds")

# tirar so pt e pico
acess_rio_pt_pico <- acess_rio %>%
  filter(mode == "transit" & pico == 1)

# fazer grafico
acess_rio %>%
  mutate(TMIEM = ifelse(TMIEM > 30, 30, TMIEM)) %>%
  ggplot()+
  geom_sf(aes(fill = TMIEM), color = NA) 




# 2) CMA empregos 60 min - PT (duas cidades) - BHe Fortaleza ----------------------------------

# abrir acess for e bho e for e tirar so empregos 60 min pico pt
acess_for <- read_rds("../data/output_access/acess_for_2019.rds") %>% 
  filter(mode == "transit" & pico == 1)

acess_bho <- read_rds("../data/output_access/acess_bho_2019.rds") %>%
  filter(mode == "transit" & pico == 1)

library(patchwork)


# fazer plots
acess_for %>%
  ggplot()+
  geom_sf(aes(fill = CMATT60), color = NA)+
  viridis::scale_fill_viridis(option = "B",
                              labels = scales::percent) +
  theme_for_CMA()+
  labs(fill = "",
       title = "") +
acess_bho %>%
  ggplot()+
  geom_sf(aes(fill = CMATT60), color = NA)+
  viridis::scale_fill_viridis(option = "B",
                              labels = scales::percent) +
  theme_for_CMA()+
  labs(fill = "",
       title = "")



# 3) CMP - Gráfico de pontos do acesso a pé educação infantil - população negra e bra --------

# abrir o acess de todas as cidades e juntar

acess_cmp_todas_walk <- lapply(dir("../data/output_access/", full.names = TRUE), read_rds) %>%
  rbindlist(fill = TRUE) %>%
  # tirar so caminhada
  filter(mode == "walk")

# tem que filtrar zonas que tenha alguma escola de edu infantil!
hex_todos <- lapply(dir("../data/hex_agregados/", full.names = TRUE, pattern = "09"), read_rds) %>%
  rbindlist() %>%
  filter(edu_infantil != 0)
  
acess_cmp_todas_edu <- acess_cmp_todas %>%
  filter(origin %in% hex_todos$id_hex) %>%
  # selecionar so acess passiva negros e brancos 15 min
  select(city, CMPPB15, CMPPN15)

# grafico!


# grafico!
acess_cmp_todas_edu %>%
  # testar algumas cidades
  # filter(city %in% c("for", "bho", "rio")) %>%
  gather(indicador, valor, CMPPB15:CMPPN15) %>%
  ggplot()+
  geom_boxplot(aes(y = valor, x = indicador)) +
  coord_flip()+
  facet_wrap(~city, scales = "free_x")

  
  


  
  
# 4) Um gráfico de pontos do acesso de bicicleta de educação média (TODAS CIDADES) - decil, 1, 10  e média ---

# abrir o acess de todas as cidades e juntar

acess_cma_todas_bike <- lapply(dir("../data/output_access/", full.names = TRUE), read_rds) %>%
  rbindlist(fill = TRUE) %>%
  # tirar so bike
  filter(mode == "bike")

# grafico!


# grafico!
library(ggalt)
library(hrbrthemes)

acess_cma_todas_bike %>%
  select(city, CMAEM30) %>%
  # ajeitar as cidade
  mutate(cidade = ifelse(city == "bel", "bho", ifelse(city == "sao", "spo", ifelse(city == "por", "poa", city)))) %>%
  mutate(cidade = factor(cidade, levels = munis_df$abrev_muni, labels = munis_df$name_muni)) %>%
  # calcular os valores
  group_by(cidade) %>%
  summarise(mean = mean(CMAEM30, na.rm = TRUE),
            P10 = quantile(CMAEM30, 0.1, na.rm = TRUE),
            P90 = quantile(CMAEM30, 0.9, na.rm = TRUE)) %>%
  mutate(cidade = forcats::fct_reorder(cidade, P90)) %>%
  ggplot()+
  geom_dumbbell(aes(x = P10, xend = P90, y = cidade), 
                size=3, color="#e3e2e1", 
                colour_x = "#5b8124", colour_xend = "#bad744",
                dot_guide=TRUE, dot_guide_size=0.25) +
  geom_point(aes(x = mean, y = cidade), shape = 18, color = "red", size = 4)+
  scale_x_percent()+
  theme_ipsum_rc(grid= "X")+
  labs(x = "")







# 5) Box plot por renda - acesso a emprego de PT - 60 min --------------------------------


# abrir acessibilidade
path_acess <- dir("../data/output_access/", 
                  full.names = TRUE, 
                  pattern = paste0(munis_df[modo == "todos"]$abrev_muni, collapse = "|"))

acess <- lapply(path_acess, read_rds) %>% rbindlist() %>%
  # pegar so TP
  filter(mode == "transit")

# # Pegar arquivo com os hexagonos com as atividades
# dir_hex <- sprintf("../data/hex_agregado_quintil/hex_agregado_quintil_%s.rds", cidade)

# # abrir oportunidades com hexagonos
# hexagonos_sf <- read_rds(dir_hex) %>%
#   st_set_geometry(NULL) %>%
#   select(id_hex, pop_total, quintil) %>%
#   ungroup()
# 
# # trazer a informacoa da renda para o hexagono na base da acessibilidade
# acess_quintil <- merge(setDT(acess), setDT(hexagonos_sf), 
#                        by.x = "origin",
#                        by.y = "id_hex",
#                        all.x = TRUE)
# 
# # tirar populacao zero
# acess_quintil <- acess_quintil[pop_total > 0]

# BOXPLOT ----------------------------------------------------------------------------------------

# title <- bquote("Distribuição da acessibilidade por"~bold(.("transporte público"))~"à"~bold(.("oportunidades de trabalho")))

baseplot2 <- theme_minimal(base_family = "Roboto Condensed") +
  theme( 
    axis.text.y  = element_text(face="bold")
    # ,axis.text.x  = element_text(face="bold")
    ,panel.grid.minor = element_blank()
    ,strip.text = element_text(size = 8, face ="bold")
    ,legend.text = element_text(size = 8)
    , legend.position = "top"
    , axis.title = element_text(size = 8)
    , axis.text.x = element_blank()
    , title = element_text(size = 9)
    , plot.margin=unit(c(2,0,0,0),"mm")
    , axis.ticks.x = element_blank()
    , axis.line.x = element_blank()
  )

acess %>%
  # # Wide to long
  # gather(threshold, acess_abs, CMA_TT_15:CMA_TT_90) %>%
  # mutate(threshold1 = as.integer(str_extract(threshold, "\\d+$"))) %>%
  # Refactor quintil
  # mutate(quintil1 = quintil - 1) %>%
  ggplot()+
  geom_boxplot(aes(x = factor(quintil.x), y = CMATT60, color = factor(quintil.x)), 
               outlier.colour=rgb(.5,.5,.5, alpha=0.2))+
  # facet_grid(threshold_name ~ ., scales = "free_y")+
  facet_wrap(~city, scales = "free")+
  scale_color_brewer(palette = "RdBu")+
  # hrbrthemes::theme_ipsum_rc() +
  labs(color = "Decil de renda",
       x = "",
       y = "Quantidade de oportunidades acessíveis",
       title = title) + 
  guides(color=guide_legend(nrow=1)) +
  baseplot2
  