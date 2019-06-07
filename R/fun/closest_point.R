closest_point <- function(df1, df2, coords = c("lon", "lat")) {

  
  df2$id_arrocha <- 1:nrow(df2)
  
  closest <- RANN::nn2(select(df2, coords[1], coords[2]), select(df1, coords[1], coords[2]), 1)
  
  arrocha <- closest$nn.idx %>% as.vector() #isola os indexs
  
  df1_v1 <- df1 %>% 
    mutate(id_arrocha = arrocha) %>% #junta a minha bilhetagem
    left_join(df2, by = c("id_arrocha"), suffix = c(".bilhetagem", ".parada"))
  #left_join(select(stops, id_parada, id_stop)) %>%
  #mutate(intervalo = floor_date(hora, "15 min"))
  
  return(df1_v1)
  
  
}