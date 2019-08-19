

linhas <- st_read("C:\\Users\\b2003009140218\\Downloads\\2017-20190411T165748Z-001\\2017", 
                  options = "ENCODING=ISO-UTF-8") %>%
  st_transform(4326) %>%
  filter(Cidade %in% c("Fortaleza", "Belo Horizonte", "Rio de Janeiro")) %>%
  select(Cidade, Modo, Corredor)

st_write(linhas, "junk/shape.shp")

readr::write_rds(linhas, "../data/linhas_HMcapacidade/linhas_HMcapacidade.rds")

  
