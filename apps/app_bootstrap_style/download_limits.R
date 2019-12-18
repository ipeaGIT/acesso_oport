# get city centroids

source("../../R/fun/setup.R")

limits <- lapply(munis_df$code_muni, geobr::read_municipality)

limits1 <- do.call(rbind, limits) %>%
  st_sf(crs = 4326) %>%
  st_centroid() %>%
  sfc_as_cols()

# save
write_rds(limits1, "cities_centroids.rds")
