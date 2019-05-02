
So we have two geographies: census tracts and squares. The census tract is a geography with data collected on field and aggregated on different spatial resolutions, based on the population density of the area. Two main variables are available in the tracts: population and income. The squares are geographies that have the same size (200 x 200 meters) and contain only the population variable.

Our goal here is to aggregate the income variable from the tracts to the squares, and the weight used would not only be the area, but also the population of each square. Weighting by the square's population mainly avoid problems when you have a large census tract that may contain people living only in a small area, providing a better distribution of the tract income between the squares.

For reproducibility reasons we only included a sample of the tracts and squares that contain both situations found: a large census tract with many squares within it and smaller census tracts cutting squares in the middle.

``` r
library(sf)
library(dplyr)
library(readr)

# Files
download.file("https://github.com/ipeaGIT/acesso_oport/raw/master/test/shapes.RData", "shapes.RData")
load("shapes.RData")

# Open tracts and calculate area
tract <- tract %>%
  mutate(area_tract = st_area(.))

# Open squares and calculate area
square <- square %>%
  mutate(area_square = st_area(.))


ui <-
  # Create spatial units for all intersections between the tracts and the squares (we're calling these "piece")
  st_intersection(square, tract) %>%
  # Calculate area for each piece
  mutate(area_piece = st_area(.)) %>%
  # Compute the proportion of each tract that's inserted in that piece
  mutate(area_prop_tract = area_piece/area_tract) %>%
  # Compute the proportion of each square that's inserted in that piece
  mutate(area_prop_square =  area_piece/area_square) %>%
  # Based on the square's population, compute the population tha lives in that piece
  mutate(pop_prop_square = square_pop * area_prop_square) %>%
  # Compute the population proportion of each square that is within the tract
  group_by(id_tract) %>%
  mutate(sum = sum(pop_prop_square)) %>%
  ungroup() %>%
  # Compute population of each piece whitin the tract
  mutate(pop_prop_square_in_tract =  pop_prop_square/sum) %>%
  # Compute income within each piece
  mutate(income_piece = tract_incm* pop_prop_square_in_tract)

# Final agreggation by squares
ui_fim <- ui %>%
  # Group by squares and population and sum the income for each piece
  group_by(id_square, square_pop) %>%
  summarise(square_income = sum(income_piece, na.rm = TRUE))
```
