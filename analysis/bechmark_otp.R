library(microbenchmark)
library(furrr)

# BENCHMARK ---------------------------------------------------------

# usando purrr
bench_map <- microbenchmark::microbenchmark(
  otp_vai("data/hex_municipio/fortaleza/hex_fortaleza.shp", "fortaleza"), times = 4L)


# usando furrr
plan(multiprocess)

bench_futuremap <- microbenchmark::microbenchmark(
  otp_vai("data/hex_municipio/fortaleza/hex_fortaleza.shp", "fortaleza"), times = 4L)

