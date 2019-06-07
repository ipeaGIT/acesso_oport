#' ## GTFS
#' 
#' O GTFS do Rio de Janeiro apresenta algumas inconsistências no arquivo ``stop_times.txt``.
#' 
## ----gtfs----------------------------------------------------------------


# OTP RIO!!!!!!!!!1 -------------------------------------------------------

# path_otp <- "otp/programs/otp.jar" # On Linux
# 
# path_data <- "otp"
# 
# log <- otp_build_graph(otp = path_otp, dir = path_data, router = "rio",
#                        memory = 16)
# 
# 
# otpcon <- otp_connect()
# 
# 
# system("java -Xmx4G -jar \"otp/programs/otp.jar\" --build \"otp/graphs/rio")

# Error:
# Caused by: org.onebusaway.gtfs.serialization.mappings.InvalidStopTimeException: invalid stop time: 00:00:-6


# VERIFICAR O ERRO --------------------------------------------------------

stop_times <- fread("gtfs_teste/gtfs_rio_00_20171218/stop_times.txt", sep = ",") 

teste1 <- stop_times %>%
  select(arrival_time, departure_time) %>%
  filter(!grepl("\\d{2}:\\d{2}:\\d{2}", arrival_time))



# CORRIGIR O ERRO ---------------------------------------------------------


stop_times_new <- stop_times %>%
  mutate(arrival_time = ifelse(grepl("\\d{2}:\\d{2}:\\d{2}", arrival_time), arrival_time, "00:00:06")) %>%
  mutate(departure_time = ifelse(grepl("\\d{2}:\\d{2}:\\d{2}", departure_time), departure_time, "00:00:06"))


# TESTAR SE A CORREÇÃO FUNCIONOU ------------------------------------------


stop_times_new %>%
  filter(!grepl("\\d{2}:\\d{2}:\\d{2}", arrival_time))

# OK!!!!!!!!!!

# SALVAR, ENTAO! ----------------------------------------------------------


data.table::fwrite(stop_times_new, "gtfs_teste/gtfs_rio_novo/stop_times.txt", quote = TRUE)


#' 
