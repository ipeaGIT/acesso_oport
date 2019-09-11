# FUNCAO PARA CONSTRUIR GRAPH (SO RODAR UMA VEZ!) -------------------------

construir_graph <- function(cidade) {
  
  # Os arquivos de gtfs e .obj devem estar na pasta "cidade"
  
  otp_build_graph(otp = "../otp/programs/otp.jar", dir = "../otp", router = cidade, memory = 6) 
  
}
 





# FUNCAO PARA LIGAR SERVIDOR DO OTP DA CIDADE -----------------------------



ligar_servidor <- function(cidade) {
  
  if (Sys.info()[1] == "Linux") {
    
    command <- sprintf("java -Xmx4G -jar ../otp/programs/otp.jar --router %s --graphs ../otp/graphs --server", cidade)
    
    system(command, intern = FALSE, wait = FALSE)
    otp_for <- otp_connect(router = cidade)
    
  } else {
    
  otp_setup(otp = "../otp/programs/otp.jar", dir = "../otp", router = cidade)
  otp_for <- otp_connect(router = cidade)
  
  }
}
