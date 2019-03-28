# FUNCAO PARA CONSTRUIR GRAPH (SO RODAR UMA VEZ!) -------------------------

construir_graph <- function(cidade) {
  
  # Os arquivos de gtfs e .obj devem estar na pasta "cidade"
  
  otp_build_graph(otp = "../otp/programs/otp.jar", dir = "../otp", router = cidade) 
  
}






# FUNCAO PARA LIGAR SERVIDOR DO OTP DA CIDADE -----------------------------



ligar_servidor <- function(cidade) {
  
  otp_setup(otp = "../otp/programs/otp.jar", dir = "../otp", router = cidade)
  otp_for <- otp_connect(router = cidade)
  
}
