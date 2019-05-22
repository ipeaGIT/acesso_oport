aplicar_otp <- function(cidade, data, res = "08", all_modes = TRUE) {
  
  if (all_modes == FALSE) {
    
    py_nome <- sprintf("otp_%s_%s_%s.py", cidade, data, res) }
  
  else {
    
    py_nome <- sprintf("otp_%s_%s_%s_paral_allmodes.py", cidade, data, res)
    
  }
  
  comando <- sprintf("cd ../otp && java -jar programs/jython.jar -Dpython.path=programs/otp.jar py/%s", py_nome)
  
  shell(comando)
  
}