# teste R5


shell("cd ../teste_r5 && dir")


command <- "cd ../teste_r5 && java -jar r5-4.5.3.jar com.conveyal.r5.R5Main point --build graphs"

shell(command)
