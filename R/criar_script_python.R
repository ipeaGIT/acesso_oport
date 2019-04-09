criar_script_python <- function(city, data, res) {
  
  # arquivo_por_res <- function(city, data, res) {
    
  # Start writing to an output file
  output_file <- sprintf("../otp/py/python_script_%s_%s_%s.py", city, data, res)
  
  sink(output_file)
  
  ano <- substr(data, 1, 4)
  mes <- substr(data, 6, 7)
  dia <- substr(data, 9, 10)
  
  cat(
"#!/usr/bin/jython
from org.opentripplanner.scripting.api import OtpsEntryPoint
      
")
  
  cat("
# Instantiate an OtpsEntryPoint",
sprintf("otp = OtpsEntryPoint.fromArgs(['--graphs', 'graphs', '--router', '%s'])", city),
"
", sep = "\n")
  
  cat(
"# Start timing the code
import time
start_time = time.time()
  

"

)
  
  cat(
"# Get the default router",
sprintf("router = otp.getRouter('%s')", city), 
"
", sep = "\n")
  
  cat(
"# Create a default request for a given departure time
req = otp.createRequest()", sep = "\n")
  
  cat(
sprintf("req.setDateTime(%s, %s, %s, 7, 00, 00)  # set departure time", ano, mes, dia))
  
  cat(
"req.setMaxTimeSec(7200)                   # set a limit to maximum travel time (seconds)
req.setModes('WALK,BUS,RAIL')             # define transport mode
req.setMaxTimeSec(7200)                   # set a limit to maximum travel time (seconds)
# req.maxWalkDistance = 500                 # set maximum walking distance ( kilometers ?)
# req.walkSpeed = walkSpeed                 # set average walking speed ( meters ?)
# req.bikeSpeed = bikeSpeed                 # set average cycling speed (miles per hour ?)
# ?ERROR req.setSearchRadiusM(500)                 # set max snapping distance to connect trip origin to street network
    
# for more routing options, check: http://dev.opentripplanner.org/javadoc/0.19.0/org/opentripplanner/scripting/api/OtpsRoutingRequest.html
    
    
"

)
  
  cat(
"# Read Points of Destination - The file points.csv contains the columns GEOID, X and Y.",
sprintf("points = otp.loadCSVPopulation('points/points_%s_%s.csv', 'Y', 'X')
dests = otp.loadCSVPopulation('points/points_%s_%s.csv', 'Y', 'X')", city, res, city, res), 
"", sep = "\n")
  
  cat(
"# Create a CSV output
matrixCsv = otp.createCSVOutput()
matrixCsv.setHeader([ 'origin', 'destination', 'walk_distance', 'travel_time', 'boardings' ])
    
# Start Loop
for origin in points:
  print \"Processing origin: \", origin
  req.setOrigin(origin)
  spt = router.plan(req)
  if spt is None: continue
    
  # Evaluate the SPT for all points
  result = spt.eval(dests)
    
  # Add a new row of result in the CSV output
  for r in result:
    matrixCsv.addRow([ origin.getStringData('GEOID'), r.getIndividual().getStringData('GEOID'), r.getWalkDistance() , r.getTime(),  r.getBoardings() ])
    
  ")

  cat(
"# Save the result",
sprintf("matrixCsv.save('../data/output_ttmatrix/traveltime_matrix_%s_%s.csv')", city, res),
" 
", sep = "\n")
    
  cat(
"# Stop timing the code
print(\"Elapsed time was %g seconds\" % (time.time() - start_time))")
    
  
    # Stop writing to the file
    sink()
  
    
}
  


# big_teste("for", "2015-02-02", '08')
