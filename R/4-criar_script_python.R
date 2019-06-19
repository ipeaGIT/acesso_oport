criar_script_python <- function(municipio, data, res) {
  
  
  
  # Start writing to an output file
  output_file <- sprintf("../otp/py/otp_%s_%s_%s.py", municipio, data, res)
  
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
sprintf("otp = OtpsEntryPoint.fromArgs(['--graphs', 'graphs', '--router', '%s'])", municipio),
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
sprintf("router = otp.getRouter('%s')", municipio), 
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
dests = otp.loadCSVPopulation('points/points_%s_%s.csv', 'Y', 'X')", municipio, res, municipio, res), 
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
sprintf("matrixCsv.save('../data/output_ttmatrix/traveltime_matrix_%s_%s.csv')", municipio, res),
" 
", sep = "\n")
    
  cat(
"# Stop timing the code
print(\"Elapsed time was %g seconds\" % (time.time() - start_time))")
    
  
    # Stop writing to the file
    sink()
  
    
}
  
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################


# CRIAR SCRIPT PYTHON PARALELO ALL MODES! ---------------------------------


criar_script_python_paral_modes <- function(municipio, data, res, from = 7, until = 9, every = 30, 
                                            time_threshold = 7200) {
  
  
  
  # Start writing to an output file
  output_file <- sprintf("../otp/py/otp_%s_%s_%s_paral_allmodes.py", municipio, data, res)
  
  sink(output_file)
  
  ano <- substr(data, 1, 4)
  mes <- substr(data, 6, 7)
  dia <- substr(data, 9, 10)
  

# COMEÇA AQUI O SCRIPT ----------------------------------------------------

  
  
  
  cat(
  "# Start timing the code",
  "import time",
  "start_time = time.time()",  
  "",
  "# INPUT ###################################################################################################",
  "",
  "# max number of threads to use in parallel",
  "max_threads = 20",
  "",
  "# Trips",
  sprintf("fromm = %s             # departure time start", from),
  sprintf("until = %s            # departure time end", until),
  sprintf("every = %s            # frequency (every 30 minutes)", every),
  sprintf("time_threshold = %s # Max travel time in seconds | 1h = 3600 seconds , 2h = 7200 seconds", time_threshold),
  "",
  "# date of trips",
  sprintf("year= %s", ano),
  sprintf("month = %s", mes),
  sprintf("day = %s", dia),
  sprintf("city = '%s'", municipio), 
  "",
  "###################################################################################################",
  "",
  "# import garbage collector (celan RAM memory)",
  "import gc",
  "gc.collect()",
  "",
  "# THREADED VERSION OF OTP SCRIPT",
  "import threading",
  "from time import sleep",
  "",
  "#!/usr/bin/jython",
  "from org.opentripplanner.scripting.api import OtpsEntryPoint",
  "",
  "# Instantiate an OtpsEntryPoint",
  sprintf("otp = OtpsEntryPoint.fromArgs(['--graphs', 'graphs', '--router', '%s'])", municipio),
  "",
  "# Get the default router",
  sprintf("router = otp.getRouter('%s')", municipio),
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "###   Public Transport   Public Transport",   
  "###   Public Transport   Public Transport",   
  "###   Public Transport   Public Transport",
  "",
  "# make a list of jobs to do",
  "jobs = []",
  "for h in range(fromm, until):",
  "  for m in range(0,60,every):",
  "    jobs.append((h,m))",
  "",
  "# define a function describing a complete job",
  "# I just copy-pasted what you had in the loop into here",
  "def do_the_stuff(h,m):",
  "", 
  "  # Read Points of Destination - The file points.csv contains the columns GEOID, X and Y [inside]",
  sprintf("  points = otp.loadCSVPopulation('points/points_%s_%s.csv', 'Y', 'X')", municipio, res),
  sprintf("  dests = otp.loadCSVPopulation('points/points_%s_%s.csv', 'Y', 'X')", municipio, res),
  "",  
  "  # Create a default request for a given time",
  "  req = otp.createRequest()",
  "  req.setDateTime(year, month, day, h, m, 00)",
  "  req.setMaxTimeSec( time_threshold )",
  "  req.setModes('WALK,TRANSIT,BUS,TRAM,RAIL,SUBWAY')",
  "",  
  "  # Create a CSV output",
  "  matrixCsv = otp.createCSVOutput()",
  "  matrixCsv.setHeader([ 'city', 'mode', 'depart_time', 'origin', 'destination', 'walk_distance', 'travel_time' ])",
  "",
  "  # Start Loop",
  "  for origin in points:",
  "    print \"Processing sto Transit: \", str(h)+\"-\"+str(m),\" \", origin.getStringData('id_hex'), 'on ',threading.current_thread()",
  "    req.setOrigin(origin)",
  "    spt = router.plan(req)",
  "    if spt is None: continue",
  "",
  "    # Evaluate the SPT for all points",
  "    result = spt.eval(dests)",
  "",
  "    # Add a new row of result in the CSV output",
  "    for r in result:",
  "      matrixCsv.addRow([ city, 'transit', str(h) + \":\" + str(m) + \":00\", origin.getStringData('id_hex'), r.getIndividual().getStringData('id_hex'), r.getWalkDistance() , r.getTime()])",
  "",
  "  # Save the result",
  sprintf("  matrixCsv.save('ttmatrix_%s_pt_%s_'+ str(h)+\"-\"+str(m) + '.csv')", municipio, res),
  "",
  "# ^ that ^ function has to be defined before it's called",
  "# the threading bit is down here vvv",
  "",
  "# how many threads do you want?",
  "#max_threads = int(raw_input('max threads (int) ? --> '))",
  "# start looping over jobs",
  "while len(jobs) > 0:",
  "  if threading.active_count() < max_threads + 1:",
  "    h,m = jobs.pop()",
  "    thread = threading.Thread(target=do_the_stuff,args=(h,m))",
  "    #		thread.daemon = True",
  "    thread.start()",
  "  else:",
  "    sleep(0.1)",
  "# now wait for all daemon threads to end before letting",
  "# the main thread die. Otherwise stuff will get cut off",
  "# before it's finished",
  "while threading.active_count() > 1:",
  " sleep(0.1)",
  "print 'ALL JOBS COMPLETED!'",
  "###############################################################",
  "",
  "",
  "",
  "",
  "########################################################################################################",
  "##### sINGLE DEPARTURE TIME #####",
  "##########################################################################################################",
  "##########################################################################################################",
  "##########################################################################################################",
  "##########################################################################################################",
  "",
  "gc.collect()",
  "",
  "",
  "# Read Points of Destination - The file points.csv contains the columns GEOID, X and Y.",
  sprintf("points = otp.loadCSVPopulation('points/points_%s_%s.csv', 'Y', 'X')", municipio, res),
  sprintf("dests = otp.loadCSVPopulation('points/points_%s_%s.csv', 'Y', 'X')", municipio, res),
  "",
  "",
  "",
  "",
  "###   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   ###",
  "###   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   ###",
  "###   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   WALK   ###",
  "",
  "",
  "",
  "for h in range(10, 11):",
  "  for m in range(0,60, 60):",
  "",
  "    # Create a default request for a given time",
  "    req = otp.createRequest()",
  "    req.setDateTime(year, month, day, h, m, 00)",
  "    req.setMaxTimeSec(time_threshold)", 
  "    req.setModes('WALK')",
  "",
  "",
  "    # Create a CSV output",
  "    matrixCsv = otp.createCSVOutput()",
  "    matrixCsv.setHeader([ 'city', 'mode', 'depart_time', 'origin', 'destination', 'distance', 'travel_time' ]) # travel_time in seconds",
  "",
  "    # Start Loop",
  "    for origin in points:",
  "      print \"Processing sto WALK\", str(h)+\"-\"+str(m),\" \", origin.getStringData('id_hex')",
  "      req.setOrigin(origin)",
  "      spt = router.plan(req)",
  "      if spt is None: continue",
  "",
  "      # Evaluate the SPT for all points",
  "      result = spt.eval(dests)",
  "",
  "      # Add a new row of result in the CSV output",
  "      for r in result:",
  "        matrixCsv.addRow([ city, 'walk', str(h) + \":\" + str(m) + \":00\", origin.getStringData('id_hex'), r.getIndividual().getStringData('id_hex'), r.getWalkDistance() , r.getTime()])",
  "",
  "    # Save the result",
  sprintf("    matrixCsv.save('ttmatrix_%s_walk_%s.csv')", municipio, res),
  "    gc.collect()",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "###   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   ###",
  "###   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   ###",
  "###   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   BIKE   ###",
  "",
  "",
  "",
  "for h in range(10, 11):",
  "  for m in range(0,60, 60):",
  "",
  "    # Create a default request for a given time",
  "    req = otp.createRequest()",
  "    req.setDateTime(year, month, day, h, m, 00)",
  "    req.setMaxTimeSec(time_threshold)", 
  "    req.setModes('BICYCLE')",
  "",
  "",
  "    # Create a CSV output",
  "    matrixCsv = otp.createCSVOutput()",
  "    matrixCsv.setHeader([ 'city', 'mode', 'depart_time', 'origin', 'destination', 'distance', 'travel_time' ]) # travel_time in seconds",
  "",
  "    # Start Loop",
  "    for origin in points:",
  "      print \"Processing sto bike\", str(h)+\"-\"+str(m),\" \", origin.getStringData('id_hex')",
  "      req.setOrigin(origin)",
  "      spt = router.plan(req)",
  "      if spt is None: continue",
  "",
  "      # Evaluate the SPT for all points",
  "      result = spt.eval(dests)",
  "",
  "      # Add a new row of result in the CSV output",
  "      for r in result:",
  "        matrixCsv.addRow([ city, 'bike', str(h) + \":\" + str(m) + \":00\", origin.getStringData('id_hex'), r.getIndividual().getStringData('id_hex'), r.getWalkDistance() , r.getTime()])",
  "",
  "    # Save the result",
  sprintf("    matrixCsv.save('ttmatrix_%s_bike_%s.csv')", municipio, res),
  "    gc.collect()",
  "",
  "",
  "",
  "#################################################",
  "",
  "# Stop timing the code",
  "print 'ALL JOBS COMPLETED!'",
  "print(\"Elapsed time was %g seconds\" % (time.time() - start_time))",
  
  
  
  sep = "\n")
  
    # Stop writing to the file
    sink()
  
    
}


# criar_script_python_paral_modes("for", "2018-12-05", "07", 7, 9)

