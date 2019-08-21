  
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################


# CRIAR SCRIPT PYTHON PARALELO ALL MODES! ---------------------------------

# Argumentos interessantes (mais informacoes http://dev.opentripplanner.org/apidoc/1.0.0/resource_PlannerResource.html):
# waitReluctance: How much worse is waiting for a transit vehicle than being on a transit vehicle, as a multiplier. (df = 1)
# walkReluctance: A multiplier for how bad walking is, compared to being in transit for equal lengths of time. (df = 2)
# transferPenalty: An additional penalty added to boardings after the first,  roughly equivalent to seconds

criar_script_python_paral_modes <- function(cidade, data, momento, 
                                            time_threshold = 7200, max_walk_distance = 800
                                            ) {
  
  
  ano <- substr(data, 1, 4)
  mes <- substr(data, 6, 7)
  dia <- substr(data, 9, 10)
  hora <- substr(momento, 1, 2)
  minuto <- substr(momento, 4, 5)
  
  # Start writing to an output file
  output_file <- sprintf("../otp/py/%s/otp_%s_%s_%s%s_paral_allmodes.py", cidade, cidade, data, hora, minuto)
  
  sink(output_file)
  
  hora1 <- as.integer(hora)
  minuto1 <- as.integer(minuto)

# COMEÇA AQUI O SCRIPT ----------------------------------------------------

  
  
  
  cat(
  "from __future__ import print_function",
  "import os",
  "from org.opentripplanner.scripting.api import OtpsEntryPoint",
  "from time import sleep",
  "import threading",
  "import shutil",
  "import time",
  "import gc",
  "",
  "# INPUT ###################################################################################################",
  "",
  "# max number of threads to use in parallel",
  "max_threads = 16",
  "",
  "# Trips",
  "fromm = 10             # departure time start",
  "until = 14             # departure time end",
  "every = 30            # frequency (every 30 minutes)",
  sprintf("time_threshold = %s # Max travel time in seconds | 1h = 3600 seconds , 2h = 7200 seconds", time_threshold),
  sprintf("max_walk_distance = %s # Max walk distance", max_walk_distance),
  "",
  "# date of trips",
  sprintf("year= %s", ano),
  sprintf("month = %s", mes),
  sprintf("day = %s", dia),
  sprintf("city = '%s'", cidade), 
  sprintf("h = %i", hora1), 
  sprintf("m = %i", minuto1),
  "",
  "###################################################################################################",
  "",
  "# import garbage collector (celan RAM memory)",
  "gc.collect()",
  "",
  "# Start timing the code",
  "start_time = time.time()",
  "",
  "# THREADED VERSION OF OTP SCRIPT",
  "",
  "# Instantiate an OtpsEntryPoint",
  sprintf("otp = OtpsEntryPoint.fromArgs(['--graphs', 'graphs', '--router', '%s'])", cidade),
  "",
  "# Get the default router",
  sprintf("router = otp.getRouter('%s')", cidade),
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
  
  "###################################################################################################",
  "###################################################################################################",
  "",
  "gc.collect()",
  "",
  sprintf("filename = 'points_corrigidos/points_corrigido_%s_09.csv'", cidade),
  "batch_size = 128",
  "files = []",
  "try:",
  "  os.mkdir('temp_data')",
  "except:",
  "  print('folder already exists, continuing')",
  "",
  "with open(filename, 'r') as fin:",
  "  header = fin.next()",
  "  count = 0",
  "  for line in fin:",
  "    if count % 128 == 0:",
  "      fname = 'temp_data/' + str(count) + '_temp_points.csv'",
  "      files.append(fname)",
  "      try:",
  "        fout.close()",
  "      except:",
  "        pass",
  "      fout = open(fname, 'w')",
  "      fout.write(header)",
  "    fout.write(line)",
  "    count += 1",
  "  fout.close()",
  "",
  "###################################################################################################",
  "# 500 ############################################################################################",
  "",
  "",
  "# make a list of jobs to do",
  "jobs = []",
  "for points in files:",
  "  jobs.append(points)",
  "",
  "",
  "def do_the_stuff(p):",
  "",  
  "  # Read Points of Destination - The file points.csv contains the columns GEOID, X and Y [inside]",
  "  points = otp.loadCSVPopulation(p, 'Y', 'X')",
  "  dests = otp.loadCSVPopulation(filename, 'Y', 'X')",
  "",
  "  # Create a default request for a given time",
  "  req = otp.createRequest()",
  "  req.setDateTime(year, month, day, h, m, 00)",
  "  req.setMaxTimeSec(time_threshold)  # 1h = 3600 seconds , 2h = 7200 seconds",
  "  # 'WALK,TRANSIT,BUS,RAIL,SUBWAY'",
  "  req.setModes('WALK,TRANSIT,BUS,RAIL,SUBWAY,TRAM')",
  "",
  "  # Create a CSV output",
  "  matrixCsv = otp.createCSVOutput()",
  "  matrixCsv.setHeader(['city', 'year', 'depart_time', 'origin', 'destination', 'walk_distance', 'travel_time', 'boardings'])",
  "",
  "  # Start Loop",
  "  for origin in points:",
  "    print(\"Processing origin: \", str(h) + \"-\" + str(m), \" \", origin.getStringData('id_hex'), 'on ', threading.current_thread())",
  "    req.setOrigin(origin)",
  "    spt = router.plan(req)",
  "    if spt is None:",
  "      continue",
  "",
  "    # Evaluate the SPT for all points",
  "    result = spt.eval(dests)",
  "",
  "    # Add a new row of result in the CSV output",
  "    for r in result:",
  "      matrixCsv.addRow([city, 'transit', str(h) + \":\" + str(m) + \":00\", origin.getStringData('id_hex'), r.getIndividual().getStringData('id_hex'), r.getWalkDistance(), r.getTime(), r.getBoardings()])",
  "",
  "  # Save the result",
  sprintf("  matrixCsv.save('../data/output_ttmatrix/%s/ttmatrix_%s_pt_'+ str(h)+\"-\"+str(m) + p.split('/')[1])", cidade, cidade),
  "  gc.collect()",
  "",
  "",
  "# ^ that ^ function has to be defined before it's called",
  "# the threading bit is down here vvv",
  "#",
  "",
  "# how many threads do you want?",
  "#max_threads = int(raw_input('max threads (int) ? --> '))",
  "# start looping over jobs",
  "threads = []",
  "while len(jobs) > 0:",
  "  if threading.active_count() < max_threads + 1:",
  "    p = jobs.pop()",
  "    print(p)",
  "    thread = threading.Thread(target=do_the_stuff, args=(p,))",
  "    #thread.daemon = True",
  "    thread.start()",
  "    threads.append(thread)",
  "  else:",
  "    sleep(0.1)",
  "# now wait for all daemon threads to end before letting",
  "# the main thread die. Otherwise stuff will get cut off",
  "# before it's finished",
  "while threading.active_count() > 1:",
  "  sleep(0.1)",
  "print('ALL JOBS COMPLETED!')",
  "for t in threads:",
  "  t.join()",
  "",
  "print(\"Elapsed time was %g seconds\" % (time.time() - start_time))",
  "",
  "",
  "shutil.rmtree('temp_data')",
  "",
  
  
  
  sep = "\n")
  
    # Stop writing to the file
    sink()
  
    
}


criar_script_python_paral_modes("for", "2018-12-05", momento = "07:15:00")

