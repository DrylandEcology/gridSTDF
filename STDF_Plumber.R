# web interface
library(plumber)

#r <- plumb('functions/simulationFunctions.R')
r <- plumb('Git/shorttermdroughtforecaster/functions/simulationFunctions.R')

r$run(host = "0.0.0.0",port = 8080) # https://www.r-bloggers.com/hosting-a-plumber-api-on-aws/
r$run(port = 8080)

#https://www.rplumber.io

#curl 10.12.7.42:8080/gatherDataAndExecuteSW?"lat= 43.3737&lng=-111.58&soils=2&comp=2"
#curl http://127.0.0.1:8080/gatherDataAndExecuteSW?"lat=43.3737&lng=-111.588&soils=2&comp=1&sand=50&clay=15" > data.json
