# # Run 1
HistDataNormMean <- HistDataAll[HistDataAll$Year %in% 1981:2010, ]
HistDataNormMean$Year <- NULL
HistDataNormMean <- setnames(setDT(HistDataNormMean)[ ,sapply(.SD, function(x) list(med=median(x),
                                                                                     x10=quantile(x, .1, na.rm = TRUE),
                                                                                     x90 = quantile(x, .9, na.rm = TRUE))),
                                                       .(Day)],
                              c('Day', sapply(names(HistDataNormMean)[-c(2)], paste0, c(".med", ".10", ".90"))))# get all means and sds!!!
#  -------------------------------------------------------------------------------------------------

# Run 2 - with future anomaly data
AllOut <- AnomalyData1[[1]]
MonthlyAnoms <- AnomalyData1[[2]]

#FutureData <- AllOut[AllOut$Date > c(Sys.Date()), ]

AllOut <- setorder(AllOut, run, Date)
AllOut$run <- AllOut$Year <- AllOut$Day <- NULL 

AnomRunStats <- setnames(setDT(AllOut)[, sapply(.SD, function(x) list(med=median(x), 
                                                                             x10=quantile(x, .1, na.rm = TRUE), 
                                                                             x90 = quantile(x, .9, na.rm = TRUE))),
                                              .(Date)],
                        c('Date', sapply(names(AllOut)[-c(11)], paste0, c(".med", ".10", '.90'))))# get all means and sds!!!


AnomRunStats <- AnomRunStats[AnomRunStats$Date > Sys.Date() - 183, ] # 6 month lead ins 
AnomRunStats$Time <- ifelse(AnomRunStats$Date < Sys.Date(), 'Observed', 'Future')

# write out consolidated data ---------------------------------------------------------------
fwrite(HistDataNormMean, 'ExampleData/HistDataNormMean.csv') 
#fwrite(MonthlyAnoms, 'ExampleData/MonthlyAnoms.csv')
fwrite(AnomRunStats, 'ExampleData/AnomRunStats.csv')
