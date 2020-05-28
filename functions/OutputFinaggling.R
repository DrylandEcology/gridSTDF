
# # Run 1
HistDataNormMean <- HistDataAll[HistDataAll$Year %in% 1980:2009, ]
HistDataNormMean$Year <- NULL
HistDataNormMean <- setnames(setDT(HistDataNormMean)[ ,sapply(.SD, function(x) list(med=median(x),
                                                                                     x10=quantile(x, .1, na.rm = TRUE),
                                                                                     x90 = quantile(x, .9, na.rm = TRUE))),
                                                       .(Day)],
                              c('Day', sapply(names(HistDataNormMean)[-c(2)], paste0, c(".med", ".10", ".90"))))# get all means and sds!!!

fwrite(HistDataNormMean, 'ExampleData/HistDataNormMean.csv') 

# Run 2 - with future anomaly data
AnomSave <- AnomalyData1[[2]]
PPTMonthlyAnoms <- AnomalyData1[[3]]
TempMonthlyAnoms <- AnomalyData1[[4]]
AnomalyData <- AnomalyData1[[1]]

# Create Future Data
# Manipulate data dates -------------------------------------------------------------------------------------
head(AnomalyData)
#AnomalyData$Month <- month(AnomalyData$Date)
#AnomalyData$Year <- year(AnomalyData$Date)
FutureData <- AnomalyData[AnomalyData$Date > c(Sys.Date()), ]

AnomalyData <- setorder(AnomalyData, run, Date)
AnomalyData$run <- AnomalyData$Year <- AnomalyData$Day <- NULL 

names(AnomalyData)
AnomRunStats <- setnames(setDT(AnomalyData)[, sapply(.SD, function(x) list(med=median(x), 
                                                                             x10=quantile(x, .1, na.rm = TRUE), 
                                                                             x90 = quantile(x, .9, na.rm = TRUE))),
                                              .(Date)],
                        c('Date', sapply(names(AnomalyData)[-c(11)], paste0, c(".med", ".10", '.90'))))# get all means and sds!!!


AnomRunStats <- AnomRunStats[AnomRunStats$Date > Sys.Date() - 183, ] # 6 month lead ins 
AnomRunStats$Time <- ifelse(AnomRunStats$Date < Sys.Date(), 'Observed', 'Future')

fwrite(AnomSave, 'ExampleData/AnomSave.csv')
fwrite(PPTMonthlyAnomsAll, 'ExampleData/PPTMonthlyAnomsAll.csv')
fwrite(TempMonthlyAnomsAll, 'ExampleData/TempMonthlyAnomsAll.csv')
fwrite(AnomRunStats, 'ExampleData/AnomRunStats.csv')
