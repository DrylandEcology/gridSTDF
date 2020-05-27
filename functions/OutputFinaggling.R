
# Run 0
HistDataAll <- getOutputs(sw_out0)
HistDataAll <- HistDataAll[HistDataAll$Day != 366,]

# First get cum sum and rolling sum for ppt
HistDataAll <- setorder(HistDataAll, Year, Day)
HistDataAll$ppt_rollsum <- rollapply(HistDataAll$ppt,  width = 30, FUN = sum, fill = 'extend', align = 'center')# for historical data can just calc continuously 30 day sum
# ggplot() + 
#   geom_line(data = HistDataAll, aes(x = Day, y = ppt_rollsum, color = as.factor(Year))) 
#   geom_line(data =HistDataNormMean, aes(x = Day, y = ppt_rollsum.med), color = 'black') +
#   geom_ribbon(data = HistDataNormMean,aes(x = Day, ymin = ppt_rollsum.10, ymax = ppt_rollsum.90), color = 'lightgrey')

HistDataAll$avgC_rollmean <- runmean(HistDataAll$avg_C,  k = 30, endrule = 'mean', align = 'right')# for historical data can just calc continuously 30 day sum
HistDataAll$VWCShallow_rollmean <- runmean(HistDataAll$Shallow,  k = 30, endrule = 'mean', align = 'right')# for historical data can just calc continuously 30 day sum
HistDataAll$VWCInter_rollmean <- runmean(HistDataAll$Intermediate,  k = 30, endrule = 'mean', align = 'right')# for historical data can just calc continuously 30 day sum
HistDataAll$VWCDeep_rollmean <- runmean(HistDataAll$Deep,  k = 30, endrule = 'mean', align = 'right')# for historical data can just calc continuously 30 day sum

HistDataNormMean <- HistDataAll[HistDataAll$Year %in% 1980:2009, ]
HistDataNormMean <- setnames(setDT(HistDataNormMean)[ ,sapply(.SD, function(x) list(med=median(x),
                                                                                    x10=quantile(x, .1, na.rm = TRUE),
                                                                                    x90 = quantile(x, .9, na.rm = TRUE))),
                                                      .(Day)],
                             c('Day', sapply(names(HistDataNormMean)[-c(2)], paste0, c(".med", ".10", ".90"))))# get all means and sds!!!

# Run 1 - historical - weather generator data
# HistGenDataAll <- getOutputs(sw_out1)
# HistGenDataAll <- setorder(HistGenDataAll, Year, Day)
# HistGenDataAll <- setDT(HistGenDataAll)[,ppt_cumsum:=cumsum(ppt), .(Year)]
# HistGenDataAll$ppt_rollsum <- rollsum(HistGenDataAll$ppt, k = 30, fill = NA)# for historical data can just calc continuously 30 day sum
# 
# 
# HistGenData <- setnames(setDT(HistGenDataAll)[, sapply(.SD, function(x) list(med=median(x), 
#                                                                              x10=quantile(x, .1, na.rm = TRUE), 
#                                                                              x90 = quantile(x, .9, na.rm = TRUE))),
#                                               .(Day)],
#                         c('Day', sapply(names(HistGenDataAll)[-c(2)], paste0, c(".med", ".10", '.90'))))# get all means and sds!!!

# Run 2 - with future anomaly data
AnomSave <- AnomalyData1[[2]]
AnomalyData <- AnomalyData1[[1]]

# Create Future Data
# Manipulate data dates -------------------------------------------------------------------------------------
head(AnomalyData)
AnomalyData$Date <- as.Date(strptime(paste(AnomalyData$Year, AnomalyData$Day), format="%Y %j"), format="%m-%d-%Y")
AnomalyData$Month <- month(AnomalyData$Date)
AnomalyData$Year <- year(AnomalyData$Date)
AnomalyData <- AnomalyData[AnomalyData$Date < '2021-05-01',] # Eliminate data that is greater than one year from now

FutureData <- AnomalyData[AnomalyData$Date > c(Sys.Date()), ]

AnomalyData <- setorder(AnomalyData, run, Date)
AnomalyData <- AnomalyData[,c('run','Date', 'Year', 'Month','avg_C', 'ppt', 'Shallow', 'Intermediate', 'Deep')]

AnomalyData$ppt_rollsum <- rollapply(AnomalyData$ppt,  width = 30, FUN = sum, fill = 'extend', align = 'center')# for historical data can just calc continuously 30 day sum
AnomalyData$avgC_rollmean <- runmean(AnomalyData$avg_C,  k = 30, endrule = 'mean', align = 'right')# for historical data can just calc continuously 30 day sum
AnomalyData$VWCShallow_rollmean <- runmean(AnomalyData$Shallow,  k = 30, endrule = 'mean', align = 'right')# for historical data can just calc continuously 30 day sum
AnomalyData$VWCInter_rollmean <- runmean(AnomalyData$Intermediate,  k = 30, endrule = 'mean', align = 'right')# for historical data can just calc continuously 30 day sum
AnomalyData$VWCDeep_rollmean <- runmean(AnomalyData$Deep,  k = 30, endrule = 'mean', align = 'right')# for historical data can just calc continuously 30 day sum

AnomalyData$run <- NULL
AnomRunStats <- setnames(setDT(AnomalyData)[, sapply(.SD, function(x) list(med=median(x), 
                                                                             x10=quantile(x, .1, na.rm = TRUE), 
                                                                             x90 = quantile(x, .9, na.rm = TRUE))),
                                              .(Date)],
                        c('Date', sapply(names(AnomalyData)[-c(2)], paste0, c(".med", ".10", '.90'))))# get all means and sds!!!


AnomRunStats <- AnomRunStats[AnomRunStats$Date > Sys.Date() - 183, ] # 6 month lead ins 
AnomRunStats$Time <- ifelse(AnomRunStats$Date < Sys.Date(), 'Observed', 'Future')
