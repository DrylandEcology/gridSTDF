# This script contains figures pertaining to whether the NWS anomalies are being successfully integrated 
# and reflected into the SW runs

library(ggplot2)
library(plyr)

# Plot 1 - What are the anomolies for each month and run?
head(AnomSave)
str(AnomSave)
AnomSave$MN <- as.numeric(AnomSave$MN)
AnomSave2 <- melt(AnomSave, id.vars = 'MN')
AnomSave2$run <- rep(1:30, each = 12)

ggplot() + 
  geom_line(data = AnomSave2, aes(x = MN, y = value, color = as.factor(run))) + 
  theme_bw() + 
  facet_wrap(~ variable, ncol = 1, scales = 'free') +
  theme(legend.position = "none")
ggsave('~/Desktop/CDI_2019/Figures/AnomalyLine.png', height = 4, width = 6)

AnomSave2$MN <- factor(AnomSave2$MN, levels = c(5:12, 1:4))
ggplot() + 
  geom_boxplot(data = AnomSave2, aes(x = MN, y = value, group = MN, color = variable)) + 
  theme_bw() + 
  facet_wrap(~ variable, ncol = 1, scales = 'free') +
  theme(legend.position = "none")
ggsave('~/Desktop/CDI_2019/Figures/AnomalyBoxplots.png', height = 4, width = 6)


Mayvals2 <- AnomSave[AnomSave$MN == 5, 'tempAnom']
sum(Mayvals2 > 0)/30
hist(Mayvals2)

# Diagnostic plot ---- scatters of dailys ...

names(FutureData)
FutureData$runx <- sapply(strsplit(FutureData$run, '_'), '[', 1)
FutureData$runy <- sapply(strsplit(FutureData$run, '_'), '[', 2)
runYear <- data.frame(runy = 1:30, Year = 1980:2009)
FutureData$Year <- NULL
FutureData <- plyr::join(FutureData, runYear)

# Historical 
HistDataNormMean <- HistDataAll[HistDataAll$Year %in% 1980:2009, ]
HistDataNormMean$Date <- as.Date(strptime(paste(HistDataNormMean$Year, HistDataNormMean$Day), format="%Y %j"), format="%m-%d-%Y")
HistDataNormMean$Month <- month(HistDataNormMean$Date)
names(HistDataNormMean)[3:4] <- c('Hist.Temp', 'Hist.ppt')

DiffDaily <- plyr::join(FutureData[,c(1,4,6,13, 15)], HistDataNormMean[,c(1:4, 14)])
DiffDaily$SW_TempAnom <- DiffDaily$avg_C - DiffDaily$Hist.Temp
DiffDaily$SW_PPTAnom <- DiffDaily$ppt / DiffDaily$Hist.ppt

####
names(AnomSave)[3] <- 'Month'
AnomSave$runx <- rep(1:30, each = 12)

# join
DiffDaily <- plyr::join(DiffDaily, AnomSave)
DiffDaily <- DiffDaily[DiffDaily$Month != 5, ]
dim(DiffDaily)
Sample <- DiffDaily[sample(nrow(DiffDaily), 30000), ]
Sample <- Sample[!is.na(Sample$Month),]
# # plot
ggplot() + geom_point(data = Sample, aes(tempAnom, SW_TempAnom)) + 
  geom_abline(intercept = 0, slope = 1, color = 'red') + 
  facet_wrap(Month~., nrow = 2) +
  theme_bw() + theme(legend.position = 'bottom')
ggsave('~/Desktop/CDI_2019/Figures/DailyScatter_AnomvsSWOuts_Temp.png', width =5, height =5)

ggplot() + geom_point(data = Sample, aes(pptAnom_CF, SW_PPTAnom)) + 
  geom_abline(intercept = 0, slope = 1, color = 'red') + 
  facet_wrap(Month~., nrow = 2) +
  theme_bw() + theme(legend.position = 'bottom')
ggsave('~/Desktop/CDI_2019/Figures/DailyScatter_AnomvsSWOuts_PPT.png', width =5, height =5)


#historical monthlys to future monthly

# plot
ggplot() + geom_point(data = MonthlyScatter2, aes(tempAnom, SW_TempAnom, color = as.factor(Month))) + 
  geom_abline(intercept = 0, slope = 1, color = 'red') + 
  theme_bw() +
  theme_bw() + theme(legend.position = 'bottom')

ggsave('~/Desktop/CDI_2019/Figures/MonthlyScatter_AnomvsSWOuts_Temp2.png', width =5, height =5)

MonthlyScatterMelt <- melt(MonthlyScatter, id.vars = c('runx', 'Month', 'MN'))
MonthlyScatterMelt1 <- MonthlyScatterMelt[MonthlyScatterMelt$variable %in% c('tempAnom','SW_TempAnom'),]

ggplot(MonthlyScatterMelt1) + 
  geom_boxplot(aes(x = factor(Month), y = value, fill = variable)) + 
  theme_bw() +
  labs(x = 'Month') +
  theme(legend.position = 'bottom')

ggsave('~/Desktop/CDI_2019/Figures/MonthlyTemp_AnomaliesofCoefficients_BP2.png', height =3, width = 6)  


###### Precipitation -----------------------------------------------------------------------

ggplot() + geom_point(data = MonthlyScatter2, aes(pptAnom_CF, SW_PPTAnom, color = as.factor(Month))) + 
  geom_abline(intercept = 0, slope = 1, color = 'red') + 
  theme_bw() + theme(legend.position = 'bottom')
ggsave('~/Desktop/CDI_2019/Figures/MonthlyScatter_AnomvsSWOuts_PPT2.png', width =5, height =5)

MonthlyScatterMelt2 <- MonthlyScatterMelt[MonthlyScatterMelt$variable %in% c('pptAnom_CF','SW_PPTAnom'),]

ggplot(MonthlyScatterMelt2) + 
  geom_boxplot(aes(x = factor(Month), y = value, fill = variable)) + 
  theme_bw() +
  labs(x = 'Month') +
  theme(legend.position = 'bottom')

ggsave('~/Desktop/CDI_2019/Figures/MonthlyPPT_AnomaliesofCoefficients_BP.png', height =3, width = 6)  



# # Plot 2 - Compare Historical to Hist.GEn ----------------------------------------------------------
# head(HistDataNormMean)
# #head(HistGenData)
# HistDataNormMean$Type <- 'Historical'
# #HistGenData$Type <- 'Historical - Generated'                                              
# 
# AllData <- plyr::rbind.fill(HistDataNormMean, HistGenData)
# AllData <- setorder(AllData, Type, Day) 
# names(AllData)
# 
# ggplot(data = AllData, aes(Day, avg_C.med, ymin = avg_C.10, ymax = avg_C.90, color = Type, fill = Type)) +
#   geom_line() + 
#   geom_ribbon(alpha = .2, color = NA) + 
#   theme_bw() + 
#   theme(legend.position = 'none') +
#   labs(y ='average temp (C)')
# 
# 
# ggsave('~/Desktop/CDI_2019/Figures/HistvsHistGen_Temp.png', height = 4, width = 6)
# 
# ggplot(data = AllData, aes(Day, ppt_cumsum.med, ymin = ppt_cumsum.10, ymax = ppt_cumsum.90, color = Type)) +
#   geom_line() + 
#   geom_ribbon(alpha = .2, color = NA) +
#   theme_bw() + 
#   theme(legend.position = 'bottom') +
#   labs(y ='cumlative ppt (cm)')
# ggsave('~/Desktop/CDI_2019/Figures/HistvsHistGen_PPT.png', height = 4, width = 6)
# 
# 
# # Plot 3 compare wgen coefficients  - NoAnom and Anom
# # temp
# minNoAnom <- data.frame(temp = res2$mkv_woy$wTmin_C, Type = 'NoAnomaly', par = 'min')
# maxNoAnom <- data.frame(temp = res2$mkv_woy$wTmax_C, Type = 'NoAnomaly', par = 'max')
# minAnom <- data.frame(temp = res3$mkv_woy$wTmin_C, Type = 'Anomaly', par = 'min')
# maxAnom <- data.frame(temp = res3$mkv_woy$wTmax_C, Type = 'Anomaly', par = 'max')
# 
# all <- rbind(minNoAnom, maxNoAnom, minAnom, maxAnom)
# all$Week <- c(rep(1:53, 4))
# ggplot(all, aes(Week, temp, linetype = par, color = Type)) + 
#   geom_line() + 
#   theme_bw() +
#   theme(legend.position = 'bottom')
# ggsave('~/Desktop/CDI_2019/Figures/WeekTempCoeffsAnoms.png', height = 4, width = 6)
# 
# # ppt
# avgNoAnom <- data.frame(avg = res2$mkv_doy$PPT_avg, sd = res2$mkv_doy$PPT_sd, Type = 'NoAnomaly')
# avgAnom <- data.frame(avg = res3$mkv_doy$PPT_avg, sd = res3$mkv_doy$PPT_sd, Type = 'Anomaly')
# 
# all <- rbind(avgNoAnom, avgAnom)
# ggplot(all, aes(avg, sd, color = Type)) + 
#   geom_point(size = .5) + 
#   theme_bw() +
#   theme(legend.position = 'bottom')
# ggsave('~/Desktop/CDI_2019/Figures/DailyPPTCoeffsAnoms.png', height = 4, width = 6)
# 

# Plots 4 ----  Scatterplots of input weather gen data vs. output SW data
# Temp - do weekly (because that is how the coefficients are applied) and then monthly.

#### Weekly for temps - Comparre max and min C to the actual wgen coefficients
FutureWeeklyTemps <- setDT(FutureData)[,.(max = mean(max_C), min = mean(min_C)), .(Week, run)]
FutureWeeklyTemps <- melt(FutureWeeklyTemps, id.vars = c('Week', 'run'))
FutureWeeklyTemps$Type <- 'SW_Outs'
FutureWeeklyTemps <- dcast(FutureWeeklyTemps, Week + variable + run ~ Type)
head(FutureWeeklyTemps)

WeeklyTemps$Week <- rep(1:53, 100)
names(WeeklyTemps)[1:2] <- c('max', 'min')
WeeklyTemps <- melt(WeeklyTemps,id.vars = c('Week', 'run'))
WeeklyTemps$Type <- 'WGen'
WeeklyTemps <- dcast(WeeklyTemps, Week + variable + run ~ Type)
head(WeeklyTemps)

AllWeeklyTemps <- plyr::join(WeeklyTemps, FutureWeeklyTemps)

ggplot() +
  geom_point(data = AllWeeklyTemps, aes(SW_Outs, WGen), size = 0.01) +
  facet_wrap(.~variable, scales = 'free') + 
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, color = 'red')
ggsave('~/Desktop/CDI_2019/Figures/Temp_WGEN_SWOuts_Weekly_Scatter.png', height =3, width = 6)  

# Weekly differences from no anomaly data ---------------------------------------------------
minNoAnom <- data.frame(NoAnomaly = res2$mkv_woy$wTmin_C,  variable = 'min', Week = 1:53)
maxNoAnom <- data.frame(NoAnomaly = res2$mkv_woy$wTmax_C,  variable = 'max', Week = 1:53)

NoAnom <- rbind(minNoAnom, maxNoAnom)
AllWeeklyTemps <- plyr::join(AllWeeklyTemps, NoAnom)

AllWeeklyTemps$Diffs <- AllWeeklyTemps$WGen - AllWeeklyTemps$NoAnomaly
ggplot(AllWeeklyTemps) + 
  geom_vline(xintercept = c(19, 22), color = 'purple') +
  geom_hline(yintercept = 0, col = 'red') +
  geom_boxplot(aes(x = Week, y = Diffs, group = Week)) + 
  # geom_point(aes(x = Week, y = NoAnomaly), color = 'red') +
  facet_wrap(.~variable, nrow = 2, scales = 'free') +
  theme_bw() 

ggsave('~/Desktop/CDI_2019/Figures/WeeklyTemp_AnomaliesofCoefficients_BP.png', height =3, width = 6)  

AllWeeklyTemps$Greater <- AllWeeklyTemps$WGen > AllWeeklyTemps$NoAnomaly
AllWeeklyTemps <- setDT(AllWeeklyTemps)[,.(Greater = sum(Greater)/100),.(Week, variable)]

ggplot(AllWeeklyTemps) + 
  geom_vline(xintercept = c(19, 22), color = 'purple') +
  geom_line(aes(x = Week, y = Greater)) + 
  facet_wrap(.~variable, nrow = 2, scales = 'free') +
  theme_bw() 

######### MONTHLY DIFFERENCES --------------------------------------------------------------
# Calculate the difference between future monthlys and weather generator monthlies

FutureMonth <- setDT(FutureData)[, .(SW_Out_Temp = median(avg_C),
                                    SW_Out_PPT = sum(ppt)),
                               .(Year, Month, run)]

HistGenDataAll$Date <- as.Date(strptime(paste(HistGenDataAll$Year, HistGenDataAll$Day), format="%Y %j"), format="%m-%d-%Y")
HistGenDataAll$Month <- month(HistGenDataAll$Date)

HistMonth <- setDT(HistGenDataAll)[,. (Hist_Temp = median(avg_C),
                                         Hist_PPT = sum(ppt)),
                                     .(Month, Year)]
HistMonth <- HistMonth[,.(Hist_Temp = median(Hist_Temp), Hist_PPT = median(Hist_PPT)),.(Month)]

DiffMonth <- plyr::join(FutureMonth, HistMonth)
DiffMonth$SW_TempAnom <- DiffMonth$SW_Out_Temp - DiffMonth$Hist_Temp
DiffMonth$SW_PPTAnom <- DiffMonth$SW_Out_PPT / DiffMonth$Hist_PPT

AnomSave$Month <- AnomSave$MN
AnomSave$run <- rep(1:100, each = 12)

MonthlyScatter <- plyr::join(DiffMonth, AnomSave)
MonthlyScatter2 <- MonthlyScatter[MonthlyScatter$Month != 5, ]

ggplot() + geom_point(data = MonthlyScatter2, aes(tempAnom, SW_TempAnom)) + 
  geom_abline(intercept = 0, slope = 1, color = 'red') + 
  theme_bw()
ggsave('~/Desktop/CDI_2019/Figures/MonthlyScatter_AnomvsSWOuts_Temp.png', width =5, height =5)

MonthlyScatterMelt <- melt(MonthlyScatter, id.vars = c('Year','run', 'Month', 'MN'))
MonthlyScatterMelt1 <- MonthlyScatterMelt[MonthlyScatterMelt$variable %in% c('tempAnom','SW_TempAnom'),]

ggplot(MonthlyScatterMelt1) + 
   geom_boxplot(aes(x = factor(Month), y = value, fill = variable)) + 
  theme_bw() +
  labs(x = 'Month') +
  theme(legend.position = 'bottom')

ggsave('~/Desktop/CDI_2019/Figures/MonthlyTemp_AnomaliesofCoefficients_BP.png', height =3, width = 6)  


###### Precipitation -----------------------------------------------------------------------

ggplot() + geom_point(data = MonthlyScatter2, aes(pptAnom_CF, SW_PPTAnom)) + 
  geom_abline(intercept = 0, slope = 1, color = 'red') + 
  theme_bw()
ggsave('~/Desktop/CDI_2019/Figures/MonthlyScatter_AnomvsSWOuts_PPT.png', width =5, height =5)

MonthlyScatterMelt2 <- MonthlyScatterMelt[MonthlyScatterMelt$variable %in% c('pptAnom_CF','SW_PPTAnom'),]

ggplot(MonthlyScatterMelt2) + 
  geom_boxplot(aes(x = factor(Month), y = value, fill = variable)) + 
  theme_bw() +
  labs(x = 'Month') +
  theme(legend.position = 'bottom')

ggsave('~/Desktop/CDI_2019/Figures/MonthlyPPT_AnomaliesofCoefficients_BP.png', height =3, width = 6)  


#### Compare monthly sum of ppt from SW future to the sum values for a month in the weather generator
MonthlyPPTs$Month <- month(as.Date(MonthlyPPTs$Day,origin = '2020-01-01'))
MonthlyPPTs <- setDT(MonthlyPPTs)[,.(WGen = sum(WGen)), .(Month, run)]
MonthlyPPTsAll <- plyr::join(MonthlyScatter, MonthlyPPTs)

# Maybe this is no good to look at because the wgen sums will /always/ br larger.
MonthlyPPTsAll <-MonthlyPPTsAll[!MonthlyPPTsAll$Month == 5,]
ggplot() +
  geom_point(data = MonthlyPPTsAll, aes(SW_Out_PPT, WGen)) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, color = 'red')
ggsave('~/Desktop/CDI_2019/Figures/MonthlyPPT_WGEN_SWOuts_Scatter.png',  height =3, width = 6)  

