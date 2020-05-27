library(ggplot2)

# Make a figure that is 18 months of data, 6 in the past, 12 in the future.
# 1 is long-term means (future corrected against the historical generated data.)
# 2 is just anomalies
# Smoothed lines for everything!
## rolling means for ppt and VWC
## rolling sums for temp
# Only show daily data for observed past!

############### Manipulate historical data --------------------------------------------------

# fix dates so that (1) Time repearts itself 1.5 times and (2) it sees these averages as historical date . 
HistDataNormMean$Year <- ifelse(HistDataNormMean$Day < yday(Sys.Date()), 2021, 2020)
HistDataNormMean$Date <- as.Date(strptime(paste(HistDataNormMean$Year, HistDataNormMean$Day), format="%Y %j"), format="%m-%d-%Y")
HistDataNormMean <- setorder(HistDataNormMean, Date) 

#  Grab Data that is also the same as the previous 6 months
HistDataNormMean_18MNs <- HistDataNormMean[HistDataNormMean$Date > Sys.Date() + 183, ]
HistDataNormMean_18MNs$Year <- HistDataNormMean_18MNs$Year - 1
HistDataNormMean_18MNs$Date <- as.Date(strptime(paste(HistDataNormMean_18MNs$Year, HistDataNormMean_18MNs$Day), format="%Y %j"), format="%m-%d-%Y")

# Append
HistDataNormMean_18MNs <- rbind(HistDataNormMean_18MNs, HistDataNormMean)

# Go to the first day of the same month of the next year
HistDataNormMean_18MNs <- HistDataNormMean_18MNs[HistDataNormMean_18MNs$Date < '2021-05-01',]

# Inspect
names(HistDataNormMean_18MNs)
ggplot(HistDataNormMean_18MNs, aes(Date, avgC_rollmean.med)) + geom_line()
ggplot(HistDataNormMean_18MNs, aes(Date, ppt_rollsum.med)) + geom_line()
ggplot(HistDataNormMean_18MNs, aes(Date, VWCInter_rollmean.med)) + geom_line()

############## TEMPERATURE --------------------------------------------------
# Create data.frame with necessary info
HistTemp <- HistDataNormMean_18MNs[,c('Date', 'avgC_rollmean.med', 'avgC_rollmean.10', 'avgC_rollmean.90')]
names(HistTemp)[2:4] <- paste0('Hist.', names(HistTemp)[2:4])

FutTemp <- AnomRunStats[,c('Date', 'avg_C.med', 'avgC_rollmean.med', 'avgC_rollmean.10', 'avgC_rollmean.90')]
names(FutTemp)[2:5] <- paste0('Fut.', names(FutTemp)[2:5])

TempDF <- merge(FutTemp, HistTemp) 

TempDF$Time <- ifelse(TempDF$Date < Sys.Date(), 'Observed', 'Future')

# Panel 1 - absolutes ----------------------------------------------------------
# For plotting .... if the data is observed there isn't and should be no avg_C.10, 25, 75, 90 , etc.
TempDF$Fut.avg_C.med <- ifelse(TempDF$Time == 'Future', NA, TempDF$Fut.avg_C.med)

ggplot(TempDF) +
  # repeating long-term historical daily median
  geom_ribbon( aes(x = Date, y = Hist.avgC_rollmean.med, 
                  ymin = Hist.avgC_rollmean.10, ymax = Hist.avgC_rollmean.90,  alpha=0.1), fill = 'lightgrey') +
  geom_line(aes(Date, Hist.avgC_rollmean.med)) +
  
  # future quantiles
  geom_line(aes(Date, Fut.avgC_rollmean.10), color = 'darkcyan')  + 
  geom_line(aes(Date, Fut.avgC_rollmean.90), color = 'darkcyan')  +
  
  # observed past dailys
  geom_line(aes(Date, Fut.avg_C.med, color = Time), size = .3) +
  
  # observed and future median
  geom_line(aes(Date, Fut.avgC_rollmean.med, color = Time), size = 1.1) +
  
  # theme
  theme_bw() + theme(legend.position = "none") + 
  scale_color_manual(values = c('darkcyan', 'darkgoldenrod3')) +
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
  labs(y = 'temperature (°C)')

ggsave('~/Desktop/CDI_2019/Figures/ObservedAndFuture_TEMP_Median_Quantiles.png', height = 4, width = 8)

# Panel 2 - differences ----------------------------------------------------------

# Differences between future and historical
TempDF$Diffs.Med <- TempDF$Fut.avgC_rollmean.med - TempDF$Hist.avgC_rollmean.med 
TempDF$Diffs.10 <- TempDF$Fut.avgC_rollmean.10 - TempDF$Hist.avgC_rollmean.med 
TempDF$Diffs.90 <- TempDF$Fut.avgC_rollmean.90 - TempDF$Hist.avgC_rollmean.med 

# Historical diffs. - gray shaded area
TempDF$Hist.avg_C.rollmean.90.diff <- TempDF$Hist.avgC_rollmean.90 - TempDF$Hist.avgC_rollmean.med
TempDF$Hist.avg_C.rollmean.10.diff <- TempDF$Hist.avgC_rollmean.10 - TempDF$Hist.avgC_rollmean.med

# break things up
TempDF$Type <- ifelse(TempDF$Diffs.Med > 0, 'pos', 'neg')

# Put anomalies on the figure
AnomSave$Year <- ifelse(AnomSave$MN < 5, 2021, 2020)
AnomSave$Date <- as.Date(paste0(AnomSave$Year, '-' , AnomSave$MN, '-15'), format = '%Y-%m-%d')
TempMonthlyAnoms$ForecastDiff <- TempMonthlyAnoms$ForecastedMEAN - TempMonthlyAnoms$ClimatologicalMEAN
PPTMonthlyAnoms$ForecastDiff <- PPTMonthlyAnoms$ForecastedMEAN - PPTMonthlyAnoms$ClimatologicalMEAN

currMonth <- month(Sys.Date())
monthDF <- data.frame(TempMonthlyAnoms[,'LEAD'])
monthDF$Month <- monthDF$LEAD + currMonth 

#    monthDF$Month <- monthDF$LEAD + currMonth
# }
monthDF$Month <- ifelse(monthDF$Month > 12, monthDF$Month - 12, monthDF$Month)
monthDF <- monthDF[1:12,] # don't need that 13th forecast

ForecastedAnoms1 <- ForecastedAnoms2 <-  data.frame()
for(m in c(monthDF$Month)){ # for each month, m, in a year, nn
  print(m)
  leadMain <- monthDF[monthDF$Month == m, 'LEAD']
  leads <- c(leadMain, leadMain -1, leadMain - 2)
  
  if(m == currMonth) leads <- 1
  if(m == currMonth + 1) leads <- c(1, 2)
  
  Anoms <- TempMonthlyAnoms[TempMonthlyAnoms$LEAD %in% leads,c('ForecastDiff', 'ForecastedSD')]
  Anoms$ForecastDiff <- Anoms$ForecastDiff * (5/9)
  Anoms$ForecastedSD <- Anoms$ForecastedSD * (5/9)
  
  ForecastedAnoms1 <- rbind(ForecastedAnoms1, data.frame( m, Anoms))
  
  Anoms <- PPTMonthlyAnoms[PPTMonthlyAnoms$LEAD %in% leads,c('ForecastDiff', 'ForecastedSD_in')]
  ForecastedAnoms2 <- rbind(ForecastedAnoms2, data.frame( m, Anoms))
  
}

ForecastedAnoms1$Year <- ifelse(ForecastedAnoms1$m < 5, 2021, 2020)
ForecastedAnoms1$Date <- as.Date(paste0(ForecastedAnoms1$Year, '-' , ForecastedAnoms1$m, '-05'), format = '%Y-%m-%d')

ggplot(TempDF) + 
  # 0 line and 10- 90% for the historical
  geom_ribbon(aes(Date, ymin = Hist.avg_C.rollmean.10.diff, 
                  ymax = Hist.avg_C.rollmean.90.diff, alpha=0.01), fill = 'lightgrey') +
  geom_line(aes(Date, Hist.avg_C.rollmean.10.diff), size = .1, color = 'black') +
  geom_line(aes(Date,  Hist.avg_C.rollmean.90.diff), size = .1, color = 'black') +
  
  # Observed Differences from past as bars
  geom_bar(aes(Date, Diffs.Med, fill = Type), stat = "identity", size = .1) +

  # thick line of differences
  geom_line(aes(Date, Diffs.Med, color = Time), size = .8) +
  
  # 10 and 90 future diffs
  geom_line(aes(Date, Diffs.10, color = Time))  + 
  geom_line(aes(Date, Diffs.90, color = Time))  +
  
  # ANOMALIES
  geom_boxplot(data = AnomSave, aes(Date, tempAnom, group = MN), width = 7, alpha = 0.8) +
  geom_point(data = AnomSave, aes(Date, tempAnom, group = MN), shape = 21, size =.5, fill = NA) +
  geom_pointrange(data = ForecastedAnoms1, aes(Date, ForecastDiff,
                                              ymin = ForecastDiff - ForecastedSD,
                                              ymax = ForecastDiff + ForecastedSD), shape = 21, fill = 'black', color = 'magenta') +
  # theme
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  geom_hline(yintercept = 0) +
  
  theme_bw() + theme(legend.position = "none")  +
  scale_fill_manual(values = c('blue', 'red')) +
   scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
   scale_color_manual(values = c('darkcyan', 'darkgoldenrod3')) +
   
  labs(y = 'temperature diffs (°C)')
ggsave('~/Desktop/CDI_2019/Figures/ObservedAndFuture_TEMP_Median_DIFFERENCES.png', height = 4, width = 8)

# --------------------------------------------------------------------------------------------------------------------------------
###### Precipitation -----------------------------------------------------------------------------------------------

# Create data.frame with necessary info
HistPPT <- HistDataNormMean_18MNs[,c('Date','ppt_rollsum.med', 'ppt_rollsum.10', 'ppt_rollsum.90')]
names(HistPPT)[2:4] <- paste0('Hist.', names(HistPPT)[2:4])

FutPPT <- AnomRunStats[,c('Date', 'ppt.med', 'ppt_rollsum.med', 'ppt_rollsum.10', 'ppt_rollsum.90')]
names(FutPPT)[2:5] <- paste0('Fut.', names(FutPPT)[2:5])

PPTDF <- merge(FutPPT, HistPPT) 

PPTDF$Time <- ifelse(PPTDF$Date < Sys.Date(), 'Observed', 'Future')

# Panel 1 - absolutes ----------------------------------------------------------
# For plotting .... if the data is observed there isn't and should be no avg_C.10, 25, 75, 90 , etc.
PPTDF$Fut.ppt.med <- ifelse(PPTDF$Time == 'Future', NA, PPTDF$Fut.ppt.med)
PPTDF$Fut.ppt_rollsum.10 <- ifelse(PPTDF$Time == 'Observed', NA, PPTDF$Fut.ppt_rollsum.10)
PPTDF$Fut.ppt_rollsum.90 <- ifelse(PPTDF$Time == 'Observed', NA, PPTDF$Fut.ppt_rollsum.90)

ggplot(PPTDF) +
  # repeating long-term historical daily median
  geom_ribbon( aes(x = Date, y = Hist.ppt_rollsum.med, 
                   ymin = Hist.ppt_rollsum.10, ymax = Hist.ppt_rollsum.90,  alpha=0.1), fill = 'lightgrey') +
  geom_line(aes(Date, Hist.ppt_rollsum.med)) +
  
  # future quantiles
  geom_line(aes(Date, Fut.ppt_rollsum.10), color = 'darkcyan')  + 
  geom_line(aes(Date, Fut.ppt_rollsum.90), color = 'darkcyan')  +
  
  # observed past dailys
  geom_bar(aes(Date, Fut.ppt.med, color = Time), stat = 'identity',size = .3) +
  
  # observed and future median
  geom_line(aes(Date, Fut.ppt_rollsum.med, color = Time), size = 1.5) +
  
  # theme
  theme_bw() + theme(legend.position = "none") + 
  scale_color_manual(values = c('darkcyan', 'darkgoldenrod3')) +
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
  labs(y = 'precipitation (cm)')

ggsave('~/Desktop/CDI_2019/Figures/ObservedAndFuture_PPT_Median_Quantiles.png', height = 4, width = 8)

# Panel 2 - differences ----------------------------------------------------------

# Differences between future and historical
PPTDF$Diffs.Med <- PPTDF$Fut.ppt_rollsum.med - PPTDF$Hist.ppt_rollsum.med 
PPTDF$Diffs.10 <- PPTDF$Fut.ppt_rollsum.10 - PPTDF$Hist.ppt_rollsum.med 
PPTDF$Diffs.90 <- PPTDF$Fut.ppt_rollsum.90 - PPTDF$Hist.ppt_rollsum.med 

# Historical diffs. - gray shaded area
PPTDF$Hist.ppt_rollsum.90.diff <- PPTDF$Hist.ppt_rollsum.90 - PPTDF$Hist.ppt_rollsum.med
PPTDF$Hist.ppt_rollsum.10.diff <- PPTDF$Hist.ppt_rollsum.10 - PPTDF$Hist.ppt_rollsum.med

# break things up
PPTDF$Type <- ifelse(PPTDF$Diffs.Med > 0, 'pos', 'neg')

PPTDF$Diffs.10 <- ifelse(PPTDF$Time == 'Observed', NA, PPTDF$Diffs.10)
PPTDF$Diffs.90 <- ifelse(PPTDF$Time == 'Observed', NA, PPTDF$Diffs.90)

ForecastedAnoms2$Year <- ifelse(ForecastedAnoms2$m < 5, 2021, 2020)
ForecastedAnoms2$Date <- as.Date(paste0(ForecastedAnoms2$Year, '-' , ForecastedAnoms2$m, '-05'), format = '%Y-%m-%d')
ForecastedAnoms2$ForecastedSD_cm <- ForecastedAnoms2$ForecastedSD_in * 2.54

ggplot(PPTDF) + 
  # 0 line and 10- 90% for the historical
  geom_ribbon(aes(Date, ymin = Hist.ppt_rollsum.10.diff, 
                  ymax = Hist.ppt_rollsum.90.diff, alpha=0.01), fill = 'lightgrey') +
  
  # Observed Differences from past as bars
  geom_bar(aes(Date, Diffs.Med, fill = Type), stat = "identity", size = .1) +
  
  # thick line of differences
  geom_line(aes(Date, Diffs.Med, color = Time), size = 1) +
  
  # 10 and 90 future diffs
  geom_line(aes(Date, Diffs.10, color = Time))  + 
  geom_line(aes(Date, Diffs.90, color = Time))  +
  
  # ANOMALIES
  geom_boxplot(data = AnomSave, aes(Date, Anom_Predicted, group = MN), width = 7, alpha = 0.8) +
  geom_point(data = AnomSave, aes(Date, Anom_Predicted, group = MN), shape = 21, size =.5, fill = NA) +
  geom_pointrange(data = ForecastedAnoms2, aes(Date, ForecastDiff,
                                               ymin = ForecastDiff - ForecastedSD_cm,
                                               ymax = ForecastDiff + ForecastedSD_cm), shape = 21, fill = 'black', color = 'magenta') +
  
  # theme
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  geom_hline(yintercept = 0) +
  
  theme_bw() + theme(legend.position = "none")  +
  scale_fill_manual(values = c('brown4', 'forestgreen')) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
  scale_color_manual(values = c('darkcyan', 'darkgoldenrod3')) +
  
  labs(y = 'ppt diffs (cm)')

ggsave('~/Desktop/CDI_2019/Figures/ObservedAndFuture_PPT_Median_DIFFERENCES.png', height = 4, width = 8)


# --------------------------------------------------------------------------------------------------------------------------------
###### VWC -----------------------------------------------------------------------------------------------

# Create data.frame with necessary info
HistVWC <- HistDataNormMean_18MNs[,c('Date','VWCInter_rollmean.med', 'VWCInter_rollmean.10', 'VWCInter_rollmean.90')]
names(HistVWC)[2:4] <- paste0('Hist.', names(HistVWC)[2:4])

FutVWC <- AnomRunStats[,c('Date', 'Intermediate.med', 'VWCInter_rollmean.med', 'VWCInter_rollmean.10', 'VWCInter_rollmean.90')]
names(FutVWC)[2:5] <- paste0('Fut.', names(FutVWC)[2:5])

VWCDF <- merge(FutVWC, HistVWC) 

VWCDF$Time <- ifelse(VWCDF$Date < Sys.Date(), 'Observed', 'Future')

# Panel 1 - absolutes ----------------------------------------------------------
# For plotting .... if the data is observed there isn't and should be no avg_C.10, 25, 75, 90 , etc.
VWCDF$Fut.Intermediate.med <- ifelse(VWCDF$Time == 'Future', NA, VWCDF$Fut.Intermediate.med)

ggplot(VWCDF) +
  # repeating long-term historical daily median
  geom_ribbon( aes(x = Date, y = Hist.VWCInter_rollmean.med, 
                   ymin = Hist.VWCInter_rollmean.10, ymax = Hist.VWCInter_rollmean.90,  alpha=0.1), fill = 'lightgrey') +
  geom_line(aes(Date, Hist.VWCInter_rollmean.med)) +
  
  # future quantiles
  geom_line(aes(Date, Fut.VWCInter_rollmean.10), color = 'darkcyan')  + 
  geom_line(aes(Date, Fut.VWCInter_rollmean.90), color = 'darkcyan')  +
  
  # observed past dailys
  geom_line(aes(Date, Fut.Intermediate.med, color = Time), stat = 'identity',size = .3) +
  
  # observed and future median
  geom_line(aes(Date, Fut.VWCInter_rollmean.med, color = Time), size = 1.5) +
  
  # theme
  theme_bw() + theme(legend.position = "none") + 
  scale_color_manual(values = c('darkcyan', 'darkgoldenrod3')) +
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
  labs(y = 'VWC (cm/cm)')

ggsave('~/Desktop/CDI_2019/Figures/ObservedAndFuture_VWC_Median_Quantiles.png', height = 4, width = 8)

# Panel 2 - differences ----------------------------------------------------------

# Differences between future and historical
VWCDF$Diffs.Med <- VWCDF$Fut.VWCInter_rollmean.med - VWCDF$Hist.VWCInter_rollmean.med 
VWCDF$Diffs.10 <- VWCDF$Fut.VWCInter_rollmean.10 - VWCDF$Hist.VWCInter_rollmean.med 
VWCDF$Diffs.90 <- VWCDF$Fut.VWCInter_rollmean.90 - VWCDF$Hist.VWCInter_rollmean.med 

# Historical diffs. - gray shaded area
VWCDF$Hist.VWCInter_rollmean.90.diff <- VWCDF$Hist.VWCInter_rollmean.90 - VWCDF$Hist.VWCInter_rollmean.med
VWCDF$Hist.VWCInter_rollmean.10.diff <- VWCDF$Hist.VWCInter_rollmean.10 - VWCDF$Hist.VWCInter_rollmean.med

# break things up
VWCDF$Type <- ifelse(VWCDF$Diffs.Med > 0, 'pos', 'neg')


ggplot(VWCDF) + 
  # 0 line and 10- 90% for the historical
  geom_ribbon(aes(Date, ymin = Hist.VWCInter_rollmean.10.diff, 
                  ymax = Hist.VWCInter_rollmean.90.diff, alpha=0.01), fill = 'lightgrey') +
  
  # Observed Differences from past as bars
  geom_bar(aes(Date, Diffs.Med, fill = Type), stat = "identity", size = .1) +
  
  # thick line of differences
  geom_line(aes(Date, Diffs.Med, color = Time), size = 1) +
  
  # 10 and 90 future diffs
  geom_line(aes(Date, Diffs.10, color = Time))  + 
  geom_line(aes(Date, Diffs.90, color = Time))  +
  
  # theme
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  geom_hline(yintercept = 0) +
  
  theme_bw() + theme(legend.position = "none")  +
  scale_fill_manual(values = c('brown4', 'forestgreen')) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
  scale_color_manual(values = c('darkcyan', 'darkgoldenrod3')) +
  
  labs(y = 'VWC diffs (cm/cm)')

ggsave('~/Desktop/CDI_2019/Figures/ObservedAndFuture_VWC_Median_DIFFERENCES.png', height = 4, width = 8)




break


# of the 30 daily values how many are above or below the historical daily value
Hist$Day <- yday(Hist$Date)
Hist$Date <-  NULL
Hist <- unique(Hist)

head(AnomalyData3)
DailyTempAnoms <- AnomalyData3[,c('Day','avg_C','run')]
DailyTempAnoms <- plyr::join(DailyTempAnoms, Hist)
DailyTempAnoms$Greater <- DailyTempAnoms$avg_C > DailyTempAnoms$avg_C.med

DailyTempAnoms2 <- setDT(DailyTempAnoms)[,.(Greater = sum(Greater)/30, HistTemp = median(avg_C.med),
                                            FutureTemp = median(avg_C)),.(Day)]

ggplot() + 
  geom_line(data = DailyTempAnoms2, aes(x = Day, y = Greater)) +
  theme_bw() +
  geom_vline(xintercept = yday(Sys.Date()), color = 'darkorchid3') +
  geom_vline(xintercept =  yday(Sys.Date()) + 20, color = 'darkorchid3') 

ggsave('~/Desktop/CDI_2019/Figures/ObservedAndFuture_TEMP_AnomalyPercentGreater.png', height = 3, width = 8)




################## Precip ------------------------------------------------------------------------
############## PRECIP --------------------------------------------------
Dummy <- as.data.table(matrix(nrow = 1, ncol = ncol(HistDataNormMean)))
names(Dummy) <- names(HistDataNormMean)
Dummy$Date <- Sys.Date()-1
Dummy$ppt.mean <- AnomRunStats[AnomRunStats$Date == Sys.Date() - 1, 'pptsum']

HistDataNormMean <- rbind(Dummy, HistDataNormMean)

HistDataNormMean <- HistDataNormMean[,pptsum:=cumsum(ppt.mean)]
#HistDataNormMean <- HistDataNormMean[,pptsum10:=cumsum(ppt.10)]
#HistDataNormMean <- HistDataNormMean[,pptsum90:=cumsum(ppt.90)]

ggplot() +
  geom_line(data = AnomRunStats, aes(Date, pptsum)) +
  # geom_ribbon(data = AnomRunStats, aes(Date, pptsum, ymin = pptsum10, ymax = pptsum90,alpha=0.01)) +
  geom_line(data = HistDataNormMean, aes(Date, pptsum), color = 'red') +
  #  geom_ribbon(data = HistDataNormMean,  aes(Date, pptsum, ymin = pptsum10, ymax = pptsum90, alpha=0.01), fill = 'red') +
  theme_bw() + theme(legend.position = "none") + 
  geom_vline(xintercept = as.Date(Sys.time())) 

ggplot() +
  geom_line(data = AnomRunStats, aes(Date, inter)) +
  #geom_ribbon(data = AnomRunStats, aes(Date, temp, ymin = temp10, ymax = temp90,alpha=0.01)) +
  geom_line(data = HistDataNormMean, aes(Date, Intermediate.mean), color = 'red') +
  #geom_ribbon(data = HistDataNormMean,  aes(Date, avg_C.mean, ymin = avg_C.10, ymax = avg_C.90, alpha=0.01), fill = 'red') +
  theme_bw() + theme(legend.position = "none") + 
  geom_vline(xintercept = as.Date(Sys.time())) 

