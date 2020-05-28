library(ggplot2)
library(lubridate)
currMonth <- month(Sys.Date())

# Make a figure that is 18 months of data, 6 in the past, 12 in the future.
# 1 is long-term means 
# 2 is  anomalies
# Smoothed lines for everything!
## rolling means for temp and VWC
## rolling sums for ppy
# Only show daily data for observed past!

################ Read in Data
HistDataNormMean <- fread('ExampleData/HistDataNormMean.csv')
AnomRunStats <- fread('ExampleData/AnomRunStats.csv')
AnomSave <- fread('ExampleData/AnomSave.csv')

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
HistDataNormMean_18MNs <- HistDataNormMean_18MNs[HistDataNormMean_18MNs$Date < as.Date(paste0('2021-','06','-01')),] # 06 should be currMonth

# Inspect
# names(HistDataNormMean_18MNs)
# ggplot(HistDataNormMean_18MNs, aes(Date, avgC_rollmean.med)) + geom_line()
# ggplot(HistDataNormMean_18MNs, aes(Date, ppt_rollsum.med)) + geom_line()
# ggplot(HistDataNormMean_18MNs, aes(Date, VWCInter_rollmean.med)) + geom_line()

# Create data.frame with necessary info
HistTemp <- HistDataNormMean_18MNs[,c('Date', 'avgC_rollmean.med', 'avgC_rollmean.10', 'avgC_rollmean.90')]
names(HistTemp)[2:4] <- paste0('Hist.', names(HistTemp)[2:4])

FutTemp <- AnomRunStats[,c('Date', 'avg_C.med', 'avgC_rollmean.med', 'avgC_rollmean.10', 'avgC_rollmean.90')]
names(FutTemp)[2:5] <- paste0('Fut.', names(FutTemp)[2:5])
FutTemp$Date <- as.Date(FutTemp$Date)

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


#ggsave('~/Desktop/CDI_2019/Figures/ObservedAndFuture_TEMP_Median_Quantiles.png', height = 4, width = 8)

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


ForecastedAnoms1 <- ForecastedAnoms2 <-  data.frame()
# How months and leads relate
monthLeads <- data.frame(TempMonthlyAnoms[,'LEAD'])

if(day(Sys.Date()) < 15){
  monthLeads$Month <- monthLeads$LEAD + currMonth - 1 # a lead of one in the current month before the 15th is the current month
} else {
  monthLeads$Month <- monthLeads$LEAD + currMonth # after the 15th the lead of one is the next month
}

# leads
monthLeads$lead1 <- monthLeads$LEAD
monthLeads$lead2 <-  monthLeads$LEAD - 1
monthLeads$lead3 <-  monthLeads$LEAD - 2

monthLeads[monthLeads <= 0] <- NA 
monthLeads$Month <- ifelse(monthLeads$Month > 12, monthLeads$Month - 12, monthLeads$Month)
monthLeads <- monthLeads[1:12,]

for(m in 1:12){
  leads <- c(t(monthLeads[monthLeads$Month == m, 3:5]))
  
  Anoms <- TempMonthlyAnoms[TempMonthlyAnoms$LEAD %in% leads,c('ForecastDiff', 'ForecastedSD')]
  #Convert to Celsius
  Anoms$ForecastDiff <- Anoms$ForecastDiff * (5/9)
  Anoms$ForecastedSD <- Anoms$ForecastedSD * (5/9)
  
  ForecastedAnoms1 <- rbind(ForecastedAnoms1, data.frame( m, Anoms))
  
  # Anoms <- PPTMonthlyAnoms[PPTMonthlyAnoms$LEAD %in% leads,c('ForecastDiff', 'ForecastedSD_in')]
  # ForecastedAnoms2 <- rbind(ForecastedAnoms2, data.frame( m, Anoms))
  
}

ForecastedAnoms1$Year <- ifelse(ForecastedAnoms1$m < currMonth, 2021, 2020)
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
  geom_boxplot(data = AnomSave, aes(Date, tempAnom, group = Month), width = 7, alpha = 0.8) +
  geom_point(data = AnomSave, aes(Date, tempAnom, group = Month), shape = 21, size =.5, fill = NA) +
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
#ggsave('~/Desktop/CDI_2019/Figures/ObservedAndFuture_TEMP_Median_DIFFERENCES.png', height = 4, width = 8)
