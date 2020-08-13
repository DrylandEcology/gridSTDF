library(ggplot2)
library(lubridate)

# Make a figure that is 18 months of data, 6 in the past, 12 in the future.
# 1 is long-term means 
# 2 is  anomalies
# Smoothed lines for everything!
## rolling means for temp and VWC
## rolling sums for ppy
# Only show daily data for observed past!

######################################################################################
              ################# Data organization #################
######################################################################################

# Master Dataset --------------------------------------------------------------------
HistTemp <- HistDataNormMean_18MNs[,c('Date', 'avgC_rollmean.med', 'avgC_rollmean.10', 'avgC_rollmean.90')]
names(HistTemp)[2:4] <- paste0('Hist.', names(HistTemp)[2:4])

FutTemp <- AnomRunStats[,c('Date', 'avg_C.mean.med', 'avgC_rollmean.med', 'avgC_rollmean.10', 'avgC_rollmean.90')]
names(FutTemp)[2:5] <- paste0('Fut.', names(FutTemp)[2:5])
FutTemp$Date <- as.Date(FutTemp$Date)

TempDF <- merge(HistTemp, FutTemp) 
TempDF$Time <- ifelse(TempDF$Date < Sys.Date(), 'Observed', 'Future')

# ------------------------------------------------------------------------------------
# eliminate data two weeks after current not in the position to make statements there
TempDF[TempDF$Date %in% c((currDate-15):(currDate + 15)),
       c('Fut.avgC_rollmean.med', 'Fut.avgC_rollmean.10', 'Fut.avgC_rollmean.90')] <-NA

# Differences between future and historical
TempDF$Diffs.Med <- TempDF$Fut.avgC_rollmean.med - TempDF$Hist.avgC_rollmean.med 
TempDF$Diffs.10 <- TempDF$Fut.avgC_rollmean.10 - TempDF$Hist.avgC_rollmean.med 
TempDF$Diffs.90 <- TempDF$Fut.avgC_rollmean.90 - TempDF$Hist.avgC_rollmean.med 

# Historical diffs. - gray shaded area
TempDF$Hist.avg_C.rollmean.90.diff <- TempDF$Hist.avgC_rollmean.90 - TempDF$Hist.avgC_rollmean.med
TempDF$Hist.avg_C.rollmean.10.diff <- TempDF$Hist.avgC_rollmean.10 - TempDF$Hist.avgC_rollmean.med

# break things up by postive and negative
TempDF$Type <- ifelse(TempDF$Diffs.Med > 0, 'pos', 'neg')

######################################################################################
                    ################# Plotting Begins #################
######################################################################################

# Panel 1 ----------------------------------------------------------------------------
TempDF$Fut.avg_C.mean.med <- ifelse(TempDF$Time == 'Future', NA, TempDF$Fut.avg_C.mean.med)
TempDF$Diffs.10 <- ifelse(TempDF$Time == 'Observed', NA, TempDF$Diffs.10)
TempDF$Diffs.90 <- ifelse(TempDF$Time == 'Observed', NA, TempDF$Diffs.90)

cols <- c('Clim. Normals' = 'black', 'Future' = 'darkcyan', 'Recent Past' = 'goldenrod')

Panel1 <- ggplot(TempDF) +
  # repeating long-term historical daily median
  geom_ribbon( aes(x = Date, y = Hist.avgC_rollmean.med, 
                   ymin = Hist.avgC_rollmean.10, ymax = Hist.avgC_rollmean.90,  alpha=0.1), fill = 'lightgrey') +
  geom_line(aes(Date, Hist.avgC_rollmean.med,  color = 'Historical')) +
  
  # future quantiles
  geom_line(aes(Date, Fut.avgC_rollmean.10), color = 'darkcyan')  + 
  geom_line(aes(Date, Fut.avgC_rollmean.90), color = 'darkcyan')  +
  
  # observed past dailys
  geom_line(aes(Date, Fut.avg_C.mean.med, color = Time), size = .3) +
  
  # observed and future median
  geom_line(aes(Date, Fut.avgC_rollmean.med, color = Time), size = 1.1) +
  
  # theme
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal") +
  guides(alpha = FALSE) +
  scale_color_manual(name = '', values = c('darkcyan', 'black', 'darkgoldenrod3')) +
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
  labs(y = 'temperature (°C)')

suppressWarnings(plot(Panel1))

ggsave('figureCode/presentationFigs/savedFigs/TEMP_ObservedAndFuture_Median_Quantiles.png', height = 4, width = 8)

# Panel 2 ----------------------------------------------------------------------------
Panel2 <- ggplot(TempDF) + 
  
  # 0 line and 10 - 90% for the historical -----
  geom_ribbon(aes(Date, ymin = Hist.avg_C.rollmean.10.diff, 
                  ymax = Hist.avg_C.rollmean.90.diff, alpha=0.01), fill = 'lightgrey') +
  geom_line(aes(Date, Hist.avg_C.rollmean.10.diff), size = .1, color = 'black') +
  geom_line(aes(Date,  Hist.avg_C.rollmean.90.diff), size = .1, color = 'black') +
  
  # Observed Differences from past as bars -----
  geom_bar(aes(Date, Diffs.Med, fill = Type), stat = "identity", size = .1) +
  
  # thick line of differences
  geom_line(aes(Date, Diffs.Med, color = Time), size = .8) +
  
  # 10 and 90 future diffs
  geom_line(aes(Date, Diffs.10, color = Time))  + 
  geom_line(aes(Date, Diffs.90, color = Time))  +
  
# theme
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  geom_hline(yintercept = 0) +
  
  theme_bw() + theme(legend.position = "none")  +
  scale_fill_manual(values = c('blue', 'red')) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
  scale_color_manual(values = c('darkcyan', 'darkgoldenrod3')) +
  
  labs(y = 'temperature diffs (°C)')

suppressWarnings(plot(Panel2))
ggsave('figureCode/presentationFigs/savedFigs/TEMP_ObservedAndFuture_Median_DIFFERENCES.png', height = 4, width = 8)

Panel2Anoms <- Panel2 + 
  # Generated Anoms
  geom_boxplot(data = MonthlyAnoms, aes(Date, tempAnom, group = Month), fatten = NULL, width = 7,
               alpha = 0.8, outlier.size = -1) +
  stat_summary(data = MonthlyAnoms, aes(Date, tempAnom, group = Month, ymax = ..y.., ymin = ..y..),
               fun.y = mean, geom = "errorbar", size = 1.2, color = 'limegreen') +
  #geom_point(data = MonthlyAnoms, aes(Date, tempAnom, group = Month), shape = 21, size =.5, fill = NA) +
  # NWS dots and lines
  stat_summary(data = NWSAnomsAll1, aes(Date, Anom_C, ymax = ..y.., ymin = ..y..),
               fun.y = mean, geom = "errorbar", size = 1.2, color = 'purple') +
  geom_pointrange(data = NWSAnomsAll1, aes(Date, Anom_C,
                                               ymin = Anom_C - ForecastedSD_Temp_C,
                                               ymax = Anom_C + ForecastedSD_Temp_C), shape = 21, fill = 'black', color = 'magenta') 
suppressWarnings(plot(Panel2Anoms))
  