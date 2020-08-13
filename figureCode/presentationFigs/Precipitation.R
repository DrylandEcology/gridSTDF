library(ggplot2)
library(lubridate)

# Make a figure that is 18 months of data, 6 in the past, 12 in the future.
# 1 is long-term means 
# 2 is  anomalies
# Smoothed lines for everything!
## rolling means for temp and VWC
## rolling sums for ppt
# Only show daily data for observed past!

######################################################################################
################# Data organization #################
######################################################################################

# Master Dataset --------------------------------------------------------------------
HistPPT <- HistDataNormMean_18MNs[,c('Date','ppt_rollsum.med', 'ppt_rollsum.10', 'ppt_rollsum.90')]
names(HistPPT)[2:4] <- paste0('Hist.', names(HistPPT)[2:4])

FutPPT <- AnomRunStats[,c('Date', 'ppt.mean.med', 'ppt_rollsum.med', 'ppt_rollsum.10', 'ppt_rollsum.90')]
names(FutPPT)[2:5] <- paste0('Fut.', names(FutPPT)[2:5])
FutPPT$Date <- as.Date(FutPPT$Date)

PPTDF <- merge(FutPPT, HistPPT) 

PPTDF$Time <- ifelse(PPTDF$Date < Sys.Date(), 'Observed', 'Future')

# eliminate data two weeks after current not in the position to make statements there
PPTDF[PPTDF$Date %in% c((currDate-15):(currDate + 15)),
       c('Fut.ppt_rollsum.med', 'Fut.ppt_rollsum.10', 'Fut.ppt_rollsum.90')] <- NA

# differences ------------------------------------------------------------------------

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

######################################################################################
            ################# Plotting Begins #################
######################################################################################

# Panel 1 ----------------------------------------------------------------------------
Observed <- PPTDF[Time == 'Observed', ]
PPTDF$Fut.ppt_rollsum.10 <- ifelse(PPTDF$Time == 'Observed', NA, PPTDF$Fut.ppt_rollsum.10)
PPTDF$Fut.ppt_rollsum.90 <- ifelse(PPTDF$Time == 'Observed', NA, PPTDF$Fut.ppt_rollsum.90)


Panel1 <- ggplot(PPTDF) +
  # repeating long-term historical daily median
  geom_ribbon( aes(x = Date, y = Hist.ppt_rollsum.med, 
                   ymin = Hist.ppt_rollsum.10, ymax = Hist.ppt_rollsum.90,  alpha=0.1), fill = 'lightgrey') +
  geom_line(aes(Date, Hist.ppt_rollsum.med, color = 'Historical')) +
  
  # future quantiles
  geom_line(aes(Date, Fut.ppt_rollsum.10), color = 'darkcyan')  + 
  geom_line(aes(Date, Fut.ppt_rollsum.90), color = 'darkcyan')  +
  
  # observed past dailys
  geom_bar(data = Observed, aes(Date, Fut.ppt.mean.med), color = 'darkgoldenrod3' ,stat = 'identity',size = .3) +
  
  # observed and future median
  geom_line(aes(Date, Fut.ppt_rollsum.med, color = Time), size = 1.5) +
  
  # theme
  theme_bw() +   
  theme(legend.position = 'bottom') +
  guides(alpha = FALSE, fill = FALSE) +
  scale_color_manual(name = '', values = c('darkcyan', 'black', 'darkgoldenrod3')) +
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
  labs(y = 'precipitation (cm)')

plot(Panel1)
ggsave('figureCode/presentationFigs/savedFigs/PPT_ObservedAndFuture_Median_Quantiles.png', height = 4, width = 8)


# Panel 2 ----------------------------------------------------------------------------

Panel2 <- ggplot(PPTDF) + 
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
  
  # theme
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  geom_hline(yintercept = 0) +
  
  theme_bw() + theme(legend.position = "none")  +
  scale_fill_manual(values = c('brown4', 'forestgreen')) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
  scale_color_manual(values = c('darkcyan', 'darkgoldenrod3')) +
  
  labs(y = 'ppt diffs (cm)')

plot(Panel2)
ggsave('figureCode/presentationFigs/savedFigs/PPT_ObservedAndFuture_Median_DIFFERENCES.png', height = 4, width = 8)

Panel2Anoms <- Panel2 + 
  geom_boxplot(data = MonthlyAnoms2, aes(Date, pptAnom_cm, group = Month), 
               fatten = NULL, width = 7, alpha = 0.8, outlier.size = -1) +
  stat_summary(data = MonthlyAnoms2, aes(Date, pptAnom_cm, group = Month, ymax = ..y.., ymin = ..y..),
               fun.y = mean, geom = "errorbar", size = 1.2, color = 'limegreen') +
  # NWS dots and lines
  stat_summary(data = NWSMeans1, aes(Date, meanForecastDiff, ymax = ..y.., ymin = ..y..),
               fun.y = mean, geom = "errorbar", size = 1.2, color = 'purple') +
  geom_pointrange(data = NWSAnomsAll2, aes(Date, Anom_cm,
                                           ymin = Anom_cm - ForecastedSD_PPT_cm,
                                           ymax = Anom_cm + ForecastedSD_PPT_cm), shape = 21, fill = 'black', color = 'magenta')
plot(Panel2Anoms)

