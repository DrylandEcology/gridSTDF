library(ggplot2)
library(lubridate)

# Make a figure that is 18 months of data, 6 in the past, 12 in the future.
# 1 is long-term means 
# 2 is  anomalies
# Smoothed lines for everything!
## rolling means for TEMP and VWC
## rolling sums for ppt
# Only show daily data for observed past!

######################################################################################
################# Data organization #################
######################################################################################

# Master Dataset --------------------------------------------------------------------
HistVWC <- HistDataNormMean_18MNs[,c('Date', 'VWC.Shallow_rollmean.med', 'VWC.Shallow_rollmean.10', 'VWC.Shallow_rollmean.90')]
names(HistVWC)[2:4] <- paste0('Hist.', names(HistVWC)[2:4])

FutVWC <- AnomRunStats[,c('Date', 'Shallow.mean.med', 'VWC.Shallow_rollmean.med', 'VWC.Shallow_rollmean.10', 'VWC.Shallow_rollmean.90')]
names(FutVWC)[2:5] <- paste0('Fut.', names(FutVWC)[2:5])
FutVWC$Date <- as.Date(FutVWC$Date)

VWCDF <- merge(FutVWC, HistVWC) 
VWCDF$Time <- ifelse(VWCDF$Date < Sys.Date(), 'Observed', 'Future')

# ------------------------------------------------------------------------------------
# eliminate data two weeks after current not in the position to make statements there
VWCDF[VWCDF$Date %in%c((currDate-15):(currDate + 15)),
       c('Fut.VWCShallow_rollmean.med', 'Fut.VWCShallow_rollmean.10', 'Fut.VWCShallow_rollmean.90')] <-NA

# differences ------------------------------------------------------------------------

# Differences between future and historical
VWCDF$Diffs.Med <- VWCDF$Fut.VWCShallow_rollmean.med - VWCDF$Hist.VWCShallow_rollmean.med 
VWCDF$Diffs.10 <- VWCDF$Fut.VWCShallow_rollmean.10 - VWCDF$Hist.VWCShallow_rollmean.med 
VWCDF$Diffs.90 <- VWCDF$Fut.VWCShallow_rollmean.90 - VWCDF$Hist.VWCShallow_rollmean.med 

# Historical diffs. - gray shaded area
VWCDF$Hist.avg_C.rollmean.90.diff <- VWCDF$Hist.VWCShallow_rollmean.90 - VWCDF$Hist.VWCShallow_rollmean.med
VWCDF$Hist.avg_C.rollmean.10.diff <- VWCDF$Hist.VWCShallow_rollmean.10 - VWCDF$Hist.VWCShallow_rollmean.med

# break things up by postive and negative
VWCDF$Type <- ifelse(VWCDF$Diffs.Med > 0, 'pos', 'neg')

######################################################################################
################# Plotting Begins #################
######################################################################################

# Panel 1 ----------------------------------------------------------------------------
VWCDF$Fut.avg_C.med <- ifelse(VWCDF$Time == 'Future', NA, VWCDF$Fut.Shallow.mean.med)
VWCDF$Diffs.10 <- ifelse(VWCDF$Time == 'Observed', NA, VWCDF$Diffs.10)
VWCDF$Diffs.90 <- ifelse(VWCDF$Time == 'Observed', NA, VWCDF$Diffs.90)

Panel1 <- ggplot(VWCDF) +
  # repeating long-term historical daily median
  geom_ribbon( aes(x = Date, y = Hist.VWCShallow_rollmean.med, 
                   ymin = Hist.VWCShallow_rollmean.10, ymax = Hist.VWCShallow_rollmean.90,  alpha=0.1), fill = 'lightgrey') +
  geom_line(aes(Date, Hist.VWCShallow_rollmean.med, color = 'Historical')) +
  
  # future quantiles
  geom_line(aes(Date, Fut.VWCShallow_rollmean.10), color = 'darkcyan')  + 
  geom_line(aes(Date, Fut.VWCShallow_rollmean.90), color = 'darkcyan')  +
  
  # observed past dailys
  geom_line(aes(Date, Fut.avg_C.med, color = Time), size = .3) +
  
  # observed and future median
  geom_line(aes(Date, Fut.VWCShallow_rollmean.med, color = Time), size = 1.1) +
  
  # theme
  theme_bw() + theme(legend.position = "bottom",
                     legend.title = element_blank()) + 
  guides(alpha = FALSE) +
  scale_color_manual(values = c('darkcyan', 'black', 'darkgoldenrod3')) +
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
  labs(y = 'VWC (cm/cm)')

plot(Panel1)
ggsave('figureCode/presentationFigs/savedFigs/VWC_Shallow_ObservedAndFuture_Median_Quantiles.png', height = 4, width = 8)

# Panel 2 ----------------------------------------------------------------------------
Panel2 <- ggplot(VWCDF) + 
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
  
  # theme
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  geom_hline(yintercept = 0) +
  
  theme_bw() + theme(legend.position = "none")  +
  scale_fill_manual(values = c('brown4', 'forestgreen')) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
  scale_color_manual(values = c('darkcyan', 'darkgoldenrod3')) +
  
  labs(y = 'VWC diffs (cm/cm)')

plot(Panel2)
ggsave('figureCode/presentationFigs/savedFigs/VWC_Shallow_ObservedAndFuture_Median_DIFFERENCES.png', height = 4, width = 8)

