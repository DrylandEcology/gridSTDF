
AllVarData <- read.csv('ExampleData/AllVarData.csv', stringsAsFactors = FALSE)
AllVarData$Date <- as.Date(AllVarData$Date)
indx <- grep('avg_C', names(AllVarData))
TempData <- AllVarData[,c(1,indx)]
######################################################################################
################# PREP --------------------------------------------------------------- 
######################################################################################

HistDataNormMean_18MNs

# Historical Climatological diffs. - gray shaded area
HistDataNormMean_18MNs$Hist.avg_C.rollmean.90.diff <- HistDataNormMean_18MNs$avg_C_rollmean.90 - HistDataNormMean_18MNs$avg_C_rollmean.med
HistDataNormMean_18MNs$Hist.avg_C.rollmean.10.diff <- HistDataNormMean_18MNs$avg_C_rollmean.10 - HistDataNormMean_18MNs$avg_C_rollmean.med

TempData$Type2 <- ifelse(TempData$NearFut.avg_C.Diffs.Med > 0, 'pos', 'neg')
TempData$Type <- ifelse(TempData$RecentPast.avg_C.Diffs.Med > 0, 'pos', 'neg')

######################################################################################
################# Plotting Begins ---------------------------------------------------
######################################################################################

Panel1 <- ggplot() +
  #  climatological historical daily median & quantiles
  geom_ribbon(data = HistDataNormMean_18MNs, aes(x = Date, y = avg_C_rollmean.med, 
                                                 ymin = avg_C_rollmean.10, ymax = avg_C_rollmean.90,  alpha=0.1), fill = 'lightgrey') +
  geom_line(data = HistDataNormMean_18MNs, aes(Date, avg_C_rollmean.med),  color = 'black') +
  
  # recent past dailys
  geom_line(data = TempData, aes(Date, RecentPast.avg_C.mean.med), color = 'goldenrod', size = .3) +
  # recent past median
  geom_line(data = TempData, aes(Date, RecentPast.avg_C_rollmean.med), color = 'goldenrod', size = 1.1) +
  
  # future quantiles & median 
  geom_line(data = TempData, aes(Date, NearFut.avg_C.mean.10), color = 'darkcyan')  + 
  geom_line(data = TempData, aes(Date, NearFut.avg_C.mean.90), color = 'darkcyan')  +
  geom_line(data = TempData, aes(Date, NearFut.avg_C.mean.med), color = 'darkcyan', size = 1.1)  +
  
  # theme
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal") +
  guides(alpha = FALSE) +
  #scale_color_manual(name = '', values = c('darkcyan', 'black', 'darkgoldenrod3')) +
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
  labs(y = 'temperature (°C)')

suppressWarnings(plot(Panel1))
ggsave('figureCode/presentationFigs/savedFigs/Temp_Absolute_NewVersion.png', height = 4, width = 8)

# Panel 2 ----------------------------------------------------------------------------
Panel2 <- ggplot() + 
  
  # 0 line and 10 - 90% for the historical -----
geom_ribbon(data = HistDataNormMean_18MNs, aes(Date, ymin = Hist.avg_C.rollmean.10.diff, 
                                               ymax = Hist.avg_C.rollmean.90.diff, alpha=0.01), fill = 'lightgrey') +
  geom_line(data = HistDataNormMean_18MNs, aes(Date, Hist.avg_C.rollmean.10.diff), size = .1, color = 'black') +
  geom_line(data = HistDataNormMean_18MNs, aes(Date,  Hist.avg_C.rollmean.90.diff), size = .1, color = 'black') +
  
  # Observed Differences from past as bars -----
geom_bar(data = TempData, aes(Date, RecentPast.avg_C.Diffs.Med, fill = Type), stat = "identity", size = .1) +
  geom_bar(data = TempData, aes(Date, NearFut.avg_C.Diffs.Med, fill = Type2), stat = "identity", size = .1) +
  
  # thick line of differences
  geom_line(data = TempData, aes(Date, RecentPast.avg_C.Diffs.Med), color = 'goldenrod', size = .8) +
  geom_line(data = TempData, aes(Date, NearFut.avg_C.Diffs.Med), color = 'darkcyan', size = .8) +
  
  # 10 and 90 future diffs
  geom_line(data = TempData, aes(Date, NearFut.avg_C.Diffs.10), color = 'darkcyan')  + 
  geom_line(data = TempData, aes(Date, NearFut.avg_C.Diffs.90), color = 'darkcyan')  +
  
  # theme
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  geom_hline(yintercept = 0) +
  
  theme_bw() + theme(legend.position = "none")  +
  scale_fill_manual(values = c('blue', 'red')) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
  #scale_color_manual(values = c('darkcyan', 'darkgoldenrod3')) +
  labs(y = 'temperature diffs (°C)')

suppressWarnings(plot(Panel2))
ggsave('figureCode/presentationFigs/savedFigs/Temp_Diffs_NewVersion2.png', height = 4, width = 8)

