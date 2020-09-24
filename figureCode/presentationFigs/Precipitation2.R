AllVarData <- read.csv('ExampleData/AllVarData.csv', stringsAsFactors = FALSE)
AllVarData$Date <- as.Date(AllVarData$Date)
indx <- grep('ppt', names(AllVarData))
PPTdata <- AllVarData[,c(1,indx)]

######################################################################################
################# PREP --------------------------------------------------------------- 
######################################################################################

HistDataNormMean_18MNs
# Historical Climatological diffs. - gray shaded area
HistDataNormMean_18MNs$Hist.ppt_rollsum.90.diff <- HistDataNormMean_18MNs$ppt_rollsum.90 - HistDataNormMean_18MNs$ppt_rollsum.med
HistDataNormMean_18MNs$Hist.ppt_rollsum.10.diff <- HistDataNormMean_18MNs$ppt_rollsum.10 - HistDataNormMean_18MNs$ppt_rollsum.med

PPTdata$Type <- ifelse(PPTdata$RecentPast.ppt.Diffs.Med > 0, 'pos', 'neg')
PPTdata$Type2 <- ifelse(PPTdata$NearFut.ppt.Diffs.Med > 0, 'pos', 'neg')

######################################################################################
################# Plotting Begins ---------------------------------------------------
######################################################################################

Panel1 <- ggplot() +
  #  climatological historical daily median & quantiles
  geom_ribbon(data = HistDataNormMean_18MNs, aes(x = Date, y = ppt_rollsum.med, 
                                                 ymin = ppt_rollsum.10, ymax = ppt_rollsum.90,  alpha=0.1), fill = 'lightgrey') +
  geom_line(data = HistDataNormMean_18MNs, aes(Date, ppt_rollsum.med),  color = 'black') +

   # recent past dailys
   geom_bar(data = PPTdata, aes(Date,RecentPast.ppt.mean.med), stat = 'identity', color = 'goldenrod', size = .1) +
   # recent past median
   geom_line(data = PPTdata, aes(Date, RecentPast.ppt_rollsum.med), color = 'goldenrod', size = 1.1) +
  
  # future quantiles & median 
  geom_line(data = PPTdata, aes(Date, NearFut.ppt.sum.10), color = 'darkcyan')  + 
  geom_line(data = PPTdata, aes(Date, NearFut.ppt.sum.90), color = 'darkcyan')  +
  geom_line(data = PPTdata, aes(Date, NearFut.ppt.sum.med), color = 'darkcyan', size = 1.1)  +
  
  # theme
  theme_bw() +   
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
  labs(y = 'precipitation (cm)')

plot(Panel1)
ggsave('figureCode/presentationFigs/savedFigs/PPT_Absolute_NewVersion.png', height = 4, width = 8)


# Panel 2 ----------------------------------------------------------------------------

Panel2 <- ggplot() + 
  
  # 0 line and 10 - 90% for the historical -----
  geom_ribbon(data = HistDataNormMean_18MNs, aes(Date, ymin = Hist.ppt_rollsum.10.diff, 
                                               ymax = Hist.ppt_rollsum.90.diff, alpha=0.01), fill = 'lightgrey') +
  geom_line(data = HistDataNormMean_18MNs, aes(Date, Hist.ppt_rollsum.10.diff), size = .1, color = 'black') +
  geom_line(data = HistDataNormMean_18MNs, aes(Date,  Hist.ppt_rollsum.90.diff), size = .1, color = 'black') +
  
  # Observed Differences from past as bars -----
  geom_bar(data = PPTdata, aes(Date, RecentPast.ppt.Diffs.Med, fill = Type), stat = "identity", size = .1) +
  geom_bar(data = PPTdata, aes(Date, NearFut.ppt.Diffs.Med, fill = Type2), stat = "identity", size = .1) +
  
  # thick line of differences
  geom_line(data = PPTdata, aes(Date, RecentPast.ppt.Diffs.Med), color = 'goldenrod', size = .8) +
  geom_line(data = PPTdata, aes(Date, NearFut.ppt.Diffs.Med), color = 'darkcyan', size = .8) +
  
  # 10 and 90 future diffs
  geom_line(data = PPTdata, aes(Date, NearFut.ppt.Diffs.10), color = 'darkcyan')  + 
  geom_line(data = PPTdata, aes(Date, NearFut.ppt.Diffs.90), color = 'darkcyan')  +
  
  # theme
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  geom_hline(yintercept = 0) +
  theme_bw() + theme(legend.position = "none")  +
  scale_fill_manual(values = c('brown4', 'forestgreen')) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
  labs(y = 'ppt diffs (cm)')

plot(Panel2)

ggsave('figureCode/presentationFigs/savedFigs/PPT_Diffs_NewVersion.png', height = 4, width = 8)

