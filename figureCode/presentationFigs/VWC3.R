AllVarData <- read.csv('ExampleData/AllVarData.csv', stringsAsFactors = FALSE)
lastWeatherDate <- read.csv('ExampleData/lastWeatherDate.csv')
lastWeatherDate <- as.Date(lastWeatherDate[,1])
AllVarData$Date <- as.Date(AllVarData$Date)
indx <- grep('VWC.Shallow', names(AllVarData))
VWCdata <- AllVarData[,c(1,indx)]

######################################################################################
################# PREP --------------------------------------------------------------- 
######################################################################################

VWCdata$Type <- ifelse(VWCdata$RecentPast.VWC.Shallow.Diffs.Med > 0, 'pos', 'neg')
VWCdata$Type2 <- ifelse(VWCdata$NearFut.VWC.Shallow.Diffs.Med > 0, 'pos', 'neg')

######################################################################################
################# Plotting Begins #################
######################################################################################

Panel1 <- ggplot() +
  # repeating long-term historical daily median
  geom_ribbon(data = VWCdata, aes(x = Date, y = VWC.Shallow_rollmean.med, 
                   ymin = VWC.Shallow_rollmean.10, ymax = VWC.Shallow_rollmean.90,  alpha=0.1), fill = 'lightgrey') +
  geom_line(data = VWCdata, aes(Date, VWC.Shallow_rollmean.med), color = 'black') +
  
  # recent past dailys
  geom_line(data = VWCdata, aes(Date, RecentPast.VWC.Shallow.mean.med), color = 'goldenrod', size = .3) +
  # recent past median
  geom_line(data = VWCdata, aes(Date, RecentPast.VWC.Shallow_rollmean.med), color = 'goldenrod', size = 1.1) +
  
   # VWCdata quantiles & median 
  geom_line(data = VWCdata, aes(Date, NearFut.VWC.Shallow.mean.10), color = 'darkcyan')  + 
  geom_line(data = VWCdata, aes(Date, NearFut.VWC.Shallow.mean.90), color = 'darkcyan')  +
  geom_line(data = VWCdata, aes(Date, NearFut.VWC.Shallow.mean.med), color = 'darkcyan', size = 1.1)  +
  
  
  # theme
  theme_bw() + theme(legend.position = "bottom",
                     legend.title = element_blank()) + 
  guides(alpha = FALSE) +
  #scale_color_manual(values = c('darkcyan', 'black', 'darkgoldenrod3')) +
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  geom_vline(xintercept = as.Date(lastWeatherDate), color = 'blue', lty = 'dashed') +
  
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
  labs(y = 'VWC (cm/cm)')

plot(Panel1)
ggsave('figureCode/presentationFigs/savedFigs/VWC_Absolute_NewVersion.png', height = 4, width = 8)

# Panel 2 ----------------------------------------------------------------------------
Panel2 <- ggplot() + 
  
  # 0 line and 10 - 90% for the historical -----
geom_ribbon(data = VWCdata, aes(Date, ymin = VWC.Shallow_roll.10.diff, 
                                               ymax = VWC.Shallow_roll.90.diff, alpha=0.01), fill = 'lightgrey') +
  geom_line(data = VWCdata, aes(Date, VWC.Shallow_roll.10.diff), size = .1, color = 'black') +
  geom_line(data = VWCdata, aes(Date,  VWC.Shallow_roll.90.diff), size = .1, color = 'black') +
  
  # Observed Differences from past as bars -----
geom_bar(data = VWCdata, aes(Date, RecentPast.VWC.Shallow.Diffs.Med, fill = Type), stat = "identity", size = .1) +
  geom_bar(data = VWCdata, aes(Date, NearFut.VWC.Shallow.Diffs.Med, fill = Type2), stat = "identity", size = .1) +
  
  # thick line of differences
  geom_line(data = VWCdata, aes(Date, RecentPast.VWC.Shallow.Diffs.Med), color = 'goldenrod', size = .8) +
  geom_line(data = VWCdata, aes(Date, NearFut.VWC.Shallow.Diffs.Med), color = 'darkcyan', size = .8) +
  
  # 10 and 90 VWCdata diffs
  geom_line(data = VWCdata, aes(Date, NearFut.VWC.Shallow.Diffs.10), color = 'darkcyan')  + 
  geom_line(data = VWCdata, aes(Date, NearFut.VWC.Shallow.Diffs.90), color = 'darkcyan')  +
  
  # theme
  geom_vline(xintercept = as.Date(Sys.time()), color = 'darkorchid3') +
  geom_vline(xintercept = as.Date(lastWeatherDate), color = 'blue', lty = 'dashed') +
  
  geom_hline(yintercept = 0) +
  
  theme_bw() + theme(legend.position = "none")  +
  scale_fill_manual(values = c('brown4', 'forestgreen')) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y", expand = c(0,0)) +
  #scale_color_manual(values = c('darkcyan', 'darkgoldenrod3')) +
  labs(y = 'VWC diffs (cm/cm)')

plot(Panel2)
ggsave('figureCode/presentationFigs/savedFigs/VWC_Diff_NewVersion.png', height = 4, width = 8)

