library(ggplot2)


# temperature
data <- read.csv('projects/06-TestOutputs/DataOneSite.csv')
data$fake_date <- 1:549

ggplot() +
  #  climatological historical daily median & quantiles
  geom_ribbon(data = data, aes(x = fake_date, ymin = avg_C_rollmean.10, 
                               ymax = avg_C_rollmean.90,  alpha=0.1), 
              fill = 'lightgrey') +
  geom_line(data = data, aes(fake_date, avg_C_rollmean.med),  color = 'black') +
  
  # recent past dailys
  #geom_line(data = data, aes(fake_date, RecentPast.avg_C.mean.med), color = 'goldenrod', size = .3) +
  # recent past median
  #geom_line(data = data, aes(fake_date, RecentPast.avg_C_rollmean.med), color = 'goldenrod', size = 1.1) +
  
  # future quantiles & median 
  geom_line(data = data, aes(fake_date, NearFut.avg_C.mean.10), color = 'darkcyan')  + 
  geom_line(data = data, aes(fake_date, NearFut.avg_C.mean.90), color = 'darkcyan')  +
  #geom_line(data = data, aes(fake_date, NearFut.avg_C.mean.med.), color = 'darkcyan', size = 1.1)  
  
  # theme
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal") +
  guides(alpha = FALSE) +

  #scale_x_fake_date(fake_date_breaks = "2 months", fake_date_labels = "%m-%Y", expand = c(0,0)) +
  labs(y = 'Temperature (°C)')


# diffs ------------------------------------------------------------------------------------------------
data$Type2 <- ifelse(data$NearFut.avg_C.Diffs.Med > 0, 'pos', 'neg')

ggplot() + 
  
  # Observed Differences from past as bars -----
  #geom_bar(data = data, aes(fake_date, RecentPast.avg_C.Diffs.Med, fill = Type), stat = "identity", size = .1) +
  geom_bar(data = data, aes(fake_date, NearFut.avg_C.Diffs.Med, fill = Type2), stat = "identity", size = .1) +
  
  # thick line of differences
  #geom_line(data = data, aes(fake_date, RecentPast.avg_C.Diffs.Med), color = 'goldenrod', size = .8) +
  geom_line(data = data, aes(fake_date, NearFut.avg_C.Diffs.Med), color = 'darkcyan', size = .8) +
  
  # 10 and 90 future diffs
  geom_line(data = data, aes(fake_date, NearFut.avg_C.Diffs.10), color = 'darkcyan')  + 
  geom_line(data = data, aes(fake_date, NearFut.avg_C.Diffs.90), color = 'darkcyan')  +
  
  geom_hline(yintercept = 0) +
  
  theme_bw() + 
  theme(legend.position = "none")  +
  scale_fill_manual(values = c('red')) +
  #scale_x_date(fake_date_breaks = "2 months", fake_date_labels = "%m-%Y", expand = c(0,0)) +
  labs(y = 'Temperature Diffs (°C)')





# VWC
data <- read.csv('projects/06-TestOutputs/vwcDataOneSite.csv')
data$fake_date <- 1:549

ggplot() +
  #  climatological historical daily median & quantiles
  geom_ribbon(data = data, aes(x = fake_date, ymin = VWC.Intermediate_rollmean.10, 
                               ymax = VWC.Intermediate_rollmean.90,  alpha=0.1), 
              fill = 'lightgrey') +
  geom_line(data = data, aes(fake_date, VWC.Intermediate_rollmean.med,  color = 'black')) +
  
  # recent past dailys
  #geom_line(data = data, aes(fake_date, RecentPast.avg_C.mean.med), color = 'goldenrod', size = .3) +
  # recent past median
  #geom_line(data = data, aes(fake_date, RecentPast.avg_C_rollmean.med), color = 'goldenrod', size = 1.1) +
  
  # future quantiles & median 
  geom_line(data = data, aes(fake_date, NearFut.VWC.Intermediate.mean.10), color = 'darkcyan')  + 
  geom_line(data = data, aes(fake_date, NearFut.VWC.Intermediate.mean.90), color = 'darkcyan')  +
  geom_line(data = data, aes(fake_date, NearFut.VWC.Intermediate.mean.med), color = 'darkcyan', size = 1.1)  +
  
  # theme
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal") +
  guides(alpha = FALSE) +
  
  #scale_x_fake_date(fake_date_breaks = "2 months", fake_date_labels = "%m-%Y", expand = c(0,0)) +
  labs(y = 'VWC (cm/cm)')
  

# diffs ------------------------------------------------------------------------------------------------
data$Type2 <- ifelse(data$NearFut.VWC.Intermediate.Diffs.Med > 0, 'pos', 'neg')

ggplot() + 
  
  # Observed Differences from past as bars -----
#geom_bar(data = data, aes(fake_date, RecentPast.avg_C.Diffs.Med, fill = Type), stat = "identity", size = .1) +
geom_bar(data = data, aes(fake_date, NearFut.VWC.Intermediate.Diffs.Med, fill = Type2), stat = "identity", size = .1) +
  
  # thick line of differences
  #geom_line(data = data, aes(fake_date, RecentPast.avg_C.Diffs.Med), color = 'goldenrod', size = .8) +
  geom_line(data = data, aes(fake_date, NearFut.VWC.Intermediate.Diffs.Med), color = 'darkcyan', size = .8) +
  
  # 10 and 90 future diffs
  geom_line(data = data, aes(fake_date, NearFut.VWC.Intermediate.Diffs.10), color = 'darkcyan')  + 
  geom_line(data = data, aes(fake_date, NearFut.VWC.Intermediate.Diffs.90), color = 'darkcyan')  +
  
  geom_hline(yintercept = 0) +
  
  theme_bw() + 
  theme(legend.position = "none")  +
  scale_fill_manual(values = c('brown4', 'forestgreen')) +
  labs(y = 'VWC diffs (cm/cm)')





