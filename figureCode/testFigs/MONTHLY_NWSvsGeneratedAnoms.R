# Plots comparing the distribution of generated anomaly data at the Monthly level
# to NWS values for the same monthly periods
#https://stackoverflow.com/questions/6967664/ggplot2-histogram-with-normal-curve 

print('Density plots of monthly anomalies used in SOILWAT2 vs NWS')

source('../figureCode/FigureDataPrep.R')
MonthlyAnoms2 <- MonthlyAnoms # monthly anamoly data entered into SW
MonthlyAnoms2$Month <- factor(MonthlyAnoms2$Month, levels = c(currMonth:12, 1:(currMonth-1)))
generatedMean <- setDT(MonthlyAnoms2)[,.(meanGenAnom = mean(tempAnom)), .(Month)]

cols <- c("Generated Monthlys" = "#7AFF33", "NWS Data" = "#CA33FF")

# ----- 30 sets of monthly anomalies vs. affiliated NWS data
# temperature
NWSMeans <- setDT(NWSAnomsAll1)[,.(meanAnom = mean(Anom_C)), .(m)]
names(NWSMeans)[1] <- 'Month'
NWSMeans$Month <- factor(NWSMeans$Month, levels = c(currMonth:12, 1:(currMonth-1)))
NWSMeans$Year <- c(rep(2021, length(1:(currMonth-1))), rep(2020, length(currMonth:12)))
NWSMeans$Date <- as.Date(paste0(NWSMeans$Year, '-', NWSMeans$Month, '-05'))

tempHist <- ggplot(MonthlyAnoms2, aes(tempAnom)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth =.1)+
  geom_density(alpha=.2, fill="green") +
  facet_wrap(~Month, scales = 'free') +
  labs(title = 'Temperature anomalies (C) by Month') +
  geom_vline(data = NWSMeans, aes(xintercept = meanAnom, color = 'NWS Data'), size = 1.5) +
  geom_vline(data = generatedMean, aes(xintercept = meanGenAnom, color = 'Generated Monthlys'), linetype = 'dashed', size = 1.5)+
  theme_bw() +
  scale_color_manual(name="",values=cols) +
  theme(legend.position = 'bottom')

tempBP <- ggplot() + 
  geom_boxplot(data = MonthlyAnoms2, aes(Date, tempAnom, group = Month), fatten = NULL, 
               width = 7, alpha = 0.8, outlier.shape = NA, outlier.size = -1) +
  stat_summary(data = MonthlyAnoms2, aes(Date, tempAnom, group = Month, ymax = ..y.., ymin = ..y..,
                                         color = 'Generated Monthlys'),
               fun = mean, geom = "errorbar", size = 1.2) +
  #geom_point(data = MonthlyAnoms2, aes(Date, tempAnom, group = Month), shape = 21, size =.5, fill = NA) +
  # NWS dots and lines
  stat_summary(data = NWSMeans, aes(Date, meanAnom, ymax = ..y.., ymin = ..y.., color = 'NWS Data'),
               fun = mean, geom = "errorbar", size = 1.2) +
  geom_pointrange(data = NWSAnomsAll1, aes(Date, Anom_C,
                                           ymin = Anom_C - ForecastedSD_Temp_C,
                                           ymax = Anom_C + ForecastedSD_Temp_C), shape = 21, fill = 'black', color = 'magenta') +
  labs(title = 'Temperature anomalies (C) by Month') +
  theme_bw() +
  scale_color_manual(name="Means",values=cols) +
  theme(legend.position = 'bottom')
  
suppressWarnings(print(tempHist))
suppressWarnings(print(tempBP))
  
# precip

# cm
NWSMeans1 <- setDT(NWSAnomsAll2)[,.(meanForecastDiff = mean(Anom_cm)), .(m)]
names(NWSMeans1)[1] <- 'Month'
NWSMeans1$Month <- factor(NWSMeans1$Month, levels = c(currMonth:12, 1:(currMonth-1)))
NWSMeans1$Year <- c(rep(2021, length(1:(currMonth-1))), rep(2020, length(currMonth:12)))
NWSMeans1$Date <- as.Date(paste0(NWSMeans1$Year, '-', NWSMeans1$Month, '-05'))
NWSMeans1

generatedMean <- setDT(MonthlyAnoms2)[,.(meanGenAnom = mean(pptAnom_cm)), .(Month)]
generatedMean

# hists
PPTcmHist <- ggplot(MonthlyAnoms2, aes(pptAnom_cm)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="green") +
  facet_wrap(~Month, scales = 'free') +
  labs(title = 'Precipitation anomalies (cm) by Month') +
  geom_vline(data = NWSMeans1, aes(xintercept = meanForecastDiff, color = 'NWS Data'), size = 1.5) +
  geom_vline(data = generatedMean, aes(xintercept = meanGenAnom, color = 'Generated Monthlys'), linetype = 'dashed', size = 1.5)+
  theme_bw() +
  xlim(-2, 2) +
  scale_color_manual(name="",values=cols) +
  theme(legend.position = 'bottom')

# boxplots
PPTcmBP <-ggplot() +
  geom_boxplot(data = MonthlyAnoms2, aes(Date, pptAnom_cm, group = Month), fatten = NULL, 
               width = 7, alpha = 0.8,outlier.shape = NA, outlier.size = -1) +
  stat_summary(data = MonthlyAnoms2, aes(Date, pptAnom_cm, group = Month, ymax = ..y.., ymin = ..y..,
                                         color = 'Generated Monthlys'),
               fun = mean, geom = "errorbar", size = 1.2) +
  #geom_point(data = MonthlyAnoms2, aes(Date, pptAnom_cm, group = Month), shape = 21, size =.5, fill = NA) +
  # NWS dots and lines
 stat_summary(data = NWSMeans1, aes(Date, meanForecastDiff, ymax = ..y.., ymin = ..y.., color = 'NWS Data'),
               fun = mean, geom = "errorbar", size = 1.2) +
  geom_pointrange(data = NWSAnomsAll2, aes(Date, Anom_cm,
                                           ymin = Anom_cm - ForecastedSD_PPT_cm,
                                           ymax = Anom_cm + ForecastedSD_PPT_cm), shape = 21, fill = 'black', color = 'magenta') +
  theme_bw() +
  scale_color_manual(name="Means",values=cols) +
  theme(legend.position = 'bottom')


# CF
NWSMeans2 <- setDT(NWSAnomsAll2)[,.(meanForecastDiff = mean(Anom_CF)), .(m)]
names(NWSMeans2)[1] <- 'Month'
NWSMeans2$Month <- factor(NWSMeans2$Month, levels = c(currMonth:12, 1:(currMonth-1)))
NWSMeans2$Year <- c(rep(2021, length(1:(currMonth-1))), rep(2020, length(currMonth:12)))
NWSMeans2$Date <- as.Date(paste0(NWSMeans2$Year, '-', NWSMeans2$Month, '-05'))

generatedMean <- setDT(MonthlyAnoms2)[,.(meanGenAnom = mean(pptAnom_CF)), .(Month)]

# hists
PPTCFHist <-ggplot(MonthlyAnoms2, aes(pptAnom_CF)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="green") +
  facet_wrap(~Month, scales = 'free') +
  labs(title = 'Precipitation anomalies (CF) by Month', x = 'Precipitation CF') +
  geom_vline(data = NWSMeans2, aes(xintercept = meanForecastDiff, color = 'NWS Data'), size = 1.5) +
  geom_vline(data = generatedMean, aes(xintercept = meanGenAnom, color = 'Generated Monthlys'), linetype = 'dashed', size = 1.5)+
  theme_bw() +
  xlim(0, 3) + 
  scale_color_manual(name="",values=cols) +
  theme(legend.position = 'bottom')

# boxplots
PPTCFBP <-ggplot() +
  geom_boxplot(data = MonthlyAnoms2, aes(Date, pptAnom_CF, group = Month), fatten = NULL, 
               width = 7, alpha = 0.8, outlier.shape = NA, outlier.size = -1 )+
  stat_summary(data = MonthlyAnoms2, aes(Date, pptAnom_CF, group = Month, ymax = ..y.., ymin = ..y..,
                                         color = 'Generated Monthlys'),
               fun = mean, geom = "errorbar", size = 1.2) +
 # geom_point(data = MonthlyAnoms2, aes(Date, pptAnom_CF, group = Month), shape = 21, size =.5, fill = NA) +
  # NWS dots and lines
  stat_summary(data = NWSMeans2, aes(Date, meanForecastDiff, ymax = ..y.., ymin = ..y.., color = 'NWS Data'),
               fun = mean, geom = "errorbar", size = 1.2) +
  geom_point(data = NWSAnomsAll2, aes(Date, Anom_CF), shape = 21, fill = 'black', color = 'magenta') +
  labs(title = 'Precipitation anomalies (CF) by Month', y = 'Precipitation CF') +
 
   scale_color_manual(name="Means",values=cols) +

  theme_bw() + theme(legend.position = 'bottom')

suppressWarnings(print(PPTCFHist))
suppressWarnings(print(PPTCFBP))
suppressWarnings(print(PPTcmHist))
suppressWarnings(print(PPTcmBP))
