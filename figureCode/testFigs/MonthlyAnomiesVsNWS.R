# Plot comparing monthly means to LEADS
print('Density plots of monthly anomalies used in SOILWAT2')
source('figureCode/presentationFigs/FigureDataPrep.R')
AnomSave2 <- AnomSave
AnomSave2$Month <- factor(AnomSave2$Month, levels = c(6:12, 1:5))

# ----- 30 sets of monthly anomalies vs. affiliated NWS data
# temperature
NWSMeans <- setDT(NWSAnomsAll1)[,.(meanForecastDiff = mean(ForecastDiff)), .(m)]
names(NWSMeans)[1] <- 'Month'
generatedMean <- setDT(AnomSave2)[,.(meanGenAnom = mean(tempAnom)), .(Month)]
NWSMeans$Month <- factor(NWSMeans$Month, levels = c(6:12, 1:5))
NWSMeans$Year <- c(rep(2021,5), rep(2020, 7))
NWSMeans$Date <- as.Date(paste0(NWSMeans$Year, '-', NWSMeans$Month, '-05'))

ggplot(AnomSave2, aes(tempAnom)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth =.1)+
  geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(~Month, scales = 'free') +
  labs(title = 'Temperature anomalies by Month') +
  geom_vline(data = NWSMeans, aes(xintercept = meanForecastDiff), color = 'purple') +
  geom_vline(data = generatedMean, aes(xintercept = meanGenAnom), color = 'limegreen', linetype = 'dashed')+
  theme_bw()

ggplot() + 
  geom_boxplot(data = AnomSave2, aes(Date, tempAnom, group = Month), fatten = NULL, width = 7, alpha = 0.8) +
  stat_summary(data = AnomSave2, aes(Date, tempAnom, group = Month, ymax = ..y.., ymin = ..y..),
               fun.y = mean, geom = "errorbar", size = 1.2, color = 'limegreen') +
  geom_point(data = AnomSave2, aes(Date, tempAnom, group = Month), shape = 21, size =.5, fill = NA) +
  # NWS dots and lines
  stat_summary(data = NWSMeans, aes(Date, meanForecastDiff, ymax = ..y.., ymin = ..y..),
               fun.y = mean, geom = "errorbar", size = 1.2, color = 'purple') +
  geom_pointrange(data = NWSAnomsAll1, aes(Date, ForecastDiff,
                                           ymin = ForecastDiff - ForecastedSD,
                                           ymax = ForecastDiff + ForecastedSD), shape = 21, fill = 'black', color = 'magenta') +
   theme_bw()
  

  
# precip

NWSMeans <- setDT(NWSAnomsAll2)[,.(meanForecastDiff = mean(ForecastDiff)), .(m)]
names(NWSMeans)[1] <- 'Month'
generatedMean <- setDT(AnomSave2)[,.(meanGenAnom = mean(pptAnom_cm)), .(Month)]
NWSMeans$Month <- factor(NWSMeans$Month, levels = c(6:12, 1:5))
NWSMeans$Year <- c(rep(2021,5), rep(2020, 7))
NWSMeans$Date <- as.Date(paste0(NWSMeans$Year, '-', NWSMeans$Month, '-05'))

# hists
ggplot(AnomSave2, aes(pptAnom_cm)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = .5)+
  geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(~Month, scales = 'free') +
  labs(title = 'Precipitation anomalies by Month') +
  geom_vline(data = NWSMeans, aes(xintercept = meanForecastDiff), color = 'purple') +
  geom_vline(data = generatedMean, aes(xintercept = meanGenAnom), color = 'limegreen', linetype = 'dashed')+
  theme_bw()

# boxplots
ggplot() + 
  geom_boxplot(data = AnomSave2, aes(Date, pptAnom_cm, group = Month), fatten = NULL, width = 7, alpha = 0.8) +
  stat_summary(data = AnomSave2, aes(Date, pptAnom_cm, group = Month, ymax = ..y.., ymin = ..y..),
               fun.y = mean, geom = "errorbar", size = 1.2, color = 'limegreen') +
  geom_point(data = AnomSave2, aes(Date, pptAnom_cm, group = Month), shape = 21, size =.5, fill = NA) +
  # NWS dots and lines
  stat_summary(data = NWSMeans, aes(Date, meanForecastDiff, ymax = ..y.., ymin = ..y..),
               fun.y = mean, geom = "errorbar", size = 1.2, color = 'purple') +
  geom_pointrange(data = NWSAnomsAll2, aes(Date, ForecastDiff,
                                           ymin = ForecastDiff - ForecastedSD_cm,
                                           ymax = ForecastDiff + ForecastedSD_cm), shape = 21, fill = 'black', color = 'magenta') + 
  theme_bw()



