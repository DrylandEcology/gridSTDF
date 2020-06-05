print('Density plots of predicted LEAD Anomalies')
# the anomaly from NWS is the thick blue line. Sample distribution of NWS data is blue
# the mean of the our generated anomlies is the dashed red line. Distribution in pink
# 100 Future realizations as density plots

#------ Check generated anamalies against NWS anomalies
tempNWSDensity <- TempAnoms[,.(vals = rnorm(100, mean = Anom_C, sd = ForecastedSD_Temp_C)),
                            .(LEAD)]

tempGenAnoms <- data.table(generatedAnomData[, , "dT_C"])
tempGenAnoms$LEAD <- factor(tempGenAnoms$LEAD, levels = c(1:12))
tempGenAnoms$LEAD <- row.names(tempGenAnoms)
tempGenAnoms <- melt(tempGenAnoms, id.vars = 'LEAD')

tempGenAnomsMean <- tempGenAnoms[,.(dT_mean = mean(value)),.(LEAD)]

ggplot() +

  geom_density(data = tempNWSDensity, aes(x = vals), alpha=.2, fill="blue") +
  geom_density(data = tempGenAnoms, aes(x = value), alpha=.2, fill="#FF6666") +
  
  facet_wrap(~LEAD, scales = 'free') +
  labs(title = 'Temperature by LEAD; generated anomalies') +
  
  geom_vline(data = tempGenAnomsMean, aes(xintercept = dT_mean), color = 'blue')+
  geom_vline(data = TempAnoms[1:12,], aes(xintercept = Anom_C), color = 'red', lty = 'dashed')+
  theme_bw()


# -----------
# get distribution of nws vals
pptNWSDensity <- PPTAnoms[,.(vals = rnorm(100, mean = ForecastedMEAN_PPT_PO, sd = ForecastedSD)), .(LEAD)]

pptNWSDensity <- merge(pptNWSDensity, PPTAnoms[,c('LEAD', 'PO', 'ForecastedSD', 'ForecastedMEAN_PPT_PO', 'ForecastedMEAN_PPT_cm', 'ClimatatologicalMEAN_PPT_cm')])
pptNWSDensity$ForecastedVal_in <- (pptNWSDensity$vals) ^ (1/pptNWSDensity$PO)
pptNWSDensity$ForecastedVal_cm <- pptNWSDensity$ForecastedVal_in * 2.54
pptNWSDensity$PPT_CF <- pptNWSDensity$ForecastedVal_cm / pptNWSDensity$ClimatatologicalMEAN_PPT_cm
summary(pptNWSDensity)

# experiment with number in untransformed units
pptGenAnoms <- data.table(generatedAnomData[, , "PT_GenForecasted_PO"])
pptGenAnoms$LEAD <- row.names(pptGenAnoms)
pptGenAnoms$LEAD <- factor(pptGenAnoms$LEAD, levels = c(1:12))
pptGenAnoms <- melt(pptGenAnoms, id.vars = 'LEAD')
pptGenAnomsMean <- pptGenAnoms[,.(Forecast_mean = mean(value)),.(LEAD)]

ggplot() +
 geom_density(data = pptGenAnoms, aes(value), alpha=.2, fill="#FF6666") +
  geom_density(data = pptNWSDensity, aes(vals), alpha=.2, fill="blue") + 
  
  facet_wrap(~LEAD, scales = 'free') +
  labs(title = 'Precipitation by LEAD; Forecast PO') +
  
  geom_vline(data = pptGenAnomsMean, aes(xintercept = Forecast_mean), color = 'red')+
  geom_vline(data = PPTAnoms[1:12, ], aes(xintercept = ForecastedMEAN_PPT_PO), color = 'blue',  lty = 'dashed')+
  theme_bw()

# ----------------- in centimeters ------------------------------------
pptGenAnoms <- data.table(generatedAnomData[, , "PPT_GenForecasted_cm"])
pptGenAnoms$LEAD <- row.names(pptGenAnoms)
pptGenAnoms <- melt(pptGenAnoms, id.vars = 'LEAD')
pptGenAnomsMean <- pptGenAnoms[,.(Forecast_mean = mean(value)),.(LEAD)]

ggplot() +
  geom_density(data = pptGenAnoms, aes(value), alpha=.2, fill="#FF6666") +
  geom_density(data = pptNWSDensity, aes(ForecastedVal_cm), alpha=.2, fill="blue") +
  
  facet_wrap(~LEAD, scales = 'free') +
  labs(title = 'Precipitation by LEAD; Forecast cm') +
  
  geom_vline(data = pptGenAnomsMean, aes(xintercept = Forecast_mean), color = 'red')+
  geom_vline(data = PPTAnoms[1:12, ], aes(xintercept = ForecastedMEAN_PPT_cm), color = 'blue',  lty = 'dashed')+
  theme_bw()


# -----------------------------------------------------------
pptGenAnoms <- data.table(generatedAnomData[, , "PPT_CF"])
pptGenAnoms$LEAD <- row.names(pptGenAnoms)
pptGenAnoms <- melt(pptGenAnoms, id.vars = 'LEAD')
pptGenAnomsMean <- pptGenAnoms[,.(PPT_CF = mean(value)),.(LEAD)]

ggplot() +
  #geom_histogram(pptGenAnoms,  aes(x = value, y=..density..), colour="black", fill="white")+
  
  geom_density(data = pptGenAnoms, aes(value), alpha=.2, fill="#FF6666") +
  geom_density(data = pptNWSDensity, aes(PPT_CF), alpha=.2, fill="blue") +
  
  facet_wrap(~LEAD, scales = 'free') +
  labs(title = 'Precipitation by LEAD; generated forecast cm') +
  
  geom_vline(data = pptGenAnomsMean, aes(xintercept = PPT_CF), color = 'red') +
  geom_vline(data = PPTAnoms[1:12, ], aes(xintercept = Anom_CF), color = 'blue',  lty = 'dashed')+
  theme_bw()

