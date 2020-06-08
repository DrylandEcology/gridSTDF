print('Density plots of predicted LEAD Anomalies')
# the anomaly from NWS is the thick blue line. Sample distribution of NWS data is blue
# the mean of the our generated anomlies is the dashed red line. Distribution in pink
# theoretical distribution in green
# 100 Future realizations as density plots

#------ Check generated anamalies against NWS anomalies
# NWS random sample
tempNWSDensity <- TempAnoms[,.(vals = rnorm(100, mean = Anom_C, sd = ForecastedSD_Temp_C)),
                            .(LEAD)]
tempNWSDensityMean <- tempNWSDensity[,.(Anom_mean = mean(vals)), .(LEAD)]

# Theoretical sample
tempTheoreticalDensity <- data.frame()

for(i in 1:12){
  z <- subset(TempAnoms, LEAD == i)
  x <-  seq(z$Anom_C - (z$ForecastedSD_Temp_C * 4), 
            z$Anom_C + (z$ForecastedSD_Temp_C * 4), length = 3000) 
  y <-  dnorm(x, mean = z$Anom_C, sd = z$ForecastedSD_Temp_C)
  
  tempTheoreticalDensity <- rbind(tempTheoreticalDensity, data.frame(x = x, y = y, LEAD = i))
}
tempTheoreticalDensityMean <-setDT(tempTheoreticalDensity)[,.(Anom_mean = mean(x)),.(LEAD)]

# generated anomalies
tempGenAnoms <- data.table(generatedAnomData[, , "dT_C"])
tempGenAnoms$LEAD <- factor(tempGenAnoms$LEAD, levels = c(1:12))
tempGenAnoms$LEAD <- row.names(tempGenAnoms)
tempGenAnoms <- melt(tempGenAnoms, id.vars = 'LEAD')
tempGenAnomsMean <- tempGenAnoms[,.(dT_mean = mean(value)),.(LEAD)]

ggplot() +
  
  geom_point(data = tempTheoreticalDensity, aes(x = x, y = y), color = 'darkgreen',  size = .1) +
  geom_density(data = tempNWSDensity, aes(x = vals), alpha=.2, fill="blue") +
  geom_density(data = tempGenAnoms, aes(x = value), alpha=.2, fill="#FF6666") +
  
  facet_wrap(~LEAD, scales = 'free') +
  labs(title = 'Temperature by LEAD; generated anomalies') +
  
  geom_vline(data = tempGenAnomsMean, aes(xintercept = dT_mean), color = 'blue')+
  geom_vline(data = tempNWSDensityMean, aes(xintercept = Anom_mean), color = 'red', lty = 'dashed') +
  geom_vline(data = tempTheoreticalDensityMean, aes(xintercept = Anom_mean), color = 'darkgreen', lty = 'twodash') +
  theme_bw()


# ---------------------
# get distribution of nws vals
pptNWSDensity <- PPTAnoms[,.(vals = rnorm(100, mean = ForecastedMEAN_PPT_PO, sd = ForecastedSD)), .(LEAD)]
pptNWSDensityMean <- pptNWSDensity[,.(Anom_mean = mean(vals)), .(LEAD)]

pptNWSDensity <- merge(pptNWSDensity, PPTAnoms[,c('LEAD', 'PO', 'ForecastedSD', 'ForecastedMEAN_PPT_PO', 'ForecastedMEAN_PPT_cm', 'ClimatatologicalMEAN_PPT_cm')])
pptNWSDensity$ForecastedVal_in <- (pptNWSDensity$vals) ^ (1/pptNWSDensity$PO)
pptNWSDensity$ForecastedVal_cm <- pptNWSDensity$ForecastedVal_in * 2.54
pptNWSDensity$PPT_CF <- pptNWSDensity$ForecastedVal_cm / pptNWSDensity$ClimatatologicalMEAN_PPT_cm
#summary(pptNWSDensity)

# theoreritcal distribution
pptTheoreticalDensity <- data.frame()

for(i in 1:12){
  z <- subset(PPTAnoms, LEAD == i)
  x <-  seq(z$ForecastedMEAN_PPT_PO - (z$ForecastedSD * 4), 
           z$ForecastedMEAN_PPT_PO + (z$ForecastedSD * 4), length = 3000) 
  y <-  dnorm(x, mean = z$ForecastedMEAN_PPT_PO, sd = z$ForecastedSD)
  
  pptTheoreticalDensity <- rbind(pptTheoreticalDensity, data.frame(x = x, y = y, LEAD = i))
}
pptTheoreticalDensityMean <- setDT(pptTheoreticalDensity)[,.(Anom_mean = mean(x)),.(LEAD)]

pptTheoreticalDensity <- merge(pptTheoreticalDensity, PPTAnoms[,c('LEAD', 'PO', 'ClimatatologicalMEAN_PPT_cm')])
pptTheoreticalDensity$ForecastedVal_in <- (pptTheoreticalDensity$x) ^ (1/pptTheoreticalDensity$PO)
pptTheoreticalDensity$ForecastedVal_cm <- pptTheoreticalDensity$ForecastedVal_in * 2.54
pptTheoreticalDensity$PPT_CF <- pptTheoreticalDensity$ForecastedVal_cm / pptTheoreticalDensity$ClimatatologicalMEAN_PPT_cm

# experiment with number in untransformed units
pptGenAnoms <- data.table(generatedAnomData[, , "PT_GenForecasted_PO"])
pptGenAnoms$LEAD <- row.names(pptGenAnoms)
pptGenAnoms$LEAD <- factor(pptGenAnoms$LEAD, levels = c(1:12))
pptGenAnoms <- melt(pptGenAnoms, id.vars = 'LEAD')
pptGenAnomsMean <- pptGenAnoms[,.(Forecast_mean = mean(value)),.(LEAD)]

ggplot() +
  
  geom_point(data = pptTheoreticalDensity, aes(x = x, y = y), color = 'darkgreen',  size = .1) +
  geom_density(data = pptGenAnoms, aes(value), alpha=.2, fill="#FF6666") +
  geom_density(data = pptNWSDensity, aes(vals), alpha=.2, fill="blue") + 
  
  facet_wrap(~LEAD, scales = 'free') +
  labs(title = 'Precipitation by LEAD; Forecast PO') +
  
  geom_vline(data = pptGenAnomsMean, aes(xintercept = Forecast_mean), color = 'red')+
  geom_vline(data = pptNWSDensityMean, aes(xintercept = Anom_mean), color = 'red', lty = 'dashed') +
  geom_vline(data = pptTheoreticalDensityMean, aes(xintercept = Anom_mean), color = 'darkgreen', lty = 'twodash') +
  theme_bw()

# ----------------- in centimeters ------------------------------------
pptGenAnoms <- data.table(generatedAnomData[, , "PPT_GenForecasted_cm"])
pptGenAnoms$LEAD <- row.names(pptGenAnoms)
pptGenAnoms <- melt(pptGenAnoms, id.vars = 'LEAD')
pptGenAnomsMean <- pptGenAnoms[,.(Forecast_mean = mean(value)),.(LEAD)]

ggplot() +
  geom_point(data = pptTheoreticalDensity, aes(x = ForecastedVal_cm, y = y), color = 'darkgreen',  size = .1) +
  geom_density(data = pptGenAnoms, aes(value), alpha=.2, fill="#FF6666") +
  geom_density(data = pptNWSDensity, aes(ForecastedVal_cm), alpha=.2, fill="blue") +
  
  facet_wrap(~LEAD, scales = 'free') +
  labs(title = 'Precipitation by LEAD; Forecast cm') +
  
  geom_vline(data = pptGenAnomsMean, aes(xintercept = Forecast_mean), color = 'red')+
  geom_vline(data = PPTAnoms[1:12, ], aes(xintercept = ForecastedMEAN_PPT_cm), color = 'blue',  lty = 'dashed')+
  theme_bw()


# -----------------------------------------------------------
pptGenAnoms <- data.table(generatedAnomData [, , "PPT_CF"])
pptGenAnoms$LEAD <- row.names(pptGenAnoms)
pptGenAnoms <- melt(pptGenAnoms, id.vars = 'LEAD')
pptGenAnomsMean <- pptGenAnoms[,.(PPT_CF = mean(value)),.(LEAD)]

ggplot() +
  geom_point(data = pptTheoreticalDensity, aes(x = PPT_CF, y = y), color = 'darkgreen', size = .1) +
  
  geom_density(data = pptGenAnoms, aes(value), alpha=.2, fill="#FF6666") +
  geom_density(data = pptNWSDensity, aes(PPT_CF), alpha=.2, fill="blue") +
  
  facet_wrap(~LEAD, scales = 'free') +
  labs(title = 'Precipitation by LEAD; generated Correction Facotr') +
  
  geom_vline(data = pptGenAnomsMean, aes(xintercept = PPT_CF), color = 'red') +
  geom_vline(data = PPTAnoms[1:12, ], aes(xintercept = Anom_CF), color = 'blue',  lty = 'dashed')+
  theme_bw()

