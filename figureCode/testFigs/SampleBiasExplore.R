AllMeansMV <- data.table()
AllMeansUV <- data.table()
s <- 30
for(i in 1:100){
  
  ### MV
  generatedAnomData <- suppressWarnings(generateAnomalyData(monthlyWdata, TempAnoms, PPTAnoms, 
                                    leads = 1:12, Nleads, n = s))
  
  pptGenAnoms <- data.table(generatedAnomData [, , "PPT_CF"])
  pptGenAnoms$LEAD <- row.names(pptGenAnoms)
  pptGenAnoms <- melt(pptGenAnoms, id.vars = 'LEAD')
  pptGenAnomsMedian <- pptGenAnoms[,.(MVGenMedPPT = median(value)),.(LEAD)]
  pptGenAnomsMean <- pptGenAnoms[,.(MVGenMeanPPT = mean(value)),.(LEAD)]
  
  tempGenAnoms <- data.table(generatedAnomData [, , "dT_C"])
  tempGenAnoms$LEAD <- row.names(tempGenAnoms)
  tempGenAnoms <- melt(tempGenAnoms, id.vars = 'LEAD')
  tempGenAnomsMedian <- tempGenAnoms[,.(MVGenMedTemp = median(value)),.(LEAD)]
  tempGenAnomsMean <- tempGenAnoms[,.(MVGenMeanTemp = mean(value)),.(LEAD)]
  
  AllMeansMV <- rbind(AllMeansMV, cbind(pptGenAnomsMean, tempGenAnomsMean))
  
  ### UV
  pptNWSDensity <- PPTAnoms[,.(UVNWS_vals = rnorm(s, mean = ForecastedMEAN_PPT_PO, sd = ForecastedSD)), .(LEAD)]
  
  pptNWSDensity <- merge(pptNWSDensity, PPTAnoms[,c('LEAD', 'PO', 'ClimatatologicalMEAN_PPT_cm')])
  pptNWSDensity$UVNWS_ForecastedVal_in <- (pptNWSDensity$UVNWS_vals) ^ (1/pptNWSDensity$PO)
  pptNWSDensity$UVNWS_ForecastedVal_cm <- pptNWSDensity$UVNWS_ForecastedVal_in * 2.54
  pptNWSDensity$UVNWS_PPT_CF <- pptNWSDensity$UVNWS_ForecastedVal_cm / pptNWSDensity$ClimatatologicalMEAN_PPT_cm
  pptNWSDensityMedian <- pptNWSDensity[,.(UVNWSMed_PPT = median(UVNWS_PPT_CF)),.(LEAD)]
  pptNWSDensityMean <- pptNWSDensity[,.(UVNWSMean_PPT = mean(UVNWS_PPT_CF)),.(LEAD)]
  
  tempNWSDensity <- TempAnoms[,.(UVNWS_vals = rnorm(s, mean = Anom_C, sd = ForecastedSD_Temp_C)), .(LEAD)]
  #tempNWSDensity <- merge(tempNWSDensity, TempAnoms[,c('LEAD', 'ClimatologicalMEAN_Temp_C')])
  #tempNWSDensity$Anom_C <- tempNWSDensity$UVNWS_vals - tempNWSDensity$ClimatologicalMEAN_Temp_C
  tempNWSDensityMedian <- tempNWSDensity[,.(UVNWSMed_Temp = median(UVNWS_vals)),.(LEAD)]
  tempNWSDensityMean <- tempNWSDensity[,.(UVNWSMean_Temp = mean(UVNWS_vals)),.(LEAD)]
  
  
  AllMeansUV <- rbind(AllMeansUV, cbind(pptNWSDensityMean, tempNWSDensityMean))
  
}
summary(AllMeansUV)
summary(AllMeansMV)

names(AllMeansMV)[1] <- names(AllMeansUV)[1] <- 'LEAD1'
#AllMeansUV[,1] <- NULL

NWS_PPT_CF <- data.frame( PPTAnoms[,c('LEAD','ForecastedMEAN', 'ClimatologicalMEAN', 'Anom_CF')])
MVMean <- AllMeansMV[,.(MVGenMean = mean(MVGenMeanPPT, na.rm = TRUE)),.(LEAD)]
UVMean <- AllMeansUV[,.(UVGenMean = mean(UVNWSMean_PPT, na.rm = TRUE)),.(LEAD)]
  
SamplePlotPPT <- ggplot() + 
  # Multivariate in green
  geom_density(data = AllMeansMV, aes(x = MVGenMeanPPT),  color = 'black', fill = 'green', alpha = .5) +
 # univariate in orange
  #geom_histogram(data = AllMeansUV, aes(x = UVNWSMean_PPT),  color = 'black', fill = 'orange', alpha = .5, binwidth = .005) +

  geom_vline(data = MVMean, aes(xintercept = MVGenMean), color = 'darkgreen') +
#  geom_vline(data = UVMean, aes(xintercept = UVGenMean), color = 'orange', size = 2) +
  # purple nws cf
  geom_vline(data = NWS_PPT_CF, aes(xintercept = Anom_CF), color = 'magenta', size = 1.5)  +
  facet_wrap(~ LEAD, scales = 'free', nrow = 6) +
  labs(x = 'Correction Factor', title = 'Distribution of PPT correction factor means across a 100 datasets') +
  
  theme_bw() 
SamplePlotPPT
ggsave('figureCode/testFigs/SamplingExplorePPT.png', height = 6, width = 6)

## Same fig for temperature

NWS_Temp_CF <- data.frame( TempAnoms[,c('LEAD','ForecastedMEAN_Temp_C', 'ClimatologicalMEAN_Temp_C', 'Anom_C')])
MVMean <- AllMeansMV[,.(MVGenMean = mean(MVGenMeanTemp, na.rm = TRUE)),.(LEAD)]
UVMean <- AllMeansUV[,.(UVGenMean = mean(UVNWSMean_Temp, na.rm = TRUE)),.(LEAD)]



SamplePlotTemp <- ggplot() + 
  # Multivariate in green
  geom_density(data = AllMeansMV, aes(x = MVGenMeanTemp),  color = 'black', fill = 'green', alpha = .5) +
  # univariate in orange
  
  #geom_histogram(data = AllMeansUV, aes(x = UVNWSMean_Temp),  color = 'black', fill = 'orange', alpha = .5, binwidth = .03) +
  
  geom_vline(data = MVMean, aes(xintercept = MVGenMean), color = 'darkgreen',size = 1.5) +
  #geom_vline(data = UVMean, aes(xintercept = UVGenMean), color = 'orange', size = 2) +
  # purple nws anom
  geom_vline(data = NWS_Temp_CF, aes(xintercept = Anom_C), color = 'magenta', size = 1.5, lty = 'dashed')  +
  facet_wrap(~ LEAD, scales = 'free', nrow = 6) +
  
  labs(x = 'Anomaly in Celsius', title = 'Distribution of Anomaly means across a 100 datasets') +
  theme_bw() 
SamplePlotTemp
ggsave('figureCode/testFigs/SamplingExploreTemp.png', height = 6, width = 6)



