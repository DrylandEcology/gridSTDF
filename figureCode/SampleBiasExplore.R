AllMediansMV <- data.table()
AllMediansUV <- data.table()
s <- 1000
for(i in 1:1000){
  
  ### MV
  generatedAnomData <- generateAnomalyData(monthlyWdata, TempAnoms, PPTAnoms, 
                                    leads = 1:12, Nleads, n = s)
  
  pptGenAnoms <- data.table(generatedAnomData [, , "PPT_CF"])
  pptGenAnoms$LEAD <- row.names(pptGenAnoms)
  pptGenAnoms <- melt(pptGenAnoms, id.vars = 'LEAD')
  pptGenAnomsMedian <- pptGenAnoms[,.(MVGenMedPPT = median(value)),.(LEAD)]

  tempGenAnoms <- data.table(generatedAnomData [, , "dT_C"])
  tempGenAnoms$LEAD <- row.names(tempGenAnoms)
  tempGenAnoms <- melt(tempGenAnoms, id.vars = 'LEAD')
  tempGenAnomsMedian <- tempGenAnoms[,.(MVGenMedTemp = median(value)),.(LEAD)]
  
  AllMediansMV <- rbind(AllMediansMV, cbind(pptGenAnomsMedian, tempGenAnomsMedian))
  
  ### UV
  pptNWSDensity <- PPTAnoms[,.(UVNWS_vals = rnorm(s, mean = ForecastedMEAN_PPT_PO, sd = ForecastedSD)), .(LEAD)]
  
  pptNWSDensity <- merge(pptNWSDensity, PPTAnoms[,c('LEAD', 'PO', 'ClimatatologicalMEAN_PPT_cm')])
  pptNWSDensity$UVNWS_ForecastedVal_in <- (pptNWSDensity$UVNWS_vals) ^ (1/pptNWSDensity$PO)
  pptNWSDensity$UVNWS_ForecastedVal_cm <- pptNWSDensity$UVNWS_ForecastedVal_in * 2.54
  pptNWSDensity$UVNWS_PPT_CF <- pptNWSDensity$UVNWS_ForecastedVal_cm / pptNWSDensity$ClimatatologicalMEAN_PPT_cm
  pptNWSDensityMedian <- pptNWSDensity[,.(UVNWSMed_PPT = median(UVNWS_PPT_CF)),.(LEAD)]
  
  tempNWSDensity <- TempAnoms[,.(UVNWS_vals = rnorm(s, mean = ForecastedMEAN_Temp_C, sd = ForecastedSD_Temp_C)), .(LEAD)]
  tempNWSDensity <- merge(tempNWSDensity, TempAnoms[,c('LEAD', 'ClimatologicalMEAN_Temp_C')])
  tempNWSDensity$Anom_C <- tempNWSDensity$UVNWS_vals - tempNWSDensity$ClimatologicalMEAN_Temp_C
  tempNWSDensityMedian <- tempNWSDensity[,.(UVNWSMed_Temp = median(Anom_C)),.(LEAD)]
  
  
  AllMediansUV <- rbind(AllMediansUV, cbind(pptNWSDensityMedian, tempNWSDensityMedian))
  
}

AllMediansMV[,1] <- NULL
AllMediansUV[,1] <- NULL

NWS_PPT_CF <- data.frame( PPTAnoms[,c('LEAD','ForecastedMEAN', 'ClimatologicalMEAN', 'Anom_CF')])
MVMed <- AllMediansMV[,.(MVGenMed = median(MVGenMedPPT, na.rm = TRUE)),.(LEAD)]
UVMed <- AllMediansUV[,.(UVGenMed = median(UVNWSMed_PPT, na.rm = TRUE)),.(LEAD)]
  
SamplePlotPPT <- ggplot() + 
  # Multivariate in green
  geom_histogram(data = AllMediansMV, aes(x = MVGenMedPPT),  color = 'black', fill = 'green', alpha = .5, binwidth = .005) +
 # univariate in orange
  geom_histogram(data = AllMediansUV, aes(x = UVNWSMed_PPT),  color = 'black', fill = 'orange', alpha = .5, binwidth = .005) +

  geom_vline(data = MVMed, aes(xintercept = MVGenMed), color = 'green',size = 1.5) +
  geom_vline(data = UVMed, aes(xintercept = UVGenMed), color = 'orange', size = 2) +
  # purple nws cf
  geom_vline(data = NWS_PPT_CF, aes(xintercept = Anom_CF), color = 'magenta', size = 1.5, lty = 'dashed')  +
  facet_wrap(~ LEAD, scales = 'free', nrow = 6) +
  labs(y = 'CorrectionFactor') +
  
  theme_bw() 
SamplePlotPPT
ggsave('figureCode/SamplingExplorePPT.png', height = 12, width = 8)

## Same fig for temperature

NWS_Temp_CF <- data.frame( TempAnoms[,c('LEAD','ForecastedMEAN_Temp_C', 'ClimatologicalMEAN_Temp_C', 'Anom_C')])
MVMed <- AllMediansMV[,.(MVGenMed = median(MVGenMedTemp, na.rm = TRUE)),.(LEAD)]
UVMed <- AllMediansUV[,.(UVGenMed = median(UVNWSMed_Temp, na.rm = TRUE)),.(LEAD)]



SamplePlotTemp <- ggplot() + 
  # Multivariate in green
  geom_histogram(data = AllMediansMV, aes(x = MVGenMedTemp),  color = 'black', fill = 'green', alpha = .5, binwidth = .005) +
  # univariate in orange
  geom_histogram(data = AllMediansUV, aes(x = UVNWSMed_Temp),  color = 'black', fill = 'orange', alpha = .5, binwidth = .005) +
  
  geom_vline(data = MVMed, aes(xintercept = MVGenMed), color = 'green',size = 1.5) +
  geom_vline(data = UVMed, aes(xintercept = UVGenMed), color = 'orange', size = 2) +
  # purple nws anom
  geom_vline(data = NWS_Temp_CF, aes(xintercept = Anom_C), color = 'magenta', size = 1.5, lty = 'dashed')  +
  facet_wrap(~ LEAD, scales = 'free', nrow = 6) +
  
  labs(y = 'Anomaly in Celsius') +
  theme_bw() 
SamplePlotTemp
ggsave('figureCode/SamplingExploreTemp.png', height = 12, width = 8)



