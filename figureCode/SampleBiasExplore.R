AllMediansMV <- data.table()
AllMediansUV <- data.table()
s <- 1000
for(i in 1:1000){
  
  ### MV
  generatedAnomData <- generateAnomalyData(monthlyWdata, TempAnoms, PPTAnoms, 
                                    leads = 1:12, Nleads, n = 1000)
  
  pptGenAnoms <- data.table(generatedAnomData [, , "PPT_CF"])
  pptGenAnoms$LEAD <- row.names(pptGenAnoms)
  pptGenAnoms <- melt(pptGenAnoms, id.vars = 'LEAD')
  pptGenAnomsMedian <- pptGenAnoms[,.(MVGenMed = median(value)),.(LEAD)]

  AllMediansMV <- rbind(AllMediansMV, pptGenAnomsMedian)
  
  ### UV
  pptNWSDensity <- PPTAnoms[,.(UVNWS_vals = rnorm(s, mean = ForecastedMEAN_PPT_PO, sd = ForecastedSD)), .(LEAD)]
  
  pptNWSDensity <- merge(pptNWSDensity, PPTAnoms[,c('LEAD', 'PO', 'ClimatatologicalMEAN_PPT_cm')])
  pptNWSDensity$UVNWS_ForecastedVal_in <- (pptNWSDensity$UVNWS_vals) ^ (1/pptNWSDensity$PO)
  pptNWSDensity$UVNWS_ForecastedVal_cm <- pptNWSDensity$UVNWS_ForecastedVal_in * 2.54
  pptNWSDensity$UVNWS_PPT_CF <- pptNWSDensity$UVNWS_ForecastedVal_cm / pptNWSDensity$ClimatatologicalMEAN_PPT_cm
  pptNWSDensityMedian <- pptNWSDensity[,.(UVNWSMed = median(UVNWS_PPT_CF)),.(LEAD)]
  
  
  AllMediansUV <- rbind(AllMediansUV, pptNWSDensityMedian)
  
}

NWS_PPT_CF <- data.frame( PPTAnoms[,c('LEAD','ForecastedMEAN', 'ClimatologicalMEAN', 'Anom_CF')])
MVMed <- AllMediansMV[,.(MVGenMed = median(MVGenMed, na.rm = TRUE)),.(LEAD)]
UVMed <- AllMediansUV[,.(UVGenMed = median(UVNWSMed, na.rm = TRUE)),.(LEAD)]
  
SamplePlot <- ggplot() + 
  # Multivariate in green
  geom_histogram(data = AllMedians, aes(x = MVGenMed),  color = 'black', fill = 'green', alpha = .5, binwidth = .005) +
 # univariate in orange
  geom_histogram(data = AllMediansUV, aes(x = UVNWSMed),  color = 'black', fill = 'orange', alpha = .5, binwidth = .005) +

  geom_vline(data = MVMed, aes(xintercept = MVGenMed), color = 'green',size = 1.5) +
  geom_vline(data = UVMed, aes(xintercept = UVGenMed), color = 'orange', size = 2) +
  # purple nws cf
  geom_vline(data = NWS_PPT_CF, aes(xintercept = Anom_CF), color = 'magenta', size = 1.5, lty = 'dashed')  +
  facet_wrap(~ LEAD, scales = 'free', nrow = 6) +
  theme_bw() 
SamplePlot

ggsave('figureCode/SamplingExplore.png', height = 12, width = 8)



