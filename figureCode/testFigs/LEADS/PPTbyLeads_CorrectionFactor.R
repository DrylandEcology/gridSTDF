source('figureCode/testFigs/LEADS/PPTByLeadPrep.R')

# ---------------------- CF -----------------------------
# CF values calculated as forecasted val / CLIMATOLOGICAL MEDIAN
# 1 (Orange)
pptNWSDensityMean <- pptNWSDensity[,.(UVNWSmean = mean(UVNWS_PPT_CF)), .(LEAD)]
pptNWSDensityMedian <- pptNWSDensity[,.(UVNWSmedian = median(UVNWS_PPT_CF)), .(LEAD)]

# 2 (Green)
pptGenAnoms <- data.table(generatedAnomData [, , "PPT_CF"])
pptGenAnoms$LEAD <- row.names(pptGenAnoms)
pptGenAnoms <- melt(pptGenAnoms, id.vars = 'LEAD')
pptGenAnomsMean <- pptGenAnoms[,.(MVGenMean = mean(value)),.(LEAD)]
pptGenAnomsMedian <- pptGenAnoms[,.(MVGenMed = median(value)),.(LEAD)]

# 3 Analytical (BLUE)
pptAnalyticalDensityMean <- setDT(pptAnalyticalDensity)[,.(AnalyticalMean_CF = mean(x_CF)),.(LEAD)]
pptAnalyticalDensityMedian <- setDT(pptAnalyticalDensity)[,.(AnalyticalMedian_CF = median(x_CF)),.(LEAD)]

# Make Table of means
AllPPTMeans_CF <- cbind(pptNWSDensityMean, MVGenMean = pptGenAnomsMean$MVGenMean, 
                        NWS_PPT_CF = PPTAnoms$Anom_CF)
AllPPTMedians_CF <- cbind(pptNWSDensityMedian, MVGenMed = pptGenAnomsMedian$MVGenMed, 
                         NWS_PPT_CF = PPTAnoms$Anom_CF)

# plot

# scale!
bw3 <- .25 # binwidth .. important for scaling y axis to a count
#pptAnalyticalDensity$y_CF_scale <- pptAnalyticalDensity$y_CF * bw3 * nobs

p3 <- ggplot(pptAnalyticalDensity, aes(x = x_CF)) + 
  geom_histogram(data = pptNWSDensity, aes(x = UVNWS_PPT_CF), color = 'black', fill = 'orange', alpha = .2, binwidth = bw3) +
  geom_histogram(data = pptGenAnoms, aes(x = value),  color = 'black', fill = 'green', alpha = .2,  binwidth = bw3) +
  
  facet_wrap(~ LEAD, scales = 'free', nrow =4) +
  theme_bw() 

p3Means <- p3 +  
  
  labs(title = 'Precipitation by LEAD; Correction Factor); With mean lines') +
  
  #means
  geom_vline(data = pptGenAnomsMean, aes(xintercept = MVGenMean), color = 'green', size = 1.3)+
  geom_vline(data = pptNWSDensityMean, aes(xintercept = UVNWSmean), color = 'orange', size = 1.3, lty = 'dashed') +
 # geom_vline(data = pptAnalyticalDensityMean, aes(xintercept = AnalyticalMean_CF), color = 'blue', size = 1.5) +
  geom_vline(data = PPTAnoms[1:12,], aes(xintercept = Anom_CF), color = 'purple', size = 1.3,  lty = 'dotted')


p3Medians <- p3 +
  
  labs(title = 'Precipitation by LEAD; Correction Factor; with median lines') +
  
  geom_vline(data = pptGenAnomsMedian, aes(xintercept = MVGenMed), color = 'green', size = 1.3) +
  geom_vline(data = pptNWSDensityMedian, aes(xintercept = UVNWSmedian), color = 'orange', size = 1.3, lty = 'dashed')+
  geom_vline(data = pptAnalyticalDensityMedian, aes(xintercept = AnalyticalMedian_CF), color = 'blue', size = 1.3) +
  
  geom_vline(data = PPTAnoms[1:12,], aes(xintercept = Anom_CF), color = 'purple', size = 1.3, lty = 'dotted') 


p3MediansZoom <- p3Medians +    xlim(0, 2.5)
# ---------------------------------
print('Table of Means - Precipitation Correction Factor')
print(AllPPTMeans_CF)
#p3Means
print(p3Means +    xlim(0, 2.5))
#ggsave(paste0('figureCode/testFigs/LEADS/figsaves/pptCFMeans', Nrep,'.png'), height = 12, width = 12)

#print('Table of Medians - Correction Factor')
#AllPPTMedians_CF
#p3Medians
#p3MediansZoom

#ggsave(paste0('figureCode/testFigs/LEADS/figsaves/pptCFMedians', Nrep,'.png'), height = 12, width = 12)
