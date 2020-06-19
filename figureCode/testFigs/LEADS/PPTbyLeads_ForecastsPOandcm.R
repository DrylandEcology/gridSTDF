print('Density plots of predicted LEAD Anomalies')

PPTAnoms <- fread('ExampleData/PPTAnoms.csv')
generatedAnomData <- readRDS('ExampleData/generatedAnomData_PPTBiasCorrected')

# PRECIP
# ----------------------------------------------------------------------------------
# --------- 1: NWS random univariate sample (orange)
s <- 30

pptNWSDensity <- PPTAnoms[,.(UVNWS_vals = rnorm(s, mean = ForecastedMEAN_PPT_PO, sd = ForecastedSD)), .(LEAD)]
pptNWSDensityMean <- pptNWSDensity[,.(UVNWSmean = mean(UVNWS_vals)), .(LEAD)]
pptNWSDensityMedian <- pptNWSDensity[,.(UVNWSmedian = median(UVNWS_vals)), .(LEAD)]

# conversions!
pptNWSDensity <- merge(pptNWSDensity, PPTAnoms[,c('LEAD', 'PO', 'ClimatatologicalMEAN_PPT_cm')])
pptNWSDensity$UVNWS_ForecastedVal_in <- (pptNWSDensity$UVNWS_vals) ^ (1/pptNWSDensity$PO)
pptNWSDensity$UVNWS_ForecastedVal_cm <- pptNWSDensity$UVNWS_ForecastedVal_in * 2.54
pptNWSDensity$UVNWS_PPT_CF <- pptNWSDensity$UVNWS_ForecastedVal_cm / pptNWSDensity$ClimatatologicalMEAN_PPT_cm

print('Table of NWS randomly sampled univariate values')
pptNWSDensity

# ---------- 3: Analytical distribution (BLUE)
pptAnalyticalDensity <- data.frame()

for(L in 1:12) {
  z <- subset(PPTAnoms, LEAD == L)

  # sequence of values within 4 sds of the mean
  climmean <- z$ClimatatologicalMEAN_PPT_cm
  meanL <- z$ForecastedMEAN_PPT_PO
  sdL <- z$ForecastedSD
  PO <- z$PO
  
  x <-  seq(meanL - (sdL * 4), 
            meanL + (sdL * 4), length = s)
  
  x_in <-  x ^ (1/PO)
  x_cm <- x_in * 2.54
  x_CF <- x_cm/climmean
  
  # predictions of curve
  y <-  dnorm(x, mean = meanL, sd = sdL)
  # convert
  y_in <- y ^ (1/PO)
  y_cm <- y_in * 2.54
  y_CF <- y_cm/climmean
  
  data <- data.frame(x = x, x_in = x_in, x_cm = x_cm, x_CF = x_CF,
                     y = y,  y_in = y_in, y_cm = y_cm, y_CF = y_CF,
                     LEAD = L, mean = meanL, sd =sdL, PO = PO )
  
  pptAnalyticalDensity <- rbind(pptAnalyticalDensity, data)
  
}

print('Analytical Distribution')
pptAnalyticalDensity

break
############# Transformed Units -------------------------------

pptAnalyticalDensityMean <- setDT(pptAnalyticalDensity)[,.(AnalyticalMean_PO = mean(x)),.(LEAD)]
pptAnalyticalDensityMedian <- setDT(pptAnalyticalDensity)[,.(AnalyticalMedian_PO = median(x)),.(LEAD)]

# ---------- 2: Generated multivariate anomalies (Green)
# transformed forecast
pptGenAnoms <- data.table(generatedAnomData[, , "PT_GenForecasted_PO"])
pptGenAnoms$LEAD <- row.names(pptGenAnoms)
pptGenAnoms$LEAD <- factor(pptGenAnoms$LEAD, levels = c(1:12))
print('Multivariate sampled precipitation values - total forecast amount in transformed units')

#pptGenAnoms
pptGenAnoms <- melt(pptGenAnoms, id.vars = 'LEAD')
pptGenAnomsMean <- pptGenAnoms[,.(MVGenMean = mean(value)),.(LEAD)]
pptGenAnomsMedian <- pptGenAnoms[,.(MVGenMed = median(value)),.(LEAD)]


# Make Table of means 
AllPPTMeans_PO <- cbind(pptNWSDensityMean, MVGenMean = pptGenAnomsMean$MVGenMean, 
                        AnalyticalMean = pptAnalyticalDensityMean$AnalyticalMean_PO, NWS_Forecast_Mean_PO = PPTAnoms$ForecastedMEAN_PPT_PO, PO = PPTAnoms$PO)
AllPPTMedians_PO <- cbind(pptNWSDensityMedian, MVGenMedian = pptGenAnomsMedian$MVGenMed, 
                          AnalyticalMedian = pptAnalyticalDensityMedian$AnalyticalMedian_PO, NWS_Forecast_Mean_PO = PPTAnoms$ForecastedMEAN_PPT_PO, PO = PPTAnoms$PO)

# scale!
bw1 <- .2 # binwidth .. important for scaling y axis to a count
pptAnalyticalDensity$y_scale <- pptAnalyticalDensity$y * bw1 * s

p1 <- ggplot(pptAnalyticalDensity, aes(x = x)) + 
  geom_histogram(data = pptNWSDensity, aes(x = UVNWS_vals), color = 'black', fill = 'orange', alpha = .2, binwidth = bw1) +
  geom_histogram(data = pptGenAnoms, aes(x = value),  color = 'black', fill = 'green', alpha = .2,  binwidth = bw1) +
  geom_line(aes(y = y_scale), color = 'blue', size = 1.5) + 
  
  facet_wrap(~ LEAD, scales = 'free', nrow = 4) +
  
  labs(title = 'Precipitation by LEAD; Total forecasted precip (PO)') +
  theme_bw()

p1Means <- p1 +
  
  labs(title = 'Precipitation by LEAD; Total forecasted precip (PO) 
       with Mean Lines') +
  
  # these are means
  geom_vline(data = pptGenAnomsMean, aes(xintercept = MVGenMean), color = 'green',  size = 1.5)+
  geom_vline(data = pptNWSDensityMean, aes(xintercept = UVNWSmean), color = 'orange',  size = 1.5) +
  geom_vline(data = pptAnalyticalDensityMean, aes(xintercept = AnalyticalMean_PO), color = 'blue',  size = 1.5) +
  geom_vline(data = PPTAnoms[1:12,], aes(xintercept = ForecastedMEAN_PPT_PO), color = 'purple',  size = 1.5) 
  
p1Medians <- p1Means +
  
  labs(title = 'Precipitation by LEAD; Total forecasted precip (PO)
       with Median (dashed) vs. Mean (solid) Lines') +

  # these are medians
  geom_vline(data = pptGenAnomsMedian, aes(xintercept = MVGenMed), color = 'green', lty = 'dashed')+
  geom_vline(data = pptNWSDensityMedian, aes(xintercept = UVNWSmedian), color = 'orange', lty = 'dashed') +
  geom_vline(data = pptAnalyticalDensityMedian, aes(xintercept = AnalyticalMedian_PO), color = 'blue', lty = 'dashed')

print('Table of Means - Total Forecasted PPT (Transformed Units)')

#PPTAnoms
AllPPTMeans_PO
print(p1Means)
print('Table of Medians - Forecasted PPT (Transformed Units)')
AllPPTMedians_PO
print(p1Medians)


# ----------------- in centimeters ------------------------------------
# 1 UV NWS (orange)
pptNWSDensityMean <- pptNWSDensity[,.(UVNWSmean = mean(UVNWS_ForecastedVal_cm)), .(LEAD)]
pptNWSDensityMedian <- pptNWSDensity[,.(UVNWSmedian = median(UVNWS_ForecastedVal_cm)), .(LEAD)]

# 2 MV Gen (Green)
pptGenAnoms <- data.table(generatedAnomData[, , "PPT_GenForecasted_cm"]) # to see how these numbers  are converted to cm .... need to visit generateAnomaly
pptGenAnoms$LEAD <- row.names(pptGenAnoms)
#print('Multivariate sampled precipitation values - total forecast amount in cm')
#pptGenAnoms
pptGenAnoms <- melt(pptGenAnoms, id.vars = 'LEAD')
pptGenAnomsMean <- pptGenAnoms[,.(MVGenMean = mean(value)),.(LEAD)]
pptGenAnomsMedian <- pptGenAnoms[,.(MVGenMed = median(value)),.(LEAD)]

# 3 Analytical (BLUE)
pptAnalyticalDensityMean <- setDT(pptAnalyticalDensity)[,.(AnalyticalMean_cm = mean(x_cm)),.(LEAD)]
pptAnalyticalDensityMedian <- setDT(pptAnalyticalDensity)[,.(AnalyticalMedian_cm = median(x_cm)),.(LEAD)]

# Make Table of means/median
AllPPTMeans_CM <- cbind(pptNWSDensityMean, MVGenMean = pptGenAnomsMean$MVGenMean,
                        AnalyticalMean = pptAnalyticalDensityMean$AnalyticalMean_cm, NWS_Forecast_Mean_cm = PPTAnoms$ForecastedMEAN_PPT_cm)
AllPPTMedians_CM <- cbind(pptNWSDensityMedian, MVGenMedian = pptGenAnomsMedian$MVGenMed, 
                          AnalyticalMedian = pptAnalyticalDensityMedian$AnalyticalMedian_cm, NWS_Forecast_Mean_cm = PPTAnoms$ForecastedMEAN_PPT_cm)

# plot

# scale!
bw2 <- 2 # binwidth .. important for scaling y axis to a count
pptAnalyticalDensity$y_cm_scale <- pptAnalyticalDensity$y_cm * 500

p2 <- ggplot(pptAnalyticalDensity, aes(x = x_cm)) + 
  geom_histogram(data = pptNWSDensity, aes(x = UVNWS_ForecastedVal_cm), color = 'black', fill = 'orange', alpha = .2, binwidth = bw2) +
  geom_histogram(data = pptGenAnoms, aes(x = value),  color = 'black', fill = 'green', alpha = .2,  binwidth = bw2) +
  #geom_line(aes(y = y_cm_scale), color = 'blue', size = 1.5) + 
  
  facet_wrap(~ LEAD, scales = 'free', nrow =4) +
  
  labs(title = 'Precipitation by LEAD; Total forecasted precip (cm)') +
  theme_bw()

p2Means <- p2 +  
  
  labs(title = 'Precipitation by LEAD; Total forecasted precip (cm); With mean lines') +
  
  #means
  geom_vline(data = pptGenAnomsMean, aes(xintercept = MVGenMean), color = 'green', size = 1.5)+
  geom_vline(data = pptNWSDensityMean, aes(xintercept = UVNWSmean), color = 'orange', size = 1.5) +
  geom_vline(data = pptAnalyticalDensityMean, aes(xintercept = AnalyticalMean_cm), color = 'blue', size = 1.5) +
  geom_vline(data = PPTAnoms[1:12,], aes(xintercept = ForecastedMEAN_PPT_cm), color = 'purple', size = 1.5)

p2Medians <- p2Means +  
  
  labs(title = 'Precipitation by LEAD; Total forecasted precip (cm); With mean (solid)
       and medians (dashed) lines') +
  
  # these are medians
  geom_vline(data = pptGenAnomsMedian, aes(xintercept = MVGenMed), color = 'green', lty = 'dashed')+
  geom_vline(data = pptNWSDensityMedian, aes(xintercept = UVNWSmedian), color = 'orange', lty = 'dashed') +
  geom_vline(data = pptAnalyticalDensityMedian, aes(xintercept = AnalyticalMedian_cm), color = 'blue', lty = 'dashed')



print('Table of Means - Total Forecasted PPT (cm)')
AllPPTMeans_CM
p2Means
print('Table of Medians - Total Forecasted PPT (cm)')
AllPPTMedians_CM
p2Medians

