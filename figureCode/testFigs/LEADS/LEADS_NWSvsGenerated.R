print('Density plots of predicted LEAD Anomalies')
# the anomaly from NWS is the thick blue line. Sample distribution of NWS data is blue
# the mean of the our generated anomlies is the dashed red line. Distribution in pink
# theoretical distribution in green
# 100 Future realizations as density plots

TempAnoms <- fread('ExampleData/TempAnoms.csv')
PPTAnoms <- fread('ExampleData/PPTAnoms.csv')
generatedAnomData <- readRDS('ExampleData/generatedAnomData')

#------ Check generated anamalies against NWS anomalies
# --------- 1: NWS random univariate sample (MAGENTA)
tempNWSDensity <- TempAnoms[,.(vals = rnorm(30, mean = Anom_C, sd = ForecastedSD_Temp_C)),
                            .(LEAD)]
tempNWSDensityMean <- tempNWSDensity[,.(Anom_mean = mean(vals)), .(LEAD)]

# ---------- 2: Generated multivariate anomalies (GREEN)
tempGenAnoms <- data.table(generatedAnomData[, , "dT_C"])
tempGenAnoms$LEAD <- factor(tempGenAnoms$LEAD, levels = c(1:12))
tempGenAnoms$LEAD <- row.names(tempGenAnoms)
tempGenAnoms <- melt(tempGenAnoms, id.vars = 'LEAD')
tempGenAnomsMean <- tempGenAnoms[,.(dT_mean = mean(value)),.(LEAD)]

# ----------- 3: Analytical sample (BLUE)
nobs = 30
tempAnalyticalDensity <- data.frame()

  for(L in 1:12){
    z <- subset(TempAnoms, LEAD == L)
    
    meanL <- z$Anom_C
    sdL <- z$ForecastedSD_Temp_C
  
    x <-  seq(meanL - (sdL * 4), 
              meanL + (sdL * 4), length = nobs)
    
    y <-  dnorm(x, mean = z$Anom_C, sd = z$ForecastedSD_Temp_C)
    
    data <- data.frame(x = x, y = y, 
                       LEAD = L, mean = meanL, sd =sdL )
    
    tempAnalyticalDensity <- rbind(tempAnalyticalDensity, data)
  }

tempAnalyticalDensityMean <-setDT(tempAnalyticalDensity)[,.(Anom_mean = mean(x)),.(LEAD)]
tempAnalyticalDensityMean
TempAnoms$Anom_C

bw <- .5
tempAnalyticalDensity$y_scale <- tempAnalyticalDensity$y * bw * nobs

ggplot() +
  
  geom_histogram(data = tempNWSDensity, aes(x = vals), color = 'black', alpha=.2, fill="purple", binwidth = bw) +
  geom_histogram(data = tempGenAnoms, aes(x = value),color = 'black', alpha=.2, fill="green", binwidth = bw) +
  geom_line(data = tempAnalyticalDensity, aes(x = x, y = y_scale), color = 'blue',  size = 1.1) +
  
  facet_wrap(~LEAD, scales = 'free') +
  labs(title = 'Temperature by LEAD; Anomalies (C)') +
  
  geom_vline(data = tempGenAnomsMean, aes(xintercept = dT_mean), color = 'green')+
  geom_vline(data = tempNWSDensityMean, aes(xintercept = Anom_mean), color = 'purple', lty = 'dashed') +
  geom_vline(data = TempAnoms, aes(xintercept = Anom_C), color = 'blue', lty = 'twodash') +
  theme_bw()


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
pptNWSDensity$UVNWS_PPT_CF_MEAN <- pptNWSDensity$UVNWS_ForecastedVal_cm / pptNWSDensity$ClimatatologicalMEAN_PPT_cm

print('Table of NWS randomly sampled univariate values')
pptNWSDensity

# ---------- 3: Analytical distribution (BLUE)
pptAnalyticalDensity <- data.frame()

for(L in 1:12) {
  z <- subset(PPTAnoms, LEAD == L)
  zz <- subset(pptNWSDensity, LEAD == L)
  
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
AllPPTMeans_PO <- cbind(pptNWSDensityMean, pptGenAnomsMean, pptAnalyticalDensityMean, NWS_Forecast_Mean_PO = PPTAnoms$ForecastedMEAN_PPT_PO, PO = PPTAnoms$PO)
AllPPTMedians_PO <- cbind(pptNWSDensityMedian, pptGenAnomsMedian, pptAnalyticalDensityMedian, NWS_Forecast_Mean_PO = PPTAnoms$ForecastedMEAN_PPT_PO, PO = PPTAnoms$PO)

# scale!
bw1 <- .2 # binwidth .. important for scaling y axis to a count
pptAnalyticalDensity$y_scale <- pptAnalyticalDensity$y * bw1 * s
pptAnalyticalDensityMean <- setDT(pptAnalyticalDensity)[,.(AnalyticalMean_PO = mean(x)),.(LEAD)]
pptAnalyticalDensityMedian <- setDT(pptAnalyticalDensity)[,.(AnalyticalMedian_PO = median(x)),.(LEAD)]

p1 <- ggplot(pptAnalyticalDensity, aes(x = x)) + 
  geom_histogram(data = pptGenAnoms, aes(x = value),  color = 'black', fill = 'green', alpha = .2,  binwidth = bw1) +
  geom_histogram(data = pptNWSDensity, aes(x = UVNWS_vals), color = 'black', fill = 'orange', alpha = .2, binwidth = bw1) +
  geom_line(aes(y = y_scale), color = 'blue', size = 1.5) + 
  
  facet_wrap(~ LEAD, scales = 'free', nrow = 6) +
  
  labs(title = 'Precipitation by LEAD; Total forecasted precip (PO)') +
  theme_bw()

p1Means <- p1 +
  
  labs(title = 'Precipitation by LEAD; Total forecasted precip (PO) 
       with Mean Lines') +
  
  geom_vline(data = pptGenAnomsMean, aes(xintercept = MVGenMean), color = 'green')+
  geom_vline(data = pptNWSDensityMean, aes(xintercept = UVNWSmean), color = 'orange') +
  geom_vline(data = pptAnalyticalDensityMean, aes(xintercept = AnalyticalMean_PO), color = 'blue') +
  geom_vline(data = PPTAnoms[1:12,], aes(xintercept = ForecastedMEAN_PPT_PO), color = 'purple') 

p1Medians <- p1 +
  
  labs(title = 'Precipitation by LEAD; Total forecasted precip (PO)
       with Median (dashed) vs. Mean (solid) Lines') +
  # these are means
  geom_vline(data = pptGenAnomsMean, aes(xintercept = MVGenMean), color = 'green')+
  geom_vline(data = pptNWSDensityMean, aes(xintercept = UVNWSmean), color = 'orange') +
  geom_vline(data = pptAnalyticalDensityMean, aes(xintercept = AnalyticalMean_PO), color = 'blue') +
  geom_vline(data = PPTAnoms[1:12,], aes(xintercept = ForecastedMEAN_PPT_PO), color = 'purple') +

  # these are medians
 geom_vline(data = pptGenAnomsMedian, aes(xintercept = MVGenMed), color = 'green', lty = 'dashed', size = 1.5)+
  geom_vline(data = pptNWSDensityMedian, aes(xintercept = UVNWSmedian), color = 'orange', lty = 'dashed', size = 1.5) +
  geom_vline(data = pptAnalyticalDensityMedian, aes(xintercept = AnalyticalMedian_PO), color = 'blue', lty = 'dashed', size = 1.5)

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
print('Multivariate sampled precipitation values - total forecast amount in cm')
#pptGenAnoms

pptGenAnoms <- melt(pptGenAnoms, id.vars = 'LEAD')
pptGenAnomsMean <- pptGenAnoms[,.(MVGenMean = mean(value)),.(LEAD)]
pptGenAnomsMedian <- pptGenAnoms[,.(MVGenMed = median(value)),.(LEAD)]

# 3 Analytical (BLUE)
pptAnalyticalDensityMean <- setDT(pptAnalyticalDensity)[,.(AnalyticalMean_cm = mean(x_cm)),.(LEAD)]
pptAnalyticalDensityMedian <- setDT(pptAnalyticalDensity)[,.(AnalyticalMedian_cm = median(x_cm)),.(LEAD)]

# Make Table of means/median
AllPPTMeans_CM <- cbind(pptNWSDensityMean, pptGenAnomsMean, pptAnalyticalDensityMean, NWS_Forecast_Mean_cm = PPTAnoms$ForecastedMEAN_PPT_cm)
AllPPTMedians_CM <- cbind(pptNWSDensityMedian, pptGenAnomsMedian, pptAnalyticalDensityMedian, NWS_Forecast_Mean_cm = PPTAnoms$ForecastedMEAN_PPT_cm)

# plot

# scale!
bw2 <- 2 # binwidth .. important for scaling y axis to a count
pptAnalyticalDensity$y_cm_scale <- pptAnalyticalDensity$y_cm * 500

p2 <- ggplot(pptAnalyticalDensity, aes(x = x_cm)) + 
  geom_histogram(data = pptGenAnoms, aes(x = value),  color = 'black', fill = 'green', alpha = .2,  binwidth = bw2) +
  geom_histogram(data = pptNWSDensity, aes(x = UVNWS_ForecastedVal_cm), color = 'black', fill = 'orange', alpha = .2, binwidth = bw2) +
  #geom_line(aes(y = y_cm_scale), color = 'blue', size = 1.5) + 
  
  facet_wrap(~ LEAD, scales = 'free', nrow =4) +
  
  labs(title = 'Precipitation by LEAD; Total forecasted precip (cm)') +
  theme_bw()
 
p2Means <- p2 +  
  
  labs(title = 'Precipitation by LEAD; Total forecasted precip (cm); With mean lines') +
  
  #means
  geom_vline(data = pptGenAnomsMean, aes(xintercept = MVGenMean), color = 'green')+
  geom_vline(data = pptNWSDensityMean, aes(xintercept = UVNWSmean), color = 'orange') +
  geom_vline(data = pptAnalyticalDensityMean, aes(xintercept = AnalyticalMean_cm), color = 'blue') +
  geom_vline(data = PPTAnoms[1:12,], aes(xintercept = ForecastedMEAN_PPT_cm), color = 'purple')

p2Medians <- p2 +  
  
  labs(title = 'Precipitation by LEAD; Total forecasted precip (cm); With mean (solid)
       and medians (dashed) lines') +
  
  #means
  geom_vline(data = pptGenAnomsMean, aes(xintercept = MVGenMean), color = 'green')+
  geom_vline(data = pptNWSDensityMean, aes(xintercept = UVNWSmean), color = 'orange') +
  geom_vline(data = pptAnalyticalDensityMean, aes(xintercept = AnalyticalMean_cm), color = 'blue') +
  geom_vline(data = PPTAnoms[1:12,], aes(xintercept = ForecastedMEAN_PPT_cm), color = 'purple') +
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

# ---------------------- CF -----------------------------
# CF values calculated as forecasted val / CLIMATOLOGICAL MEDIAN
# 1 (Orange)
pptNWSDensityMean <- pptNWSDensity[,.(UVNWSmean = mean(UVNWS_PPT_CF_MEAN)), .(LEAD)]
pptNWSDensityMedian <- pptNWSDensity[,.(UVNWSmedian = median(UVNWS_PPT_CF_MEAN)), .(LEAD)]

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
AllPPTMeans_CF <- cbind(pptNWSDensityMean, pptGenAnomsMean, pptAnalyticalDensityMean, NWS_PPT_CF = PPTAnoms$Anom_CF)
AllPPTMedians_CF <- cbind(pptNWSDensityMedian, pptGenAnomsMedian, pptAnalyticalDensityMedian, NWS_PPT_CF = PPTAnoms$Anom_CF)

# plot

# scale!
bw3 <- .25 # binwidth .. important for scaling y axis to a count
#pptAnalyticalDensity$y_CF_scale <- pptAnalyticalDensity$y_CF * bw3 * nobs

p3 <- ggplot(pptAnalyticalDensity, aes(x = x_CF)) + 
  geom_histogram(data = pptGenAnoms, aes(x = value),  color = 'black', fill = 'green', alpha = .2,  binwidth = bw3) +
  geom_histogram(data = pptNWSDensity, aes(x = UVNWS_PPT_CF_MEAN), color = 'black', fill = 'orange', alpha = .2, binwidth = bw3) +
  #geom_line(aes(y = y_CF_scale), color = 'blue', size = 1.5)  +
  
  facet_wrap(~ LEAD, scales = 'free', nrow =4) +
  theme_bw() 

p3Medians <- p3 +
  
  labs(title = 'Precipitation by LEAD; Correction Factor') +
  
  geom_vline(data = pptGenAnomsMedian, aes(xintercept = MVGenMed), color = 'green') +
  geom_vline(data = pptNWSDensityMedian, aes(xintercept = UVNWSmedian), color = 'orange')+
  geom_vline(data = pptAnalyticalDensityMedian, aes(xintercept = AnalyticalMedian_CF), color = 'blue') +
  
  geom_vline(data = PPTAnoms[1:12,], aes(xintercept = Anom_CF), color = 'purple') 


p3MediansZoom <- p3Medians +    xlim(0, 2.5)
# ---------------------------------
print('Table of Medians - Correction Factor')
AllPPTMedians_CF
p3Medians
p3MediansZoom



ggsave('figureCode/')
