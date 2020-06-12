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
tempNWSDensity <- TempAnoms[,.(vals = rnorm(1000, mean = Anom_C, sd = ForecastedSD_Temp_C)),
                            .(LEAD)]
tempNWSDensityMean <- tempNWSDensity[,.(Anom_mean = mean(vals)), .(LEAD)]

# ---------- 2: Generated multivariate anomalies (GREEN)
tempGenAnoms <- data.table(generatedAnomData[, , "dT_C"])
tempGenAnoms$LEAD <- factor(tempGenAnoms$LEAD, levels = c(1:12))
tempGenAnoms$LEAD <- row.names(tempGenAnoms)
tempGenAnoms <- melt(tempGenAnoms, id.vars = 'LEAD')
tempGenAnomsMean <- tempGenAnoms[,.(dT_mean = mean(value)),.(LEAD)]

# ----------- 3: Analytical sample (BLUE)
nobs = 1000
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
# --------- 1: NWS random univariate sample (Purple)
pptNWSDensity <- PPTAnoms[,.(vals = rnorm(1000, mean = ForecastedMEAN_PPT_PO, sd = ForecastedSD)), .(LEAD)]
pptNWSDensityMean <- pptNWSDensity[,.(Anom_mean = mean(vals)), .(LEAD)]

pptNWSDensity <- merge(pptNWSDensity, PPTAnoms[,c('LEAD', 'PO', 'ForecastedSD', 'ForecastedMEAN_PPT_PO', 'ForecastedMEAN_PPT_cm', 'ClimatatologicalMEAN_PPT_cm')])
pptNWSDensity$ForecastedVal_in <- (pptNWSDensity$vals) ^ (1/pptNWSDensity$PO)
pptNWSDensity$ForecastedVal_cm <- pptNWSDensity$ForecastedVal_in * 2.54
pptNWSDensity$PPT_CF <- pptNWSDensity$ForecastedVal_cm / pptNWSDensity$ClimatatologicalMEAN_PPT_cm

summary(pptNWSDensity)
# ---------- 2: Generated multivariate anomalies (Green)
# transformed forecast
pptGenAnoms <- data.table(generatedAnomData[, , "PT_GenForecasted_PO"])
pptGenAnoms$LEAD <- row.names(pptGenAnoms)
pptGenAnoms$LEAD <- factor(pptGenAnoms$LEAD, levels = c(1:12))
pptGenAnoms <- melt(pptGenAnoms, id.vars = 'LEAD')
pptGenAnomsMean <- pptGenAnoms[,.(Forecast_mean = mean(value)),.(LEAD)]

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
            meanL + (sdL * 4), length = nobs)
  
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

# scale!
bw1 <- .2 # binwidth .. important for scaling y axis to a count
nobs <- 1000 # sample size
pptAnalyticalDensity$y_scale <- pptAnalyticalDensity$y * bw1 * nobs

ggplot(pptAnalyticalDensity, aes(x = x)) + 
  geom_histogram(data = pptGenAnoms, aes(x = value),  color = 'black', fill = 'green', alpha = .2,  binwidth = bw1) +
  geom_histogram(data = pptNWSDensity, aes(x = vals), color = 'black', fill = 'purple', alpha = .2, binwidth = bw1) +
  geom_line(aes(y = y_scale), color = 'blue', size = 1.5) + 
  
  facet_wrap(~ LEAD, scales = 'free') +
  
  labs(title = 'Precipitation by LEAD; Total forecasted precip (PO)') +
  
  geom_vline(data = pptGenAnomsMean, aes(xintercept = Forecast_mean), color = 'green')+
  geom_vline(data = pptNWSDensityMean, aes(xintercept = Anom_mean), color = 'purple', lty = 'dashed') +
  geom_vline(data = pptAnalyticalDensity, aes(xintercept = mean), color = 'blue', lty = 'twodash') +
  theme_bw()

# ----------------- in centimeters ------------------------------------
# 1 (BLUE)
pptNWSDensityMean <- pptNWSDensity[,.(Anom_mean = mean(ForecastedVal_cm)), .(LEAD)]

# 2 (RED)
pptGenAnoms <- data.table(generatedAnomData[, , "PPT_GenForecasted_cm"])
pptGenAnoms$LEAD <- row.names(pptGenAnoms)
pptGenAnoms <- melt(pptGenAnoms, id.vars = 'LEAD')
pptGenAnomsMean <- pptGenAnoms[,.(Forecast_mean = mean(value)),.(LEAD)]

# 3 (GREEN)
# scale!
bw2 <- 2 # binwidth .. important for scaling y axis to a count
pptAnalyticalDensity$y_cm_scale <- pptAnalyticalDensity$y_cm * 500
summary(pptAnalyticalDensity)

ggplot(pptAnalyticalDensity, aes(x = x_cm)) + 
  geom_histogram(data = pptGenAnoms, aes(x = value),  color = 'black', fill = 'green', alpha = .2,  binwidth = bw2) +
  geom_histogram(data = pptNWSDensity, aes(x = ForecastedVal_cm), color = 'black', fill = 'purple', alpha = .2, binwidth = bw2) +
 # geom_line(aes(y = y_cm_scale), color = 'blue', size = 1.5) + 
  
  facet_wrap(~ LEAD, scales = 'free') +
  
  labs(title = 'Precipitation by LEAD; Total forecasted precip (cm)') +
  
  geom_vline(data = pptGenAnomsMean, aes(xintercept = Forecast_mean), color = 'green')+
  geom_vline(data = pptNWSDensityMean, aes(xintercept = Anom_mean), color = 'purple', lty = 'dashed') +
  geom_vline(data = PPTAnoms[1:12,], aes(xintercept = ForecastedMEAN_PPT_cm), color = 'blue', lty = 'twodash') 

  
# -----------------------------------------------------------
# 1 (BLUE)
pptNWSDensityMean <- pptNWSDensity[,.(Anom_mean = mean(PPT_CF)), .(LEAD)]

# 2 (RED)
pptGenAnoms <- data.table(generatedAnomData [, , "PPT_CF"])
pptGenAnoms$LEAD <- row.names(pptGenAnoms)
pptGenAnoms <- melt(pptGenAnoms, id.vars = 'LEAD')
pptGenAnomsMean <- pptGenAnoms[,.(PPT_CF = mean(value)),.(LEAD)]

# 3 (GREEN)
# scale!
bw3 <- .25 # binwidth .. important for scaling y axis to a count
pptAnalyticalDensity$y_CF_scale <- pptAnalyticalDensity$y_CF * bw3 * nobs

ggplot(pptAnalyticalDensity, aes(x = x_CF)) + 
  geom_histogram(data = pptGenAnoms, aes(x = value),  color = 'black', fill = 'green', alpha = .2,  binwidth = bw3) +
  geom_histogram(data = pptNWSDensity, aes(x = PPT_CF), color = 'black', fill = 'purple', alpha = .2, binwidth = bw3) +
  geom_line(aes(y = y_CF_scale), color = 'blue', size = 1.5)  +
  
  facet_wrap(~LEAD, scales = 'free') +
  labs(title = 'Precipitation by LEAD; Correction Factor') +
  
  geom_vline(data = pptGenAnomsMean, aes(xintercept = PPT_CF), color = 'green') +
  geom_vline(data = pptNWSDensityMean, aes(xintercept = Anom_mean), color = 'purple',  lty = 'dashed')+
  geom_vline(data = PPTAnoms[1:12,], aes(xintercept = Anom_CF), color = 'blue', lty = 'twodash') +

  theme_bw()  
 # xlim(0, 2.5)
   
