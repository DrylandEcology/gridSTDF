#print('Density plots of predicted LEAD Anomalies')

#PPTAnoms <- fread('ExampleData/PPTAnoms.csv')
#generatedAnomData <- readRDS('ExampleData/generatedAnomData_PPTBiasCorrected')

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

#print('Table of NWS randomly sampled univariate values')
#pptNWSDensity

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

#print('Analytical Distribution')
#pptAnalyticalDensity
