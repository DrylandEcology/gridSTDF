print('Density plots of predicted LEAD Anomalies')

TempAnoms <- fread('ExampleData/TempAnoms.csv')
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
  
  geom_histogram(data = tempNWSDensity, aes(x = vals), color = 'black', alpha=.2, fill="orange", binwidth = bw) +
  geom_histogram(data = tempGenAnoms, aes(x = value),color = 'black', alpha=.2, fill="green", binwidth = bw) +
  geom_line(data = tempAnalyticalDensity, aes(x = x, y = y_scale), color = 'blue',  size = 1.1) +
  
  facet_wrap(~LEAD, scales = 'free') +
  labs(title = 'Temperature by LEAD; Anomalies (C)') +
  
  geom_vline(data = tempGenAnomsMean, aes(xintercept = dT_mean), color = 'green')+
  geom_vline(data = tempNWSDensityMean, aes(xintercept = Anom_mean), color = 'orange') +
  geom_vline(data = TempAnoms, aes(xintercept = Anom_C), color = 'blue') +
  theme_bw()

