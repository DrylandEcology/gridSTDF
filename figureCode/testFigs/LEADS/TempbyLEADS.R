#print('Density plots of predicted LEAD Anomalies')

#TempAnoms <- fread('ExampleData/TempAnoms.csv')
#generatedAnomData <- readRDS('ExampleData/generatedAnomData')

#------ Check generated anamalies against NWS anomalies
# --------- 1: NWS random univariate sample (MAGENTA)
# tempNWSDensity <- TempAnoms[,.(vals = rnorm(30, mean = Anom_C, sd = ForecastedSD_Temp_C)),
#                             .(LEAD)]
# tempNWSDensityMean <- tempNWSDensity[,.(Univariate_mean = mean(vals)), .(LEAD)]

# ---------- 2: Generated multivariate anomalies (GREEN)
tempGenAnoms <- data.table(generatedAnomData[, , "dT_C"])
tempGenAnoms$LEAD <- factor(tempGenAnoms$LEAD, levels = c(1:12))
tempGenAnoms$LEAD <- row.names(tempGenAnoms)
tempGenAnoms <- melt(tempGenAnoms, id.vars = 'LEAD')
tempGenAnomsMean <- tempGenAnoms[,.(dT_mean = mean(value)),.(LEAD)]

# # ----------- 3: Analytical sample (BLUE)
# nobs = 30
# tempAnalyticalDensity <- data.frame()
# 
# for(L in 1:12){
#   z <- subset(TempAnoms, LEAD == L)
#   
#   meanL <- z$Anom_C
#   sdL <- z$ForecastedSD_Temp_C
#   
#   x <-  seq(meanL - (sdL * 4), 
#             meanL + (sdL * 4), length = nobs)
#   
#   y <-  dnorm(x, mean = z$Anom_C, sd = z$ForecastedSD_Temp_C)
#   
#   data <- data.frame(x = x, y = y, 
#                      LEAD = L, mean = meanL, sd =sdL )
#   
#   tempAnalyticalDensity <- rbind(tempAnalyticalDensity, data)
# }
# 
# tempAnalyticalDensityMean <-setDT(tempAnalyticalDensity)[,.(Anom_mean = mean(x)),.(LEAD)]
# tempAnalyticalDensityMean
# TempAnoms$Anom_C
# 
# bw <- .5
# tempAnalyticalDensity$y_scale <- tempAnalyticalDensity$y * bw * nobs
# 
# 
# table
# data.table
AllTempMeans <- cbind(MVGen_TempAnom_Mean = tempGenAnomsMean$dT_mean, NWS_TempAnom = TempAnoms$Anom_C)
print(AllTempMeans)

# plot
cols <- c("MV Generated Values" = "#7AFF33", "NWS Data" = "#CA33FF")

TempLead <- ggplot() +
  
 # geom_histogram(data = tempNWSDensity, aes(x = vals), color = 'black', alpha=.2, fill="orange", binwidth = bw) +
  geom_histogram(data = tempGenAnoms, aes(x = value),color = 'black', alpha=.2, fill="green", binwidth = .5) +
 # geom_line(data = tempAnalyticalDensity, aes(x = x, y = y_scale), color = 'blue',  size = 1.1) +
  
  facet_wrap(~LEAD, scales = 'free') +
  labs(title = 'Temperature anomalies (C) by Lead') +
  
  geom_vline(data = tempGenAnomsMean, aes(xintercept = dT_mean, color = 'MV Generated Values'), size = 1.5)+
  geom_vline(data = TempAnoms, aes(xintercept = Anom_C, color = 'NWS Data'), size = 1.5, lty = 'dashed') +
  theme_bw() +
  scale_color_manual(name="",values=cols) +
  theme(legend.position = 'bottom')
plot(TempLead)
