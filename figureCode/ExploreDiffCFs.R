# Approach 1 : Calculating multivariate correction factors all in transformed units
# Approach 2: Calculate correction factor in cm relative mean
# Approach 3: Calculate correction factor in cm relative median
# 
# Make figure
# * On top of that draw vertical lines 
# * Calculate quantiles for each distribution that match up with NWS quantiles

rm(list = ls(all = TRUE))
library(data.table)
library(ggplot2)

Approach1 <- readRDS('ExampleData/generatedAnomData_Approach1')
Approach1 <- data.table(Approach1[, , "PPT_CF"]) # to see how these numbers  are converted to cm .... need to visit generateAnomaly
Approach1$LEAD <- row.names(Approach1)
Approach1 <- melt(Approach1, id.vars = 'LEAD')
Approach1Quantiles <- Approach1[,.(q2. = quantile(value, .02), q10. = quantile(value, .10), q30. = quantile(value, .30),
                                   q50. = quantile(value, .50), q70. = quantile(value, .70), q90. = quantile(value, .90), 
                                   q98. = quantile(value, .98)),
                                  .(LEAD)]
Approach1QuantilesM <- melt(Approach1Quantiles, id.vars = 'LEAD')
names(Approach1QuantilesM)[3] <- 'Approach1_TransformedSpace'

Approach2 <- readRDS('ExampleData/generatedAnomData_Approach2')
Approach2 <- data.table(Approach2[, , "PPT_CF"]) # to see how these numbers  are converted to cm .... need to visit generateAnomaly
Approach2$LEAD <- row.names(Approach2)
Approach2 <- melt(Approach2, id.vars = 'LEAD')

Approach2Quantiles <- Approach2[,.(q2. = quantile(value, .02), q10. = quantile(value, .10), q30. = quantile(value, .30),
                                   q50. = quantile(value, .50), q70. = quantile(value, .70), q90. = quantile(value, .90), 
                                   q98. = quantile(value, .98)),
                               .(LEAD)]
Approach2QuantilesM <- melt(Approach2Quantiles, id.vars = 'LEAD')
names(Approach2QuantilesM)[3] <- 'Approach2_ClimMean'


Approach3 <- readRDS('ExampleData/generatedAnomData_Approach3')
Approach3 <- data.table(Approach3[, , "PPT_CF"]) # to see how these numbers  are converted to cm .... need to visit generateAnomaly
Approach3$LEAD <- row.names(Approach3)
Approach3 <- melt(Approach3, id.vars = 'LEAD')

Approach3Quantiles <- Approach3[,.(q2. = quantile(value, .02), q10. = quantile(value, .10), q30. = quantile(value, .30),
                                  q50. = quantile(value, .50), q70. = quantile(value, .70), q90. = quantile(value, .90), 
                                  q98. = quantile(value, .98)),
                               .(LEAD)]
Approach3QuantilesM <- melt(Approach3Quantiles, id.vars = 'LEAD')
names(Approach3QuantilesM)[3] <- 'Approach3_ClimMedian'

############ Univariate sampling ---------------------------

############ NWS data ----------------
PPTAnoms <- fread('ExampleData/PPTAnoms.csv')

NWS_CFs <- PPTAnoms[,c('LEAD', '2.', '10.', '30.', '50.', '70.', '90.', '98.')]
names(NWS_CFs) <- c('LEAD', '98.', '90.', '70.', '50.', '30.', '10.', '2.')
NWS_CFs <- melt(NWS_CFs, id.vars = 'LEAD')
NWS_CFs <- plyr::join(NWS_CFs, PPTAnoms[,c('LEAD', 'ClimatologicalMEDIAN_PPT_in', 'ClimatologicalMEAN')])
NWS_CFs$NWS_value_CF_Median <- NWS_CFs$value/NWS_CFs$ClimatologicalMEDIAN_PPT_in
NWS_CFs$NWS_value_CF_Mean <- NWS_CFs$value/NWS_CFs$ClimatologicalMEAN

NWS_CFs$variable <- paste0('q',NWS_CFs$variable)


#Table
AllCFs <- plyr::join(Approach2QuantilesM, Approach3QuantilesM)
AllCFs <- plyr::join(AllCFs, NWS_CFs[,c('LEAD', 'variable', 'NWS_value_CF_Median', 'NWS_value_CF_Mean')])
AllCFs
names(AllCFs)[2] <- 'quantile'
AllCFsM <- melt(AllCFs, id.vars = c('LEAD', 'quantile'))
# Plot!
AllCFsM$quantile <- factor(AllCFsM$quantile, levels = c('q2.','q10.', 'q30.', 'q50.', 'q70.','q90.' ,'q98.'))

AllCFsM5 <- subset(AllCFsM, LEAD == 5)
ggplot() +
  geom_point(data = AllCFsM5, aes(x = quantile, y = value, color = variable, shape = variable)) +

  #facet_wrap(.~LEAD, nrow = 6) +
  theme_bw() + theme(legend.position = 'bottom')

