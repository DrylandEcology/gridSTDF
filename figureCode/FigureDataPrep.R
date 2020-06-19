library(ggplot2)
# prep data for final outputs. data here is used in temp and ppt and vwc plots

######################################################################################
################# Data organization #################
######################################################################################

currMonth <- month(Sys.Date())

# Historical Data -------------------------------------------------------------------

# fix dates so that (1) Time repearts itself 1.5 times and (2) it sees these averages as historical date . 
HistDataNormMean$Year <- ifelse(HistDataNormMean$Day < yday(Sys.Date()), 2021, 2020)
HistDataNormMean$Date <- as.Date(strptime(paste(HistDataNormMean$Year, HistDataNormMean$Day), format="%Y %j"), format="%m-%d-%Y")
HistDataNormMean <- setorder(HistDataNormMean, Date) 

#  Grab Data that is also the same as the previous 6 months
HistDataNormMean_18MNs <- HistDataNormMean[HistDataNormMean$Date > Sys.Date() + 183, ]
HistDataNormMean_18MNs$Year <- HistDataNormMean_18MNs$Year - 1
HistDataNormMean_18MNs$Date <- as.Date(strptime(paste(HistDataNormMean_18MNs$Year, HistDataNormMean_18MNs$Day), format="%Y %j"), format="%m-%d-%Y")

# Append
HistDataNormMean_18MNs <- rbind(HistDataNormMean_18MNs, HistDataNormMean)

# Go to the first day of the same month of the next year
HistDataNormMean_18MNs <- HistDataNormMean_18MNs[HistDataNormMean_18MNs$Date < as.Date(paste0('2021-',currMonth,'-01')),] # 06 should be currMonth



# Anomalies -------------------------------------------------------------------------------------------------
# these are the 12 monthly anomalies generated
MonthlyAnoms$Year <- ifelse(MonthlyAnoms$Month < currMonth, 2021, 2020)
MonthlyAnoms$Date <- as.Date(paste0(MonthlyAnoms$Year, '-' , MonthlyAnoms$Month, '-15'), format = '%Y-%m-%d')

# How months and leads relate ----------------------
monthLeads <- makeMonthLeadRelationshipTable(TempAnoms)

NWSAnomsAll1 <-  NWSAnomsAll2 <- data.frame()

for(m in 1:12){
  mLeads <- c(t(monthLeads[monthLeads$Month == m, 3:5]))
  
  #  Temp
  NWSAnoms <- TempAnoms[TempAnoms$LEAD %in% mLeads,c('Anom_C', 'ForecastedSD_Temp_C')]
  NWSAnomsAll1 <- rbind(NWSAnomsAll1, data.frame( m, NWSAnoms))
  
  # PPT
  NWSAnoms <- PPTAnoms[PPTAnoms$LEAD %in% mLeads,c('Anom_cm', 'Anom_CF','ForecastedSD_PPT_cm')]
  NWSAnomsAll2 <- rbind(NWSAnomsAll2, data.frame( m, NWSAnoms))
}

NWSAnomsAll1 <- unique(NWSAnomsAll1)
NWSAnomsAll1$Year <- ifelse(NWSAnomsAll1$m < 6, 2021, 2020)
NWSAnomsAll1$Date <- as.Date(paste0(NWSAnomsAll1$Year, '-' , NWSAnomsAll1$m, '-05'), format = '%Y-%m-%d')

NWSAnomsAll2 <- unique(NWSAnomsAll2)
NWSAnomsAll2$Year <- ifelse(NWSAnomsAll2$m < 6, 2021, 2020)
NWSAnomsAll2$Date <- as.Date(paste0(NWSAnomsAll2$Year, '-' , NWSAnomsAll2$m, '-05'), format = '%Y-%m-%d')
