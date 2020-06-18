
# Anomalies -------------------------------------------------------------------------------------------------
currMonth <- month(Sys.Date())
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
