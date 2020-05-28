library(ggplot2)
library(lubridate)

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
HistDataNormMean_18MNs <- HistDataNormMean_18MNs[HistDataNormMean_18MNs$Date < as.Date(paste0('2021-','06','-01')),] # 06 should be currMonth

# Anomalies -------------------------------------------------------------------------------------------------

# these are the 12 monthly anomalies generated
AnomSave$Year <- ifelse(AnomSave$Month < 6, 2021, 2020)
AnomSave$Date <- as.Date(paste0(AnomSave$Year, '-' , AnomSave$Month, '-15'), format = '%Y-%m-%d')

# these are the NWS anomalies and lead anomalies
TempAnoms$ForecastDiff <- TempAnoms$ForecastedMEAN - TempAnoms$ClimatologicalMEAN
PPTAnoms$ForecastDiff <- PPTAnoms$ForecastedMEAN - PPTAnoms$ClimatologicalMEAN

# How months and leads relate ----------------------
monthLeads <- data.frame(TempAnoms[,'LEAD'])

if(day(Sys.Date()) < 15){
  monthLeads$Month <- monthLeads$LEAD + currMonth - 1 # a lead of one in the current month before the 15th is the current month
} else {
  monthLeads$Month <- monthLeads$LEAD + currMonth # after the 15th the lead of one is the next month
}
# leads
monthLeads$lead1 <- monthLeads$LEAD
monthLeads$lead2 <-  monthLeads$LEAD - 1
monthLeads$lead3 <-  monthLeads$LEAD - 2

monthLeads[monthLeads <= 0] <- NA 
monthLeads$Month <- ifelse(monthLeads$Month > 12, monthLeads$Month - 12, monthLeads$Month)
monthLeads <- monthLeads[1:12,]

NWSAnomsAll1 <-  NWSAnomsAll2 <- data.frame()
for(m in 1:12){
  leads <- c(t(monthLeads[monthLeads$Month == m, 3:5]))
  
  #  Temp
  NWSAnoms <- TempAnoms[TempAnoms$LEAD %in% leads,c('ForecastDiff', 'ForecastedSD')]
  NWSAnoms <- NWSAnoms * (5/9) # convert
  NWSAnomsAll1 <- rbind(NWSAnomsAll1, data.frame( m, NWSAnoms))

  # PPT
  NWSAnoms <- PPTAnoms[PPTAnoms$LEAD %in% leads,c('ForecastDiff', 'ForecastedSD_in')]
  NWSAnoms <- NWSAnoms * 2.54 # convert
  NWSAnomsAll2 <- rbind(NWSAnomsAll2, data.frame( m, NWSAnoms))
}

NWSAnomsAll1 <- unique(NWSAnomsAll1)
NWSAnomsAll1$Year <- ifelse(NWSAnomsAll1$m < 6, 2021, 2020)
NWSAnomsAll1$Date <- as.Date(paste0(NWSAnomsAll1$Year, '-' , NWSAnomsAll1$m, '-05'), format = '%Y-%m-%d')

NWSAnomsAll2 <- unique(NWSAnomsAll2)
NWSAnomsAll2$Year <- ifelse(NWSAnomsAll2$m < 6, 2021, 2020)
NWSAnomsAll2$Date <- as.Date(paste0(NWSAnomsAll2$Year, '-' , NWSAnomsAll2$m, '-05'), format = '%Y-%m-%d')
names(NWSAnomsAll2)[3] <- 'ForecastedSD_cm'