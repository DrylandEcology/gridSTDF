library(ggplot2)
# prep data for final outputs. data here is used in temp and ppt and vwc plots

######################################################################################
################# Data organization #################
######################################################################################
readIn <- 0

currMonth <- month(Sys.Date())

# Historical Data -------------------------------------------------------------------
if(readIn){
  HistDataNormMean <- fread('ExampleData/HistDataNormMean.csv')
  MonthlyAnoms <- fread('ExampleData/MonthlyAnoms.csv')
  TempAnoms <- fread('ExampleData/TempAnoms.csv')
  PPTAnoms <- fread('ExampleData/PPTAnoms.csv')
  AnomRunStats <- fread('ExampleData/AnomRunStats.csv')
  
}

# fix dates so that (1) Time repeats itself 1.5 times and (2) it sees these averages as historical date . 
# Restructure Dates
todayMonthDay <- format(Sys.Date() , format="%m-%d")

#  Get data for prior 6 months
sixMonthsAgo <- format(Sys.Date() - 183,  format="%m-%d")
r1 <- which(grepl(todayMonthDay, HistDataNormMean$Date))
r2 <- which(grepl(sixMonthsAgo, HistDataNormMean$Date))
dateIndex <- if( r2 > r1) { 
    c(r2:366, 1:(r1- 1))
  } else{
    c(r2:(r1 - 1))  
  }
HistDataNormMean_18MNs <- HistDataNormMean[dateIndex,]

HistDataNormMean_18MNs$Year <- ifelse(HistDataNormMean_18MNs$Date > todayMonthDay, 2019, 2020)
HistDataNormMean_18MNs$Date <- as.Date(paste(HistDataNormMean_18MNs$Year, HistDataNormMean_18MNs$Date, sep = '-'))
HistDataNormMean_18MNs <- setorder(HistDataNormMean_18MNs, Date) 

#  Format historical record 1 year forward fromt today's date ----------------------------------------------
HistDataNormMean$Year <- ifelse(HistDataNormMean$Date > todayMonthDay, 2020, 2021)
# when you convert to Date, elimantes Feb. 29 if that year does not have a 2-29
HistDataNormMean$Date <- as.Date(paste(HistDataNormMean$Year, HistDataNormMean$Date, sep = '-'))
HistDataNormMean <- setorder(HistDataNormMean, Date) 

# Append
HistDataNormMean_18MNs <- rbind(HistDataNormMean_18MNs, HistDataNormMean)
HistDataNormMean_18MNs <- HistDataNormMean_18MNs[complete.cases(HistDataNormMean_18MNs), ]

# Go to the first day of the same month of the next year
HistDataNormMean_18MNs <- HistDataNormMean_18MNs[HistDataNormMean_18MNs$Date < as.Date(paste0('2021-',currMonth + 1,'-01')),] # 06 should be currMonth

ggplot() +
 # geom_line(data = AllOut2, aes(Date, avgC_rollmean, color = run_Year)) +
  geom_line(data = HistDataNormMean_18MNs, aes(Date, avgC_rollmean.med), color = 'purple') +
  
 # geom_line(data = AnomRunStats, aes(Date, avgC_rollmean.med),
#            color = 'limegreen')   +
  geom_vline(aes(xintercept = as.Date('2020-02-29')))


# Anomalies -------------------------------------------------------------------------------------------------
# these are the 12 monthly anomalies generated
MonthlyAnoms$Year <- ifelse(MonthlyAnoms$Month < currMonth , 2021, 2020)
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
  NWSAnoms$Anom_cm <- NWSAnoms$Anom_cm/3
  NWSAnomsAll2 <- rbind(NWSAnomsAll2, data.frame( m, NWSAnoms))
}

NWSAnomsAll1 <- unique(NWSAnomsAll1)
NWSAnomsAll1$Year <- ifelse(NWSAnomsAll1$m < 7, 2021, 2020)
NWSAnomsAll1$Date <- as.Date(paste0(NWSAnomsAll1$Year, '-' , NWSAnomsAll1$m, '-05'), format = '%Y-%m-%d')

NWSAnomsAll2 <- unique(NWSAnomsAll2)
NWSAnomsAll2$Year <- ifelse(NWSAnomsAll2$m < 7, 2021, 2020)
NWSAnomsAll2$Date <- as.Date(paste0(NWSAnomsAll2$Year, '-' , NWSAnomsAll2$m, '-05'), format = '%Y-%m-%d')

