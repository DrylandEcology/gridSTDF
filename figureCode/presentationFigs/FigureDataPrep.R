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
