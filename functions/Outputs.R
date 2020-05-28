getOutputs <- function(sw_out, future = FALSE) {
  
  # Temp and Precip
  Temp1 <- data.frame(sw_out@TEMP@Day)
  Temp1 <- Temp1[, c('Year', 'Day', 'avg_C')]
 
  PPT1 <-  data.frame(sw_out@PRECIP@Day)
  PPT1 <- PPT1[, c('Year', 'Day', 'ppt')]

  # SWP ---------------------------------------------------
  # Step 1 - Get weighted mean across depths for VWC
  VWC1 <-  data.frame(sw_out@VWCMATRIC@Day)
  #if(!future) VWC1 <- VWC1[VWC1$Year != min(VWC1$Year), ]
  
  #VWC1$Month <- month(strptime(paste(VWC1$Year, VWC1$Day, sep = '-'), format = "%Y-%j"))
  VWC1 <- melt(VWC1, id.vars = c('Year', 'Day'))
  VWC1 <-  merge(VWC1, SoilsDF) 
  VWC1 <- setDT(VWC1)[,.(VWC = weighted.mean(value, width)),
                      .(Year, Day, Depth)]
  VWC1 <- dcast(VWC1, Year +  Day ~ Depth, value.var = 'VWC')

  # Join up
  Data <- merge(Temp1, PPT1)
  Data <- merge(Data, VWC1)
  
  # testing .....
  Data2 <- Data[Data$Day != 366,]

  # get rolling sums and means for ppt and temps
  Data2 <- setorder(Data2, Year, Day)
  Data2$ppt_rollsum <- rollapply(Data2$ppt,  width = 30, FUN = sum, fill = 'extend', align = 'center')# for historical data can just calc continuously 30 day sum
  
  Data2$avgC_rollmean <- runmean(Data2$avg_C,  k = 30, endrule = 'mean', align = 'right')# for historical data can just calc continuously 30 day sum
  Data2$VWCShallow_rollmean <- runmean(Data2$Shallow,  k = 30, endrule = 'mean', align = 'right')# for historical data can just calc continuously 30 day sum
  Data2$VWCInter_rollmean <- runmean(Data2$Intermediate,  k = 30, endrule = 'mean', align = 'right')# for historical data can just calc continuously 30 day sum
  Data2$VWCDeep_rollmean <- runmean(Data2$Deep,  k = 30, endrule = 'mean', align = 'right')# for historical data can just calc continuously 30 day sum
  
  if(future == TRUE) {
  Data2$Date <- as.Date(strptime(paste(Data2$Year, Data2$Day), format="%Y %j"), format="%m-%d-%Y")
  Data2 <- Data2[Data2$Date < as.Date(paste0('2021-', '06','-01' )),] # as.Date(paste('2021-', month(Sys.Date()),'-01' ))
  }
   return(Data2)
  
}
