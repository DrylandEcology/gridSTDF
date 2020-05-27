getOutputs <- function(sw_out, future = FALSE) {
  
  # Temp and Precip
  Temp1 <- data.frame(sw_out@TEMP@Day)
  if(!future) Temp1 <- Temp1[, c('Year', 'Day', 'avg_C')]
  if(future) Temp1 <- Temp1#[, c('Year', 'Day', 'avg_C')]
  
  PPT1 <-  data.frame(sw_out@PRECIP@Day)
  if(!future) PPT1 <- PPT1[, c('Year', 'Day', 'ppt')]
  if(future) PPT1 <- PPT1[, c('Year', 'Day', 'ppt')]
  
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
  
  return(Data)
  
}
