Data2 <- HistDataAll1

benchmark(
  't1' = {
  zoo::rollapply(Data2$ppt,  width = 30, FUN = sum, fill = 'extend', align = 'center')# for historical data can just calc continuously 30 day sum
  },
  't2' = {
    x <- frollsum(Data2[,'ppt'], 30, align = 'center') ### so much faster
    x[1:14] <- x[15]
    x[(length(x)-14):length(x)] <- x[length(x)-15]
    }
  
)

Data2$ppt_rollsum <- zoo::rollapply(Data2$ppt,  width = 30, FUN = sum, fill = 'extend', align = 'center')# for historical data can just calc continuously 30 day sum
#Data2$ppt_rollsum2 <- 
Data2$ppt_rollsum2 <- frollsum(Data2[,'ppt'], 30, align = 'center')
Data2$ppt_rollsum2[1:14] <- Data2$ppt_rollsum2[15]
Data2$ppt_rollsum2[(length(Data2$ppt_rollsum2)-14):length(Data2$ppt_rollsum2)] <- Data2$ppt_rollsum2[length(Data2$ppt_rollsum2)-15]




benchmark(
  't1' = {
    Data2$avg_C_rollmean <- caTools::runmean(Data2$avg_C,  k = 30, endrule = 'mean', align = 'center')#
  },
  't2' = {
    Data2$avg_C_rollmean2 <- frollmean(Data2[,'avg_C'], 30, align = 'center')
  }
  
)
Data2$avg_C_rollmean <- caTools::runmean(Data2$avg_C,  k = 30, endrule = 'mean', align = 'center')#
Data2$avg_C_rollmean2 <- frollmean(Data2[,'avg_C'], 30, align = 'center')
Data2$avg_C_rollmean2 <- frollmean(Data2[,'avg_C'], 30, align = 'center')
Data2$avg_C_rollmean2 <- frollmean(Data2[,'avg_C'], 30, align = 'center')
Data2$avg_C_rollmean2 <- frollmean(Data2[,'avg_C'], 30, align = 'center')

Data2$avg_C_rollmean2 <- frollmean(Data2[,'avg_C'], 30, align = 'center')
Data3 <- Data2[,.SD(frollmean), .SDcols = c('avg_C')]

Data2$avg_C_rollmean2 <- frollmean(Data2[,'avg_C'], 30, align = 'center')
Data2$avg_C_rollmean2 <- frollmean(Data2[,'avg_C'], 30, align = 'center')
Data2$avg_C_rollmean2 <- frollmean(Data2[,'avg_C'], 30, align = 'center')
Data2$VWC.Shallow_rollmean <- caTools::runmean(Data2$VWC.Shallow,  k = 30, endrule = 'mean', align = 'center')

    