### Load data
# grid Met data
wdata <- fread('ExampleData/wdata.csv')
wdata$Date <- as.Date(wdata$Date)
# all monthly anomalies
MonthlyAnoms <- fread('ExampleData/MonthlyAnoms.csv')
# HistData
HistDataNormMean <- fread('ExampleData/HistDataNormMean.csv')


### apply anomalies
yearlydat <- MonthlyAnoms[1:12,]
years <- 1981:2010
wdata2 <- wdata[wdata$Year %in% years, ]
weathAnomAll <- suppressWarnings(integrateAnomalyData(wdata2, yearlydat))
weathAnomAll2 <- weathAnomAll
weathAnomAll2$Date <- as.Date(strptime(paste(weathAnomAll2$Year, weathAnomAll2$DOY), format="%Y %j"), format="%m-%d-%Y")

#Look at the weathAnom and wdata rolling mean looks time series normal
wdata2$avg_C <- rowMeans(wdata2[,c('Tmax_C', 'Tmin_C')])
wdata2$avgC_RollMean <- runmean(wdata2$avg_C,  k = 30, endrule = 'mean', align = 'center')

weathAnomAll2$avg_C <- rowMeans(weathAnomAll2[,c('Tmax_C', 'Tmin_C')])
weathAnomAll2$avgC_RollMean <- runmean(weathAnomAll2$avg_C,  k = 30, endrule = 'mean', align = 'center')

wdata3 <- wdata2[wdata2$Year %in% 1981:1985, ]
weathAnomAll3 <- weathAnomAll2[weathAnomAll2$Year %in% 1981:1985, ]


######## lots of variability in winter
yearlydat
ggplot() +
  geom_hline(yintercept = c(seq(-5, 25, by = 0.5)), color = 'gray', alpha = .1) +
  geom_line(data = wdata3, aes(x = Date, y = avgC_RollMean, group = 1)) +
  geom_line(data = weathAnomAll3, aes(x = Date, y = avgC_RollMean, group = 1), color = 'red') +
  theme_bw() + geom_hline(yintercept = 0)  +
  geom_vline(xintercept = c(as.Date('1982-01-01'),as.Date('1983-01-01'),
                            as.Date('1984-01-01'), as.Date('1985-01-01'))) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m", expand = c(0,0)) 
  


HistDataNormMean$Year <- ifelse(HistDataNormMean$Day < yday(Sys.Date()), 2021, 2020)
HistDataNormMean$Date <- as.Date(strptime(paste(HistDataNormMean$Year, HistDataNormMean$Day), format="%Y %j"), format="%m-%d-%Y")
HistDataNormMean <- setorder(HistDataNormMean, Date) 


y <- 1985

currYear <- year(Sys.Date())
currDOY <- yday(Sys.Date())

year1 <- wdata[wdata$Year == (currYear - 1), c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')]
thisYearObservedWData <- wdata[wdata$Year == currYear, c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')]
days <- dim(thisYearObservedWData)[1]

hist <- wdata[wdata$Year == y,]

### ---------
### year 2 - observed data for this year until today's date and then future, weathAnom data
### ---------
year2 <- year2Fut <- weathAnomAll[weathAnomAll$Year == y, ]
year2$Year <- as.integer(currYear) # change year

# add observed data where appropriate
year2[1:currDOY,] <- thisYearObservedWData[1:currDOY,] # this works even when currDOY is 366 and year2 DF only has 365 rows

# if "year2" is 365 days but current year is 366 days 
## what to do?
if(days == 366 & nrow(year2) != 366) {
  year2 <- rbind(year2, year2[365,])
  year2$DOY[366] <- 366
}
### ---------
## year 3 ... forecasts that run into next year (aka 2021) and then scratch data for the rest of 2021
### ---------
year3 <- year2Fut 
if(days == 366) year3 <- year3[1:365,] #if this year is 366 days, next year needs to be 365
year3$Year <- as.integer(currYear + 1)

p1 <- ggplot() +
  geom_line(data = thisYearObservedWData, aes(DOY, Tmax_C), color = 'blue') +
  theme_bw()
p1

p2 <- p1 + geom_line(data = hist,  aes(DOY, Tmax_C), color = 'red') # data for 
p2

p2 <- p2 + geom_line(data = year2Fut, aes(DOY, Tmax_C), color = 'darkgreen') # future
p2

p2 + geom_line(data = year2, aes(DOY, Tmax_C), color = 'purple', lty = 'dashed') # SW
p1 + geom_line(data = year2, aes(DOY, Tmax_C), color = 'purple', lty = 'dashed') # SW

ggplot() +
  geom_line(data = year2Fut, aes(DOY, Tmax_C), color = 'pink') +
  geom_line(data = year3, aes(DOY, Tmax_C), color = 'purple', lty = 'dashed') +
  theme_bw()
  
# SW

weathAnomOneSim <- rbind(year1, year2, year3)
weathAnomOneSim$avg_C <- rowMeans(weathAnomOneSim[,c('Tmax_C', 'Tmin_C')])
weathAnomOneSim$rollmean_avgC <- runmean(weathAnomOneSim$avg_C,  k = 30, endrule = 'mean', align = 'center')#
weathAnomOneSim$Date <- as.Date(strptime(paste(weathAnomOneSim$Year, weathAnomOneSim$DOY), format="%Y %j"), format="%m-%d-%Y")

ggplot() +
  geom_hline(yintercept = c(seq(-15, 25, by = 0.5)), color = 'gray', alpha = .1) +
  
  geom_line(data = weathAnomOneSim, aes(Date, avg_C), color = 'purple') +

  geom_line(data = HistDataNormMean, aes(Date, avgC_rollmean.med), color = 'red') +

  geom_line(data = weathAnomOneSim, aes(Date, rollmean_avgC)) +
  theme_bw()

weathAnomOneSim2 <- weathAnomOneSim[weathAnomOneSim$Year >= 2020, ]
weathAnomOneSim2[360:390,]
ggplot() +
  geom_line(data = weathAnomOneSim2, aes(Date, avg_C), color = 'purple') +
  
  geom_line(data = HistDataNormMean, aes(Date, avgC_rollmean.med), color = 'red') +
  
  geom_line(data = weathAnomOneSim2, aes(Date, rollmean_avgC)) +
  theme_bw()





break






# ----------------------------------

AllOut$runx <- sapply(strsplit(AllOut$run, '_'), '[', 1)
AllOut$runy <- sapply(strsplit(AllOut$run, '_'), '[', 2)

AllOut2 <- subset(AllOut, runx == 1)
AllOut2 <- subset(AllOut2, Year >= 2020)
AllOut3 <- subset(AllOut2, runy %in% 1981:1985)
AllOut4 <- subset(AllOut2, runy %in% 2000:2010)



ggplot() +
  geom_line(data = HistDataNormMean, aes(Date, avgC_rollmean.med)) +

  geom_line(data = AllOut2, aes(Date, avgC_rollmean , color = runy)) +
  
  geom_line(data = AnomRunStats, aes(Date, avgC_rollmean.med), color = 'black', size = 1.1) +
  
  theme_bw() +
  theme(legend.position = 'none')


####### -------------
wdata <- fread('ExampleData/wdata.csv')
weathAnomAll

y <- 1984

histdata <- wdata[wdata$Year == y, c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')]

# observed weather -----------------------------------------------
thisYearObservedWData <- wdata[wdata$Year == currYear, c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')]
days <- dim(thisYearObservedWData)[1]

### ---------
### year 2 - observed data for this year until today's date and then future, weathAnom data
### ---------
year2 <- year2Fut <- weathAnomAll[weathAnomAll$Year == y, ]
year2$Year <- as.integer(currYear) # change year

# add observed data where appropriate
year2[1:currDOY,] <- thisYearObservedWData[1:currDOY,] # this works even when currDOY is 366 and year2 DF only has 365 rows

### ---------
## year 3 ... forecasts that run into next year (aka 2021) and then scratch data for the rest of 2021
### ---------
year3 <- year2Fut  

if(days == 366) year3 <- year3[1:365,] #if this year is 366 days, next year needs to be 365
year3$Year <- as.integer(currYear + 1)

p1 <- ggplot() +
  geom_line(data = thisYearObservedWData, aes(DOY, Tmax_C), color = 'blue') +
  theme_bw()
p1
  
p2 <- p1 + geom_line(data = histdata,  aes(DOY, Tmax_C), color = 'red') # data for 
p2

p2 <- p2 + geom_line(data = year2Fut, aes(DOY, Tmax_C), color = 'darkgreen') # future
p2

p2 + geom_line(data = year2, aes(DOY, Tmax_C), color = 'purple', lty = 'dashed') # SW
p1 + geom_line(data = year2, aes(DOY, Tmax_C), color = 'purple', lty = 'dashed') # SW
p1 + geom_line(data = year3, aes(DOY, Tmax_C), color = 'purple', lty = 'dashed') # SW


# grid Met data
wdata <- fread('ExampleData/wdata.csv')

wdata3 <- wdata[wdata$Year %in% c(1980:2010), ]
names(wdata3)
wdata3$avg_C <- rowMeans(wdata3[,c('Tmax_C', 'Tmin_C')])
wdata3$avgC_RollMean <- runmean(wdata3$avg_C,  k = 30, endrule = 'mean', align = 'center')
# adjust days of year
currDOY
wdata3$DOY2 <- wdata3$DOY - currDOY
wdata3$DOY2 <- ifelse(wdata3$DOY2 < 0, wdata3$DOY2 + 365, wdata3$DOY2)
wdata3$Date <- as.Date(wdata3$Date)
str(wdata3)

wdata4 <- wdata3[wdata3$Year %in% 1990:1994, ]

p1 <- ggplot(wdata4, aes(Date)) +
  geom_line(aes(y=avg_C)) +
  geom_line(aes(y = avgC_RollMean), color = 'red') +
  theme_bw() 
  
p1
ggplotly(p1)





