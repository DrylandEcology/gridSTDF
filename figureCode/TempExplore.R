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

# historical weather data after soilwat
HistDataNormMean2 <- HistDataAll[HistDataAll$Year %in% 1981:2010, ]


p2 <- ggplot() +
  #geom_line(aes(avg_C)) +
  geom_line(data = HistDataNormMean2, aes(Date, y = avgC_rollmean)) +
  geom_line(data = HistDataNormMean, aes(Date, y = avgC_rollmean.med)) 
  
  theme_bw() +
  theme(legend.position = 'none') 
#ggplotly(p1)
ggplotly(p2)








weathAnomOneSim2 <- weathAnomOneSim
weathAnomOneSim2$Date <- as.Date(strptime(paste(weathAnomOneSim2$Year, weathAnomOneSim2$DOY), format="%Y %j"), format="%m-%d-%Y")
weathAnomOneSim2$Tmean <- rowMeans(weathAnomOneSim2[,c('Tmax_C', 'Tmin_C')])
weathAnomOneSim2$RollMean <- runmean(weathAnomOneSim2$Tmean,  k = 30, endrule = 'mean', align = 'center')
ggplot(weathAnomOneSim2, aes(Date)) +
  geom_line(aes(y=Tmean), color = 'blue') +
  geom_line(aes(y = RollMean))