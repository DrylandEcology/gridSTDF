rm(list = ls(all = TRUE))

library(rvest)
library(zoo)
library(data.table)

getCPCData <- function(url, var) {
  
  #Reading the HTML code from the website
  tmp <- read_html(url)
  tmpData <- tmp %>%
    html_nodes(xpath = "/html/body") %>%
    html_text() 
  
  # format -------------------------------------------------------------------------------------------
  tmpData2 <- unlist(strsplit(tmpData, split = "\n"))
  # ---- get names ...
  Names <- tmpData2[2]
  Names <- unlist(strsplit(Names, split = " |  |   |    |     |      "))
  Names <- Names[!Names %in% c("F", "C")]
  Names[19:22] <- paste0(rep(c('Forecasted', 'Climatological'),2), Names[19:22])
  
  # finish formatting
  tmpData2 <- tmpData2[-(1:2)]
  tmpData3 <- unlist(strsplit(tmpData2, split = " |  |   "))
  
  if(any(nchar(tmpData3) > 7)){
    tmpData4 <- c()
    for(i in 1:length(tmpData3)) {
    
        if(nchar(tmpData3[i]) > 7) {
          x <- substring(tmpData3[i], c(1, 6, 13), c(5, 12, 19))
          #print(x)
        } else {
            x <- tmpData3[i]
            }
    
      tmpData4 <- c(tmpData4, x)
    }
  } else {
    tmpData4 <- tmpData3
  }
  
  
  tmpData4 <- matrix(tmpData4, ncol = 1326)
  tmpData4 <- t(tmpData4)
  
  tmpData4 <- data.frame(tmpData4, stringsAsFactors = FALSE)
  names(tmpData4) <- Names
  tmpData4[] <- lapply(tmpData4, as.numeric)
  
  # get mean of anomalies ------------------------------------------------------------------------------------------
  if(var == 'temp'){
    tmpData4[,c('ForecastedMEAN', 'ClimatologicalMEAN', 'ForecastedSD', 'ClimatologicalSD')] <- (tmpData4[,c('ForecastedMEAN', 'ClimatologicalMEAN', 'ForecastedSD', 'ClimatologicalSD')] - 32) * (5/9)
  }
  if(var == 'ppt'){
    tmpData4[,c('ForecastedMEAN', 'ClimatologicalMEAN', 'ForecastedSD', 'ClimatologicalSD')] <- tmpData4[,c('ForecastedMEAN', 'ClimatologicalMEAN', 'ForecastedSD', 'ClimatologicalSD')] * 2.54
  }
  
  tmpData4$AnomalyMEAN <- tmpData4$ForecastedMEAN -  tmpData4$ClimatologicalMEAN
  tmpData4$AnomalySD <-  tmpData4$ForecastedSD -  tmpData4$ClimatologicalSD
  
  tmpData4 <- tmpData4[order(tmpData4$CD, tmpData4$LEAD),]
  
  tmpData5 <- setDT(tmpData4)[,.( MeanAnomaly = round(rollapply(AnomalyMEAN, 3, mean, na.rm = TRUE, fill = NA, align = 'right', partial = 1), 4),
                                  SDAnomaly = round(rollapply(AnomalySD, 3, mean, na.rm = TRUE, fill = NA, align = 'right', partial = 1), 4)),
    .(MN, CD)]
  
  # format months... just keep 12 months, and eliminate the 13th (i.e. if it is Nov. 2019, keep Nov. 2019 -> Oct. 2020, and elminate Nov. 2020)
  CurrentMN <- unique(tmpData5$MN)
  MNseq <- c(CurrentMN:12, 1:CurrentMN) # create sequence for months that these calculated means now represent
  MNseq[13] <- NA
  tmpData5$MN <- rep(MNseq, 102)
  tmpData5 <- tmpData5[complete.cases(tmpData5), ]
  
  tmpData5
}

tempurl <- 'https://www.cpc.ncep.noaa.gov/pacdir/NFORdir/HUGEdir2/cpcllftd.dat'
tempData <- getCPCData(tempurl, 'temp')  
summary(tempData)

ppturl <- 'https://www.cpc.ncep.noaa.gov/pacdir/NFORdir/HUGEdir2/cpcllfpd.dat'
pptData <- getCPCData(ppturl, 'ppt')
summary(pptData)


write.csv(tempData, 'CurrentAnomalyTempData.csv', row.names = FALSE)
write.csv(pptData, 'CurrentAnomalyPPTData.csv', row.names = FALSE)
