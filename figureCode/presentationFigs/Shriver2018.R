library(data.table)
library(ggplot2)
library(ggrepel)

# Read in data
Future_Shriver2018 <- fread('ExampleData/Future_Shriver2018.csv')
Hist_Shriver2018 <- fread('ExampleData/Hist_Shriver2018.csv')

# prep
Hist_Shriver2018$TP <- '1 Historical'
Future_Shriver2018$run_year <- sapply(strsplit(Future_Shriver2018$run, '_'), '[', 2)
Future_Shriver2018$run_sim <- sapply(strsplit(Future_Shriver2018$run, '_'), '[', 1)

# subset for year  -----------------------------------------------------------------------------
# this probability is for the year AFTER seeding
# YEAR - is the year the values are predicted for
### if it is before DOY 250 than can only predict for seeding in the year before because
### we don't have enough future data yet to say how the currYear will turn out.
### < 250, predict y - 1, i.e. July 22, 2020 - predicting if Fall 2019 will be successful
### > 250, predict y, i.e. Sept 1, 2020 - predicting if Fall 2020 will be successful

currDOY <- yday(Sys.Date())
if(currDOY < 250) {
  Future_Shriver2018 <- Future_Shriver2018[Year == 2019]
} else{
  Future_Shriver2018 <- Future_Shriver2018[Year == 2020]
  
}

Future_Shriver2018$TP <- '2 AllSims'
Future_Shriver20182 <- Future_Shriver2018
Future_Shriver20182$TP <-  as.character(paste0('3 ',unique(Future_Shriver20182$Year)))

# Make two figures ------------------------------------------------------------
Hist_Shriver20182 <- Hist_Shriver2018[Year %in% 1981:2010,]

Future_ShriverMedians <- Future_Shriver2018[, .(Prob = median(Prob)), .(run_sim)]
Future_ShriverMedians$TP <- '4 Medians'

# Figure 1 - Train of thought
#  4 bps
## hist, all 900 futures, boxplots of means,30 bps of sims (Var across years), 

Estab2020Plot <- ggplot() +
  geom_boxplot(data = Hist_Shriver20182, aes(TP, Prob),  alpha = 0.8, fill = 'gray') +
  geom_boxplot(data = Future_Shriver2018, aes(TP, Prob),  alpha = 0.8, fill = 'darkcyan') +
  geom_boxplot(data = Future_Shriver20182, aes(TP , Prob, group = run_sim),  alpha = 0.8, fill = 'darkcyan') +
  geom_boxplot(data = Future_ShriverMedians, aes(TP , Prob),  alpha = 0.8, fill = 'darkcyan') +
  
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = '', y = 'probability of establishment', title = 'Forecast for Sagebrush Establishment in 2020\n(Seeded Fall 2019)') +
  scale_x_discrete(labels=c("1 Historical" = "Historical", "2 AllSims" = "All Simulations",
                            "3 2019" = "Ind. Realizations", "4 Medians" = 'Medians')) +
  scale_y_continuous(limits = c(0,1)) 
  

dat <- ggplot_build(Estab2020Plot)$data[[3]]
Estab2020Plot <- Estab2020Plot + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                       y=middle, yend=middle), colour="red")
dat <- ggplot_build(Estab2020Plot)$data[[4]]

Estab2020Plot <- Estab2020Plot + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                                            y=middle, yend=middle), colour="red",)
Estab2020Plot

ggsave('figureCode/presentationFigs/savedFigs/Shriver_Walkthrough_Fig1_2020.png', width = 8)  

# FOR Estab. 2021 ------------------------------------------------------------------------------------
Future_Shriver2018 <- fread('ExampleData/Future_Shriver2018.csv')
Future_Shriver2018$run_year <- sapply(strsplit(Future_Shriver2018$run, '_'), '[', 2)
Future_Shriver2018$run_sim <- sapply(strsplit(Future_Shriver2018$run, '_'), '[', 1)
Future_Shriver2018 <- Future_Shriver2018[Year == 2020]
Future_Shriver2018$TP <- '2 AllSims'
Future_Shriver20182 <- Future_Shriver2018
Future_Shriver20182$TP <-  as.character(paste0('3 ',unique(Future_Shriver20182$Year)))
Future_ShriverMeds <- Future_Shriver2018[, .(Prob = median(Prob)), .(run_sim)]
Future_ShriverMeds$TP <- '4 Medians'

# Realization - vocab for ur predicited NWS anomaly
# 3th - estimates of different realizations
# The 4th boxplot - collapse year to  year variability,  show variation in realizationas

# add red liens to third boxplot
Estab2021Plot <- ggplot() +
  geom_boxplot(data = Hist_Shriver20182, aes(TP, Prob),  alpha = 0.8, fill = 'gray') +
  geom_boxplot(data = Future_Shriver2018, aes(TP, Prob),  alpha = 0.8, fill = 'darkcyan') +
  geom_boxplot(data = Future_Shriver20182, aes(TP , Prob, group = run_sim),  alpha = 0.8, fill = 'darkcyan') +
  geom_boxplot(data = Future_ShriverMeds, aes(TP , Prob),  alpha = 0.8, fill = 'darkcyan') +
  
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5)) +
  
  labs(x = '', y = 'probability of establishment', title = 'Forecast for Sagebrush Establishment in 2021\n(Seeded Fall 2020)') +

scale_x_discrete(labels=c("1 Historical" = "Historical", "2 AllSims" = "All Simulations",
                          "3 2020" = "Ind. Realizations", "4 Medians" = 'Medians')) +
  scale_y_continuous(limits = c(0,1)) 
  

dat <- ggplot_build(Estab2021Plot)$data[[3]]
Estab2021Plot <- Estab2021Plot + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                                            y=middle, yend=middle), colour="red")
dat <- ggplot_build(Estab2021Plot)$data[[4]]
Estab2021Plot <- Estab2021Plot + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                                                            y=middle, yend=middle), colour="red",)
Estab2021Plot
ggsave('figureCode/presentationFigs/savedFigs/Shriver_Walkthrough_Fig1_2021.png', width =8 )  


# Figure 2 -------------------------------------------------------------------------
# hist, points of last 10 years, future, with numerical values
## write in proportion above or below historical median - "forecast probability"
# show recent years as points --------------------------------------------

Future_Shriver2018 <- fread('ExampleData/Future_Shriver2018.csv')
Future_Shriver2018$run_year <- sapply(strsplit(Future_Shriver2018$run, '_'), '[', 2)
Future_Shriver2018$run_sim <- sapply(strsplit(Future_Shriver2018$run, '_'), '[', 1)
Future_Shriver2018 <- Future_Shriver2018[Year %in% 2019:2020]
Future_ShriverMeds <- Future_Shriver2018[, .(Prob = median(Prob)), .(run_sim, Year)]
Future_ShriverMeds$TP <- as.character(Future_ShriverMeds$Year)

# calculate forecast probability
HistMed <- median(Hist_Shriver20182$Prob)

# 2020
AboveBelow <- Future_ShriverMeds[Year ==2019, 'Prob'] > HistMed
ForecastAbove2020 <- round((sum(AboveBelow == TRUE)/40) * 100, 2)
ForecastBelow2020 <- round((sum(AboveBelow == FALSE)/40) * 100, 2)

# 2021
AboveBelow <- Future_ShriverMeds[Year == 2020, 'Prob'] > HistMed
ForecastAbove2021 <- round((sum(AboveBelow == TRUE)/40) * 100, 2)
ForecastBelow2021 <- round((sum(AboveBelow == FALSE)/40) * 100, 2)

ggplot() +
  geom_boxplot(data = Hist_Shriver20182, aes(TP, Prob),  alpha = 0.8, fill = 'gray') +
  
  geom_boxplot(data = Future_ShriverMeds, aes(TP , Prob),  alpha = 0.8, fill = 'darkcyan') +

  geom_point(data = Hist_Shriver2018[Hist_Shriver2018$Year > 2010], aes(TP, Prob, color = as.factor(Year)), shape = 8) +
  geom_text_repel(data = Hist_Shriver2018[Hist_Shriver2018$Year > 2010], 
                  aes(TP, Prob, label = Year, color = as.factor(Year)), direction = 'y', hjust = 4, size = 3.5) +
  theme_classic(base_size = 14) + 
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  labs(x = '', y = 'probability of establishment',
       title = ' Forecasted probability of conditions supporting\nestablishment for post-fire sagebrush seeding\n  (Shriver et al 2018)') +
  scale_x_discrete(labels=c("1 Historical" = "Historical\n(1981 - 2010)", "2019" = 'Forecasted\nEstablishment (2020)', "2020" = 'Forecasted\nEstablishment (2021)')) + 
  scale_y_continuous(limits = c(0,1)) +
  
  # add forecast probability estab 2020 
  geom_label(aes(x = '2019', y = HistMed + .2, 
                 label = paste0(ForecastAbove2020, '% chance that\n probability of establishment\nis above historical average'))) +
  geom_label(aes(x = '2019', y = HistMed - .3, 
                 label = paste0(ForecastBelow2020, '% chance that\n probability of establishment\nis below historical average'))) +

  # add forecast probability estab 2021
  geom_label(aes(x = '2020', y = HistMed + .2, 
                label = paste0(ForecastAbove2021, '% chance that\n probability of establishment\nis above historical average'))) +
  geom_label(aes(x = '2020', y = HistMed - .3, 
                label = paste0(ForecastBelow2021, '% chance that\n probability of establishment\nis below historical average'))) 

ggsave('figureCode/presentationFigs/savedFigs/Shriver_Example_Fig2.png', width = 8)  
              
  
