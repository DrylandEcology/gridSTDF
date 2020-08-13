# Make two output tables of GISSM data

Hist_GISSM <- fread('ExampleData/Hist_GISSM.csv')
Future_GISSM <- fread('ExampleData/Future_GISSM.csv')
Future_GISSM$run_year <- sapply(strsplit(Future_GISSM$run, '_'), '[', 2)
Future_GISSM$run_sim <- sapply(strsplit(Future_GISSM$run, '_'), '[', 1)


Hist_Norm <- Hist_GISSM[ryear %in% 1981:2010]
colMeans(Hist_Norm[,2:3])

Hist_Recent <- Hist_GISSM[ryear %in% 2011:2019]
Hist_Recent

# make into medians of realizations
Future_GISSM_2020 <- Future_GISSM[ryear == 2020]
colMeans(Future_GISSM_2020[,2:3])
Future_GISSM_2020_Means <- Future_GISSM_2020[, .(SeedlingSurvival_1stSeason = median(SeedlingSurvival_1stSeason)),
                                               .(ryear, run_sim)]
summary(Future_GISSM_2020_Means)                                             
                                             

Future_GISSM_2021 <- Future_GISSM[ryear == 2021]
colMeans(Future_GISSM_2021[,2:3])
Future_GISSM_2021_Means <- Future_GISSM_2021[, .(SeedlingSurvival_1stSeason = mean(SeedlingSurvival_1stSeason)),
                                               .(ryear, run_sim)]
summary(Future_GISSM_2021_Means)  

# calculate forecast probability
HistMean <- mean(Hist_Norm$SeedlingSurvival_1stSeason)

AboveBelow <- Future_GISSM_2020_Means$SeedlingSurvival_1stSeason > HistMean
ForecastAbove2020 <- round((sum(AboveBelow == TRUE)/40) * 100, 2)
ForecastBelow2020 <- round((sum(AboveBelow == FALSE)/40) * 100, 2)

AboveBelow <- Future_GISSM_2021_Means$SeedlingSurvival_1stSeason > HistMean
ForecastAbove2021 <- round((sum(AboveBelow == TRUE)/40) * 100, 2)
ForecastBelow2021 <- round((sum(AboveBelow == FALSE)/40) * 100, 2)

Hist_Norm$Type <- 'Historical Average\n(1981 - 2010)'
Future_GISSM_2021_Means$Type <- 'xForecasted\nSurvival (2021)'
Future_GISSM_2020_Means$Type <- 'xForecasted\nSurvival (2020)'

ggplot() + 
  geom_point(data = Hist_Norm, aes(Type, y = mean(SeedlingSurvival_1stSeason)), size = 3, shape = 21, fill = 'gray') + 
  geom_boxplot(data = Future_GISSM_2020_Means, aes(Type, SeedlingSurvival_1stSeason), fill = 'darkcyan') +
  geom_boxplot(data = Future_GISSM_2021_Means, aes(Type, SeedlingSurvival_1stSeason), fill = 'darkcyan') +
  
  theme_classic(base_size = 14) + 
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  labs(x = '', y = 'probability', 
       title = 'Forecasted probability of conditions supporting sagebrush seedling survival\nfor natural regeneration in an intact sagebrush plant community\n(Schlaepfer et al. 2014)') +
  
  scale_x_discrete(labels=c("xForecasted\nSurvival (2021)" = 'Forecasted\nSurvival (2021)', "xForecasted\nSurvival (2020)" = 'Forecasted\nSurvival (2020)')) + 
  scale_y_continuous(limits = c(0,1)) +
  
  # labels -----------------------------------

  geom_label(aes(x = 'xForecasted\nSurvival (2020)', y =.6, 
                 label = paste0(ForecastAbove2020, '% chance that\n probability of survival\nis above historical average')))  +

  geom_label(aes(x = 'xForecasted\nSurvival (2021)', y =.6, 
               label = paste0(ForecastAbove2021, '% chance that\n probability of survival\nis above historical average')))  

ggsave('figureCode/presentationFigs/savedFigs/GISSM_Example_Fig1.png')  

break
# table 1 - 


Future_GISSM_2020_Means$run_sim <- Future_GISSM_2021_Means$run_sim <- NULL

Table1 <- rbind(Hist_Norm, Future_GISSM_2020_Means, Future_GISSM_2021_Means)
Table1 <- Table1[
               ,.(SeedlingSurvival_1stSeason_Mean = round(mean(SeedlingSurvival_1stSeason), 2),
              SeedlingSurvival_1stSeason_sd = round(sd(SeedlingSurvival_1stSeason), 2)),
             .(Type)]
str(Table1)
print(Table1)

# figure 1 ----------

# proportion of values that are greater or smaller than the historical median


