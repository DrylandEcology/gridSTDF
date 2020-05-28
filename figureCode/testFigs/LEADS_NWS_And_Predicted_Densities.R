TempAnoms <- fread('ExampleData/TempMonthlyAnomsAll.csv') 
TempAnoms$NWSAnom <- TempAnoms$ForecastedMEAN - TempAnoms$ClimatologicalMEAN

# density
NWSAnom <- data.frame(unique(TempAnoms[,c('LEAD', 'NWSAnom')]))
ggplot(TempAnoms, aes(Anom_F)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(~LEAD, scales = 'free') +
  geom_vline(data = NWSAnom, aes(xintercept = NWSAnom), color = 'red', size = 1.4)+
  theme_bw()

# ---------------------------------------------------------------------
PPTAnoms <- fread('ExampleData/PPTMonthlyAnomsAll.csv') 
PPTAnoms$NWSAnom <- PPTAnoms$ForecastedMEAN/ PPTAnoms$ClimatologicalMEAN
# density
NWSAnom <- data.frame(unique(PPTAnoms[,c('LEAD', 'NWSAnom')]))
ggplot(PPTAnoms, aes(Anom_CF)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(~LEAD, scales = 'free') +
  geom_vline(data = NWSAnom, aes(xintercept = NWSAnom), color = 'red', size = 1.4)+
  theme_bw()
