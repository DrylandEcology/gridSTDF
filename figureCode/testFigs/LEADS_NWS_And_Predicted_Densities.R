print('Density plots of predicted LEAD Anomalies')
# the anomaly from NWS is the thick red line. 
# the mean of the realization is the dashed blue line
# 30 Future realizations as denisty/histograms

TempAnoms$NWSAnom <- TempAnoms$ForecastedMEAN - TempAnoms$ClimatologicalMEAN
OurAnoms <- setDT(TempAnoms)[,.(Anom_F = mean(Anom_F)), .(LEAD)]

ggplot(TempAnoms, aes(Anom_F)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(~LEAD, scales = 'free') +
  labs(title = 'Temperature anomalies by LEAD') + 
  geom_vline(aes(xintercept = NWSAnom), color = 'red')+
  geom_vline(data = OurAnoms, aes(xintercept = Anom_F), color = 'blue', linetype = 'dashed')+
  
  theme_bw()

# ---------------------------------------------------------------------
PPTAnoms$NWSAnom <- PPTAnoms$ForecastedMEAN/ PPTAnoms$ClimatologicalMEAN
OurAnoms <- setDT(PPTAnoms)[,.(Anom_CF = mean(Anom_CF)), .(LEAD)]

ggplot(PPTAnoms, aes(Anom_CF)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(~LEAD, scales = 'free') +
  geom_vline(aes(xintercept = NWSAnom), color = 'red') +
  geom_vline(data = OurAnoms, aes(xintercept = Anom_CF), color = 'blue', linetype = 'dashed')+
  labs(title = 'Precipitation correction factor by LEAD') +
  theme_bw()
