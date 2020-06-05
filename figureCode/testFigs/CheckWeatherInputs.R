library(ggplot2)

histLeads <- monthlyWdata[,.(Tmean = mean(Tmean_C_rollMean), PPT = mean(PPT_cm_rollSum)), .(LEAD)] 
ggplot(monthlyWdata, aes(Tmean_C_rollMean)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(~LEAD, scales = 'free') +
  labs(title = 'Temperature by LEAD') + 
  geom_vline(data = forecast_NWS[1:12,], aes(xintercept = ClimatologicalMEAN_Temp_C), color = 'red')+
  geom_vline(data = histLeads, aes(xintercept = Tmean), color = 'blue')+
  
  theme_bw()

ggplot(monthlyWdata, aes(PPT_cm_rollSum)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(~LEAD, scales = 'free') +
  labs(title = 'Precip by LEAD') + 
  geom_vline(data = forecast_NWS[1:12,], aes(xintercept = ClimatatologicalMEAN_PPT_cm), color = 'red')+
  geom_vline(data = histLeads, aes(xintercept = PPT), color = 'blue')+
  
  theme_bw()

ggplot(meteo_anomalies_leads) +
  geom_point(aes(dPPT_cm, dT_C)) +
  facet_wrap(.~Lead)

ggplot(monthlyWdata) +
  geom_point(aes(PPT_cm_rollSum, Tmean_C_rollMean)) +
  facet_wrap(.~LEAD)

