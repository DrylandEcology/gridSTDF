# This script contains figures pertaining to whether the NWS anomalies are being successfully integrated 
# and reflected into the SW runs

library(ggplot2)
library(plyr)

# Plot 1 - What are the anomolies for each month and run?
head(AnomSave)
str(AnomSave)
AnomSave2 <- melt(AnomSave, id.vars = c('Month', 'runx'))

AnomSave2$Month <- factor(AnomSave2$Month, levels = c(6:12, 1:4))

ggplot(AnomSave2) + 
  geom_boxplot(aes(Month, value,group = Month)) + 
                 facet_wrap(variable ~ ., scales = 'free', nrow = 3) +
  theme_bw() 
  
ggplot(AnomSave2) + 
  geom_line(aes(Month, value, color = as.factor(runx))) + 
  geom_point(aes(Month, value, color = as.factor(runx))) + 
  
  facet_wrap(variable ~ ., scales = 'free', nrow = 3) +
  theme_bw() + theme(legend.position = 'none')
