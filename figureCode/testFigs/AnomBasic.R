# This script contains figures pertaining to whether the NWS anomalies are being successfully integrated 
# and reflected into the SW runs

library(ggplot2)

# Plot 1 - What are the final anomolies for each month (12) and run (30?
names(AnomSave)[4] <- 'Month'
AnomSave$runx <- rep(1:30, each = 12)
AnomSave2 <- melt(AnomSave, id.vars = c('Month', 'runx'))

  
ggplot(AnomSave2) + 
  geom_line(aes(Month, value, color = as.factor(runx))) + 
  facet_wrap(variable ~ ., scales = 'free', nrow = 3) +
  theme_bw() + theme(legend.position = 'none')

AnomSave2$Month <- factor(AnomSave2$Month, levels = c(6:12, 1:5))

ggplot(AnomSave2) + 
  geom_boxplot(aes(Month, value,group = Month)) + 
  facet_wrap(variable ~ ., scales = 'free', nrow = 3) +
  theme_bw() 
