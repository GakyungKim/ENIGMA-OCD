##################################################################
## ENIGMA-OCD adult siteROC ##
##################################################################
# adult / non_hormo / dx
rm(list = ls())
library(ggplot2)
library(dplyr)
library(magrittr)
library(forcats) #reorder factor levels by sorting along another variable

### create data matrix
Site <- c('Amsterdam', 'Bangalore', 'Capetown','Kyoto','Milan',
          'Mountsinal','Munich','Rome','Saopaulo','Seoul','Shanghai')
meanAUC <- c(0.6833, 0.6362, 0.516, 0.7415, 0.6066, 
             0.7911,0.6642, 0.5645, 0.7537, 0.6635, 0.7813)
lowAUC <- c(0.5406, 0.5639, 0.3521, 0.6143, 0.4924,
            0.6123, 0.5585, 0.4724, 0.6086, 0.5732, 0.6873)
highAUC <- c(0.826, 0.7086, 0.6799, 0.8687, 0.7208, 
             0.97, 0.7699, 0.6566, 0.8988, 0.7539, 0.8753)

siteROC <- cbind(Site, meanAUC, lowAUC, highAUC)
siteROC <- as.data.frame(siteROC)
# siteROC$site <- as.factor(siteROC$site)
# siteROC$site <- sort((unique(siteROC$site)))
siteROC$meanAUC <- as.numeric(siteROC$meanAUC)
siteROC$lowAUC <- as.numeric(siteROC$lowAUC)
siteROC$highAUC <- as.numeric(siteROC$highAUC)
#### check
siteROC %>% as_tibble()
siteROC %>% str()



#############################################################################
##### siteROC visualization#####
#############################################################################
siteROC$Site <- factor(Site, levels = Site)

ggplot(data = siteROC, aes(x = Site, y = meanAUC, color = Site)) + 
  geom_crossbar(aes(x = Site, ymin = lowAUC, ymax = highAUC), width = 0.3) + 
  scale_x_discrete(limits = rev(levels(siteROC$Site))) +
  coord_flip() +
  xlab("Site") + ylab("ROC AUC") + theme_classic() +
  scale_color_manual(values = c('#dbb7f3', '#cc9aee', '#ae5de4', '#9e3ede', '#8e23d6', 
                                '#7a1eb7', '#661999', '#51147a', '#3d0f5b', '#66105E',  '#280a3d')) +
  guides(color= 'none') +
  theme(axis.title.y = element_blank(), axis.text.y = element_text(size = 11)) 

