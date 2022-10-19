##################################################################
## ENIGMA-OCD pediatric siteROC ##
##################################################################
# pediatric / non_hormo / dx
rm(list = ls())
library(ggplot2)
library(dplyr)
library(magrittr)
library(forcats) #reorder factor levels by sorting along another variable

### create data matrix
Site <- c('Bangalore',	'Barcelona',	'British Columbia',	'Calgary',	'Chiba',	'Oxford',	'Yale',	'Zurich')
meanAUC <- c(0.8222,	0.5344,	0.6667,	0.6417,	1,	0.5167,	0.5969,	0.7851)
lowAUC <- c(0.5891,	0.375,	0.4448,	0.4314,	1,	0.3313,	0.4053,	0.5848)
highAUC <- c(1,	0.6938,	0.8886,	0.8519,	1,	0.7022,	0.7885,	0.9855)

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
siteROC$Site <-  factor(Site, levels = Site)

ggplot(data = siteROC, aes(x = Site, y = meanAUC, color = Site)) + #800 800
  geom_crossbar(aes(x = Site, ymin = lowAUC, ymax = highAUC), width = 0.3) + 
  scale_x_discrete(limits = rev(levels(siteROC$Site))) +
  coord_flip() +
  xlab("Site") + ylab("ROC AUC") + theme_classic()+
  scale_color_manual(values = c('#c4e0a7', '#a0ce72', '#70c930', 
                                '#60ac29', '#40731b', '#217638', '#1c320c', '#476a24' ))+
  guides(color= 'none') +
  theme(axis.title.y = element_blank(), axis.text.y = element_text(size = 11)) 









