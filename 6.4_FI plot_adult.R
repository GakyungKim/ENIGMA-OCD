

library(tidyverse)
library(magrittr)

rm(list = ls())


setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/3.Documentations/Tables/4. Feature importance tables/4.1. Feature importnace_DAI_adult/Trimmed_by code")


# Load dataset 
df_dx.adult <- read_csv('Dx_orig features_scaled_trimmed.csv')

# plot
plot_dx.adult <- df_dx.adult %>% 
  mutate(`DTI index` = ifelse(`DTI index` == '.', 'Demographics', `DTI index`), 
         `DTI index` = fct_relevel(`DTI index`, 'Demographics', 'FA', 'MD', 'RD', 'AD')) %>% 
  ggplot(data = .) +
  geom_point(aes(x = Rank, y = Weight, color = `DTI index`)) + 
  ggrepel::geom_text_repel(data = df_dx.adult[1:10, ], aes(x = Rank, y = Weight, label = Feature, vjust = 1), size = 5, direction = 'y', nudge_x = 5, box.padding = 0.1, max.overlaps = Inf) +
  theme_classic() +
  guides(color = guide_legend(ncol = 5)) + 
  theme(legend.position = 'bottom', )

plot_dx.adult 

# plot_dx.adult <- plot_dx.adult + coord_fixed()



#### Adult, Med 01, scaled ####
setwd(path_raw.adult)
df_med01.adult <- read_csv('Med01_orig features_scaled_trimmed.csv')
df_med01.adult %>% 
  mutate(`DTI index` = ifelse(`DTI index` == '.', 'Demographics', `DTI index`), 
         `DTI index` = fct_relevel(`DTI index`, 'Demographics', 'FA', 'MD', 'RD', 'AD')) %>% 
  ggplot(data = .) +
  geom_point(aes(x = Rank, y = Weight, color = `DTI index`)) + 
  ggrepel::geom_text_repel(data = df_med01.adult[1:10, ], aes(x = Rank, y = Weight, label = Feature, vjust = 1), size = 5, direction = 'y', max.overlaps = Inf, nudge_x =20, box.padding = 0.2) +
  theme_classic() +
  guides(color = guide_legend(ncol = 5)) + 
  theme(legend.position = 'bottom', ) -> plot_med01.adult


plot_med01.adult




#### Adult, Med 12, scaled ####
df_med12.adult <- read_csv('Med12_orig features_scaled_trimmed.csv')
df_med12.adult %>% 
  mutate(`DTI index` = ifelse(`DTI index` == '.' | is.na(`DTI index`), 'Demographics', `DTI index`), 
         `DTI index` = fct_relevel(`DTI index`, 'Demographics', 'FA', 'MD', 'RD', 'AD')) %>% 
  ggplot(data = .) +
  geom_point(aes(x = Rank, y = Weight, color = `DTI index`)) + 
  ggrepel::geom_text_repel(data = df_med12.adult[1:10, ], aes(x = Rank, y = Weight, label = Feature, vjust = 1), size = 5, direction = 'y', max.overlaps = Inf,  nudge_x = 30, box.padding = 0.1) +
  theme_classic() +
  guides(color = guide_legend(ncol = 5)) + 
  theme(legend.position = 'bottom', ) -> plot_med12.adult

plot_med12.adult
