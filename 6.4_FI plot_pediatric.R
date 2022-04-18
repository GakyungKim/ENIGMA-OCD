

library(tidyverse)
library(magrittr)

rm(list = ls())

# path_raw.pedi <- "C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/3.Documentations/Tables/4.2. Feature importance_DAI_pediatric/Trimmed_by_code"
# setwd(path_raw.pedi)
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/3.Documentations/Tables/4. Feature importance tables/4.2. Feature importance_DAI_pediatric/Trimmed_by_code")

#### Load dataset ####
df_dx.pedi  <- read_csv('Pedi_Dx_orig features_scaled_trimmed.csv')
df_med12.pedi <- read_csv('Pedi_Med12_orig features_scaled_trimmed.csv')
df_med01.pedi <- read_csv('Pedi_Med01_orig features_scaled_trimmed.csv')

#### Dx, pediatric, scaled #####
df_dx.pedi %>% 
  mutate(`DTI index` = ifelse(`DTI index` == '.', 'Demographics', `DTI index`), 
         `DTI index` = fct_relevel(`DTI index`, 'Demographics', 'FA', 'MD', 'RD', 'AD')) %>% 
  ggplot(data = .) +
  geom_point(aes(x = Rank, y = Weight, color = `DTI index`)) + 
  ggrepel::geom_text_repel(data = df_dx.pedi[1:10, ], aes(x = Rank, y = Weight, label = Feature, vjust = 1), size = 5, direction = 'y', box.padding =0.5, max.overlaps = Inf, nudge_x = 20) +
  theme_classic() +
  guides(color = guide_legend(ncol = 5)) + 
  theme(legend.position = 'bottom', ) -> plot_dx.pedi

plot_dx.pedi
  



#### Med 01, pediatric, scaled ####

df_med01.pedi %>% 
  mutate(`DTI index` = ifelse(`DTI index` == '.'| is.na(`DTI index`), 'Demographics', `DTI index`), 
         `DTI index` = fct_relevel(`DTI index`, 'FA', 'MD', 'RD', 'AD')) %>% 
  ggplot(data = .) +
  geom_point(aes(x = Rank, y = Weight, color = `DTI index`)) + 
  ggrepel::geom_text_repel(data = df_med01.pedi[1:10, ], aes(x = Rank, y = Weight, label = Feature, vjust = 1), size = 5, direction = 'y', box.padding = 0.1, point.padding = 1, max.overlaps = Inf, nudge_x = 20) +
  theme_classic() +
  guides(color = guide_legend(ncol = 5)) + 
  theme(legend.position = 'bottom', ) -> plot_med01.pedi


plot_med01.pedi


#### Med 12, pediatric, scaled #####
df_med12.pedi %>% 
  mutate(`DTI index` = ifelse(`DTI index` == '.', 'Demographics', `DTI index`), 
         `DTI index` = fct_relevel(`DTI index`, 'Demographics', 'FA', 'MD', 'RD', 'AD')) %>% 
  ggplot(data = .) +
  geom_point(aes(x = Rank, y = Weight, color = `DTI index`)) + 
  ggrepel::geom_text_repel(data = df_med12.pedi[1:10, ], aes(x = Rank, y = Weight, label = Feature, vjust = 1), size = 5, direction = 'y', box.padding = 0.1, max.overlaps = Inf, nudge_x = 20) +
  theme_classic() +
  guides(color = guide_legend(ncol = 5)) + 
  theme(legend.position = 'bottom', ) -> plot_med12.pedi

plot_med12.pedi