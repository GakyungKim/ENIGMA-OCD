library(tidyverse)
library(magrittr)

rm(list = ls())

#### Load codebook #####
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Codebook")
df_cb <- read_csv('Codebook_v.Base.csv') # codebook:  254 x 5
df_cb_ <- read_csv('Codebook_v._.csv') # codebook:  254 x 5


df_cb


##### set working directory to directory of adult DAI results ####
path_raw <- "C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/3.Documentations/Tables/4.2. Feature importance_DAI_pediatric"
path_out <- "C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/3.Documentations/Tables/4.2. Feature importance_DAI_pediatric/Trimmed_by_code"



#### Pedi, Dx, scaled #####
# load raw
setwd(path_raw)
df_Pedi.dx.scaled <- read_csv('Pedi_Dx_orig features_scaled.csv') # 254 x 3

# Trim
df_Pedi.dx.scaled %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_Pedi.dx.scaled_trimmed # 109 x 4
# save output
setwd(path_out)
write.csv(df_Pedi.dx.scaled_trimmed, 'Pedi_Dx_orig features_scaled_trimmed.csv' , row.names = F)


#### Pedi, Med12, scaled #####
# load raw
setwd(path_raw)
df_Pedi.med12.scaled <- read_csv('Pedi_Med12_orig features_scaled.csv') #271 x 3

# Trim
df_Pedi.med12.scaled %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_Pedi.med12.scaled_trimmed #   50 x 4
# save output
setwd(path_out)
write.csv(df_Pedi.med12.scaled_trimmed, 'Pedi_Med12_orig features_scaled_trimmed.csv', row.names = F)

#### Pedi, Med01, scaled #####
# load raw
setwd(path_raw)
df_Pedi.med01.scaled <- read_csv('Pedi_Med01_orig features_scaled.csv') # 254 x 3

# Trim
df_Pedi.med01.scaled %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_Pedi.med01.scaled_trimmed # 53 x 4
# save output
setwd(path_out)
write.csv(df_Pedi.med01.scaled_trimmed, 'Pedi_Med01_orig features_scaled_trimmed.csv', row.names = F)





######## Harmonization dataset #########

#### Pedi, Dx, harmo #####
# load raw
setwd(path_raw)
df_Pedi.dx.harmo <- read_csv('Pedi_Dx_orig features_harmo.csv') # 254 x 3

# Trim
df_Pedi.dx.harmo %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_Pedi.dx.harmo_trimmed # 117 x 4
# save output
setwd(path_out)
write.csv(df_Pedi.dx.harmo_trimmed, 'Pedi_Dx_orig features_harmo_trimmed.csv' , row.names = F)


#### Pedi, Med12, harmo #####
# load raw
setwd(path_raw)
df_Pedi.med12.harmo <- read_csv('Pedi_Med12_orig features_harmo.csv') #272 x 3

# Trim
df_Pedi.med12.harmo %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_Pedi.med12.harmo_trimmed #  36 x 4
# save output
setwd(path_out)
write.csv(df_Pedi.med12.harmo_trimmed, 'Pedi_Med12_orig features_harmo_trimmed.csv', row.names = F)

#### Pedi, Med01, harmo #####
# load raw
setwd(path_raw)
df_Pedi.med01.harmo <- read_csv('Pedi_Med01_orig features_harmo.csv') # 254 x 3

# Trim
df_Pedi.med01.harmo %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_Pedi.med01.harmo_trimmed # 44 x 4
# save output
setwd(path_out)
write.csv(df_Pedi.med01.harmo_trimmed, 'Pedi_Med01_orig features_harmo_trimmed.csv', row.names = F)





##### Harmonization with covariates #######

#### Pedi, Dx, harmo.cov #####
# load raw
setwd(path_raw)
df_Pedi.dx.harmo.cov <- read_csv('Pedi_Dx_orig features_harmo.cov.csv') # 254 x 3

# Trim
df_Pedi.dx.harmo.cov %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_Pedi.dx.harmo.cov_trimmed # 116 x 4
# save output
setwd(path_out)
write.csv(df_Pedi.dx.harmo.cov_trimmed, 'Pedi_Dx_orig features_harmo.cov_trimmed.csv' , row.names = F)


#### Pedi, Med12, harmo.cov #####
# load raw
setwd(path_raw)
df_Pedi.med12.harmo.cov <- read_csv('Pedi_Med12_orig features_harmo.cov.csv') #271 x 3

# Trim
df_Pedi.med12.harmo.cov %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_Pedi.med12.harmo.cov_trimmed # 43 x 4
# save output
setwd(path_out)
write.csv(df_Pedi.med12.harmo.cov_trimmed, 'Pedi_Med12_orig features_harmo.cov_trimmed.csv', row.names = F)

#### Pedi, Med01, harmo.cov #####
# load raw
setwd(path_raw)
df_Pedi.med01.harmo.cov <- read_csv('Pedi_Med01_orig features_harmo.cov.csv') # 254 x 3

# Trim
df_Pedi.med01.harmo.cov %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_Pedi.med01.harmo.cov_trimmed #  52 x 4
# save output
setwd(path_out)
write.csv(df_Pedi.med01.harmo.cov_trimmed, 'Pedi_Med01_orig features_harmo.cov_trimmed.csv', row.names = F)
