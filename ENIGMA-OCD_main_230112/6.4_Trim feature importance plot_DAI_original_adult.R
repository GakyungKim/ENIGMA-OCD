library(tidyverse)
library(magrittr)


#### Load codebook #####
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Codebook")
df_cb <- read_csv('Codebook_v.Base.csv') # codebook:  254 x 5
df_cb_ <- read_csv('Codebook_v._.csv') # codebook:  254 x 5


df_cb


##### set working directory to directory of adult DAI results ####
path_raw <- "C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/3.Documentations/Tables/4.1. Feature importnace_DAI_adult"
path_out <- "C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/3.Documentations/Tables/4.1. Feature importnace_DAI_adult/Trimmed_by code"

#### Adult, Dx, scaled #####
# load raw
setwd(path_raw)
df_adult.dx.scaled <- read_csv('Dx_orig features_scaled.csv') # 254 x 3

# Trim
df_adult.dx.scaled %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_adult.dx.scaled_trimmed # 68 x 3
# save output
setwd(path_out)
write.csv(df_adult.dx.scaled_trimmed, 'Dx_orig features_scaled_trimmed.csv' , row.names = F)


#### Adult, Med12, scaled #####
# load raw
setwd(path_raw)
df_adult.med12.scaled <- read_csv('Med12_orig features_scaled.csv') # 254 x 3

# Trim
df_adult.med12.scaled %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_adult.med12.scaled_trimmed #  98 x 4
# save output
setwd(path_out)
write.csv(df_adult.med12.scaled_trimmed, 'Med12_orig features_scaled_trimmed.csv', row.names = F)

#### Adult, Med01, scaled #####
# load raw
setwd(path_raw)
df_adult.med01.scaled <- read_csv('Med01_orig features_scaled.csv') # 254 x 3

# Trim
df_adult.med01.scaled %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_adult.med01.scaled_trimmed # 125 x 4
# save output
setwd(path_out)
write.csv(df_adult.med01.scaled_trimmed, 'Med01_orig features_scaled_trimmed.csv', row.names = F)




######## Harmonization dataset ##########

##### Adult, Dx, harmo #####
# load raw
setwd(path_raw)
df_adult.dx.harmo <- read_csv('Dx_orig features_harmo.csv') # 251 x 3

# Trim
df_adult.dx.harmo %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_adult.dx.harmo_trimmed #  85 x 4
# save output
setwd(path_out)
write.csv(df_adult.dx.harmo_trimmed, 'Dx_orig features_harmo_trimmed.csv', row.names = F)



#### Adult, Med12, harmo ####
# load raw
setwd(path_raw)
df_adult.med12.harmo <- read_csv('Med12_orig features_harmo.csv') # 251 x 3

# Trim
df_adult.med12.harmo %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_adult.med12.harmo_trimmed #  105 x 4
# save output
setwd(path_out)
write.csv(df_adult.med12.harmo_trimmed, 'Med12_orig features_harmo_trimmed.csv', row.names = F)



##### Adult, Med01, harmo #####
# load raw
setwd(path_raw)
df_adult.med01.harmo <- read_csv('Med01_orig features_harmo.csv') # 251 x 3

# Trim
df_adult.med01.harmo %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_adult.med01.harmo_trimmed #  148 x 4
# save output
setwd(path_out)
write.csv(df_adult.med01.harmo_trimmed, 'Med01_orig features_harmo_trimmed.csv', row.names = F)



######### Harmonizatino with covariates ##########

##### Adult, Dx, harmo with cov #####
# load raw
setwd(path_raw)
df_adult.dx.harmo.cov <- read_csv('Dx_orig features_harmo with cov.csv') # 251 x 3

# Trim
df_adult.dx.harmo.cov %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_adult.dx.harmo.cov_trimmed #  134 x 4
# save output
setwd(path_out)
write.csv(df_adult.dx.harmo.cov_trimmed, 'Dx_orig features_harmo with cov_trimmed.csv', row.names = F)



#### Adult, Med12, harmo with cov ####
# load raw
setwd(path_raw)
df_adult.med12.harmo.cov <- read_csv('Med12_orig features_harmo with cov.csv') # 251 x 3

# Trim
df_adult.med12.harmo.cov %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_adult.med12.harmo.cov_trimmed #  150 x 4
# save output
setwd(path_out)
write.csv(df_adult.med12.harmo.cov_trimmed, 'Med12_orig features_harmo with cov_trimmed.csv', row.names = F)



##### Adult, Med01, harmo with cov #####
# load raw
setwd(path_raw)
df_adult.med01.harmo.cov <- read_csv('Med01_orig features_harmo with cov.csv') # 251 x 3

# Trim
df_adult.med01.harmo.cov %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_adult.med01.harmo.cov_trimmed #  135 x 4
# save output
setwd(path_out)
write.csv(df_adult.med01.harmo.cov_trimmed, 'Med01_orig features_harmo with cov_trimmed.csv', row.names = F)


