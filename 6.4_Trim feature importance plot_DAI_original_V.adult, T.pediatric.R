

rm(list = ls())

#### Load codebook #####
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Codebook")
df_cb <- read_csv('Codebook_v.Base.csv') # codebook:  254 x 5
df_cb_ <- read_csv('Codebook_v._.csv') # codebook:  254 x 5


df_cb


#### Validation on adult, test on pediatric #####
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/3.Documentations/Tables")

df_fi <- read_csv('Feature importance_DAI_V.adult.T.pediatric.csv')
# Trim
df_fi %>% 
  select(- `Standard Deviation of Relative Importance` ) %>% 
  filter(`Relative Importance` > 0.003) %>% 
  left_join(df_cb_, by = c("Original Feature" = "Feature_orig")) %>% 
  select(Feature_Hemis, `DTI index`, `Relative Importance`) %>% 
  rename(Feature = Feature_Hemis, 
         Weight = `Relative Importance`) %>% 
  mutate(Rank = row.names(.)) %>% 
  relocate(Rank) -> df_fi_trimmed #  6 x 4

# save output
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/3.Documentations/Tables")
write.csv(df_fi_trimmed, 'Feature importance_DAI_V.adult.T.pediatric_trimmed.csv' , row.names = F)
