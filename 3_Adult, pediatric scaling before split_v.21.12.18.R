library(tidyverse)
library(caret)
##### log #####
# 1. column order
# 2. target variable name: Dx, Med.UnmedOCD
# 3. subjecykey 
# Age also normalized

setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Analysis/1_Data split")
df_T.Dx_adult <- read_csv('T.Dx_Adult_1336_v.cleaned.21.11.22.csv')


setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Analysis/Pediatric/1_Data split")
df_T.Dx_ped <- read_csv('T.Dx_Pediatric_317_v.21.12.18.csv')




#### z scaling - Target Outcome: Dx ####
head(names(df_T.Dx_adult), n=30)
tail(names(df_T.Dx_adult), n = 30)

# head(names(df_T.Dx_Test), n=30)
# tail(names(df_T.Dx_Test), n = 30)

T.Dx_locs_non_num <- c(#which(names(df_T.Dx_adult) == "subjectkey"), 
  which(names(df_T.Dx_adult) == 'Dx'), which(names(df_T.Dx_adult) == 'Site'),  
  which(names(df_T.Dx_adult) == "subjectID"),
  #which(names(df_T.Dx_adult) == 'Age'),
  which(names(df_T.Dx_adult) == 'AgeSQ'),
  which(names(df_T.Dx_adult) == 'Sex'), which(names(df_T.Dx_adult) == 'Med'),
  which(names(df_T.Dx_adult) == 'AO'), which(names(df_T.Dx_adult) == 'Dur'),
  which(names(df_T.Dx_adult) == 'Sev'), which(names(df_T.Dx_adult) == 'Agr_Check'),
  which(names(df_T.Dx_adult) == 'Clean'), which(names(df_T.Dx_adult) == 'Sex_Rel'),
  which(names(df_T.Dx_adult) == 'Hoard'), which(names(df_T.Dx_adult) == 'Ord'),
  which(names(df_T.Dx_adult) == 'Anx'), which(names(df_T.Dx_adult) == 'CurAnx'),
  which(names(df_T.Dx_adult) == 'Dep'), which(names(df_T.Dx_adult) == 'CurDep'))


T.Dx_locs_non_num_ID <- c( which(names(df_T.Dx_adult) == "subjectkey"), 
                           which(names(df_T.Dx_adult) == 'Dx'), which(names(df_T.Dx_adult) == 'Site'),  
                           which(names(df_T.Dx_adult) == "subjectID"),
                           #which(names(df_T.Dx_adult) == 'Age'),
                           which(names(df_T.Dx_adult) == 'AgeSQ'),
                           which(names(df_T.Dx_adult) == 'Sex'), which(names(df_T.Dx_adult) == 'Med'),
                           which(names(df_T.Dx_adult) == 'AO'), which(names(df_T.Dx_adult) == 'Dur'),
                           which(names(df_T.Dx_adult) == 'Sev'), which(names(df_T.Dx_adult) == 'Agr_Check'),
                           which(names(df_T.Dx_adult) == 'Clean'), which(names(df_T.Dx_adult) == 'Sex_Rel'),
                           which(names(df_T.Dx_adult) == 'Hoard'), which(names(df_T.Dx_adult) == 'Ord'),
                           which(names(df_T.Dx_adult) == 'Anx'), which(names(df_T.Dx_adult) == 'CurAnx'),
                           which(names(df_T.Dx_adult) == 'Dep'), which(names(df_T.Dx_adult) == 'CurDep'))


## Train set
df_T.Dx_adult_num  <- df_T.Dx_adult[, -T.Dx_locs_non_num]
df_T.Dx_adult_non_num  <- df_T.Dx_adult[, T.Dx_locs_non_num_ID]

df_T.Dx_adult_num_z_norm <- preProcess(df_T.Dx_adult_num, method = c("center", "scale"))
df_T.Dx_adult_num_z_norm2 = predict(df_T.Dx_adult_num_z_norm, df_T.Dx_adult_num)
df_T.Dx_adult_num_z_norm3 <- merge(df_T.Dx_adult_non_num, df_T.Dx_adult_num_z_norm2, by = 'subjectkey') %>% as_tibble()

# change variable order
df_T.Dx_adult_num_z_norm3 <-df_T.Dx_adult_num_z_norm3 %>%  relocate(c(subjectkey, Dx, Site, subjectID, Age))


#### z scaling - pediatric - Target Outcome: Dx ####
head(names(df_T.Dx_ped), n=30)
tail(names(df_T.Dx_ped), n = 30)


T.Dx_locs_non_num <- c(#which(names(df_T.Dx_ped) == "subjectkey"), 
  which(names(df_T.Dx_ped) == 'Dx'), which(names(df_T.Dx_ped) == 'Site'),  
  which(names(df_T.Dx_ped) == "subjectID"),
  #which(names(df_T.Dx_ped) == 'Age'),
  which(names(df_T.Dx_ped) == 'AgeSQ'),
  which(names(df_T.Dx_ped) == 'Sex'), which(names(df_T.Dx_ped) == 'Med'),
  which(names(df_T.Dx_ped) == 'AO'), which(names(df_T.Dx_ped) == 'Dur'),
  which(names(df_T.Dx_ped) == 'Sev'), which(names(df_T.Dx_ped) == 'Agr_Check'),
  which(names(df_T.Dx_ped) == 'Clean'), which(names(df_T.Dx_ped) == 'Sex_Rel'),
  which(names(df_T.Dx_ped) == 'Hoard'), which(names(df_T.Dx_ped) == 'Ord'),
  which(names(df_T.Dx_ped) == 'Anx'), which(names(df_T.Dx_ped) == 'CurAnx'),
  which(names(df_T.Dx_ped) == 'Dep'), which(names(df_T.Dx_ped) == 'CurDep'))


T.Dx_locs_non_num_ID <- c( which(names(df_T.Dx_ped) == "subjectkey"), 
                           which(names(df_T.Dx_ped) == 'Dx'), which(names(df_T.Dx_ped) == 'Site'),  
                           which(names(df_T.Dx_ped) == "subjectID"),
                           #which(names(df_T.Dx_ped) == 'Age'),
                           which(names(df_T.Dx_ped) == 'AgeSQ'),
                           which(names(df_T.Dx_ped) == 'Sex'), which(names(df_T.Dx_ped) == 'Med'),
                           which(names(df_T.Dx_ped) == 'AO'), which(names(df_T.Dx_ped) == 'Dur'),
                           which(names(df_T.Dx_ped) == 'Sev'), which(names(df_T.Dx_ped) == 'Agr_Check'),
                           which(names(df_T.Dx_ped) == 'Clean'), which(names(df_T.Dx_ped) == 'Sex_Rel'),
                           which(names(df_T.Dx_ped) == 'Hoard'), which(names(df_T.Dx_ped) == 'Ord'),
                           which(names(df_T.Dx_ped) == 'Anx'), which(names(df_T.Dx_ped) == 'CurAnx'),
                           which(names(df_T.Dx_ped) == 'Dep'), which(names(df_T.Dx_ped) == 'CurDep'))


## Train set
df_T.Dx_ped_num  <- df_T.Dx_ped[, -T.Dx_locs_non_num]
df_T.Dx_ped_non_num  <- df_T.Dx_ped[, T.Dx_locs_non_num_ID]

df_T.Dx_ped_num_z_norm <- preProcess(df_T.Dx_ped_num, method = c("center", "scale"))
df_T.Dx_ped_num_z_norm2 = predict(df_T.Dx_ped_num_z_norm, df_T.Dx_ped_num)
df_T.Dx_ped_num_z_norm3 <- merge(df_T.Dx_ped_non_num, df_T.Dx_ped_num_z_norm2, by = 'subjectkey') %>% as_tibble()

# change variable order
df_T.Dx_ped_num_z_norm3 <-df_T.Dx_ped_num_z_norm3 %>%  relocate(c(subjectkey, Dx, Site, subjectID, Age))




df_T.Dx_adult_num_z_norm3 %>% xtabs(~ Dx, data = ., addNA = T) %>% addmargins() # 1,336 x 272
df_T.Dx_ped_num_z_norm3  %>% xtabs(~ Dx, data = ., addNA = T) %>% addmargins()# 317 x 272


setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Analysis/2_z-scaled")
write.csv(df_T.Dx_adult_num_z_norm3, row.names = F, file = 'T.Dx_Adult_1336_v.z.norm.21.12.18.csv')

setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Analysis/Pediatric/2_z-scaled")
write.csv(df_T.Dx_ped_num_z_norm3, row.names = F, file = 'T.Dx_Pediatric_317_v.z.norm.211218.csv' )

