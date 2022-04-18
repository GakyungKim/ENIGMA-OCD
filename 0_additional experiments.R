library(tidyverse)
library(magrittr)
rm(list = ls())
# path
path_source <- "C:/Users/±Ë∫∏∞‚/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Analysis/2_z-scaled"
#####################################
##### adult - med01 ºˆ¡§ #####
#####################################
setwd(paste0(path_source, '/1.adult'))

df_adult_med12_train <- read_csv('T.MedUnmedOCD_Adult_S.Train_547_v.z.norm.211123.csv') # 547 x 273
df_adult_med12_test <- read_csv('T.MedUnmedOCD_Adult_S.Test_137_v.z.norm.211123.csv') #  137 x 273

##### check 
table(df_adult_med12_train$Site, df_adult_med12_train$Med.UnmedOCD)
# Amsterdam, Kyoto, Shanghai

##### new test, train set
# test set
df_adult_med12_test2 <- df_adult_med12_train %>% 
  filter(Site == 'Amsterdam' |Site == 'Kyoto' |Site == 'Shanghai') %>%  # 119
  bind_rows(df_adult_med12_test) # 256 x 273
# train set
df_adult_med12_train2 <- df_adult_med12_train %>% 
  filter(Site != 'Amsterdam' & Site != 'Kyoto' & Site != 'Shanghai') #  428 x 273

#### save new adult med12 µ•¿Ã≈Õ
setwd(paste0(path_source, '/1.adult'))
write.csv(row.names = F, df_adult_med12_train2, 'T.MedUnmedOCD_Adult_S.Train2_428_v.z.norm.220302.csv')
write.csv(row.names = F, df_adult_med12_test2, 'T.MedUnmedOCD_Adult_S.Test2_256_v.z.norm.220302.csv')


#####################################
#### pediatric - med12 Ω««Ë ºˆ¡§ ####
#####################################
setwd(paste0(path_source, '/2.pediatric'))
getwd()

df_pedi_med12_train <- read_csv('T.MedUnmedOCD_Pediatric_S.Train_140_v.z.norm.211218.csv') # 547 x 273
df_pedi_med12_test <- read_csv('T.MedUnmedOCD_Pediatric_S.Test_35_v.z.norm.211218.csv') #  137 x 273

##### check 
table(df_pedi_med12_train$Site, df_pedi_med12_train$Med.UnmedOCD)
# Calgary         

#### new dataset
df_pedi_med12_train2<- df_pedi_med12_train %>% 
  filter(Site != 'Calgary') #  126 x 273

df_pedi_med12_test2<- df_pedi_med12_train %>% 
  filter(Site == 'Calgary') %>% bind_rows(df_pedi_med12_test) # 49 x 273
#### save dataset
setwd(paste0(path_source, '/2.pediatric'))
write.csv(row.names = F, df_pedi_med12_train2, 'T.MedUnmedOCD_Pediatric_S.Train2_126_v.z.norm.220302.csv')
write.csv(row.names = F, df_pedi_med12_test2, 'T.MedUnmedOCD_Pediatric_S.Test2_49_v.z.norm.220302.csv')




#####################################
##### adult - bangalore ¡¶ø‹ #####
#####################################
rm(list = ls())
path_source <- "C:/Users/±Ë∫∏∞‚/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Analysis/2_z-scaled"

setwd(paste0(path_source, '/1.adult'))
df_adult_dx_train <- read_csv('T.Dx_Adult_S.Train_1069_v.z.norm.211123.csv')
df_adult_dx_test <- read_csv('T.Dx_Adult_S.Test_267_v.z.norm.211123.csv')

# remove banaglore
df_adult_dx_train %<>% 
  filter(Site != 'Bangalore') # 839`  `
df_adult_dx_test %<>% 
  filter(Site != 'Bangalore') # 208

# save data
write.csv(row.names = F, df_adult_dx_train, 'T.Dx_Adult_S.Train2_839_v.z.norm.220302.csv')
write.csv(row.names = F, df_adult_dx_test, 'T.Dx_Adult_S.Test2_208_v.z.norm.220302.csv')



#####################################
#### [4] med OCD vs. HC #####
#####################################
#####################################
rm(list = ls())
path_source <- "C:/Users/±Ë∫∏∞‚/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Analysis/2_z-scaled"

setwd(paste0(path_source, '/1.adult'))
df_adult_dx_total <- 


#####################################
###### Data description of extra experiments ##########

##### 1. adult - bangalore ¡¶ø‹
path_source <- "C:/Users/±Ë∫∏∞‚/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Analysis/2_z-scaled"
setwd(paste0(path_source, '/1.adult'))
getwd()

df_adult_dx_train_woB <- read_csv('T.Dx_Adult_S.Train2_839_v.z.norm.220302.csv')
df_adult_dx_test_woB <- read_csv('T.Dx_Adult_S.Test2_208_v.z.norm.220302.csv')

df_adult_dx_total_woB <- df_adult_dx_train_woB %>% bind_rows(df_adult_dx_test_woB)
df_adult_dx_total_woB %>% xtabs(~Dx, data = .)



##### 2. adult - med new version
getwd()
df_adult_med12_train_new <- read_csv('T.MedUnmedOCD_Adult_S.Train2_428_v.z.norm.220302.csv')
df_adult_med12_test_new <- read_csv('T.MedUnmedOCD_Adult_S.Test2_256_v.z.norm.220302.csv')

df_adult_med12_total_new <- df_adult_med12_train_new %>% bind_rows(df_adult_med12_test_new)


df_adult_med12_total_new %>%xtabs(~Med.UnmedOCD, data = .)
df_adult_med12_train_new %>%xtabs(~Med.UnmedOCD, data = .)
df_adult_med12_test_new %>%xtabs(~Med.UnmedOCD, data = .)




