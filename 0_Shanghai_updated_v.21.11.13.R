# Data foramtting shanghai data 
# we got the data - 21.11.11


library(tidyverse)
library(naniar)
library(skimr)

setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Piras_Shanghai updated_v.2021.11.11")

#### Load dataset ####

# covariate ROI
ShanghaiCovariate = read.csv(file='DTI-covariates.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

ShanghaiFA = read.csv(file='shangai_newFA.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()


# MD
ShanghaiMD = read.csv(file='combinedROItable_MD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# RD
ShanghaiRD = read.csv(file='combinedROItable_RD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# AD
ShanghaiAD = read.csv(file='combinedROItable_AD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

ShanghaiCovariate
ShanghaiFA
ShanghaiMD
ShanghaiRD
ShanghaiAD

# ¸¸µé¾î¾ß ÇÏ´Â °Í. 
#ShanghaicombinedROItable
ShanghaiCovariate # Dx, age, Sex
ShanghaiFA # Dx, Age, Sex
ShanghaiMD # Age, Sex
ShanghaiRD # Age, Sex
ShanghaiAD # Age, Sex

#### Check column names ####
names(ShanghaiCovariate)[1] <- 'subjectID'
names(ShanghaiFA)[1] <- 'subjectID'


#### Rename variable name ####
ShanghaiCovariate <- rename(ShanghaiCovariate, Age= age)
ShanghaiCovariate <- rename(ShanghaiCovariate, Sex= sex)


#### Renaming subjectID in FA file ####

ShanghaiFA$subjectID <- gsub("HC", "", ShanghaiFA$subjectID)
ShanghaiFA$subjectID <- gsub("OCD", "", ShanghaiFA$subjectID)

table(ShanghaiFA$subjectID)


#### Arrange ####
ShanghaiCovariate <- ShanghaiCovariate %>% arrange(subjectID)
ShanghaiFA <- ShanghaiFA %>% arrange(subjectID)
ShanghaiMD <- ShanghaiMD %>% arrange(subjectID)
ShanghaiRD <- ShanghaiRD %>% arrange(subjectID)
ShanghaiAD <- ShanghaiAD %>% arrange(subjectID)



#### Merge Covariate + FA ####
ShanghaiCovariate <- ShanghaiCovariate %>% 
  rename(Age = age, Sex = sex)

ShanghaicombinedROItable <- ShanghaiCovariate %>% 
  select(-X) %>% 
  left_join(ShanghaiFA, by = c('subjectID', 'Dx', 'Age', 'Sex'))

ShanghaicombinedROItable

library(naniar)
vis_miss(ShanghaicombinedROItable)


#### writie csv ####

# combined ROI
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/1.FA")
write.csv(ShanghaicombinedROItable, 'shangai_combinedROItable_updated.v.21.11.13.csv', row.names = FALSE)

# MD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/2.MD")
write.csv(ShanghaiMD, 'Shangai_combinedROItable_MD_updated.v.21.11.13.csv', row.names = FALSE)


# RD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/3.RD")

write.csv(ShanghaiRD, 'Shangai_combinedROItable_RD_updated.v.21.11.13.csv', row.names = FALSE)

# AD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/4.AD")
write.csv(ShanghaiAD, 'Shangai_combinedROItable_AD_updated.v.21.11.13.csv', row.names = FALSE)








