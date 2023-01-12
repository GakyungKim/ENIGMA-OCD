library(tidyverse)
library(caret)

##### log #####
# 1. column order
# 2. target variable name: Dx, Med.UnmedOCD
# 3. subjecykey 
# Age also normalized

##### load dataset #####

setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Analysis/1_Data split")
# Target Outcome: Dx
df_T.Dx_Train <- read.csv('T.Dx_Adult_S.Train_1069_v.211123.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()
df_T.Dx_Test <- read.csv('T.Dx_Adult_S.Test_267_v.211122.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# Target outcome: med OCD vs. unmed OCD
df_T.MedUnmed_Train <- read.csv('T.MedUnmedOCD_Adult_S.Train_547_v.211123.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()
df_T.MedUnmed_Test <- read.csv('T.MedUnmedOCD_Adult_S.Test_137_v.211123.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# Target Outcome: unmed OCD vs. HC
df_T.UnmedHC_Train <- read.csv('T.UnmedOCDHC_Adult_S.Train_855_v.211123.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()
df_T.UnmedHC_Test <- read.csv('T.UnmedOCDHC_Adult_S.Test_214_v.211123.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# check
df_T.Dx_Train

df_T.Dx_Test
df_T.MedUnmed_Train
df_T.MedUnmed_Test
df_T.UnmedHC_Train
df_T.UnmedHC_Test


#### z scaling - Target Outcome: Dx ####
head(names(df_T.Dx_Train), n=30)
tail(names(df_T.Dx_Train), n = 30)

head(names(df_T.Dx_Test), n=30)
tail(names(df_T.Dx_Test), n = 30)

T.Dx_locs_non_num <- c(#which(names(df_T.Dx_Train) == "subjectkey"), 
  which(names(df_T.Dx_Train) == 'Dx'), which(names(df_T.Dx_Train) == 'Site'),  
  which(names(df_T.Dx_Train) == "subjectID"),
  #which(names(df_T.Dx_Train) == 'Age'),
  which(names(df_T.Dx_Train) == 'AgeSQ'),
  which(names(df_T.Dx_Train) == 'Sex'), which(names(df_T.Dx_Train) == 'Med'),
  which(names(df_T.Dx_Train) == 'AO'), which(names(df_T.Dx_Train) == 'Dur'),
  which(names(df_T.Dx_Train) == 'Sev'), which(names(df_T.Dx_Train) == 'Agr_Check'),
  which(names(df_T.Dx_Train) == 'Clean'), which(names(df_T.Dx_Train) == 'Sex_Rel'),
  which(names(df_T.Dx_Train) == 'Hoard'), which(names(df_T.Dx_Train) == 'Ord'),
  which(names(df_T.Dx_Train) == 'Anx'), which(names(df_T.Dx_Train) == 'CurAnx'),
  which(names(df_T.Dx_Train) == 'Dep'), which(names(df_T.Dx_Train) == 'CurDep'))


T.Dx_locs_non_num_ID <- c( which(names(df_T.Dx_Train) == "subjectkey"), 
                           which(names(df_T.Dx_Train) == 'Dx'), which(names(df_T.Dx_Train) == 'Site'),  
                           which(names(df_T.Dx_Train) == "subjectID"),
                           #which(names(df_T.Dx_Train) == 'Age'),
                           which(names(df_T.Dx_Train) == 'AgeSQ'),
                           which(names(df_T.Dx_Train) == 'Sex'), which(names(df_T.Dx_Train) == 'Med'),
                           which(names(df_T.Dx_Train) == 'AO'), which(names(df_T.Dx_Train) == 'Dur'),
                           which(names(df_T.Dx_Train) == 'Sev'), which(names(df_T.Dx_Train) == 'Agr_Check'),
                           which(names(df_T.Dx_Train) == 'Clean'), which(names(df_T.Dx_Train) == 'Sex_Rel'),
                           which(names(df_T.Dx_Train) == 'Hoard'), which(names(df_T.Dx_Train) == 'Ord'),
                           which(names(df_T.Dx_Train) == 'Anx'), which(names(df_T.Dx_Train) == 'CurAnx'),
                           which(names(df_T.Dx_Train) == 'Dep'), which(names(df_T.Dx_Train) == 'CurDep'))


## Train set
df_T.Dx_Train_num  <- df_T.Dx_Train[, -T.Dx_locs_non_num]
df_T.Dx_Train_non_num  <- df_T.Dx_Train[, T.Dx_locs_non_num_ID]

df_T.Dx_Train_num_z_norm <- preProcess(df_T.Dx_Train_num, method = c("center", "scale"))
df_T.Dx_Train_num_z_norm2 = predict(df_T.Dx_Train_num_z_norm, df_T.Dx_Train_num)
df_T.Dx_Train_num_z_norm3 <- merge(df_T.Dx_Train_non_num, df_T.Dx_Train_num_z_norm2, by = 'subjectkey') %>% as_tibble()

# change variable order
df_T.Dx_Train_num_z_norm3 <-df_T.Dx_Train_num_z_norm3 %>%  relocate(c(subjectkey, Dx, Site, subjectID, Age))



## Test set
df_T.Dx_Test_num  <- df_T.Dx_Test[, -T.Dx_locs_non_num]
df_T.Dx_Test_non_num  <- df_T.Dx_Test[, T.Dx_locs_non_num_ID]

df_T.Dx_Test_num_z_norm <- preProcess(df_T.Dx_Test_num, method = c("center", "scale"))
df_T.Dx_Test_num_z_norm2 = predict(df_T.Dx_Test_num_z_norm, df_T.Dx_Test_num)
df_T.Dx_Test_num_z_norm3 <- merge(df_T.Dx_Test_non_num, df_T.Dx_Test_num_z_norm2, by = 'subjectkey') %>% as_tibble()

# change variable order
df_T.Dx_Test_num_z_norm3 <-df_T.Dx_Test_num_z_norm3 %>%  relocate(c(subjectkey, Dx, Site, subjectID, Age))

# check
df_T.Dx_Train_num_z_norm3
df_T.Dx_Test_num_z_norm3

#### z scaling - Target Outcome: med OCD vs. unmed OCD ####

head(names(df_T.MedUnmed_Train), n=30)
tail(names(df_T.MedUnmed_Train), n = 30)

head(names(df_T.MedUnmed_Test), n=30)
tail(names(df_T.MedUnmed_Test), n = 30)

T.MedUnmed_locs_non_num <- c(#which(names(df_T.MedUnmed_Train) == "subjectkey"), 
  which(names(df_T.MedUnmed_Train) == 'Med.UnmedOCD'), which(names(df_T.MedUnmed_Train) == 'Site'),  
  which(names(df_T.MedUnmed_Train) == "subjectID"), which(names(df_T.MedUnmed_Train) == "Dx"),
  #which(names(df_T.MedUnmed_Train) == 'Age'),
  which(names(df_T.MedUnmed_Train) == 'AgeSQ'),
  which(names(df_T.MedUnmed_Train) == 'Sex'), which(names(df_T.MedUnmed_Train) == 'Med'),
  which(names(df_T.MedUnmed_Train) == 'AO'), which(names(df_T.MedUnmed_Train) == 'Dur'),
  which(names(df_T.MedUnmed_Train) == 'Sev'), which(names(df_T.MedUnmed_Train) == 'Agr_Check'),
  which(names(df_T.MedUnmed_Train) == 'Clean'), which(names(df_T.MedUnmed_Train) == 'Sex_Rel'),
  which(names(df_T.MedUnmed_Train) == 'Hoard'), which(names(df_T.MedUnmed_Train) == 'Ord'),
  which(names(df_T.MedUnmed_Train) == 'Anx'), which(names(df_T.MedUnmed_Train) == 'CurAnx'),
  which(names(df_T.MedUnmed_Train) == 'Dep'), which(names(df_T.MedUnmed_Train) == 'CurDep'))


T.MedUnmed_locs_non_num_ID <- c( which(names(df_T.MedUnmed_Train) == "subjectkey"), 
                                 which(names(df_T.MedUnmed_Train) == 'Med.UnmedOCD'), which(names(df_T.MedUnmed_Train) == 'Site'),  
                                 which(names(df_T.MedUnmed_Train) == "subjectID"),  which(names(df_T.MedUnmed_Train) == "Dx"),
                                 #which(names(df_T.MedUnmed_Train) == 'Age'),
                                 which(names(df_T.MedUnmed_Train) == 'AgeSQ'),
                                 which(names(df_T.MedUnmed_Train) == 'Sex'), which(names(df_T.MedUnmed_Train) == 'Med'),
                                 which(names(df_T.MedUnmed_Train) == 'AO'), which(names(df_T.MedUnmed_Train) == 'Dur'),
                                 which(names(df_T.MedUnmed_Train) == 'Sev'), which(names(df_T.MedUnmed_Train) == 'Agr_Check'),
                                 which(names(df_T.MedUnmed_Train) == 'Clean'), which(names(df_T.MedUnmed_Train) == 'Sex_Rel'),
                                 which(names(df_T.MedUnmed_Train) == 'Hoard'), which(names(df_T.MedUnmed_Train) == 'Ord'),
                                 which(names(df_T.MedUnmed_Train) == 'Anx'), which(names(df_T.MedUnmed_Train) == 'CurAnx'),
                                 which(names(df_T.MedUnmed_Train) == 'Dep'), which(names(df_T.MedUnmed_Train) == 'CurDep'))


T.MedUnmed_locs_non_num
T.MedUnmed_locs_non_num_ID


## Train set
df_T.MedUnmed_Train_num  <- df_T.MedUnmed_Train[, -T.MedUnmed_locs_non_num]
df_T.MedUnmed_Train_non_num  <- df_T.MedUnmed_Train[, T.MedUnmed_locs_non_num_ID]
df_T.MedUnmed_Train_num
df_T.MedUnmed_Train_non_num

df_T.MedUnmed_Train_num_z_norm <- preProcess(df_T.MedUnmed_Train_num, method = c("center", "scale"))
df_T.MedUnmed_Train_num_z_norm2 = predict(df_T.MedUnmed_Train_num_z_norm, df_T.MedUnmed_Train_num)
df_T.MedUnmed_Train_num_z_norm3 <- merge(df_T.MedUnmed_Train_non_num, df_T.MedUnmed_Train_num_z_norm2, by = 'subjectkey') %>% as_tibble()

# change variable order
df_T.MedUnmed_Train_num_z_norm3 <-df_T.MedUnmed_Train_num_z_norm3 %>%  relocate(c(subjectkey, Med.UnmedOCD, Site, Dx, subjectID, Age))
#check
df_T.MedUnmed_Train_num_z_norm3 # z norm
df_T.MedUnmed_Train # original


## Test set
df_T.MedUnmed_Test_num  <- df_T.MedUnmed_Test[, -T.MedUnmed_locs_non_num]
df_T.MedUnmed_Test_non_num  <- df_T.MedUnmed_Test[, T.MedUnmed_locs_non_num_ID]
df_T.MedUnmed_Test_num
df_T.MedUnmed_Test_non_num

df_T.MedUnmed_Test_num_z_norm <- preProcess(df_T.MedUnmed_Test_num, method = c("center", "scale"))
df_T.MedUnmed_Test_num_z_norm2 = predict(df_T.MedUnmed_Test_num_z_norm, df_T.MedUnmed_Test_num)
df_T.MedUnmed_Test_num_z_norm3 <- merge(df_T.MedUnmed_Test_non_num, df_T.MedUnmed_Test_num_z_norm2, by = 'subjectkey') %>% as_tibble()

# change variable order
df_T.MedUnmed_Test_num_z_norm3 <-df_T.MedUnmed_Test_num_z_norm3 %>%  relocate(c(subjectkey, Med.UnmedOCD, Site, Dx, subjectID, Age))
#check
df_T.MedUnmed_Test_num_z_norm3 # z norm
df_T.MedUnmed_Test # original


#### z scaling - Target Outcome: unmed OCD vs. HC  ####
head(names(df_T.UnmedHC_Train), n=30)
tail(names(df_T.UnmedHC_Train), n = 30)

head(names(df_T.UnmedHC_Test), n=30)
tail(names(df_T.UnmedHC_Test), n = 30)

T.UnmedHC_locs_non_num <- c(#which(names(df_T.UnmedHC_Train) == "subjectkey"), 
  which(names(df_T.UnmedHC_Train) == 'UnmedOCD.HC'), which(names(df_T.UnmedHC_Train) == 'Site'),  
  which(names(df_T.UnmedHC_Train) == "subjectID"), which(names(df_T.UnmedHC_Train) == "Dx"),
  #which(names(df_T.UnmedHC_Train) == 'Age'),
  which(names(df_T.UnmedHC_Train) == 'AgeSQ'),
  which(names(df_T.UnmedHC_Train) == 'Sex'), which(names(df_T.UnmedHC_Train) == 'Med'),
  which(names(df_T.UnmedHC_Train) == 'AO'), which(names(df_T.UnmedHC_Train) == 'Dur'),
  which(names(df_T.UnmedHC_Train) == 'Sev'), which(names(df_T.UnmedHC_Train) == 'Agr_Check'),
  which(names(df_T.UnmedHC_Train) == 'Clean'), which(names(df_T.UnmedHC_Train) == 'Sex_Rel'),
  which(names(df_T.UnmedHC_Train) == 'Hoard'), which(names(df_T.UnmedHC_Train) == 'Ord'),
  which(names(df_T.UnmedHC_Train) == 'Anx'), which(names(df_T.UnmedHC_Train) == 'CurAnx'),
  which(names(df_T.UnmedHC_Train) == 'Dep'), which(names(df_T.UnmedHC_Train) == 'CurDep'))


T.UnmedHC_locs_non_num_ID <- c( which(names(df_T.UnmedHC_Train) == "subjectkey"), 
                                which(names(df_T.UnmedHC_Train) == 'UnmedOCD.HC'), which(names(df_T.UnmedHC_Train) == 'Site'),  
                                which(names(df_T.UnmedHC_Train) == "subjectID"),  which(names(df_T.UnmedHC_Train) == "Dx"),
                                #which(names(df_T.UnmedHC_Train) == 'Age'),
                                which(names(df_T.UnmedHC_Train) == 'AgeSQ'),
                                which(names(df_T.UnmedHC_Train) == 'Sex'), which(names(df_T.UnmedHC_Train) == 'Med'),
                                which(names(df_T.UnmedHC_Train) == 'AO'), which(names(df_T.UnmedHC_Train) == 'Dur'),
                                which(names(df_T.UnmedHC_Train) == 'Sev'), which(names(df_T.UnmedHC_Train) == 'Agr_Check'),
                                which(names(df_T.UnmedHC_Train) == 'Clean'), which(names(df_T.UnmedHC_Train) == 'Sex_Rel'),
                                which(names(df_T.UnmedHC_Train) == 'Hoard'), which(names(df_T.UnmedHC_Train) == 'Ord'),
                                which(names(df_T.UnmedHC_Train) == 'Anx'), which(names(df_T.UnmedHC_Train) == 'CurAnx'),
                                which(names(df_T.UnmedHC_Train) == 'Dep'), which(names(df_T.UnmedHC_Train) == 'CurDep'))

T.UnmedHC_locs_non_num
T.UnmedHC_locs_non_num_ID


## Train set
df_T.UnmedHC_Train_num  <- df_T.UnmedHC_Train[, -T.UnmedHC_locs_non_num]
df_T.UnmedHC_Train_non_num  <- df_T.UnmedHC_Train[, T.UnmedHC_locs_non_num_ID]
df_T.UnmedHC_Train_num
df_T.UnmedHC_Train_non_num

df_T.UnmedHC_Train_num_z_norm <- preProcess(df_T.UnmedHC_Train_num, method = c("center", "scale"))
df_T.UnmedHC_Train_num_z_norm2 = predict(df_T.UnmedHC_Train_num_z_norm, df_T.UnmedHC_Train_num)
df_T.UnmedHC_Train_num_z_norm3 <- merge(df_T.UnmedHC_Train_non_num, df_T.UnmedHC_Train_num_z_norm2, by = 'subjectkey') %>% as_tibble()

# change variable order
df_T.UnmedHC_Train_num_z_norm3 <-df_T.UnmedHC_Train_num_z_norm3 %>%  relocate(c(subjectkey, UnmedOCD.HC, Site, Dx, subjectID, Age))
#check
df_T.UnmedHC_Train_num_z_norm3 # z norm
df_T.UnmedHC_Train # original

## Test set
df_T.UnmedHC_Test_num  <- df_T.UnmedHC_Test[, -T.UnmedHC_locs_non_num]
df_T.UnmedHC_Test_non_num  <- df_T.UnmedHC_Test[, T.UnmedHC_locs_non_num_ID]
df_T.UnmedHC_Test_num
df_T.UnmedHC_Test_non_num

df_T.UnmedHC_Test_num_z_norm <- preProcess(df_T.UnmedHC_Test_num, method = c("center", "scale"))
df_T.UnmedHC_Test_num_z_norm2 = predict(df_T.UnmedHC_Test_num_z_norm, df_T.UnmedHC_Test_num)
df_T.UnmedHC_Test_num_z_norm3 <- merge(df_T.UnmedHC_Test_non_num, df_T.UnmedHC_Test_num_z_norm2, by = 'subjectkey') %>% as_tibble()

# change variable order
df_T.UnmedHC_Test_num_z_norm3 <-df_T.UnmedHC_Test_num_z_norm3 %>%  relocate(c(subjectkey, UnmedOCD.HC, Site, Dx, subjectID, Age))
#check
df_T.UnmedHC_Test_num_z_norm3 # z norm
df_T.UnmedHC_Test # original



#### Save file ####
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Analysis/2_z-scaled")

# Target Outcome: Dx
write.csv(df_T.Dx_Train_num_z_norm3, file ='T.Dx_Adult_S.Train_1069_v.z.norm.211123.csv', row.names = FALSE)
write.csv(df_T.Dx_Test_num_z_norm3, file ='T.Dx_Adult_S.Test_267_v.z.norm.211123.csv', row.names = FALSE)

# Target outcome: med OCD vs. unmed OCD
write.csv(df_T.MedUnmed_Train_num_z_norm3, file ='T.MedUnmedOCD_Adult_S.Train_547_v.z.norm.211123.csv', row.names = FALSE)
write.csv(df_T.MedUnmed_Test_num_z_norm3, file ='T.MedUnmedOCD_Adult_S.Test_137_v.z.norm.211123.csv', row.names = FALSE)

# Target Outcome: unmed OCD vs. HC
write.csv(df_T.UnmedHC_Train_num_z_norm3, file ='T.UnmedOCDHC_Adult_S.Train_855_v.z.norm.211123.csv', row.names = FALSE)
write.csv(df_T.UnmedHC_Test_num_z_norm3, file ='T.UnmedOCDHC_Adult_S.Test_214_v.z.norm.211123.csv', row.names = FALSE)



