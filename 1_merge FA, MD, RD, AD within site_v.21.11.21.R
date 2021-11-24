library(tidyverse)
library(naniar)
library(skimr)

# Merging data of each site. 
# subjectID
# covariate: 17
# FA, MD, RD, AD measure: 63


#### Make list for column name  ####

list_cov <- c('Dx',
              'Age',
              'AgeSQ',
              'Sex',
              'Med',
              'AO',
              'Dur',
              'Sev',
              'Agr_Check',
              'Clean',
              'Sex_Rel',
              'Hoard',
              'Ord',
              'Anx',
              'CurAnx',
              'Dep',
              'CurDep')

list_FA <- c('ACR.FA',	             'ACR.L.FA',	             'ACR.R.FA',	             'ALIC.FA',	             'ALIC.L.FA',	             'ALIC.R.FA',	             'AverageFA',	             'BCC.FA',	             'CC.FA',	             'CGC.FA',	             'CGC.L.FA',	             'CGC.R.FA',	             'CGH.FA',	             'CGH.L.FA',	             'CGH.R.FA',	             'CR.FA',	             'CR.L.FA',	             'CR.R.FA',	             'CST.FA',	             'CST.L.FA',	             'CST.R.FA',	             'EC.FA',	             'EC.L.FA',	             'EC.R.FA',	            'FX.FA',	             'FX.ST.L.FA',	             'FX.ST.R.FA',	             'FXST.FA',	             'GCC.FA',	             'IC.FA',	            'IC.L.FA',	             'IC.R.FA',	             'IFO.FA',	             'IFO.L.FA',	             'IFO.R.FA',	             'PCR.FA',	             'PCR.L.FA',	             'PCR.R.FA',	             'PLIC.FA',	             'PLIC.L.FA',	             'PLIC.R.FA',	             'PTR.FA',	             'PTR.L.FA',	             'PTR.R.FA',	             'RLIC.FA',	             'RLIC.L.FA',	             'RLIC.R.FA',	             'SCC.FA',	             'SCR.FA',	             'SCR.L.FA',	             'SCR.R.FA',	             'SFO.FA',	             'SFO.L.FA',	             'SFO.R.FA',	             'SLF.FA',	             'SLF.L.FA',	             'SLF.R.FA',	             'SS.FA',	             'SS.L.FA',	             'SS.R.FA',	             'UNC.FA',	             'UNC.L.FA',	             'UNC.R.FA')

list_MD <- c('ACR.MD',	             'ACR.L.MD',	             'ACR.R.MD',	             'ALIC.MD',	             'ALIC.L.MD',	             'ALIC.R.MD',	             'AverageMD',	             'BCC.MD',	             'CC.MD',	             'CGC.MD',	             'CGC.L.MD',	             'CGC.R.MD',	             'CGH.MD',	             'CGH.L.MD',	             'CGH.R.MD',	             'CR.MD',	             'CR.L.MD',	             'CR.R.MD',	             'CST.MD',	             'CST.L.MD',	             'CST.R.MD',	             'EC.MD',	             'EC.L.MD',	             'EC.R.MD',	            'FX.MD',	             'FX.ST.L.MD',	             'FX.ST.R.MD',	             'FXST.MD',	             'GCC.MD',	             'IC.MD',	            'IC.L.MD',	             'IC.R.MD',	             'IFO.MD',	             'IFO.L.MD',	             'IFO.R.MD',	             'PCR.MD',	             'PCR.L.MD',	             'PCR.R.MD',	             'PLIC.MD',	             'PLIC.L.MD',	             'PLIC.R.MD',	             'PTR.MD',	             'PTR.L.MD',	             'PTR.R.MD',	             'RLIC.MD',	             'RLIC.L.MD',	             'RLIC.R.MD',	             'SCC.MD',	             'SCR.MD',	             'SCR.L.MD',	             'SCR.R.MD',	             'SFO.MD',	             'SFO.L.MD',	             'SFO.R.MD',	             'SLF.MD',	             'SLF.L.MD',	             'SLF.R.MD',	             'SS.MD',	             'SS.L.MD',	             'SS.R.MD',	             'UNC.MD',	             'UNC.L.MD',	             'UNC.R.MD')

list_RD <- c( 'ACR.RD',	             'ACR.L.RD',	             'ACR.R.RD',	             'ALIC.RD',	             'ALIC.L.RD',	             'ALIC.R.RD',	             'AverageRD',	             'BCC.RD',	             'CC.RD',	             'CGC.RD',	             'CGC.L.RD',	             'CGC.R.RD',	             'CGH.RD',	             'CGH.L.RD',	             'CGH.R.RD',	             'CR.RD',	             'CR.L.RD',	             'CR.R.RD',	             'CST.RD',	             'CST.L.RD',	             'CST.R.RD',	             'EC.RD',	             'EC.L.RD',	             'EC.R.RD',	            'FX.RD',	             'FX.ST.L.RD',	             'FX.ST.R.RD',	             'FXST.RD',	             'GCC.RD',	             'IC.RD',	            'IC.L.RD',	             'IC.R.RD',	             'IFO.RD',	             'IFO.L.RD',	             'IFO.R.RD',	             'PCR.RD',	             'PCR.L.RD',	             'PCR.R.RD',	             'PLIC.RD',	             'PLIC.L.RD',	             'PLIC.R.RD',	             'PTR.RD',	             'PTR.L.RD',	             'PTR.R.RD',	             'RLIC.RD',	             'RLIC.L.RD',	             'RLIC.R.RD',	             'SCC.RD',	             'SCR.RD',	             'SCR.L.RD',	             'SCR.R.RD',	             'SFO.RD',	             'SFO.L.RD',	             'SFO.R.RD',	             'SLF.RD',	             'SLF.L.RD',	             'SLF.R.RD',	             'SS.RD',	             'SS.L.RD',	             'SS.R.RD',	             'UNC.RD',	             'UNC.L.RD',	             'UNC.R.RD')

list_AD <- c( 'ACR.AD',	             'ACR.L.AD',	             'ACR.R.AD',	             'ALIC.AD',	             'ALIC.L.AD',	             'ALIC.R.AD',	             'AverageAD',	             'BCC.AD',	             'CC.AD',	             'CGC.AD',	             'CGC.L.AD',	             'CGC.R.AD',	             'CGH.AD',	             'CGH.L.AD',	             'CGH.R.AD',	             'CR.AD',	             'CR.L.AD',	             'CR.R.AD',	             'CST.AD',	             'CST.L.AD',	             'CST.R.AD',	             'EC.AD',	             'EC.L.AD',	             'EC.R.AD',	            'FX.AD',	             'FX.ST.L.AD',	             'FX.ST.R.AD',	             'FXST.AD',	             'GCC.AD',	             'IC.AD',	            'IC.L.AD',	             'IC.R.AD',	             'IFO.AD',	             'IFO.L.AD',	             'IFO.R.AD',	             'PCR.AD',	             'PCR.L.AD',	             'PCR.R.AD',	             'PLIC.AD',	             'PLIC.L.AD',	             'PLIC.R.AD',	             'PTR.AD',	             'PTR.L.AD',	             'PTR.R.AD',	             'RLIC.AD',	             'RLIC.L.AD',	             'RLIC.R.AD',	             'SCC.AD',	             'SCR.AD',	             'SCR.L.AD',	             'SCR.R.AD',	             'SFO.AD',	             'SFO.L.AD',	             'SFO.R.AD',	             'SLF.AD',	             'SLF.L.AD',	             'SLF.R.AD',	             'SS.AD',	             'SS.L.AD',	             'SS.R.AD',	             'UNC.AD',	             'UNC.L.AD',	             'UNC.R.AD')




# FA, MD, RD, AD: 63 variables

#### 1. Amsterdam ####
#### 1. Amsterdam - Data load####

# Number of subject
'''
79 > 72 : due to no covariate including age, sex, diagnosis
# Number of column
- combined ROI: 81
- MD: 67
- RD: 67
- AD: 67
# merged: 79*273
'''

# combined ROI
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/1.FA")
AmsterdamcombinedROItable = read.csv(file='amsterdam_combinedROItable.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# MD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/2.MD")

AmsterdamMD = read.csv(file='Amsterdam_VUMC_combinedROItable_MD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# RD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/3.RD")

AmsterdamRD = read.csv(file='Amsterdam_VUMC_combinedROItable_RD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# AD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/4.AD")

AmsterdamAD = read.csv(file='Amsterdam_VUMC_combinedROItable_AD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

AmsterdamcombinedROItable
AmsterdamMD
AmsterdamRD
AmsterdamAD

# 
names(AmsterdamcombinedROItable)[1] <- 'subjectID'


#### 1. Amsterdam - variable name setting - DTI ####

# COmbined ROI
which(colnames(AmsterdamcombinedROItable) == "ACR")
ncol(AmsterdamcombinedROItable)
names(AmsterdamcombinedROItable[19:81])
length(names(AmsterdamcombinedROItable[19:81]))
AmsterdamcombinedROItable <- AmsterdamcombinedROItable[1:18] %>% 
  bind_cols(setNames(AmsterdamcombinedROItable[19:81], list_FA))  

# MD
which(colnames(AmsterdamMD) == "ACR")
ncol(AmsterdamMD)
67-4 
AmsterdamMD <- AmsterdamMD[1:4] %>% 
  bind_cols(setNames(AmsterdamMD[5:67], list_MD))

# RD
which(colnames(AmsterdamRD) == "ACR")
ncol(AmsterdamRD)
AmsterdamRD <- AmsterdamRD[1:4] %>% 
  bind_cols(setNames(AmsterdamRD[5:67], list_RD))

# AD
which(colnames(AmsterdamAD) == 'ACR')
ncol(AmsterdamAD)
AmsterdamAD <- AmsterdamAD[1:4] %>% 
  bind_cols(setNames(AmsterdamAD[5:67], list_AD))

#### 1. Amsterdam - Arrange####
AmsterdamcombinedROItable <- AmsterdamcombinedROItable %>% 
  arrange(subjectID)
AmsterdamMD <- AmsterdamMD %>% 
  arrange(subjectID)
AmsterdamRD <- AmsterdamRD %>% 
  arrange(subjectID)
AmsterdamAD <- AmsterdamAD %>% 
  arrange(subjectID)

#### 1. Amsterdam - Merge ####
Amsterdam_merged <- AmsterdamcombinedROItable %>% 
  bind_cols(AmsterdamMD %>% select(-c(subjectID, Age, Sex, Diagnosis))) %>% 
  bind_cols(AmsterdamRD %>% select(-c(subjectID, Age, Sex, Diagnosis))) %>% 
  bind_cols(AmsterdamAD %>% select(-c(subjectID, Age, Sex, Diagnosis)))

#### 2. Bangalore ####

#### 2. Bangalore - Load data ####
'''
# Number of subject 
- combinedROI: 290
- the others: 
  # Number of column
  - combined ROI: 81 
- MD: 80 > 67
- RD: 80 > 67
- AD: 80 > 67  
# merged : 290 * 273
'''

# combined ROI
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/1.FA")

BangalorecombinedROItable = read.csv(file='bangelore_combinedroitablefa.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()
# MD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/2.MD")
BangaloreMD = read.csv(file='Bangalore_combinedROItable_MD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# RD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/3.RD")
BangaloreRD = read.csv(file='Bangalore_combinedROItable_RD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# AD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/4.AD")
BangaloreAD = read.csv(file='Bangalore_combinedROItable_AD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

BangalorecombinedROItable
BangaloreMD
BangaloreRD
BangaloreAD

#### 2. Bangalore - Variable name setting - covariates ####
names(BangalorecombinedROItable)[1] <- 'subjectID'
BangalorecombinedROItable <- rename(BangalorecombinedROItable, Sex_Rel = Sex_rel)
BangalorecombinedROItable <- rename(BangalorecombinedROItable, AgeSQ = AgeSq)

#### 2. Bangalore - Variable order setting ####
BangalorecombinedROItable<- BangalorecombinedROItable %>%  relocate(c(subjectID,
                                                                      Dx,
                                                                      Age,
                                                                      AgeSQ,
                                                                      Sex,
                                                                      Med,
                                                                      AO,
                                                                      Dur,
                                                                      Sev,
                                                                      Agr_Check,
                                                                      Clean,
                                                                      Sex_Rel,
                                                                      Hoard,
                                                                      Ord,
                                                                      Anx,
                                                                      CurAnx,
                                                                      Dep,
                                                                      CurDep))

#### 2. Bangalore - Variable name setting - DTI ####
# combined ROI
which(colnames(BangalorecombinedROItable) =="ACR.fa") # 19
ncol(BangalorecombinedROItable) # 81
BangalorecombinedROItable <- BangalorecombinedROItable[1:18] %>% 
  bind_cols(setNames(BangalorecombinedROItable[19:81], list_FA))

# MD
which(colnames(BangaloreMD) == "ACR.md") # 19
ncol(BangaloreMD) #81
BangaloreMD <- BangaloreMD[1:18] %>% 
  bind_cols(setNames(BangaloreMD[19:81], list_MD))

# RD
which(colnames(BangaloreRD) == "ACR.rd")
ncol(BangaloreRD)
BangaloreRD <- BangaloreRD[1:18] %>% 
  bind_cols(setNames(BangaloreRD[19:81], list_RD))

# AD
which(colnames(BangaloreAD) == 'ACR.ad')
ncol(BangaloreAD)
BangaloreAD <- BangaloreAD[1:18] %>% 
  bind_cols(setNames(BangaloreAD[19:81], list_AD))


#### 2. Bangalore - arrange####

BangalorecombinedROItable <- BangalorecombinedROItable %>% 
  arrange(subjectID)
BangaloreMD <- BangaloreMD %>% 
  arrange(subjectID)
BangaloreRD <- BangaloreRD %>% 
  arrange(subjectID)
BangaloreAD <- BangaloreAD %>% 
  arrange(subjectID)

#### 2. Bangalore - Merge ####

# Data merge
Bangalore_merged <- BangalorecombinedROItable %>% 
  bind_cols(BangaloreMD %>%  select(-c(subjectID, Age, Sex, Dx, AgeSq,	Med,	AO,	Dur,	Sev,	Agr_Check,	Clean,	Sex_rel,	Hoard,	Ord,	Anx,	Dep,	CurAnx,	CurDep))) %>% 
  bind_cols(BangaloreRD %>%   select(-c(subjectID, Age, Sex, Dx, AgeSq,	Med,	AO,	Dur,	Sev,	Agr_Check,	Clean,	Sex_rel,	Hoard,	Ord,	Anx,	Dep,	CurAnx,	CurDep))) %>% 
  bind_cols(BangaloreAD %>%   select(-c(subjectID, Age, Sex, Diagnosis, AgeSq,	Med,	AO,	Dur,	Sev,	Agr_Check,	Clean,	Sex_rel,	Hoard,	Ord,	Anx,	Dep,	CurAnx,	CurDep))) 


#### 3. Capetown ####

#### 3. Capetown - Load dataset ####
# combined ROI
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/1.FA")

CapetowncombinedROItable = read.csv(file='capetown_combinedROItable.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()
# MD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/2.MD")
CapetownMD = read.csv(file='CapeTown_combinedROItable_MD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()
# RD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/3.RD")
CapetownRD = read.csv(file='CapeTown_combinedROItable_RD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()
# AD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/4.AD")
CapetownAD = read.csv(file='CapeTown_combinedROItable_AD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

CapetowncombinedROItable
CapetownMD
CapetownRD
CapetownAD

#### 3. Capetown - Variable setting -covariate ####
names(CapetowncombinedROItable)[1] <- 'subjectID'
CapetowncombinedROItable <- rename(CapetowncombinedROItable, AgeSQ = agesq)


#### 3. Capetown -   Variable name setting - DTI ####
# combined ROI
which(colnames(CapetowncombinedROItable) == "ACR")
ncol(CapetowncombinedROItable)
CapetowncombinedROItable<- CapetowncombinedROItable[1:18] %>% 
  bind_cols(setNames(CapetowncombinedROItable[19:81], list_FA))

# MD
which(colnames(CapetownMD) == "ACR")
ncol(CapetownMD)
CapetownMD <- CapetownMD[1:4] %>% 
  bind_cols(setNames(CapetownMD[5:67], list_MD))
# RD
which(colnames(CapetownRD) == "ACR")
ncol(CapetownRD)
CapetownRD <- CapetownRD[1:4] %>% 
  bind_cols(setNames(CapetownRD[5:67], list_RD))
# AD
which(colnames(CapetownAD) == "ACR")
ncol(CapetownAD)
CapetownAD <- CapetownAD[1:4] %>% 
  bind_cols(setNames(CapetownAD[5:67], list_AD))

#### 3. Capetown - Arrange  ####
CapetowncombinedROItable <- CapetowncombinedROItable %>% 
  arrange(subjectID)
CapetownMD <- CapetownMD %>% 
  arrange(subjectID)
CapetownRD <- CapetownRD %>% 
  arrange(subjectID)
CapetownAD <- CapetownAD %>% 
  arrange(subjectID)

#### 3. Capetown -  Merge ####
Capetown_merged <- CapetowncombinedROItable %>% 
  bind_cols(CapetownMD %>%  select(-c(subjectID, Age, Sex, Diagnosis))) %>% 
  bind_cols(CapetownRD %>%   select(-c(subjectID, Age, Sex, Diagnosis))) %>% 
  bind_cols(CapetownAD  %>%   select(-c(subjectID, Age, Sex, Diagnosis)))



#### 4. Kyoto ####
# without Dx in combined ROI
# without AgeSQ in combined ROI

#### 4. Kyoto - Load dataset ####
# combined ROI
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/1.FA")

KyotocombinedROItable = read.csv(file='kyotocombinedROItable.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# MD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/2.MD")
KyotoMD = read.csv(file='Kyoto_combinedROItable_MD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# RD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/3.RD")
KyotoRD = read.csv(file='Kyoto_combinedROItable_RD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# AD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/4.AD")
KyotoAD = read.csv(file='Kyoto_combinedROItable_AD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

KyotocombinedROItable
KyotoMD
KyotoRD
KyotoAD

#### 4. Kyoto - set variable name ####
names(KyotocombinedROItable)[1] <- 'subjectID'
KyotocombinedROItable$AgeSQ <- NA # make AgeSQ variable 
KyotocombinedROItable <- rename(KyotocombinedROItable, Sex_Rel = Sex_ReI)

KyotocombinedROItable <- KyotocombinedROItable %>% 
  inner_join(KyotoMD %>% select(subjectID, Dx), by = 'subjectID') # Make Dx in combined ROI file

#### 4. Kyoto - set variable order - covariate####

# remove redundant variable: Sex.1
KyotocombinedROItable <- KyotocombinedROItable %>% 
  select(-Sex.1)

# reordering
KyotocombinedROItable<- KyotocombinedROItable %>%  relocate(c(subjectID,
                                                              Dx,
                                                              Age,
                                                              AgeSQ,
                                                              Sex,
                                                              Med,
                                                              AO,
                                                              Dur,
                                                              Sev,
                                                              Agr_Check,
                                                              Clean,
                                                              Sex_Rel,
                                                              Hoard,
                                                              Ord,
                                                              Anx,
                                                              CurAnx,
                                                              Dep,
                                                              CurDep))

#### 4. Kyoto - set variable order - DTI ####
# combined ROI
which(colnames(KyotocombinedROItable) == "ACR")
ncol(KyotocombinedROItable)
KyotocombinedROItable<- KyotocombinedROItable[1:18] %>% 
  bind_cols(setNames(KyotocombinedROItable[19:81], list_FA))

# MD
which(colnames(KyotoMD) == "ACR")
ncol(KyotoMD)
KyotoMD <- KyotoMD[1:4] %>% 
  bind_cols(setNames(KyotoMD[5:67], list_MD))
# RD
which(colnames(KyotoRD) == "ACR")
ncol(KyotoRD)
KyotoRD <- KyotoRD[1:4] %>% 
  bind_cols(setNames(KyotoRD[5:67], list_RD))
# AD
which(colnames(KyotoAD) == "ACR")
ncol(KyotoAD)
KyotoAD <- KyotoAD[1:4] %>% 
  bind_cols(setNames(KyotoAD[5:67], list_AD))



#### 4. Kyoto - Arrange ####
# arrange
KyotocombinedROItable <- KyotocombinedROItable %>% 
  arrange(subjectID)
KyotoMD <- KyotoMD %>% 
  arrange(subjectID)
KyotoRD <- KyotoRD %>% 
  arrange(subjectID)
KyotoAD <- KyotoAD %>% 
  arrange(subjectID)

#### 4. Kyoto - Merge ####
Kyoto_merged <- KyotocombinedROItable %>% 
  bind_cols(KyotoMD %>%  select(-c(subjectID, Age, Sex, Dx))) %>% 
  bind_cols(KyotoRD %>%   select(-c(subjectID, Age, Sex, Dx))) %>% 
  bind_cols(KyotoAD %>%   select(-c(subjectID, Age, Sex, Dx)))

Kyoto_merged







#### 5. Milan ####
#### 5. Milan - Data load ####

# combined ROI
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/1.FA")

MilancombinedROItable = read.csv(file='milan_combinedROItable.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# MD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/2.MD")
MilanMD = read.csv(file='Milan_combinedROItable_MD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# RD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/3.RD")
MilanRD = read.csv(file='Milan_combinedROItable_RD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# AD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/4.AD")
MilanAD = read.csv(file='Milan_combinedROItable_AD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

MilancombinedROItable
MilanMD
MilanRD
MilanAD

#### 5. Milan - set variable name - covariate ####
ncol(MilancombinedROItable)
MilancombinedROItable <- setNames(MilancombinedROItable[1], 'subjectID') %>% 
  bind_cols(MilancombinedROItable[2:80])

MilancombinedROItable$AgeSQ <- NA

MilanAD <- rename(MilanAD, Dx = Diagnosis)

#### 5. Milan - reorder variable ####
MilancombinedROItable<- MilancombinedROItable %>%  relocate(c(subjectID,
                                                              Dx,
                                                              Age,
                                                              AgeSQ,
                                                              Sex,
                                                              Med,
                                                              AO,
                                                              Dur,
                                                              Sev,
                                                              Agr_Check,
                                                              Clean,
                                                              Sex_Rel,
                                                              Hoard,
                                                              Ord,
                                                              Anx,
                                                              CurAnx,
                                                              Dep,
                                                              CurDep))


#### 5. Milan - set variable name  - DTI ####
which(colnames(MilancombinedROItable) == "ACR")
MilancombinedROItable <- MilancombinedROItable[1:18] %>% 
  bind_cols(setNames(MilancombinedROItable[19:81], list_FA))

# MD
which(colnames(MilanMD) == "ACR")
ncol(MilanMD)
MilanMD <- MilanMD[1:4] %>% 
  bind_cols(setNames(MilanMD[5:67], list_MD))
# RD
which(colnames(MilanRD) == "ACR")
ncol(MilanRD)
MilanRD <- MilanRD[1:4] %>% 
  bind_cols(setNames(MilanRD[5:67], list_RD))
# AD
which(colnames(MilanAD) == "ACR")
ncol(MilanAD)
MilanAD <- MilanAD[1:4] %>% 
  bind_cols(setNames(MilanAD[5:67], list_AD))



#### 5. Milan - Arrange ####
# arrange
MilancombinedROItable <- MilancombinedROItable %>% 
  arrange(subjectID)
MilanMD <- MilanMD %>% 
  arrange(subjectID)
MilanRD <- MilanRD %>% 
  arrange(subjectID)
MilanAD <- MilanAD %>% 
  arrange(subjectID)

#### 5. Milan - Merge ####
Milan_merged <- MilancombinedROItable %>% 
  bind_cols(MilanMD %>%  select(-c(subjectID, Age, Sex, Dx))) %>% 
  bind_cols(MilanRD %>%   select(-c(subjectID, Age, Sex, Dx))) %>% 
  bind_cols(MilanAD %>%   select(-c(subjectID, Age, Sex, Dx)))

Milan_merged


#### 6. Mountsinai ####
#### 6. Mountsinai - Load dataset####

# combined ROI
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/1.FA")

MountsinaicombinedROItable = read.csv(file='mountsinai_combinedROItable.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# MD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/2.MD")
MountsinaiMD = read.csv(file='MountSinai_combinedROItable_MD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# RD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/3.RD")
MountsinaiRD = read.csv(file='MountSinai_combinedROItable_RD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# AD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/4.AD")
MountsinaiAD = read.csv(file='MountSinai_combinedROItable_AD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

MountsinaicombinedROItable
MountsinaiMD
MountsinaiRD
MountsinaiAD

#### 6. Mountsinai - set variable name (covariate) ####
names(MountsinaicombinedROItable)[1] <- 'subjectID'

MountsinaiAD <- rename(MountsinaiAD, Dx = Diagnosis)

#### 6. Mountsinai - set variable name(DTI) ####
which(colnames(MountsinaicombinedROItable) == "ACR")
ncol(MountsinaicombinedROItable)
MountsinaicombinedROItable <- MountsinaicombinedROItable[1:18] %>% 
  bind_cols(setNames(MountsinaicombinedROItable[19:81], list_FA))

# MD
which(colnames(MountsinaiMD) == "ACR")
ncol(MountsinaiMD)
MountsinaiMD <- MountsinaiMD[1:4] %>% 
  bind_cols(setNames(MountsinaiMD[5:67], list_MD))
# RD
which(colnames(MountsinaiRD) == "ACR")
ncol(MountsinaiRD)
MountsinaiRD <- MountsinaiRD[1:4] %>% 
  bind_cols(setNames(MountsinaiRD[5:67], list_RD))
# AD
which(colnames(MountsinaiAD) == "ACR")
ncol(MountsinaiAD)
MountsinaiAD <- MountsinaiAD[1:4] %>% 
  bind_cols(setNames(MountsinaiAD[5:67], list_AD))


#### 6. Mountsinai - Arrange ####
MountsinaicombinedROItable <- MountsinaicombinedROItable %>% 
  arrange(subjectID)
MountsinaiMD <- MountsinaiMD %>% 
  arrange(subjectID)
MountsinaiRD <- MountsinaiRD %>% 
  arrange(subjectID)
MountsinaiAD <- MountsinaiAD %>% 
  arrange(subjectID)

#### 6. Mountsinai - merge ####

Mountsinai_merged <- MountsinaicombinedROItable %>% 
  bind_cols(MountsinaiMD %>%  select(-c(subjectID, Age, Sex, Dx))) %>% 
  bind_cols(MountsinaiRD %>%   select(-c(subjectID, Age, Sex, Dx))) %>% 
  bind_cols(MountsinaiAD %>%   select(-c(subjectID, Age, Sex, Dx)))



#### 7. Munich ####
#### 7. Munich - Data load####

# combined ROI
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/1.FA")

MunichcombinedROItable = read.csv(file='munich_combinedROItable.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# MD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/2.MD")
MunichMD = read.csv(file='Munich_combinedROItable_MD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# RD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/3.RD")
MunichRD = read.csv(file='Munich_combinedROItable_RD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# AD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/4.AD")
MunichAD = read.csv(file='Munich_combinedROItable_AD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

MunichcombinedROItable
MunichMD
MunichRD
MunichAD

#### 7. Munich - Set variable name  - covariate ####
names(MunichcombinedROItable)[1] <- 'subjectID'
MunichcombinedROItable$AgeSQ <- NA # make AgeSQ
MunichcombinedROItable <- rename(MunichcombinedROItable, Dx = dx)

MunichcombinedROItable <- rename(MunichcombinedROItable, Agr_Check = Agr_check)
MunichAD <- rename(MunichAD, Dx = Diagnosis)

#### 7. Munich - Reorder variable - covariate ####

MunichcombinedROItable<- MunichcombinedROItable %>%  relocate(c(subjectID,
                                                                Dx,
                                                                Age,
                                                                AgeSQ,
                                                                Sex,
                                                                Med,
                                                                AO,
                                                                Dur,
                                                                Sev,
                                                                Agr_Check,
                                                                Clean,
                                                                Sex_Rel,
                                                                Hoard,
                                                                Ord,
                                                                Anx,
                                                                CurAnx,
                                                                Dep,
                                                                CurDep))


#### 7. Munich - set variable name  - DTI ####
which(colnames(MunichcombinedROItable) == "ACR")
ncol(MunichcombinedROItable)
MunichcombinedROItable <- MunichcombinedROItable[1:18] %>% 
  bind_cols(setNames(MunichcombinedROItable[19:81], list_FA))

# MD
which(colnames(MunichMD) == "ACR")
ncol(MunichMD)
MunichMD <- MunichMD[1:3] %>% 
  bind_cols(setNames(MunichMD[4:66], list_MD))
# RD
which(colnames(MunichRD) == "ACR")
ncol(MunichRD)
MunichRD <- MunichRD[1:3] %>% 
  bind_cols(setNames(MunichRD[4:66], list_RD))
# AD
which(colnames(MunichAD) == "ACR")
ncol(MunichAD)
MunichAD <- MunichAD[1:4] %>% 
  bind_cols(setNames(MunichAD[5:67], list_AD))


#### 7. Munich - Arrange####
# arrange
MunichcombinedROItable <- MunichcombinedROItable %>% 
  arrange(subjectID)
MunichMD <- MunichMD %>% 
  arrange(subjectID)
MunichRD <- MunichRD %>% 
  arrange(subjectID)
MunichAD <- MunichAD %>% 
  arrange(subjectID)

#### 7. Munich - Merge ####
Munich_merged <- MunichcombinedROItable %>% 
  bind_cols(MunichMD %>%  select(-c(subjectID, Age, Sex))) %>% 
  bind_cols(MunichRD %>%   select(-c(subjectID, Age, Sex))) %>% 
  bind_cols(MunichAD %>%   select(-c(subjectID, Age, Sex, Dx)))


Munich_merged

#### 8. Rome ####
#### 8. Rome- Load Data####


# combined ROI
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/1.FA")

RomecombinedROItable = read.csv(file='rome_combinedROItable.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# MD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/2.MD")
RomeMD = read.csv(file='Rome_combinedROItable_MD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# RD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/3.RD")
RomeRD = read.csv(file='Rome_combinedROItable_RD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# AD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/4.AD")
RomeAD = read.csv(file='Rome_combinedROItable_AD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

RomecombinedROItable
RomeMD
RomeRD
RomeAD

#### 8. Rome- Set variable name (covariate) ####
RomecombinedROItable <- setNames(RomecombinedROItable[1], 'subjectID') %>% 
  bind_cols(RomecombinedROItable[2:81])

#### 8. Rome- Set variable name (DTI) ####
# combined ROI
which(colnames(RomecombinedROItable) == "ACR")
ncol(RomecombinedROItable)
RomecombinedROItable <- RomecombinedROItable[1:18] %>% 
  bind_cols(setNames(RomecombinedROItable[19:81], list_FA))
# MD
which(colnames(RomeMD) == "ACR")
ncol(RomeMD)
RomeMD <- RomeMD[1:4] %>% 
  bind_cols(setNames(RomeMD[5:67], list_MD))
# RD
which(colnames(RomeRD) == "ACR")
ncol(RomeRD)
RomeRD <- RomeRD[1:4] %>% 
  bind_cols(setNames(RomeRD[5:67], list_RD))
# AD
which(colnames(RomeAD) == "ACR")
ncol(RomeAD)
RomeAD <- RomeAD[1:4] %>% 
  bind_cols(setNames(RomeAD[5:67], list_AD))

#### 8. Rome - Arrange####
RomecombinedROItable <- RomecombinedROItable %>% 
  arrange(subjectID)
RomeMD <- RomeMD %>% 
  arrange(subjectID)
RomeRD <- RomeRD %>% 
  arrange(subjectID)
RomeAD <- RomeAD %>% 
  arrange(subjectID)

#### 8. Rome- Merge####
Rome_merged <- RomecombinedROItable %>% 
  bind_cols(RomeMD %>% select(-c(subjectID, Age, Sex, Diagnosis))) %>% 
  bind_cols(RomeRD %>% select(-c(subjectID, Age, Sex, Diagnosis))) %>% 
  bind_cols(RomeAD %>% select(-c(subjectID, Age, Sex, Diagnosis)))

Rome_merged


#### 9. Saopaulo ####
#### 9. Saopaulo - Load dataset  ####

# combined ROI
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/1.FA")

SaopaulocombinedROItable = read.csv(file='saopaulo_combinedROItable_Cha.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# MD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/2.MD")
SaopauloMD = read.csv(file='saopaolo_combinedroitable_md.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# RD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/3.RD")
SaopauloRD = read.csv(file='saopaolo_combinedroitable_rd.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# AD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/4.AD")
SaopauloAD = read.csv(file='saopaolo_combinedroitable_ad.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

SaopaulocombinedROItable
SaopauloMD
SaopauloRD
SaopauloAD

#### 9. Saopaulo - set variable name (covariate) ####
names(SaopaulocombinedROItable)[1] <- 'subjectID'

SaopaulocombinedROItable$AgeSQ <- NA

#### 9. Saopaulo - Reorder ####
SaopaulocombinedROItable<- SaopaulocombinedROItable %>%  relocate(c(subjectID,	
                                                                    Dx,	
                                                                    Age,	
                                                                    AgeSQ,	
                                                                    Sex,	
                                                                    Med,	
                                                                    AO,	
                                                                    Dur,	
                                                                    Sev,	
                                                                    Agr_Check,	
                                                                    Clean,	
                                                                    Sex_Rel,	
                                                                    Hoard,	
                                                                    Ord,	
                                                                    Anx,	
                                                                    CurAnx,	
                                                                    Dep,	
                                                                    CurDep))	


#### 9. Saopaulo -Set variable name (DTI) ####
which(colnames(SaopaulocombinedROItable) == "ACR")
ncol(SaopaulocombinedROItable)
SaopaulocombinedROItable <- SaopaulocombinedROItable[1:18] %>% 
  bind_cols(setNames(SaopaulocombinedROItable[19:81], list_FA))

# MD
which(colnames(SaopauloMD) == "ACR")
ncol(SaopauloMD)
SaopauloMD <- SaopauloMD[1:4] %>% 
  bind_cols(setNames(SaopauloMD[5:67], list_MD))
# RD
which(colnames(SaopauloRD) == "ACR")
ncol(SaopauloRD)
SaopauloRD <- SaopauloRD[1:4] %>% 
  bind_cols(setNames(SaopauloRD[5:67], list_RD))
# AD
which(colnames(SaopauloAD) == "ACR")
ncol(SaopauloAD)
SaopauloAD <- SaopauloAD[1:4] %>% 
  bind_cols(setNames(SaopauloAD[5:67], list_AD))

#### 9. Saopaulo - Arrange ####
# arrange
SaopaulocombinedROItable <- SaopaulocombinedROItable %>% 
  arrange(subjectID)
SaopauloMD <- SaopauloMD %>% 
  arrange(subjectID)
SaopauloRD <- SaopauloRD %>% 
  arrange(subjectID)
SaopauloAD <- SaopauloAD %>% 
  arrange(subjectID)

#### 9. Saopaulo - merge ####
# Data merge
Saopaulo_merged <- SaopaulocombinedROItable %>% 
  bind_cols(SaopauloMD %>% select(-c(subjectID, Age, Sex, Diagnosis))) %>% 
  bind_cols(SaopauloRD %>% select(-c(subjectID, Age, Sex, Diagnosis))) %>% 
  bind_cols(SaopauloAD %>% select(-c(subjectID, Age, Sex, Diagnosis)))

Saopaulo_merged







#### 10. Shanghai #### 
#### 10. Shanghai - Load data#### 

# combined ROI
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/1.FA")

ShanghaicombinedROItable = read.csv(file='shangai_combinedROItable_updated.v.21.11.13.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# MD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/2.MD")
ShanghaiMD = read.csv(file='Shangai_combinedROItable_MD_updated.v.21.11.13.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# RD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/3.RD")
ShanghaiRD = read.csv(file='Shangai_combinedROItable_RD_updated.v.21.11.13.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# AD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/4.AD")
ShanghaiAD = read.csv(file='Shangai_combinedROItable_AD_updated.v.21.11.13.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

ShanghaicombinedROItable
ShanghaiMD
ShanghaiRD
ShanghaiAD


#### 10. Shanghai - Set variable name (Covariate) #### 

#### 10. Shanghai - Set variable name (DTI) #### 
#### 5. Shanghai - set variable name  - DTI ####
which(colnames(ShanghaicombinedROItable) == "ACR")
ncol(ShanghaicombinedROItable)
ShanghaicombinedROItable <- ShanghaicombinedROItable[1:18] %>% 
  bind_cols(setNames(ShanghaicombinedROItable[19:81], list_FA))

# MD
which(colnames(ShanghaiMD) == "ACR")
ncol(ShanghaiMD)
ShanghaiMD <- ShanghaiMD[1:3] %>% 
  bind_cols(setNames(ShanghaiMD[4:66], list_MD))
# RD
which(colnames(ShanghaiRD) == "ACR")
ncol(ShanghaiRD)
ShanghaiRD <- ShanghaiRD[1:3] %>% 
  bind_cols(setNames(ShanghaiRD[4:66], list_RD))
# AD
which(colnames(ShanghaiAD) == "ACR")
ncol(ShanghaiAD)
ShanghaiAD <- ShanghaiAD[1:3] %>% 
  bind_cols(setNames(ShanghaiAD[4:66], list_AD))



#### 10. Shanghai - Arrange#### 
ShanghaicombinedROItable <- ShanghaicombinedROItable %>% 
  arrange(subjectID)
ShanghaiMD <- ShanghaiMD %>% 
  arrange(subjectID)
ShanghaiRD <- ShanghaiRD %>% 
  arrange(subjectID)
ShanghaiAD <- ShanghaiAD %>% 
  arrange(subjectID)



#### 10. Shanghai - Merge#### 
Shanghai_merged <- ShanghaicombinedROItable %>% 
  bind_cols(ShanghaiMD %>% select(-c(subjectID, Age, Sex))) %>% 
  bind_cols(ShanghaiRD %>% select(-c(subjectID, Age, Sex))) %>% 
  bind_cols(ShanghaiAD %>% select(-c(subjectID, Age, Sex)))

Shanghai_merged

#### 11. Seoul#### 
#### 11. Seoul - Load data#### 
# combined ROI
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/1.FA")

SeoulcombinedROItable = read.csv(file='seoul_updated_combinedROItable.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# MD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/2.MD")

SeoulMD = read.csv(file='Seoul_updated_combinedROItable_MD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# RD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/3.RD")
SeoulRD = read.csv(file='Seoul_updated_combinedROItable_RD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

# AD
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/0_Raw_data/Data_Dti/4.AD")
SeoulAD = read.csv(file='Seoul_updated_combinedROItable_AD.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

SeoulcombinedROItable
SeoulMD
SeoulRD
SeoulAD


#### 11. Seoul - set variable name (covariate)####
names(SeoulcombinedROItable)[1] <- 'subjectID'
#### 11. Seoul - Reorder #### 

#### 11. Seoul - Set variable name (DTI) #### 
#### 5. Seoul - set variable name  - DTI ####
which(colnames(SeoulcombinedROItable) == "ACR.fa")
ncol(SeoulcombinedROItable)
SeoulcombinedROItable <- SeoulcombinedROItable[1:18] %>% 
  bind_cols(setNames(SeoulcombinedROItable[19:81], list_FA))

# MD
which(colnames(SeoulMD) == "ACR.md")
ncol(SeoulMD)
SeoulMD <- SeoulMD[1] %>% 
  bind_cols(setNames(SeoulMD[2:64], list_MD))

# RD
which(colnames(SeoulRD) == "ACR.RD")
ncol(SeoulRD)
SeoulRD <- SeoulRD[1] %>% 
  bind_cols(setNames(SeoulRD[2:64], list_RD))
# AD
which(colnames(SeoulAD) == "ACR.AD")
ncol(SeoulAD)
SeoulAD <- SeoulAD[1] %>% 
  bind_cols(setNames(SeoulAD[2:64], list_AD))

#### 11. Seoul - Arrange #### 
SeoulcombinedROItable <- SeoulcombinedROItable %>% 
  arrange(subjectID)
SeoulMD <- SeoulMD %>% 
  arrange(subjectID)
SeoulRD <- SeoulRD %>% 
  arrange(subjectID)
SeoulAD <- SeoulAD %>% 
  arrange(subjectID)


#### 11. Seoul - Merge #### 
Seoul_merged <- SeoulcombinedROItable %>% 
  bind_cols(SeoulMD %>% select(-c(subjectID))) %>% 
  bind_cols(SeoulRD %>% select(-c(subjectID))) %>% 
  bind_cols(SeoulAD %>% select(-c(subjectID)))
Seoul_merged










#######################################################################

#### Merging across site ####

#######################################################################

# 1. Add Site variable
Amsterdam_merged$Site <- "Amsterdam"
Bangalore_merged$Site <- "Bangalore"
Capetown_merged$Site <- "Capetown"
Kyoto_merged$Site <- "Kyoto"
Milan_merged$Site <- "Milan"
Mountsinai_merged$Site <- "Mountsinai"
Munich_merged$Site <- "Munich"
Rome_merged$Site <- "Rome"
Saopaulo_merged$Site <- "Saopaulo"
Shanghai_merged$Site <- "Shanghai"
Seoul_merged$Site <- "Seoul"

table(Amsterdam_merged$Site)
table(Bangalore_merged$Site)
table(Capetown_merged$Site)
table(Kyoto_merged$Site)
table(Milan_merged$Site)
table(Mountsinai_merged$Site)
table(Munich_merged$Site)
table(Rome_merged$Site)
table(Saopaulo_merged$Site)
table(Shanghai_merged$Site)
table(Seoul_merged$Site)

#######################################################################

# save data 
#######################################################################

setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/1_Siteº° merged")
Amsterdam_merged <- Amsterdam_merged %>% relocate(subjectID, Site)
Bangalore_merged <- Bangalore_merged %>% relocate(subjectID, Site)
Capetown_merged <- Capetown_merged %>% relocate(subjectID, Site)
Kyoto_merged <- Kyoto_merged %>% relocate(subjectID, Site)
Milan_merged <- Milan_merged %>% relocate(subjectID, Site)
Mountsinai_merged <- Mountsinai_merged %>% relocate(subjectID, Site)
Munich_merged <- Munich_merged %>% relocate(subjectID, Site)
Rome_merged <- Rome_merged %>% relocate(subjectID, Site)
Saopaulo_merged <- Saopaulo_merged %>% relocate(subjectID, Site)
Shanghai_merged <- Shanghai_merged %>% relocate(subjectID, Site)
Seoul_merged <- Seoul_merged %>% relocate(subjectID, Site)

write.csv(Amsterdam_merged, 'Merged_Amsterdam_79_v.21.11.16.csv', row.names = FALSE)
write.csv(Bangalore_merged, 'Merged_Bangalore_290_v.21.11.16.csv', row.names = FALSE)
write.csv(Capetown_merged, 'Merged_Capetown_49_v.21.11.16.csv', row.names = FALSE)
write.csv(Kyoto_merged, 'Merged_Kyoto_76_v.21.11.16.csv', row.names = FALSE)
write.csv(Milan_merged, 'Merged_Milan_130_v.21.11.16.csv', row.names = FALSE)
write.csv(Mountsinai_merged, 'Merged_Mountsinai_34_v.21.11.16.csv', row.names = FALSE)
write.csv(Munich_merged, 'Merged_Munich_133_v.21.11.16.csv', row.names = FALSE)
write.csv(Rome_merged, 'Merged_Rome_188_v.21.11.16.csv', row.names = FALSE)
write.csv(Saopaulo_merged, 'Merged_Saopaulo_67_v.21.11.16.csv', row.names = FALSE)
#write.csv(Shanghai_merged, 'Merged_Shanghai_128_v.21.11.16.csv', row.names = FALSE)
write.csv(Shanghai_merged, 'Merged_Shanghai_128_v.21.11.21.csv', row.names = FALSE)
write.csv(Seoul_merged, 'Merged_Seoul_185_v.21.11.16.csv', row.names = FALSE)

