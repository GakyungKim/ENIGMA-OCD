library(tidyverse)
library(magrittr)
# library(naniar)
# library(skimr)
# library(rstatix)
# library(tigerstats)


# Data: train/validation set - z-scaled

setwd("C:/Users/김보겸/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Dai result/After piras updated_Adult/T.Dx_D.nonharmo scaled only adult_cv.LOSO_777_F.age.sex_21.11.23")

df_adult_val <- read_csv('train_preds_custom.csv') # 1,068 x 272


#### check target outcome - Dx ####
df_adult_val %>%  
  xtabs(~Dx, data = ., addNA = T) %>% addmargins() 

#### Make Dx_character variable ####
df_adult_val %<>% 
  mutate(Dx_charac = ifelse(Dx == 1, 'OCD', 'HC')) # 1,336 x 273

# set levels (to reorder)
df_adult_val$Dx_charac %<>% 
  factor(x = ., levels = c('OCD','HC'))

#### Make Med_12 variable ####
df_adult_val %<>% 
  mutate(Med_12 = ifelse(Med == 1, 1, 
                         ifelse(Med == 2, 2, NA)))
df_adult_val %>% 
  xtabs(~ Med_12, data = ., addNA = TRUE)

# Male
df_adult_val %<>% 
  mutate(Male = ifelse(Sex == 1, 'Male', 'Female')) # 1,336 x 274




#### Set datatype ####
df_adult_val$Sex <- as.factor(df_adult_val$Sex)
df_adult_val$Dx <- as.factor(df_adult_val$Dx) 

df_adult_val$Med <- as.factor(df_adult_val$Med) 
df_adult_val$Med_12 <- as.factor(df_adult_val$Med_12)

df_adult_val$Anx <- as.factor(df_adult_val$Anx) 
df_adult_val$Dep <- as.factor(df_adult_val$Dep) 
df_adult_val$CurAnx <- as.factor(df_adult_val$CurAnx) 
df_adult_val$CurDep <- as.factor(df_adult_val$CurDep) 

df_adult_val$Agr_Check <- as.factor(df_adult_val$Agr_Check) 
df_adult_val$Clean <- as.factor(df_adult_val$Clean) 
df_adult_val$Ord <- as.factor(df_adult_val$Ord) 
df_adult_val$Sex_Rel <- as.factor(df_adult_val$Sex_Rel) 
df_adult_val$Hoard <- as.factor(df_adult_val$Hoard) 


#### Make subset ####
df_adult_val_ocd <- df_adult_val %>% 
  filter(Dx_charac == 'OCD')




#### OCD group summary - NA correctly input? ####
# Lifetime diagnosis
# 1. Anxiety
df_adult_val_ocd %<>% 
  mutate(Anx  = ifelse(Anx ==1, 1, 
                       ifelse(Anx == 2, 2, 
                              ifelse(Anx == 0, NA, NA))), 
         Dep  = ifelse(Dep ==1, 1, 
                       ifelse(Dep == 2, 2, 
                              ifelse(Dep == 0, NA, NA))), 
         CurAnx  = ifelse(CurAnx ==1, 1, 
                          ifelse(CurAnx == 2, 2, 
                                 ifelse(CurAnx == 0, NA, NA))), 
         CurDep  = ifelse(CurDep ==1, 1, 
                          ifelse(CurDep == 2, 2, 
                                 ifelse(CurDep == 0, NA, NA))), 
         Agr_Check = ifelse(Agr_Check == 0, 0,
                            ifelse(Agr_Check == 1, 1, 
                                   ifelse(Agr_Check ==999, NA, NA))), 
         Clean = ifelse(Clean == 0, 0,
                            ifelse(Clean == 1, 1, 
                                   ifelse(Clean ==999, NA, NA))), 
         Ord = ifelse(Ord == 0, 0,
                            ifelse(Ord == 1, 1, 
                                   ifelse(Ord ==999, NA, NA))), 
         Sex_Rel = ifelse(Sex_Rel == 0, 0,
                            ifelse(Sex_Rel == 1, 1, 
                                   ifelse(Sex_Rel ==999, NA, NA))), 
         Hoard = ifelse(Hoard == 0, 0,
                            ifelse(Hoard == 1, 1, 
                                   ifelse(Hoard ==999, NA, NA))))

#### OCD group - reassign variable type ####
df_adult_val_ocd$Sex <- as.factor(df_adult_val_ocd$Sex)
df_adult_val_ocd$Dx <- as.factor(df_adult_val_ocd$Dx) 

df_adult_val_ocd$Med <- as.factor(df_adult_val_ocd$Med) 
df_adult_val_ocd$Med_12 <- as.factor(df_adult_val_ocd$Med_12)

df_adult_val_ocd$Anx <- as.factor(df_adult_val_ocd$Anx) 
df_adult_val_ocd$Dep <- as.factor(df_adult_val_ocd$Dep) 
df_adult_val_ocd$CurAnx <- as.factor(df_adult_val_ocd$CurAnx) 
df_adult_val_ocd$CurDep <- as.factor(df_adult_val_ocd$CurDep) 

df_adult_val_ocd$Agr_Check <- as.factor(df_adult_val_ocd$Agr_Check) 
df_adult_val_ocd$Clean <- as.factor(df_adult_val_ocd$Clean) 
df_adult_val_ocd$Ord <- as.factor(df_adult_val_ocd$Ord) 
df_adult_val_ocd$Sex_Rel <- as.factor(df_adult_val_ocd$Sex_Rel) 
df_adult_val_ocd$Hoard <- as.factor(df_adult_val_ocd$Hoard) 


str(df_adult_val[1:30])
str(df_adult_val_ocd[1:30])

####################################################################################
####################################################################################

#### site effects on predicted probability ####

####################################################################################
####################################################################################
####################################################################################


##########################################################################

# DV: Dx.1
# covariate : 
# Demo: Age, Sex, Site 
# Dx (if using df_adult_total)
# Clinical variable
# Average DTI 

# Result- 

###########################################################################

#### Demographic ####
# 1. Age
glm.Age_Cov.Demo.Dx <- df_adult_val %>% lm(Dx.1 ~ Age + Sex + Dx + Site, data = .)
summary(glm.Age_Cov.Demo.Dx)
anova(glm.Age_Cov.Demo.Dx)
# car::vif(glm.Age_Cov.Demo.Dx)

#### Clinical variable in OCD sample ####

# OCD illness severity score
glm.Sev_Cov.Demo.DTI <- df_adult_val_ocd %>% 
  lm(Dx.1 ~ Sev + Age + Sex  + AverageFA + AverageMD + AverageRD + AverageAD + Site, data = .) # with cov
summary(glm.Sev_Cov.Demo.DTI)
anova(glm.Sev_Cov.Demo.DTI)
car::vif(glm.Sev_Cov.Demo.DTI)

# Age at onset
glm.AO_Cov.Demo.DTI <- df_adult_val_ocd %>% 
  lm(Dx.1 ~ AO + Age + Sex +  AverageFA + AverageAD + Site, data = .) # with cov
summary(glm.AO_Cov.Demo.DTI)
anova(glm.AO_Cov.Demo.DTI)


# Duration of illness
glm.Dur_Cov.Demo.DTI <- df_adult_val_ocd %>% 
  lm(Dx.1 ~ Dur + Age + Sex  +  AverageFA  + AverageAD + Site, data = .) # with cov
summary(glm.Dur_Cov.Demo.DTI)
anova(glm.Dur_Cov.Demo.DTI)


# Medication 
glm.Med_12_Cov.Demo.DTI <- lm(data=df_adult_val_ocd, Dx.1 ~ Med_12 + Age + Sex  + AverageFA + AverageAD + Site)
summary(glm.Med_12_Cov.Demo.DTI)
anova(glm.Med_12_Cov.Demo.DTI)




# comorbidity
glm.Anx_Cov.Demo.DTI <- lm(data=df_adult_val_ocd, Dx.1 ~ Anx + Age + Sex+ AverageFA+ AverageAD + Site)
summary(glm.Anx_Cov.Demo.DTI)
anova(glm.Anx_Cov.Demo.DTI)

glm.CurAnx_Cov.Demo.DTI <- lm(data=df_adult_val_ocd, Dx.1 ~ CurAnx + Age + Sex+ AverageFA + AverageAD + Site)
summary(glm.CurAnx_Cov.Demo.DTI)
anova(glm.CurAnx_Cov.Demo.DTI)

glm.Dep_Cov.Demo.DTI <- lm(data=df_adult_val_ocd, Dx.1 ~ Dep + Age + Sex  + AverageFA + AverageAD + Site)
summary(glm.Dep_Cov.Demo.DTI)
anova(glm.Dep_Cov.Demo.DTI)

glm.CurDep_Cov.Demo.DTI <- lm(data=df_adult_val_ocd, Dx.1 ~ CurDep + Age + Sex  + AverageFA +AverageAD + Site)
summary(glm.CurDep_Cov.Demo.DTI)
anova(glm.CurDep_Cov.Demo.DTI)



# Subsymptoms
glm.Agr_Cov.Demo.DTI <- lm(data=df_adult_val_ocd, Dx.1 ~ Agr_Check + Age + Sex + AverageFA + AverageAD + Site)
summary(glm.Agr_Cov.Demo.DTI)
anova(glm.Agr_Cov.Demo.DTI)

glm.Clean_Cov.Demo.DTI <- lm(data=df_adult_val_ocd, Dx.1 ~ Clean + Age + Sex+ AverageFA +  AverageAD + Site)
summary(glm.Clean_Cov.Demo.DTI)
anova(glm.Clean_Cov.Demo.DTI)

glm.Sex_Rel_Cov.Demo.DTI <- lm(data=df_adult_val_ocd, Dx.1 ~ Sex_Rel + Age + Sex  + AverageFA + AverageAD+ Site)
summary(glm.Sex_Rel_Cov.Demo.DTI)
anova(glm.Sex_Rel_Cov.Demo.DTI)

glm.Hoard_Cov.Demo.DTI <- lm(data=df_adult_val_ocd, Dx.1 ~ Hoard + Age + Sex + AverageFA  + AverageAD + Site)
summary(glm.Hoard_Cov.Demo.DTI)
anova(glm.Hoard_Cov.Demo.DTI)

glm.Ord_Cov.Demo.DTI <- lm(data=df_adult_val_ocd, Dx.1 ~ Ord + Age + Sex + AverageFA + AverageAD+ Site)
summary(glm.Ord_Cov.Demo.DTI)
anova(glm.Ord_Cov.Demo.DTI)


###########################################################################

#### Summary - association between clinical variable and Preds #####

###########################################################################


summary(glm.Age_Cov.Demo.Dx)
summary(glm.Sev_Cov.Demo.DTI)
summary(glm.AO_Cov.Demo.DTI)
summary(glm.Dur_Cov.Demo.DTI)
summary(glm.Med_12_Cov.Demo.DTI)
summary(glm.Med_12_Cov.Demo.DTI)
summary(glm.Anx_Cov.Demo.DTI)
summary(glm.CurAnx_Cov.Demo.DTI)
summary(glm.Dep_Cov.Demo.DTI)
summary(glm.CurDep_Cov.Demo.DTI)
summary(glm.Agr_Cov.Demo.DTI)
summary(glm.Clean_Cov.Demo.DTI)
summary(glm.Sex_Rel_Cov.Demo.DTI)
summary(glm.Hoard_Cov.Demo.DTI)
summary(glm.Ord_Cov.Demo.DTI)




###########################################################################

#### Summary - Site effects on preds####

###########################################################################

anova(glm.Age_Cov.Demo.Dx)
anova(glm.Sev_Cov.Demo.DTI)
anova(glm.AO_Cov.Demo.DTI)
anova(glm.Dur_Cov.Demo.DTI)
anova(glm.Med_12_Cov.Demo.DTI)
anova(glm.Med_12_Cov.Demo.DTI)
anova(glm.Anx_Cov.Demo.DTI)
anova(glm.CurAnx_Cov.Demo.DTI)
anova(glm.Dep_Cov.Demo.DTI)
anova(glm.CurDep_Cov.Demo.DTI)
anova(glm.Agr_Cov.Demo.DTI)
anova(glm.Clean_Cov.Demo.DTI)
anova(glm.Sex_Rel_Cov.Demo.DTI)
anova(glm.Hoard_Cov.Demo.DTI)
anova(glm.Ord_Cov.Demo.DTI)





####################################################################################
####################################################################################
####################################################################################

#### site effects on individualized classification performance ####

# Reference code: Hierarchical regression for site effect on {individual prediction result}_fixed effects_0605.R
# Hierarchical glm code is not needed/ since we found the alternative method to show site effect

###########################################################################
####################################################################################
####################################################################################


##########################################################################

# DV:
# covariate :
# Demo: Age, Sex, Site
# Dx (if using df_adult_total)
# Clinical variable
# Average DTI

# Result-

###########################################################################





##################################################################################################

# 1. Dependent variable
# 1.1. list of DV
# 1) True prediction - OCD + HC
# 2) OCD - True positive vs. False Negative
# 3) HC - True Negative vs. False Positive


# 1.2. Process
# 1) add site threshold column
# 1.1) add Dx_predicted column : prediction result - OCD? or HC?
# 2) based on the threshold, add elements of confusion matrix

# 3) create Predictor > all = site dependent >> use site only / current status - do not create. 3.2~3.5
# 3.1) site
# 3.2) Dx ratio: site_pc_ocd
# 3.3) N ratio by site: site_pc_samplesize
# 3.4) Training sample ratio by site: site_pc_trainsize,
# 3.5) Training sample's diagnosis by site: site_pc_train.ocd

# 4) create DV that we will use
# 4.1) True prediction [proba_val] - TN, TP = 1, FN, FP = 0 / False_prediction_total - (reverse)
# 4.2) True prediction_OCD [proba_val_ocd] - TP = 1, FN = 0 / False_prediction_OCD - FN = 1, TP = 0
# 4.3) True prediction_HC [proba_val_hc] - TN = 1, FP =0 / False_prediction_HC - FP = 1, TN = 0

##################################################################################################

# 1.2. Process

# 1) add site threshold column
df_adult_val %<>%
  mutate(threshold = ifelse(Site == 'Amsterdam', 0.5606682, 
                            ifelse(Site == 'Bangalore', 0.5467443, 
                                   ifelse(Site == 'Capetown', 0.523335, 
                                          ifelse(Site == 'Kyoto', 0.5231088, 
                                                 ifelse(Site == 'Milan', 0.5285721, 
                                                        ifelse(Site == 'Mountsinai', 0.5327545, 
                                                               ifelse(Site == 'Munich', 0.5243237, 
                                                                      ifelse(Site == 'Rome', 0.4470824,
                                                                             ifelse(Site == 'Saopaulo', 0.5288856, 
                                                                                    ifelse(Site == 'Seoul',  0.5531548, 
                                                                                           ifelse(Site == 'Shanghai', 0.4738248, NA))))))))))))


str(df_adult_val$threshold)
hist(df_adult_val$threshold)
sum(is.na(df_adult_val$threshold)) # 0
df_adult_val %>% 
  xtabs(~ Site, addNA = TRUE, data = .)


# 1.1) add Dx_predicted column :  prediction result - OCD? or HC?
print('This is a confusion matrix. Make the confusion matrix as variable')

df_adult_val %<>%
  mutate(Dx_predicted = ifelse(Dx.1 >= threshold, 1, 0)) 

# confirmation
df_adult_val %>% 
  xtabs(~ Dx_predicted, data = ., addNA = T)

df_adult_val %>% 
  xtabs(~ Dx + Dx_predicted, data = ., addNA = T)
print('이게 바로 confusion matrix네. 이걸 변수로 만들어주자. ')



##################################################################################################
# 2) based on the threshold, add elements of confusion matrix
df_adult_val %<>% 
  mutate(ConfusionMatrix = ifelse(Dx_predicted == 0 & Dx == 0, 'TN', 
                                  ifelse(Dx_predicted ==1 & Dx == 1, 'TP', 
                                         ifelse(Dx_predicted == 0 & Dx == 1, 'FN', 
                                                ifelse(Dx_predicted == 1 & Dx == 0, 'FP', NA)))))
# check
df_adult_val %>% 
  xtabs(~Site + ConfusionMatrix, addNA = T, data = .)
sum(is.na(df_adult_val$ConfusionMatrix))

##################################################################################################

# 3) Predictor
# 3.1) site 
str(df_adult_val$Site) # factor

##################################################################################################

# 4) DV
# 4.1) True prediction [proba_val] - TN, TP = 1, FN, FP = 0 / False_prediction_total - (reverse)
df_adult_val %<>% 
  mutate(True_prediction_total = ifelse(ConfusionMatrix == 'TN' | ConfusionMatrix == 'TP', 1, 0), 
         False_prediction_total = ifelse(ConfusionMatrix == 'FN' | ConfusionMatrix == 'FP', 1, 0))

df_adult_val %>% 
  xtabs(~ ConfusionMatrix + True_prediction_total, data = ., addNA = T) # 1에 TN, TP만 있음을 확인
df_adult_val %>% 
  xtabs(~ ConfusionMatrix + False_prediction_total, data = ., addNA = T)



# 4.2) True prediction_OCD [proba_val_ocd] - TP = 1, FN = 0 / False_prediction_OCD - FN = 1, TP = 0
df_adult_val_ocd <- df_adult_val %>% 
  filter(Dx == 1)

table(df_adult_val_ocd$ConfusionMatrix) # there's only FN or TP 

df_adult_val_ocd %<>%
  mutate(True_prediction_OCD = ifelse(ConfusionMatrix == 'TP', 1, 0))

df_adult_val_ocd %>%
  mutate(False_prediction_OCD = ifelse(ConfusionMatrix == 'FN', 1, 0))
  
df_adult_val_ocd %>% 
  xtabs(~ Site + True_prediction_OCD, data = .) # check


# 4.3) True prediction_HC [proba_val_hc] - TN = 1, FP =0 / False_prediction_HC - FP = 1, TN = 0 
df_adult_val_hc <- df_adult_val %>% 
  filter(Dx == 0)
table(df_adult_val_hc$ConfusionMatrix) # there's only FP or TN 

df_adult_val_hc %<>%
  mutate(True_prediction_HC = ifelse(ConfusionMatrix == 'TN', 1, 0))

df_adult_val_hc %<>%
  mutate(False_prediction_HC = ifelse(ConfusionMatrix == 'FP', 1, 0))

  # check
table(df_adult_val_hc$True_prediction_HC)
table(df_adult_val_hc$False_prediction_HC)
addmargins(xtabs(formula = ~ Site + True_prediction_HC, data = df_adult_val_hc)) # 


#### OCD group summary - NA input correctly? ####
# Lifetime diagnosis
# 1. Anxiety
df_adult_val_ocd %<>% 
  mutate(Anx  = ifelse(Anx ==1, 1, 
                       ifelse(Anx == 2, 2, 
                              ifelse(Anx == 0, NA, NA))), 
         Dep  = ifelse(Dep ==1, 1, 
                       ifelse(Dep == 2, 2, 
                              ifelse(Dep == 0, NA, NA))), 
         CurAnx  = ifelse(CurAnx ==1, 1, 
                          ifelse(CurAnx == 2, 2, 
                                 ifelse(CurAnx == 0, NA, NA))), 
         CurDep  = ifelse(CurDep ==1, 1, 
                          ifelse(CurDep == 2, 2, 
                                 ifelse(CurDep == 0, NA, NA))), 
         Agr_Check = ifelse(Agr_Check == 0, 0,
                            ifelse(Agr_Check == 1, 1, 
                                   ifelse(Agr_Check ==999, NA, NA))), 
         Clean = ifelse(Clean == 0, 0,
                        ifelse(Clean == 1, 1, 
                               ifelse(Clean ==999, NA, NA))), 
         Ord = ifelse(Ord == 0, 0,
                      ifelse(Ord == 1, 1, 
                             ifelse(Ord ==999, NA, NA))), 
         Sex_Rel = ifelse(Sex_Rel == 0, 0,
                          ifelse(Sex_Rel == 1, 1, 
                                 ifelse(Sex_Rel ==999, NA, NA))), 
         Hoard = ifelse(Hoard == 0, 0,
                        ifelse(Hoard == 1, 1, 
                               ifelse(Hoard ==999, NA, NA))))

#### OCD group - reassign variable type ####
df_adult_val_ocd$Sex <- as.factor(df_adult_val_ocd$Sex)
df_adult_val_ocd$Dx <- as.factor(df_adult_val_ocd$Dx) 

df_adult_val_ocd$Med <- as.factor(df_adult_val_ocd$Med) 
df_adult_val_ocd$Med_12 <- as.factor(df_adult_val_ocd$Med_12)

df_adult_val_ocd$Anx <- as.factor(df_adult_val_ocd$Anx) 
df_adult_val_ocd$Dep <- as.factor(df_adult_val_ocd$Dep) 
df_adult_val_ocd$CurAnx <- as.factor(df_adult_val_ocd$CurAnx) 
df_adult_val_ocd$CurDep <- as.factor(df_adult_val_ocd$CurDep) 

df_adult_val_ocd$Agr_Check <- as.factor(df_adult_val_ocd$Agr_Check) 
df_adult_val_ocd$Clean <- as.factor(df_adult_val_ocd$Clean) 
df_adult_val_ocd$Ord <- as.factor(df_adult_val_ocd$Ord) 
df_adult_val_ocd$Sex_Rel <- as.factor(df_adult_val_ocd$Sex_Rel) 
df_adult_val_ocd$Hoard <- as.factor(df_adult_val_ocd$Hoard) 





str(df_adult_val_ocd[1:30])


##################################################################################################

# Modeling ###
# cov:Age + Sex + AverageFA + AverageAD + Sev + Dur+ Med_12 
# high correlation among average FA, MD, RD AD > FA, AD only correlation
# site related factors: site only
# why? all 5 vars - site dependent


# glm analysis by group (logistic regression)
# 1. Among OCD: 
# cov: Age + Sex + AverageFA + AverageAD + Sev + Dur+ Med_12
# dependent variableL : True_prediction_OCD
# 2. Among HC
# Issue: different results between 'summary' and 'anova' > see the results from summary & use anova for the site effect
# in anova, input order is importatnt

##################################################################################################
### summary
# 1. run a code same as clinical outcome
  # see the result firstly (using the summary, same as clinical association with preds)
# 2. use hierarchical model for site effect only
  # summarize code as above // => see the results

##########################################


#### clinical variable -> performance effect??
# dataset : df_adult_val_ocd\

# DV: True_prediction_OCD 
  # in OCD group,correctly OCD prediction?
# IV: clinical variable 
# Covariates: Demo (Age, Sex, Site), Average DTI 2 (FA, AD: MD, RD vif > 200 => exclude)

# link fuction: binomial


#### Clinical variable in OCD sample ####

# Sev
glm.Sev_Cov.Demo.DTI <- df_adult_val_ocd %>%
  glm(True_prediction_OCD ~ Sev + Age + Sex+ AverageFA + AverageAD + Site , family = binomial(link="logit"), data = .) # with cov
summary(glm.Sev_Cov.Demo.DTI)
anova(glm.Sev_Cov.Demo.DTI, test = 'Chisq')  # logisti -> chisq test selection
car::vif(glm.Sev_Cov.Demo.DTI)
# Summary for manuscript
anova(glm.Sev_Cov.Demo.DTI, test = 'Chisq')  # logisti -> chisq test selection


# Age at onset
glm.AO_Cov.Demo.DTI <- df_adult_val_ocd %>%
  glm(True_prediction_OCD ~ AO + Age + Sex +  AverageFA + AverageAD+ Site , data = ., family = binomial(link="logit")) # with cov
summary(glm.AO_Cov.Demo.DTI)
anova(glm.AO_Cov.Demo.DTI, test = 'Chisq')


# Duration of illness
glm.Dur_Cov.Demo.DTI <- df_adult_val_ocd %>%
  glm(True_prediction_OCD ~ Dur + Age + Sex +  AverageFA + AverageAD+ Site , data = ., family = binomial(link="logit")) # with cov
summary(glm.Dur_Cov.Demo.DTI)
anova(glm.Dur_Cov.Demo.DTI, test = 'Chisq')

# Medication
glm.Med_12_Cov.Demo.DTI <- df_adult_val_ocd %>%
  glm(True_prediction_OCD ~ Med_12 + Age + Sex  +  AverageFA + AverageAD+ Site, data = ., family = binomial(link="logit")) # with cov
summary(glm.Med_12_Cov.Demo.DTI)

anova(glm.Med_12_Cov.Demo.DTI, test = 'Chisq')



# comorbidity
glm.Anx_Cov.Demo.DTI <- df_adult_val_ocd %>%
  glm(True_prediction_OCD ~ Anx + Age + Sex +  AverageFA + AverageAD+ Site , data = ., family = binomial(link="logit")) # with cov
summary(glm.Anx_Cov.Demo.DTI)
anova(glm.Anx_Cov.Demo.DTI, test = 'Chisq')

glm.CurAnx_Cov.Demo.DTI <-  df_adult_val_ocd %>%
  glm(True_prediction_OCD ~ CurAnx + Age + Sex +  AverageFA + AverageAD+ Site , data = ., family = binomial(link="logit")) 
summary(glm.CurAnx_Cov.Demo.DTI)
anova(glm.CurAnx_Cov.Demo.DTI, test = 'Chisq')

glm.Dep_Cov.Demo.DTI <-  df_adult_val_ocd %>%
  glm(True_prediction_OCD ~ Dep + Age + Sex +  AverageFA + AverageAD + Site, data = ., family = binomial(link="logit")) 
summary(glm.Dep_Cov.Demo.DTI)
anova(glm.Dep_Cov.Demo.DTI, test = 'Chisq')

glm.CurDep_Cov.Demo.DTI <-  df_adult_val_ocd %>%
  glm(True_prediction_OCD ~ CurDep + Age + Sex +  AverageFA + AverageAD + Site , data = ., family = binomial(link="logit")) 
summary(glm.CurDep_Cov.Demo.DTI)
anova(glm.CurDep_Cov.Demo.DTI, test = 'Chisq')




# Subsymptoms
glm.Agr_Cov.Demo.DTI <- df_adult_val_ocd %>%
  glm(True_prediction_OCD ~ Agr_Check + Age + Sex + AverageFA + AverageAD + Site, data = ., family = binomial(link="logit")) 
summary(glm.Agr_Cov.Demo.DTI)
anova(glm.Agr_Cov.Demo.DTI, test = 'Chisq')

glm.Clean_Cov.Demo.DTI <-df_adult_val_ocd %>%
  glm(True_prediction_OCD ~ Clean + Age + Sex +  AverageFA + AverageAD + Site, data = ., family = binomial(link="logit")) 
summary(glm.Clean_Cov.Demo.DTI)
anova(glm.Clean_Cov.Demo.DTI, test = 'Chisq')

glm.Sex_Rel_Cov.Demo.DTI <- df_adult_val_ocd %>%
  glm(True_prediction_OCD ~ Sex_Rel + Age + Sex  +  AverageFA + AverageAD + Site, data = ., family = binomial(link="logit")) 
summary(glm.Sex_Rel_Cov.Demo.DTI)
anova(glm.Sex_Rel_Cov.Demo.DTI, test = 'Chisq')

glm.Hoard_Cov.Demo.DTI <- df_adult_val_ocd %>%
  glm(True_prediction_OCD ~ Hoard + Age + Sex +  AverageFA + AverageAD + Site, data = ., family = binomial(link="logit")) 
summary(glm.Hoard_Cov.Demo.DTI)
anova(glm.Hoard_Cov.Demo.DTI, test = 'Chisq')

glm.Ord_Cov.Demo.DTI <- df_adult_val_ocd %>%
  glm(True_prediction_OCD ~ Ord + Age + Sex  +  AverageFA + AverageAD + Site, data = ., family = binomial(link="logit")) 
summary(glm.Ord_Cov.Demo.DTI)
anova(glm.Ord_Cov.Demo.DTI, test = 'Chisq')




##### association between individual performance and clinical variable ####
summary(glm.Sev_Cov.Demo.DTI)
summary(glm.AO_Cov.Demo.DTI)
summary(glm.Dur_Cov.Demo.DTI)
summary(glm.Med_12_Cov.Demo.DTI)

summary(glm.Anx_Cov.Demo.DTI)
summary(glm.CurAnx_Cov.Demo.DTI)
summary(glm.Dep_Cov.Demo.DTI)
summary(glm.CurDep_Cov.Demo.DTI)

summary(glm.Agr_Cov.Demo.DTI)
summary(glm.Clean_Cov.Demo.DTI)
summary(glm.Sex_Rel_Cov.Demo.DTI)
summary(glm.Hoard_Cov.Demo.DTI)
summary(glm.Ord_Cov.Demo.DTI)




##### site effect on individual performance ####
anova(glm.Sev_Cov.Demo.DTI, test = 'Chisq')  # logisti -> chisq test selection
anova(glm.AO_Cov.Demo.DTI, test = 'Chisq')
anova(glm.Dur_Cov.Demo.DTI, test = 'Chisq')
anova(glm.Med_12_Cov.Demo.DTI, test = 'Chisq')

anova(glm.Anx_Cov.Demo.DTI, test = 'Chisq')
anova(glm.CurAnx_Cov.Demo.DTI, test = 'Chisq')
anova(glm.Dep_Cov.Demo.DTI, test = 'Chisq')
anova(glm.CurDep_Cov.Demo.DTI, test = 'Chisq')

anova(glm.Agr_Cov.Demo.DTI, test = 'Chisq')
anova(glm.Clean_Cov.Demo.DTI, test = 'Chisq')
anova(glm.Sex_Rel_Cov.Demo.DTI, test = 'Chisq')
anova(glm.Hoard_Cov.Demo.DTI, test = 'Chisq')
anova(glm.Ord_Cov.Demo.DTI, test = 'Chisq')


