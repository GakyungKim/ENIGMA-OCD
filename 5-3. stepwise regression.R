rm(list = ls())

library(tidyverse)
library(magrittr)
library(knitr)


##################################################################
## ENIGMA-OCD adult GLM ##
##################################################################

##########################################################################
########## 1. load data ########## 
##########################################################################
###### load prediction probability data
setwd("C:/Users/김보겸/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Dai result/After piras updated_Adult/T.Dx_D.nonharmo scaled only adult_cv.LOSO_777_F.age.sex_21.11.23")
train_pred <- read.csv("train_preds_custom.csv")
test_pred <- read.csv("test_preds_custom.csv")

##########################################################################
########## 2. data preprocessing ########## 
##########################################################################
##### preprocessing for the analysis - factorization
train_varName_fact <- c(which(names(train_pred) == 'Dx'), which(names(train_pred) == 'Site'), which(names(train_pred) == 'Sex'), 
                        which(names(train_pred) == 'Med'), which(names(train_pred) == 'Agr_Check'), which(names(train_pred) == 'Clean'), 
                        which(names(train_pred) == 'Sex_Rel'),which(names(train_pred) == 'Hoard'), which(names(train_pred) == 'Ord'), 
                        which(names(train_pred) == 'Anx'), which(names(train_pred) == 'CurAnx'), which(names(train_pred) == 'Dep'), 
                        which(names(train_pred) == 'CurDep'), which(names(train_pred) == 'site_int'))
train_pred[,train_varName_fact] = lapply(train_pred[,train_varName_fact], factor)
test_varName_fact <- c(which(names(test_pred) == 'Dx'), which(names(test_pred) == 'Site'), which(names(test_pred) == 'Sex'),
                       which(names(test_pred) == 'Med'), which(names(test_pred) == 'Agr_Check'), which(names(test_pred) == 'Clean'),
                       which(names(test_pred) == 'Sex_Rel'), which(names(test_pred) == 'Hoard'), which(names(test_pred) == 'Ord'), 
                       which(names(test_pred) == 'Anx'), which(names(test_pred) == 'CurAnx'), which(names(test_pred) == 'Dep'), 
                       which(names(test_pred) == 'CurDep'), which(names(test_pred) == 'site_int'))
test_pred[,test_varName_fact] = lapply(test_pred[,test_varName_fact], factor)
##### missing values / re-coding
library(dplyr)
# 999
train_pred[train_pred == 999] <- NA
test_pred[test_pred == 999] <- NA
##### filtering by Dx group
train_pred_HC <- train_pred %>% filter(Dx == 0)
train_pred_OCD <- train_pred %>% filter(Dx == 1)
test_pred_HC <- test_pred %>% filter(Dx == 0)
test_pred_OCD <- test_pred %>% filter(Dx == 1)
##### missing value processing based on ENIGMA-OCD clinical criteria
# [HC] train_pred_HC, test_pred_HC
# MED=0 / AO - / Dur - / Sev - / Agr_Check - / Clean - / Sex_Rel - / Hoard - / Ord - / Anx 0 / CurAnx 0 / Dep 0 / CurDep 0
# -
# [OCD] train_pred_OCD, test_pred_OCD
# Agr_Check 0 or 1 / Clean 0 or 1 / Sex_Rel 0 or 1 / Hoard 0 or 1 / Ord 0 or 1

train_pred_OCD$Med[train_pred_OCD$Med == 0] <- NA
train_pred_OCD$Anx[train_pred_OCD$Anx == 0] <- NA
train_pred_OCD$Dep[train_pred_OCD$Dep == 0] <- NA
train_pred_OCD$CurAnx[train_pred_OCD$CurAnx == 0] <- NA
train_pred_OCD$CurDep[train_pred_OCD$CurDep == 0] <- NA
train_pred_OCD$Agr_Check[train_pred_OCD$Agr_Check == 999] <- NA
train_pred_OCD$Agr_Check[train_pred_OCD$Agr_Check == 2] <- NA
train_pred_OCD$Clean[train_pred_OCD$Clean == 999] <- NA
train_pred_OCD$Ord[train_pred_OCD$Ord == 999] <- NA
train_pred_OCD$Sex_Rel[train_pred_OCD$Sex_Rel == 999] <- NA
train_pred_OCD$Hoard[train_pred_OCD$Hoard == 999] <- NA
test_pred_OCD$Med[test_pred_OCD$Med == 0] <- NA
test_pred_OCD$Anx[test_pred_OCD$Anx == 0] <- NA
test_pred_OCD$Dep[test_pred_OCD$Dep == 0] <- NA
test_pred_OCD$CurAnx[test_pred_OCD$CurAnx == 0] <- NA
test_pred_OCD$CurDep[test_pred_OCD$CurDep == 0] <- NA
test_pred_OCD$Agr_Check[test_pred_OCD$Agr_Check == 999] <- NA
test_pred_OCD$Clean[test_pred_OCD$Clean == 999] <- NA
test_pred_OCD$Ord[test_pred_OCD$Ord == 999] <- NA
test_pred_OCD$Sex_Rel[test_pred_OCD$Sex_Rel == 999] <- NA
test_pred_OCD$Hoard[test_pred_OCD$Hoard == 999] <- NA

##### Drop levels
train_pred_OCD <- droplevels(train_pred_OCD)
test_pred_OCD <- droplevels(test_pred_OCD)


##### Check 
train_pred_OCD %>% xtabs(~Med, data = .)
train_pred_OCD %>% xtabs(~Anx, data = .)
train_pred_OCD %>% xtabs(~Dep, data = .)
train_pred_OCD %>% xtabs(~CurAnx, data = .)
train_pred_OCD %>% xtabs(~CurDep, data = .)
train_pred_OCD %>% xtabs(~Agr_Check, data = .)
train_pred_OCD %>% xtabs(~Clean, data = .)
train_pred_OCD %>% xtabs(~Ord, data = .)
train_pred_OCD %>% xtabs(~Sex_Rel, data = .)
train_pred_OCD %>% xtabs(~Hoard, data = .)

test_pred_OCD %>% xtabs(~Med, data = .)
test_pred_OCD %>% xtabs(~Anx, data = .)
test_pred_OCD %>% xtabs(~Dep, data = .)
test_pred_OCD %>% xtabs(~CurAnx, data = .)
test_pred_OCD %>% xtabs(~CurDep, data = .)
test_pred_OCD %>% xtabs(~Agr_Check, data = .)
test_pred_OCD %>% xtabs(~Clean, data = .)
test_pred_OCD %>% xtabs(~Ord, data = .)
test_pred_OCD %>% xtabs(~Sex_Rel, data = .)
test_pred_OCD %>% xtabs(~Hoard, data = .)


##########################################################################
########## 4-2. [stepwise regression] association between clinical variables and Preds ########## 
##########################################################################
#(ref) https://mindscale.kr/course/basic-stat-r/stepwise/
#(ref) http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
#################### 4.2.1. train set #################### 
#names(train_pred_OCD)[8:20]
#[1] "Med"       "AO"        "Dur"       "Sev"       "Agr_Check" "Clean"     
#[7]"Sex_Rel"   "Hoard"     "Ord"       "Anx"       "CurAnx"   "Dep"       "CurDep"   
library(MASS)
train_pred_OCD_step <- train_pred_OCD[,colnames(train_pred_OCD)[c(3, 5, 7, 8:20, 274)]] #  552 x 17
train_pred_OCD_step_na <- na.omit(train_pred_OCD_step) # 379 x 17
model.train = lm(Dx.1 ~ 1, data = train_pred_OCD_step_na)
step.forward_train = stepAIC(model.train, 
                       direction = "forward", 
                       scope = Dx.1 ~ Med + AO + Dur + Sev + Agr_Check + Clean + Sex_Rel 
                       + Hoard + Ord + Anx + CurAnx + Dep + CurDep + Age + Sex + Site)

step.forward_train

############# final model in validation sample 
model.final_train <- lm(Dx.1 ~ Site + Age + Hoard + AO + CurDep , data = train_pred_OCD_step_na)
summary(model.final_train)


aov.final_train <- train_pred_OCD_step_na %>% rstatix::anova_test(Dx.1 ~ Site + Age + Hoard + AO + CurDep)
rstatix::get_anova_table(aov.final_train)

#################### 4.2.2. test set ####################
##### preprocessing
test_pred_OCD_step <-  test_pred_OCD[,colnames(test_pred_OCD)[c(3, 5, 7, 8:20, 274)]] # 138 x 17
test_pred_OCD_step_na <- na.omit(test_pred_OCD_step) # 104 x 17


################### 1) stepwise regression model identified in train set
model.steptrain_test <- lm(Dx.1 ~ Site + Age + Hoard + AO + CurDep, data = test_pred_OCD_step_na)
summary(model.steptrain_test)

aov.steptrain_test <- test_pred_OCD_step_na %>% rstatix::anova_test(Dx.1~ Site + Age+ Hoard + AO + CurDep)
rstatix::get_anova_table(aov.steptrain_test)



################### 2) stepwise regression in test set
model.test = lm(Dx.1 ~ 1, data = test_pred_OCD_step_na)
step.forward_test = stepAIC(model.test, 
                            direction = "forward", 
                            scope = Dx.1 ~ Med + AO + Dur + Sev + Agr_Check + Clean + Sex_Rel 
                            + Hoard + Ord + Anx + CurAnx + Dep + CurDep + Age + Sex + Site)

step.forward_test

### final model
model.final_test <- lm(Dx.1 ~ Site + Med + Age + Anx + Clean , data = test_pred_OCD_step_na)
summary(model.final_test)

aov.final_test <- test_pred_OCD_step_na %>% rstatix::anova_test(Dx.1 ~ Site + Med + Age + Anx + Clean)
rstatix::get_anova_table(aov.final_test)










##################################################################
## ENIGMA-OCD pediatric GLM ##
##################################################################

##########################################################################
########## 1. load data ########## 
##########################################################################
###### load prediction probability data
setwd("C:/Users/김보겸/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Dai result/After piras updated_Pediatric/Pedi_dx_scaled")

pedi_train_pred <- read.csv("h2oai_experiment_T.Dx_Pediatric_21.12.18_train_predictions.csv")
pedi_test_pred <- read.csv("h2oai_experiment_T.Dx_Pediatric_21.12.18_test_predictions.csv")

##########################################################################
########## 2. data preprocessing ########## 
##########################################################################
##### preprocessing for the analysis - factorization
pedi_train_varName_fact <- c(which(names(pedi_train_pred) == 'Dx'), which(names(pedi_train_pred) == 'Site'), which(names(pedi_train_pred) == 'Sex'), 
                        which(names(pedi_train_pred) == 'Med'), which(names(pedi_train_pred) == 'Agr_Check'), which(names(pedi_train_pred) == 'Clean'), 
                        which(names(pedi_train_pred) == 'Sex_Rel'),which(names(pedi_train_pred) == 'Hoard'), which(names(pedi_train_pred) == 'Ord'), 
                        which(names(pedi_train_pred) == 'Anx'), which(names(pedi_train_pred) == 'CurAnx'), which(names(pedi_train_pred) == 'Dep'), 
                        which(names(pedi_train_pred) == 'CurDep'), which(names(pedi_train_pred) == 'site_int'))
pedi_train_pred[,pedi_train_varName_fact] = lapply(pedi_train_pred[,pedi_train_varName_fact], factor)

pedi_test_varName_fact <- c(which(names(pedi_test_pred) == 'Dx'), which(names(pedi_test_pred) == 'Site'), which(names(pedi_test_pred) == 'Sex'),
                       which(names(pedi_test_pred) == 'Med'), which(names(pedi_test_pred) == 'Agr_Check'), which(names(pedi_test_pred) == 'Clean'),
                       which(names(pedi_test_pred) == 'Sex_Rel'), which(names(pedi_test_pred) == 'Hoard'), which(names(pedi_test_pred) == 'Ord'), 
                       which(names(pedi_test_pred) == 'Anx'), which(names(pedi_test_pred) == 'CurAnx'), which(names(pedi_test_pred) == 'Dep'), 
                       which(names(pedi_test_pred) == 'CurDep'), which(names(pedi_test_pred) == 'site_int'))
pedi_test_pred[,pedi_test_varName_fact] = lapply(pedi_test_pred[,pedi_test_varName_fact], factor)

##### missing values / re-coding
library(dplyr)
# 999
pedi_train_pred[pedi_train_pred == 999] <- NA
pedi_test_pred[pedi_test_pred == 999] <- NA
##### filtering by Dx group
pedi_train_pred_HC <- pedi_train_pred %>% filter(Dx == 0)
pedi_train_pred_OCD <- pedi_train_pred %>% filter(Dx == 1)
pedi_test_pred_HC <- pedi_test_pred %>% filter(Dx == 0)
pedi_test_pred_OCD <- pedi_test_pred %>% filter(Dx == 1)
##### missing value processing based on ENIGMA-OCD clinical criteria
# [HC] pedi_train_pred_HC, pedi_test_pred_HC
# MED=0 / AO - / Dur - / Sev - / Agr_Check - / Clean - / Sex_Rel - / Hoard - / Ord - / Anx 0 / CurAnx 0 / Dep 0 / CurDep 0
# -
# [OCD] pedi_train_pred_OCD, pedi_test_pred_OCD
pedi_train_pred_OCD$Med[pedi_train_pred_OCD$Med == 0] <- NA
pedi_train_pred_OCD$Anx[pedi_train_pred_OCD$Anx == 0] <- NA
pedi_train_pred_OCD$Dep[pedi_train_pred_OCD$Dep == 0] <- NA
pedi_train_pred_OCD$CurAnx[pedi_train_pred_OCD$CurAnx == 0] <- NA
pedi_train_pred_OCD$CurDep[pedi_train_pred_OCD$CurDep == 0] <- NA
pedi_train_pred_OCD$Agr_Check[pedi_train_pred_OCD$Agr_Check == 999] <- NA
pedi_train_pred_OCD$Agr_Check[pedi_train_pred_OCD$Agr_Check == 2] <- NA
pedi_train_pred_OCD$Clean[pedi_train_pred_OCD$Clean == 999] <- NA
pedi_train_pred_OCD$Clean[pedi_train_pred_OCD$Clean == 2] <- NA
pedi_train_pred_OCD$Ord[pedi_train_pred_OCD$Ord == 999] <- NA
pedi_train_pred_OCD$Ord[pedi_train_pred_OCD$Ord == 2] <- NA
pedi_train_pred_OCD$Sex_Rel[pedi_train_pred_OCD$Sex_Rel == 999] <- NA
pedi_train_pred_OCD$Sex_Rel[pedi_train_pred_OCD$Sex_Rel == 2] <- NA
pedi_train_pred_OCD$Hoard[pedi_train_pred_OCD$Hoard == 999] <- NA
pedi_train_pred_OCD$Hoard[pedi_train_pred_OCD$Hoard == 2] <- NA

pedi_test_pred_OCD$Med[pedi_test_pred_OCD$Med == 0] <- NA
pedi_test_pred_OCD$Anx[pedi_test_pred_OCD$Anx == 0] <- NA
pedi_test_pred_OCD$Dep[pedi_test_pred_OCD$Dep == 0] <- NA
pedi_test_pred_OCD$CurAnx[pedi_test_pred_OCD$CurAnx == 0] <- NA
pedi_test_pred_OCD$CurDep[pedi_test_pred_OCD$CurDep == 0] <- NA
pedi_test_pred_OCD$Agr_Check[pedi_test_pred_OCD$Agr_Check == 999] <- NA
pedi_test_pred_OCD$Agr_Check[pedi_test_pred_OCD$Agr_Check == 2] <- NA
pedi_test_pred_OCD$Clean[pedi_test_pred_OCD$Clean == 999] <- NA
pedi_test_pred_OCD$Clean[pedi_test_pred_OCD$Clean == 2] <- NA
pedi_test_pred_OCD$Ord[pedi_test_pred_OCD$Ord == 999] <- NA
pedi_test_pred_OCD$Ord[pedi_test_pred_OCD$Ord == 2] <- NA
pedi_test_pred_OCD$Sex_Rel[pedi_test_pred_OCD$Sex_Rel == 999] <- NA
pedi_test_pred_OCD$Sex_Rel[pedi_test_pred_OCD$Sex_Rel == 2] <- NA
pedi_test_pred_OCD$Hoard[pedi_test_pred_OCD$Hoard == 999] <- NA
pedi_test_pred_OCD$Hoard[pedi_test_pred_OCD$Hoard == 2] <- NA

##### Drop levels
pedi_train_pred_OCD <- droplevels(pedi_train_pred_OCD)
pedi_test_pred_OCD <- droplevels(pedi_test_pred_OCD)

##### check
pedi_train_pred_OCD %>% xtabs(~Med, data = .)
pedi_train_pred_OCD %>% xtabs(~Anx, data = .)
pedi_train_pred_OCD %>% xtabs(~Dep, data = .)
pedi_train_pred_OCD %>% xtabs(~CurAnx, data = .)
pedi_train_pred_OCD %>% xtabs(~CurDep, data = .)
pedi_train_pred_OCD %>% xtabs(~Agr_Check, data = .)
pedi_train_pred_OCD %>% xtabs(~Clean, data = .)
pedi_train_pred_OCD %>% xtabs(~Ord, data = .)
pedi_train_pred_OCD %>% xtabs(~Sex_Rel, data = .)
pedi_train_pred_OCD %>% xtabs(~Hoard, data = .)

pedi_test_pred_OCD %>% xtabs(~Med, data = .)
pedi_test_pred_OCD %>% xtabs(~Anx, data = .)
pedi_test_pred_OCD %>% xtabs(~Dep, data = .)
pedi_test_pred_OCD %>% xtabs(~CurAnx, data = .)
pedi_test_pred_OCD %>% xtabs(~CurDep, data = .)
pedi_test_pred_OCD %>% xtabs(~Agr_Check, data = .)
pedi_test_pred_OCD %>% xtabs(~Clean, data = .)
pedi_test_pred_OCD %>% xtabs(~Ord, data = .)
pedi_test_pred_OCD %>% xtabs(~Sex_Rel, data = .)
pedi_test_pred_OCD %>% xtabs(~Hoard, data = .)



##########################################################################
########## 4-2. [stepwise regression] association between clinical variables and Preds ########## 
##########################################################################
#(ref) https://mindscale.kr/course/basic-stat-r/stepwise/
#(ref) http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
#################### 4.2.1. train set #################### 
#names(train_pred_OCD)[8:20]
#[1] "Med"       "AO"        "Dur"       "Sev"       "Agr_Check" "Clean"     
#[7]"Sex_Rel"   "Hoard"     "Ord"       "Anx"       "CurAnx"   "Dep"       "CurDep"   
library(MASS)
pedi_train_pred_OCD_step <- pedi_train_pred_OCD[,colnames(pedi_train_pred_OCD)[c(3, 5, 7, 8:20, 274)]] #  140 x 17
pedi_train_pred_OCD_step_na <- na.omit(pedi_train_pred_OCD_step) # 55 x 17

pedi_model.train = lm(Dx.1 ~ 1, data = pedi_train_pred_OCD_step_na)
pedi_step.forward_train = stepAIC(pedi_model.train, 
                             direction = "forward", 
                             scope = Dx.1 ~ Med + AO + Dur + Sev + Agr_Check + Clean + Sex_Rel 
                             + Hoard + Ord + Anx + CurAnx + Dep + CurDep + Age + Sex + Site)

pedi_step.forward_train

############# final model in validation sample 
pedi_model.final_train <- lm(Dx.1 ~  Site + Dep + Agr_Check + Age , data = pedi_train_pred_OCD_step_na)
summary(pedi_model.final_train)


pedi_aov.final_train <- pedi_train_pred_OCD_step_na %>% rstatix::anova_test(Dx.1 ~Site + Dep + Agr_Check + Age)
rstatix::get_anova_table(pedi_aov.final_train)

#################### 4.2.2. test set ####################
##### preprocessing
pedi_test_pred_OCD_step <-  pedi_test_pred_OCD[,colnames(pedi_test_pred_OCD)[c(3, 5, 7, 8:20, 274)]] # 35 x 17
pedi_test_pred_OCD_step_na <- na.omit(pedi_test_pred_OCD_step) # 23 x 17

################### 1) stepwise regression model identified in train set
# cannot be applied due to data differences (contrasts는 오로지 2 또는 그 이상의 level들을 가진 요인들에만 적용할 수 있습니다)

pedi_model.steptrain_test <- lm(Dx.1 ~  Site + Dep + Agr_Check + Age, data = pedi_test_pred_OCD_step_na)
summary(pedi_model.steptrain_test)

pedi_aov.steptrain_test <- pedi_test_pred_OCD_step_na %>% rstatix::anova_test(Dx.1~  Site + Dep + Agr_Check + Age)
rstatix::get_anova_table(pedi_aov.steptrain_test)



# ################### 2) stepwise regression in test set
pedi_model.test = lm(Dx.1 ~ 1, data = pedi_test_pred_OCD_step_na)
pedi_step.forward_test = stepAIC(pedi_model.test,
                            direction = "forward",
                            scope = Dx.1 ~ Med + AO + Dur + Sev + Agr_Check + Clean + Sex_Rel
                            + Hoard + Ord + Anx + CurAnx + Dep + Age + Sex + Site)

pedi_step.forward_test

### final model
pedi_model.final_test <- lm(Dx.1 ~Med + Agr_Check + Dep + Sex_Rel, data = pedi_test_pred_OCD_step_na)
summary(pedi_model.final_test)

pedi_aov.final_test <- pedi_test_pred_OCD_step_na %>% rstatix::anova_test(Dx.1 ~Med + Agr_Check + Dep + Sex_Rel)

rstatix::get_anova_table(pedi_aov.final_test)














