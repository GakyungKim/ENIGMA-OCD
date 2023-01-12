##################################################################
## ENIGMA-OCD adult GLM ##
##################################################################

##########################################################################
########## 1. load data ########## 
##########################################################################

###### load prediction probability data
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/connectome21/2021-1/project/ENIGMA-OCD/final/4.h2oDAI/1.adult/2.NeuroCombat/T.Dx_D.harmo scaled only adult_cv.LOSO_777_F.age.sex_21.11.28")
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
train_pred_OCD$Med[train_pred_OCD$Med == 0] <- NA
train_pred_OCD$Anx[train_pred_OCD$Anx == 0] <- NA
train_pred_OCD$Dep[train_pred_OCD$Dep == 0] <- NA
train_pred_OCD$CurAnx[train_pred_OCD$CurAnx == 0] <- NA
train_pred_OCD$CurDep[train_pred_OCD$CurDep == 0] <- NA
train_pred_OCD$Agr_Check[train_pred_OCD$Agr_Check == 999] <- NA
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

train_pred_OCD <- droplevels(train_pred_OCD)
test_pred_OCD <- droplevels(test_pred_OCD)

###### normality test before the analysis
# normality test (O -> t-test ok)
# in train set
shapiro.test(train_pred$Dx.1) # W = 0.95709, p-value < 2.2e-16 (*high p-value means normality OK)
qqnorm(train_pred$Dx.1)
qqline(train_pred$Dx.1)

shapiro.test(train_pred_HC$Dx.1) # W = 0.956, p-value = 2.782e-11
qqnorm(train_pred_HC$Dx.1)
qqline(train_pred_HC$Dx.1)

shapiro.test(train_pred_OCD$Dx.1) # W = 0.95388, p-value = 4.091e-12
qqnorm(train_pred_OCD$Dx.1)
qqline(train_pred_OCD$Dx.1)

# in test set
shapiro.test(test_pred$Dx.1) # W = 0.99082, p-value = 0.0913
qqnorm(test_pred$Dx.1)
qqline(train_pred$Dx.1)

shapiro.test(test_pred_HC$Dx.1) # W = 0.98372, p-value = 0.1224
qqnorm(test_pred_HC$Dx.1)
qqline(test_pred_HC$Dx.1)

shapiro.test(test_pred_OCD$Dx.1) # W = 0.99095, p-value = 0.5175
qqnorm(test_pred_OCD$Dx.1)
qqline(test_pred_OCD$Dx.1)

##########################################################################
########## 3. sample characteristics ########## 
##########################################################################

##### description analysis of "Dx.1" (prediction probability) by "dx (diagnosis)" group
train_pred %>% 
  group_by(Dx) %>%
  summarise(mean_value = mean(Dx.1, na.rm = T), sd_value = sd(Dx.1, na.rm = T))

test_pred %>% 
  group_by(Dx) %>%
  summarise(mean_value = mean(Dx.1, na.rm = T), sd_value = sd(Dx.1, na.rm = T))

##########################################################################
########## 4. association between clinical variables and Preds ########## 
##########################################################################
# Dx.1 ~ Med + Sex + Age + Site + AverageFA + AverageMD + AverageRD + AverageAD
### demo (age + sex + ...)
agesex_1 <- glm(Dx.1 ~ Age + Sex + Dx + Site, data = train_pred)
summary(agesex_1)
aov <- aov(agesex_1)
summary(aov)

### clinical association
## set (1) predictors & (2) covariates

#################### train set #################### 
predictors_i_1 <- colnames(train_pred_OCD)[8:20] # (1) predictors # ****
#[predictors_i_1] Med, AO, Dur, Sev, Agr_Check, Clean, Sex_Rel, Hoard, Ord, Anx, CurAnx, Dep, CurDep
covars = c("Sex", "Age", "Site", "AverageFA", "AverageMD", "AverageRD", "AverageAD") # (2) covariates

## make formula
forms = paste('Dx.1 ~', predictors_i_1) # ****
for(i in 1:length(covars)){
  forms = paste(forms, '+', covars[i])
}

## empty vector for coeff, p-value
temp.names <- vector()
name_lm <- vector()

temp.names <- vector()
name_aov <- vector()

#glm
temp.coeff <- vector()
temp.p <- vector()

#aov
temp.aov <- vector()
temp.DF <- vector()
temp.MeanSq <- vector()
temp.F <- vector()
temp.Pr <- vector()

## loop for glm w/ multiple variables
for(i in 1:length(forms)){
  form = as.formula(forms[i])
  # glm
  temp.lm <- lm(formula = form, data = train_pred_OCD) # **** train_pred_OCD ***
  name_lm[i] <- predictors_i_1[i]
  temp.coeff[i] <- round(temp.lm[[1]][2], 3)
  temp.p[i] <- round(coef(summary(temp.lm))[2,4], 3)
  fdr <- round(p.adjust(temp.p, method = 'fdr'), 3)
  glm_coeff_train <- data.frame(y = name_lm, beta = temp.coeff, p = temp.p, fdr)
  
  # aov
  temp.aov <- anova(temp.lm) #ancova
  name_aov[i] <- predictors_i_1[i]
  #temp.DF[i] <- round(temp.aov[1,1],5)
  #temp.MeanSq[i] <- round(temp.aov[[3]][1],5)
  temp.F[i] <- round(temp.aov[1,4],3)
  temp.Pr[i] <- round(temp.aov[1,5],3)
  fdr_aov <- round(p.adjust(temp.Pr, method = 'fdr'), 3)
  aov_coeff_train <- data.frame(y = name_aov, f.value = temp.F, Pr = temp.Pr, fdr_aov)
}

View(glm_coeff_train)
View(aov_coeff_train)


#################### test set #################### 
predictors_i_2 <- colnames(test_pred_OCD)[c(8:20)] # (1) predictors # ****
#[predictors_i_2] Med, AO, Dur, Sev, Agr_Check, Clean, Sex_Rel, Hoard, Ord, Anx, CurAnx, Dep
#CurDep level - only 1
covars = c("Sex", "Age", "Site", "AverageFA", "AverageMD", "AverageRD", "AverageAD") # (2) covariates
print(covars)

## make formula
forms = paste('Dx.1 ~', predictors_i_2) # ****
for(i in 1:length(covars)){
  forms = paste(forms, '+', covars[i])
}

## empty vector for coeff, p-value
temp.names <- vector()
name_lm <- vector()

temp.names <- vector()
name_aov <- vector()

#glm
temp.coeff <- vector()
temp.p <- vector()

#aov
temp.aov <- vector()
temp.DF <- vector()
temp.MeanSq <- vector()
temp.F <- vector()
temp.Pr <- vector()

## loop for glm w/ multiple variables
for(i in 1:length(forms)){
  form = as.formula(forms[i])
  # glm
  temp.lm <- lm(formula = form, data = test_pred_OCD) # **** train_pred_OCD ***
  name_lm[i] <- predictors_i_2[i]
  temp.coeff[i] <- round(temp.lm[[1]][2], 3)
  temp.p[i] <- round(coef(summary(temp.lm))[2,4], 3)
  fdr <- p.adjust(temp.p, method = 'fdr')
  glm_coeff_test <- data.frame(y = name_lm, beta = temp.coeff, p = temp.p, fdr)
  
  # aov
  temp.aov <- anova(temp.lm) #ancova
  name_aov[i] <- predictors_i_1[i]
  #temp.DF[i] <- round(temp.aov[1,1],5)
  #temp.MeanSq[i] <- round(temp.aov[[3]][1],5)
  temp.F[i] <- round(temp.aov[1,4],3)
  temp.Pr[i] <- round(temp.aov[1,5],3)
  fdr_aov <- round(p.adjust(temp.Pr, method = 'fdr'), 3)
  aov_coeff_test <- data.frame(y = name_aov, f.value = temp.F, Pr = temp.Pr, fdr_aov)
}

View(glm_coeff_test)
View(aov_coeff_test)


##########################################################################
########## 5. averageFA/MD/AD/RD across site (site variability) ########## 
##########################################################################
#################### train set #################### 
siteFA <- glm(AverageFA ~ Site + Sex + Age, data = train_pred_HC) #train_pred_HC / train_pred_OCD
siteMD <- glm(AverageMD ~ Site + Sex + Age, data = train_pred_HC) #train_pred_HC / train_pred_OCD
siteAD <- glm(AverageAD ~ Site + Sex + Age, data = train_pred_HC) #train_pred_HC / train_pred_OCD
siteRD <- glm(AverageRD ~ Site + Sex + Age, data = train_pred_HC) #train_pred_HC / train_pred_OCD

aov_siteFA <- aov(siteFA)
aov_siteMD <- aov(siteMD)
aov_siteAD <- aov(siteAD)
aov_siteRD <- aov(siteRD)

summary(aov_siteFA)
summary(aov_siteMD)
summary(aov_siteAD)
summary(aov_siteRD)

#################### test set #################### 
siteFA <- glm(AverageFA ~ Site + Sex + Age, data = test_pred_HC) #test_pred_HC / test_pred_OCD
siteMD <- glm(AverageMD ~ Site + Sex + Age, data = test_pred_HC) #test_pred_HC / test_pred_OCD
siteAD <- glm(AverageAD ~ Site + Sex + Age, data = test_pred_HC) #test_pred_HC / test_pred_OCD
siteRD <- glm(AverageRD ~ Site + Sex + Age, data = test_pred_HC) #test_pred_HC / test_pred_OCD

aov_siteFA <- aov(siteFA)
aov_siteMD <- aov(siteMD)
aov_siteAD <- aov(siteAD)
aov_siteRD <- aov(siteRD)

summary(aov_siteFA)
summary(aov_siteMD)
summary(aov_siteAD)
summary(aov_siteRD)

##########################################################################
########## 6-1. [train] individual performance across site (site variability) ########## 
##########################################################################
#################### mutate threshold #################### 
library(dplyr)
train_pred <- train_pred %>%
  mutate(threshold = ifelse(Site == 'Amsterdam', 0.5982037,
                            ifelse(Site == 'Bangalore', 0.479763,
                                   ifelse(Site == 'Capetown', 0.5127607,
                                          ifelse(Site == 'Kyoto', 0.4016493,
                                                 ifelse(Site == 'Milan', 0.4998153,
                                                        ifelse(Site == 'Mountsinal', 0.2993422,
                                                               ifelse(Site == 'Munich', 0.4954218,
                                                                      ifelse(Site == 'Rome', 0.5612651, 
                                                                             ifelse(Site == 'Saopaulo', 0.5583068,
                                                                                    ifelse(Site == 'Seoul', 0.5228986,
                                                                                           ifelse(Site == 'Shanghai', 0.4732666, NA))))))))))))

#################### check data #################### 
str(train_pred$threshold)
hist(train_pred$threshold)
sum(is.na(train_pred$threshold)) # 30
train_pred %>% 
  xtabs(~ Site, addNA = TRUE, data = .)

#################### create confusion matrix variable #################### 
train_pred <- train_pred %>%
  mutate(Dx_predicted = ifelse(Dx.1 >= threshold, 1, 0)) 

train_pred <- train_pred %>% 
  mutate(ConfusionMatrix = ifelse(Dx_predicted == 0 & Dx == 0, 'TN', 
                                  ifelse(Dx_predicted ==1 & Dx == 1, 'TP', 
                                         ifelse(Dx_predicted == 0 & Dx == 1, 'FN', 
                                                ifelse(Dx_predicted == 1 & Dx == 0, 'FP', NA)))))

#################### check TRUE / FALSE #################### 
# True prediction [proba_val] - TN, TP = 1, FN, FP = 0 / False_prediction_total - (reverse)
train_pred <- train_pred %>% 
  mutate(True_prediction_total = ifelse(ConfusionMatrix == 'TN' | ConfusionMatrix == 'TP', 1, 0), 
         False_prediction_total = ifelse(ConfusionMatrix == 'FN' | ConfusionMatrix == 'FP', 1, 0))

train_pred %>% 
  xtabs(~ ConfusionMatrix + True_prediction_total, data = ., addNA = T) 
train_pred %>% 
  xtabs(~ ConfusionMatrix + False_prediction_total, data = ., addNA = T)

#################### OCD
# True prediction_OCD [proba_val_ocd] - TP = 1, FN = 0 / False_prediction_OCD - FN = 1, TP = 0
train_pred_OCD_x <- train_pred %>% filter(Dx == 1)
table(train_pred_OCD_x$ConfusionMatrix) # there's only FN or TP 

train_pred_OCD_x <- train_pred_OCD_x %>%
  mutate(True_prediction_OCD = ifelse(ConfusionMatrix == 'TP', 1, 0))

train_pred_OCD_x<- train_pred_OCD_x %>%
  mutate(False_prediction_OCD = ifelse(ConfusionMatrix == 'FN', 1, 0))

# check
table(train_pred_OCD_x$True_prediction_OCD)
table(train_pred_OCD_x$False_prediction_OCD)
addmargins(xtabs(formula = ~ Site + True_prediction_OCD, data = train_pred_OCD_x)) # 

#################### HC
# True prediction_HC [proba_val_hc] - TN = 1, FP =0 / False_prediction_HC - FP = 1, TN = 0 
train_pred_HC_x <- train_pred %>% filter(Dx == 0)
table(train_pred_HC_x$ConfusionMatrix) # there's only FP or TN 

train_pred_HC_x <- train_pred_HC_x %>%
  mutate(True_prediction_HC = ifelse(ConfusionMatrix == 'TN', 1, 0))

train_pred_HC_x <- train_pred_HC_x %>%
  mutate(False_prediction_HC = ifelse(ConfusionMatrix == 'FP', 1, 0))

# check
table(train_pred_HC_x$True_prediction_HC)
table(train_pred_HC_x$False_prediction_HC)
addmargins(xtabs(formula = ~ Site + True_prediction_HC, data = train_pred_HC_x)) # 

######################

# That is, even when adjusting for covariates (age, sex, site, and mean DTI metrics (FA, MD, AD, RD) , the site variability in classification performance persisted in OCD patients (????2 = 57.194, p< .001) and HCs (????2 = 50.303, p< .001). 


# 1. In OCD
# covariate model
# remove Average MD, RD due to vif issue
OCD_model.cov_demo2.brain2.clinical3 <- train_pred_OCD_x %>% 
  mutate(Med_12 = as.factor(ifelse(Med == 2, 2, ifelse(Med == 1, 1, NA)))) %>% 
  glm(True_prediction_OCD ~ Age + Sex + AverageFA + AverageAD + Sev + Dur+ Med_12, 
      family = binomial(link="logit"), data = .)
summary(OCD_model.cov_demo2.brain2.clinical3)

# covariate + site model
OCD_model.cov_demo2.brain2.clinical3_site <- train_pred_OCD_x %>% 
  mutate(Med_12 = as.factor(ifelse(Med == 2, 2, ifelse(Med == 1, 1, NA)))) %>% 
  glm(True_prediction_OCD ~ Age + Sex + AverageFA + AverageAD + Sev + Dur+ Med_12 + Site, 
      family = binomial(link="logit"), data = .)
summary(OCD_model.cov_demo2.brain2.clinical3_site)

# check vif
car::vif(OCD_model.cov_demo2.brain2.clinical3)
car::vif(OCD_model.cov_demo2.brain2.clinical3_site)

# site variability test
anova(OCD_model.cov_demo2.brain2.clinical3, OCD_model.cov_demo2.brain2.clinical3_site, test = 'Chisq') # chisq test : because of logistic regression             


# 2. In HC
# covariate model
HC_model.cov_demo2.brain2 <- train_pred_HC_x %>% 
  glm(True_prediction_HC ~ Age + Sex + AverageFA + AverageAD, 
      family = binomial(link = 'logit'),data = .)
summary(HC_model.cov_demo2.brain2)

# covariate + site model
HC_model.cov_demo2.brain2_site <- train_pred_HC_x %>% 
  glm(True_prediction_HC ~ Age + Sex + AverageFA + AverageAD + Site, 
      family = binomial(link = 'logit'),data = .)
summary(HC_model.cov_demo2.brain2_site)

# site variability test
anova(HC_model.cov_demo2.brain2, HC_model.cov_demo2.brain2_site, test = 'Chisq') # chisq test : because of logistic regression             
