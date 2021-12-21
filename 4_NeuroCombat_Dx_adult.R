################### NeuroCombat Harmonization ################### 
### reference: https://github.com/Jfortin1/ComBatHarmonization/tree/master/R
### procedures
# 0. load data
# 1. make a site variable as 'int' > assign it as vatch variable in step 5
# 2. search index of brain data for harmonization
# 3. check missing values
# 4. transpose the data
# 5. batch variable
# 6. main code for NeuroCombat
# 7. final shape & save the results

### 0. load data
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/connectome21/2021-1/project/ENIGMA-OCD/final/1.adult_Share_v.21.11.22/Data/Analysis_1_Data split_v.21.11.22")
df_train <- read.csv("T.Dx_Adult_S.Train_1068_v.211123.csv", header = T) #1068
df_test <- read.csv("T.Dx_Adult_S.Test_268_v.211122.csv", header = T) #268

#install package
library(devtools)
#install_github("jfortin1/CombatHarmonization/R/neuroCombat")
#install_github("jfortin1/neuroCombatData")
#install_github("jfortin1/neuroCombat_Rpackage")

#data matrix (p*n): p rows are features and n columns are participants
library(neuroCombat)

### 1. make site var to int  > assign it as vatch variable in step 5
df_train <- transform(df_train, 
                      site_int = ifelse(df_train$Site =="Amsterdam", 1, 
                                        ifelse(df_train$Site == "Bangalore", 2, 
                                               ifelse(df_train$Site == "Capetown", 3, 
                                                      ifelse(df_train$Site == "Kyoto", 4, 
                                                             ifelse(df_train$Site == "Milan", 5, 
                                                                    ifelse(df_train$Site == "Mountsinal", 6, 
                                                                           ifelse(df_train$Site=="Munich", 7, 
                                                                                  ifelse(df_train$Site=="Rome", 8, 
                                                                                         ifelse(df_train$Site=="Saopaulo", 9, 
                                                                                                ifelse(df_train$Site=="Shanghai", 10,
                                                                                                       ifelse(df_train$Site=="Seoul", 11, 12))))))))))))

df_test <- transform(df_test, 
                      site_int = ifelse(df_test$Site =="Amsterdam", 1, 
                                        ifelse(df_test$Site == "Bangalore", 2, 
                                               ifelse(df_test$Site == "Capetown", 3, 
                                                      ifelse(df_test$Site == "Kyoto", 4, 
                                                             ifelse(df_test$Site == "Milan", 5, 
                                                                    ifelse(df_test$Site == "Mountsinal", 6, 
                                                                           ifelse(df_test$Site=="Munich", 7, 
                                                                                  ifelse(df_test$Site=="Rome", 8, 
                                                                                         ifelse(df_test$Site=="Saopaulo", 9, 
                                                                                                ifelse(df_test$Site=="Shanghai", 10,
                                                                                                       ifelse(df_test$Site=="Seoul", 11, 12))))))))))))


### 2. search index of brain data for hamornization
#find the location of brain columns (initiation, end point)
## train set
which(colnames(df_train)=="ACR.FA") #21
which(colnames(df_train)=="UNC.R.AD") #272

locs.brain.init_train <- which(names(df_train) == 'ACR.FA') #21
locs.brain.end_train <- which(names(df_train)== 'UNC.R.AD') #272

brain_train <- df_train[,c(locs.brain.init_train:locs.brain.end_train)] #252 vars

## test set
which(colnames(df_test)=="ACR.FA") #21
which(colnames(df_test)=="UNC.R.AD") #272

locs.brain.init_test <- which(names(df_test) == 'ACR.FA') #21
locs.brain.end_test <- which(names(df_test)== 'UNC.R.AD') #272

brain_test <- df_test[,c(locs.brain.init_test:locs.brain.end_test)] #252 vars

#str(brain_train)
#str(brain_test)

### 3. check missing values
library(naniar)
library(dplyr)

## train set
sum(is.na(brain_train)) #741
miss_case_summary(brain_train)
miss_var_summary(brain_train) # NA due to CC.MD, CC.RD, CC.AD, ...
#View(miss_var_summary(brain_train))

brain_train_x <- brain_train #back up
brain_train_x[,sapply(brain_train_x, is.numeric)] <- lapply(brain_train_x[,sapply(brain_train_x, is.numeric)], 
                                        function(x){
                                          x <- ifelse(is.na(x), median(x, na.rm  = TRUE), x)
                                        }
)
#miss_case_summary(brain_train_x)

## test set
sum(is.na(brain_test)) #363
miss_case_summary(brain_test)
miss_var_summary(brain_test) # NA due to CC.MD, CC.RD, CC.AD, ...
#View(miss_var_summary(brain_test)) #now, there are no missing values

brain_test_x <- brain_test #back up
brain_test_x[,sapply(brain_test_x, is.numeric)] <- lapply(brain_test_x[,sapply(brain_test_x, is.numeric)], 
                                                            function(x){
                                                              x <- ifelse(is.na(x), median(x, na.rm  = TRUE), x)
                                                            }
)
#miss_case_summary(brain_train_x) #now, there are no missing values

### data z-standardization
library(caret)
## train set
train_brain_pre_z <- preProcess(brain_train_x, method = c("center", "scale"))
train_brain_z <- predict(train_brain_pre_z, brain_train_x)
train_z <- cbind(df_train[c(1:20, 273)], train_brain_z) #21:272 -> brain var / 273: site_int

## test set
test_brain_pre_z <- preProcess(brain_test_x, method = c("center", "scale"))
test_brain_z <- predict(test_brain_pre_z, brain_test_x)
test_z <- cbind(df_test[c(1:20, 273)], test_brain_z) #21:272 -> brain var / 273: site_int

### 4. transpose data
train_brain_z_t <- t(train_brain_z)
test_brain_z_t <- t(test_brain_z)

### 5. batch variable
train_batch = as.vector(df_train$site_int)
test_batch = as.vector(df_test$site_int)

### 6. Main: harmonization
##1) only site
train_harmonized <- neuroCombat(dat = train_brain_z_t, batch = train_batch, parametric = FALSE)
test_harmonized <- neuroCombat(dat = test_brain_z_t, batch = test_batch, parametric = FALSE)

##covariate
# train set
train_age <- as.vector(df_train$Age)
train_sex <- as.factor(df_train$Sex)

# test set
test_age <- as.vector(df_test$Age)
test_sex <- as.factor(df_test$Sex)

##2) covariate: age, sex
# train set
train_mod <- model.matrix(~ train_age + train_sex)
train_harmonized.cov_as <- neuroCombat(dat = train_brain_z_t, batch = train_batch, mod = train_mod)

# test set
test_mod <- model.matrix(~ test_age + test_sex)
test_harmonized.cov_as <- neuroCombat(dat=test_brain_z_t, batch = test_batch, mod = test_mod)

##3) summarize the results
# train set
train_combat = train_harmonized$dat.combat
train_combat.cov_as = train_harmonized.cov_as$dat.combat

train_combat <- t(train_combat)
train_combat.cov_as <- t(train_combat.cov_as)

# test set
test_combat = test_harmonized$dat.combat
test_combat.cov_as = test_harmonized.cov_as$dat.combat

test_combat <- t(test_combat)
test_combat.cov_as <- t(test_combat.cov_as)

### 7. final shape & save the results
# train set
train_front <- df_train[1:20]
train_back <- df_train[273]

train_harmo <- cbind(train_front, train_combat, train_back)
train_harmo.cov_as <- cbind(train_front, train_combat.cov_as, train_back)

# test set
test_front <- df_test[1:20]
test_back <- df_test[273]

test_harmo <- cbind(test_front, test_combat, test_back)
test_harmo.cov_as <- cbind(test_front, test_combat.cov_as, test_back)

# save the results
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/connectome21/2021-1/project/ENIGMA-OCD/final/3.NeuroCombat/1.adult")
# train set
write.csv(train_harmo, file="Adult_dx_train_harmo.csv", row.names = F, na = "")
write.csv(train_harmo.cov_as, file="Adult_dx_train_harmo_cov.csv", row.names = F, na = "")

# test set
write.csv(test_harmo, file="Adult_dx_test_harmo.csv", row.names = F, na = "")
write.csv(test_harmo.cov_as, file="Adult_dx_test_harmo_cov.csv", row.names = F, na = "")

