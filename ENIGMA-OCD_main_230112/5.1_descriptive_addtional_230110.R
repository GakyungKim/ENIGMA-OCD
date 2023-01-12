library(tidyverse)
library(magrittr)

setwd("~/Desktop/study_enigma-ocd/0.Data/Analysis/1_Data split")

df_total <- read_csv('T.Dx_total_1653_v.cleaned.23.01.07.csv')


#### descriptive anlaysis ####
df_total %<>% mutate(AO_binary = ifelse(AO < 18 & Dx == 1, 'childhood-onset', 
                           ifelse(AO >= 18 & Dx == 1, 'adult-onset', NA)))


##### table 1. #####
df_total %>% filter(Dx == 1) %>% xtabs(~age_cat + AO_binary, data = ., addNA = T)

##### supple table 1. #####
df_total %>% filter(Dx == 1 & age_cat == 'adult') %>% 
  xtabs(~AO_binary + Site, data = .) 

df_total %>% filter(Dx == 1 & age_cat == 'pediatric') %>% 
  xtabs(~AO_binary + Site, data = .) 
df_total %>% filter(age_cat == 'pediatric') %>% xtabs(~ Site + AO_binary, data=. )


##### supple table 2. (A) #####
df_total %>% filter(age_cat == 'adult' & set == 'train' & Dx == 1) %>% 
  xtabs(~AO_binary, data = .)

df_total %>% filter(age_cat == 'adult' & set == 'test' & Dx == 1) %>% 
  xtabs(~AO_binary, data = .)
 
##### supple table 2. (B) - adult med01 #####
setwd("~/Desktop/study_enigma-ocd/0.Data/Analysis/1_Data split/1.adult")

df_adult_med01_train <- read_csv('T.UnmedOCDHC_Adult_S.Train_855_v.211123.csv')
df_adult_med01_test <- read_csv('T.UnmedOCDHC_Adult_S.Test_214_v.211123.csv')
df_adult_med01_train$set <- 'train'
df_adult_med01_test$set <- 'test'

df_adult_med01_train %>% bind_rows(df_adult_med01_test) %>% 
  mutate(AO_binary = ifelse(AO < 18 & Dx ==1,'childhoot-onset', 
                            ifelse(AO >= 18 &Dx == 1, 'adult-onset', NA))) -> df_adult_med01
df_adult_med01 %>% xtabs(~AO_binary + set, data = .)

##### supple table 2. (C) #####
df_adult_med12_train <- read_csv('T.MedUnmedOCD_Adult_S.Train_547_v.211123.csv')
df_adult_med12_test <- read_csv('T.MedUnmedOCD_Adult_S.Test_137_v.211123.csv')
df_adult_med12_train$set <- 'train'
df_adult_med12_test$set <- 'test'
df_adult_med12_train %>% bind_rows(df_adult_med12_test) %>% 
  mutate(AO_binary = ifelse(AO < 18 & Dx ==1,'childhoot-onset', 
                            ifelse(AO >= 18 &Dx == 1, 'adult-onset', NA))) -> df_adult_med12
df_adult_med12 %>% xtabs(~AO_binary + set , data = .)


#######################################################
####### clinical variable in OCD sample #####
rm(list = ls())
# adult
setwd("~/Desktop/study_enigma-ocd/0.Data/Analysis/1_Data split")

df_total <- read_csv('T.Dx_total_1653_v.cleaned.23.01.07.csv')
df_total %<>% mutate(AO_binary = ifelse(AO < 18 & Dx == 1, 'childhood-onset', 
                                        ifelse(AO >= 18 & Dx == 1, 'adult-onset', NA)), 
                     AO_binary = as.factor(AO_binary)) 

df_total %>% filter(age_cat == 'adult') -> df_adult

df_adult %>% filter(Dx == 1) -> df_adult_ocd
df_adult_ocd %>% xtabs(~ Site+AO_binary,data = .) %>% summary()
