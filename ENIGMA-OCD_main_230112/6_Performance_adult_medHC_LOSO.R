rm(list = ls())
library(tidyverse)
library(magrittr)
library(pROC)

#### 1. Set path 
setwd("C:/Users/김보겸/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Dai result/After piras updated_Adult/MedHC_Adult_LOSO")

#### 2. Load data 
proba_val <- read_csv('h2oai_experiment_MedHC_Adult_LOSO_train_predictions.csv')
proba_test <- read_csv('h2oai_experiment_MedHC_Adult_LOSO_test_predictions.csv')

#### 3. Validation result performance ####
#### site별 데이터가 있는지 파악 => 없는 데이터: Amsterdam, Kyoto, Shanghai    

proba_val %>% 
  xtabs(~Site + Dx, data= .) %>% 
  addmargins()

proba_test %>% 
  xtabs(~Site + Dx, data= .) %>% 
  addmargins()



#### Validation set - performance ####
#### ㄴ 1. draw ROC curve of each site ####
#### trimming output ####

col_names <- c('auc_l', 'auc_mean', 'auc_h', 'threshold', 'acc_mean', 'sensi_mean', 'speci_mean', 
               'acc_l', 'acc_median', 'acc_h', 'sensi_l', 'sensi_median', 'sensi_h', 'speci_l', 'speci_median', 'speci_h')

sites <-  c("Bangalore",  "Capetown",  "Milan",      "Mountsinai", "Munich", 
            "Rome",       "Saopaulo",   "Seoul" )

n_var <- length(col_names)
n_site <- length(sites)
temp_frame <- data.frame(matrix(ncol = 16, nrow = n_site)) 


for (i in c(1:n_site)) {
  site_roc <- proba_val %>% filter(Site == sites[i]) %>% roc(MedHC, MedHC.1) 
  info_auc <- ci.auc(site_roc)
  info_metrics <- coords(site_roc, "best", ret=c("threshold", "accuracy", "sensitivity", "specificity"), transpose = FALSE)
  info_metrics_ci <- ci.coords(site_roc, x="best", input = "threshold", 
                               ret=c("accuracy", "sensitivity", "specificity"),  best.policy = 'random')
  
  # extract output
  out_col <- cbind(info_auc[1], info_auc[2], info_auc[3], 
                   info_metrics, 
                   info_metrics_ci$accuracy, info_metrics_ci$sensitivity, info_metrics_ci$specificity)
  temp_frame[i, ] <- out_col
}

colnames(temp_frame) <- col_names
rownames(temp_frame) <- sites
temp_frame %<>% 
  relocate(threshold, auc_mean, auc_l, auc_h, acc_mean, acc_l, acc_h, 
           sensi_mean, sensi_l, sensi_h, speci_mean, speci_l, speci_h) 
write.csv(temp_frame, row.names = T, 'res_proc_val.csv')


#### Replication set - performance ####
#### ㄴ 1. draw ROC curve of each site ####

test.roc <- proba_test %>% 
  roc(MedHC, MedHC.1)
auc(test.roc)
ci.auc(test.roc)
coords(test.roc, "best", ret=c("threshold", "accuracy", "sensitivity", "specificity"), transpose = FALSE)
ci.coords(test.roc, x='best' , input = "threshold", 
          ret=c("accuracy", "sensitivity", "specificity"), best.policy = 'random')



