library(tidyverse)
library(magrittr)
library(pROC)

#### 1. Set path 
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Dai result/After piras updated_Adult/T.MedUnmedOCD_adult_new_LOSO_22.03.07")


#### 2. Load data 
proba_val <- read_csv('h2oai_experiment_adult_med12_new_train_predictions.csv')
proba_test <- read_csv('h2oai_experiment_adult_med12_new_test_predictions.csv')


#### 3. Validation result performance ####
#### siteº° µ¥ÀÌÅÍ°¡ ÀÖ´ÂÁö ÆÄ¾Ç #### 

proba_val %>% 
  xtabs(~Site + Med.UnmedOCD, data= .) %>% 
  addmargins()

proba_test %>% 
  xtabs(~Site + Med.UnmedOCD, data= .) %>% 
  addmargins()



#### Validation set - performance ####
#### ¤¤ 1. draw ROC curve of each site ####

# Amsterdam.roc <- proba_val %>%
#   filter(Site == 'Amsterdam') %>%
#   roc(Med.UnmedOCD, Med_UnmedOCD.1)

Bangalore.roc <- proba_val %>%
  filter(Site == 'Bangalore') %>%
  roc(Med.UnmedOCD, Med_UnmedOCD.1)

Capetown.roc <- proba_val %>%
  filter(Site == 'Capetown') %>%
  roc(Med.UnmedOCD, Med_UnmedOCD.1)
# Kyoto.roc <- proba_val %>%
#   filter(Site == 'Kyoto') %>%
#   roc(Med.UnmedOCD, Med_UnmedOCD.1)
Milan.roc <- proba_val %>%
  filter(Site == 'Milan') %>%
  roc(Med.UnmedOCD, Med_UnmedOCD.1)

Mountsinal.roc <- proba_val %>%
  filter(Site == 'Mountsinai') %>%
  roc(Med.UnmedOCD, Med_UnmedOCD.1)
Munich.roc <- proba_val %>%
  filter(Site == 'Munich') %>%
  roc(Med.UnmedOCD, Med_UnmedOCD.1)
Rome.roc <- proba_val %>%
  filter(Site == 'Rome') %>%
  roc(Med.UnmedOCD, Med_UnmedOCD.1)


Saopaulo.roc <- proba_val %>%
  filter(Site == 'Saopaulo') %>%
  roc(Med.UnmedOCD, Med_UnmedOCD.1)

Seoul.roc <- proba_val %>%
  filter(Site == 'Seoul') %>%
  roc(Med.UnmedOCD, Med_UnmedOCD.1)
# Shanghai.roc <- proba_val %>%
#   filter(Site == 'Shanghai') %>%
#   roc(Med.UnmedOCD, Med_UnmedOCD.1)


#### ¤¤ 2. auc ####
# auc(Amsterdam.roc)
auc(Bangalore.roc)
auc(Capetown.roc)
# auc(Kyoto.roc)
auc(Milan.roc)
auc(Mountsinal.roc)
auc(Munich.roc)
auc(Rome.roc)
auc(Saopaulo.roc)
auc(Seoul.roc)
# auc(Shanghai.roc)


# ci.auc(Amsterdam.roc)
ci.auc(Bangalore.roc)
ci.auc(Capetown.roc)
# ci.auc(Kyoto.roc)
ci.auc(Milan.roc)
ci.auc(Mountsinal.roc)
ci.auc(Munich.roc)
ci.auc(Rome.roc)
ci.auc(Saopaulo.roc)
ci.auc(Seoul.roc)
# ci.auc(Shanghai.roc)


# coords(Amsterdam.roc, "best", ret=c("threshold", "accuracy", "sensitivity", "specificity"), transpose = FALSE)
coords(Bangalore.roc, "best", ret=c("threshold", "accuracy", "sensitivity", "specificity"), transpose = FALSE)
coords(Capetown.roc, "best", ret=c("threshold", "accuracy", "sensitivity", "specificity"), transpose = FALSE)
# coords(Kyoto.roc, "best", ret=c("threshold", "accuracy", "sensitivity", "specificity"), transpose = FALSE)
coords(Milan.roc, "best", ret=c("threshold", "accuracy", "sensitivity", "specificity"), transpose = FALSE)
coords(Mountsinal.roc, "best", ret=c("threshold", "accuracy", "sensitivity", "specificity"), transpose = FALSE)

coords(Munich.roc, "best", ret=c("threshold", "accuracy", "sensitivity", "specificity"), transpose = FALSE)
coords(Rome.roc, "best", ret=c("threshold", "accuracy", "sensitivity", "specificity"), transpose = FALSE)
coords(Saopaulo.roc, "best", ret=c("threshold", "accuracy", "sensitivity", "specificity"), transpose = FALSE)
coords(Seoul.roc, "best", ret=c("threshold", "accuracy", "sensitivity", "specificity"), transpose = FALSE)
# coords(Shanghai.roc, "best", ret=c("threshold", "accuracy", "sensitivity", "specificity"), transpose = FALSE)


# # ci.coords(Amsterdam.roc, x='best' , input = "threshold", 
#           ret=c("accuracy", "sensitivity", "specificity"),  best.policy = 'random')

ci.coords(Bangalore.roc, x="best", input = "threshold", 
          ret=c("accuracy", "sensitivity", "specificity"),  best.policy = 'random')

ci.coords(Capetown.roc, x='best', input = "threshold", 
          ret=c("accuracy", "sensitivity", "specificity"), best.policy = 'random')


# ci.coords(Kyoto.roc, x='best', input = "threshold", 
#           ret=c("accuracy", "sensitivity", "specificity"),  best.policy = 'random')

ci.coords(Milan.roc, x='best' , input = "threshold", 
          ret=c("accuracy", "sensitivity", "specificity"),  best.policy = 'random')


ci.coords(Mountsinal.roc, x='best', input = "threshold", 
          ret=c("accuracy", "sensitivity", "specificity"), best.policy = 'random')

ci.coords(Munich.roc, x='best', input = "threshold", 
          ret=c("accuracy", "sensitivity", "specificity"), best.policy = 'random')


ci.coords(Rome.roc, x="best", input = "threshold", 
          ret=c("accuracy", "sensitivity", "specificity"),  best.policy = 'random')

ci.coords(Saopaulo.roc, x='best' , input = "threshold", 
          ret=c("accuracy", "sensitivity", "specificity"),  best.policy = 'random')

ci.coords(Seoul.roc, x='best', input = "threshold", 
          ret=c("accuracy", "sensitivity", "specificity"), best.policy = 'random')
# 
# ci.coords(Shanghai.roc, x='best', input = "threshold", 
#           ret=c("accuracy", "sensitivity", "specificity"), best.policy = 'random')


#### trimming output ####

col_names <- c('auc_l', 'auc_mean', 'auc_h', 'threshold', 'acc_mean', 'sensi_mean', 'speci_mean', 
               'acc_l', 'acc_median', 'acc_h', 'sensi_l', 'sensi_median', 'sensi_h', 'speci_l', 'speci_median', 'speci_h')

sites <- proba_val$Site %>% table %>% row.names()
n_var <- length(col_names)
n_site <- length(sites)
temp_frame <- data.frame(matrix(ncol = 16, nrow = n_site)) 


for (i in c(1:n_site)) {
  site_roc <- proba_val %>% filter(Site == sites[i]) %>% roc(Med.UnmedOCD, Med_UnmedOCD.1) 
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
temp_frame %<>% select(-c(contains('median'))) %>% 
  relocate(threshold, auc_mean, auc_l, auc_h, acc_mean, acc_l, acc_h, 
           sensi_mean, sensi_l, sensi_h, speci_mean, speci_l, speci_h)

write.csv(temp_frame, row.names = T, 'res_proc_val.csv')





#### Replication set - performance ####
#### ¤¤ 1. draw ROC curve of each site ####

test.roc <- proba_test %>% 
  roc(Med.UnmedOCD, Med_UnmedOCD.1)
auc(test.roc)
ci.auc(test.roc)
coords(test.roc, "best", ret=c("threshold", "accuracy", "sensitivity", "specificity"), transpose = FALSE)
ci.coords(test.roc, x='best' , input = "threshold", 
          ret=c("accuracy", "sensitivity", "specificity"), best.policy = 'random')




