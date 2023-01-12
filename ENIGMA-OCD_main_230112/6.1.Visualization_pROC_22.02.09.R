
library(tidyverse)
library(pROC)
library(ggplot2)


##### Plot ²Ù¹Ì±â #####
My.plot.title <- element_text(face = "bold", hjust = 0.5, size = 20)
My.legend.position <-  c(0.67, 0.2)
My.legend.text <- element_text(size = 14)


##### Adult Dx #######
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Dai result/After piras updated_Adult/T.Dx_D.nonharmo scaled only adult_cv.LOSO_777_F.age.sex_21.11.23")
df_adult_dx_val <- read_csv('train_preds_custom.csv')
df_adult_dx_test <- read_csv('test_preds_custom.csv')

# define ROC object
roc_adult_dx_val <- roc(df_adult_dx_val$Dx, df_adult_dx_val$Dx.1)
roc_adult_dx_test <-  roc(df_adult_dx_test$Dx, df_adult_dx_test$Dx.1)
# Visualize
vis_roc_adult_dx <- ggroc(list('Validation set = ROC AUC 67.29' = roc_adult_dx_val, 'Test set = 57.19' = roc_adult_dx_test)) +  scale_color_manual(values=c('darkgreen', 'blue'), name = element_blank())  + 
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(legend.position = c(0.65, 0.2)) +
  theme(legend.text =element_text(size = 14)) + coord_fixed()
  

vis_roc_adult_dx


##### Adult Med01 #####
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Dai result/After piras updated_Adult/T.UnmedOCD.HC_D.nonharmo scaled only adult_cv.LOSO_777_F.age.sex_21.11.23")
df_adult_med01_val <- read_csv('train_preds_custom.csv')
df_adult_med01_test <- read_csv('test_preds_custom.csv')

# define ROC object
roc_adult_med01_val <- roc(df_adult_med01_val$UnmedOCD.HC, df_adult_med01_val$UnmedOCD_HC.1)
roc_adult_med01_test <- roc(df_adult_med01_test$UnmedOCD.HC, df_adult_med01_test$UnmedOCD_HC.1)

# Visualize
vis_roc_adult_med01 <- ggroc(list('Validation set = ROC AUC 63.96' = roc_adult_med01_val, 'Test set = ROC AUC 62.67' = roc_adult_med01_test)) +
  scale_color_manual(values=c('darkgreen', 'blue'), name = element_blank())  + 
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20)) +
  theme(legend.position = c(0.65, 0.2)) +
  theme(legend.text =element_text(size = 14)) + coord_fixed()






##### Adult Med12 #####
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Dai result/After piras updated_Adult/T.MedUnmedOCD_D.nonharmo scaled only adult_cv.LOSO_777_F.age.sex_21.11.23")

df_adult_med12_val <- read_csv('train_preds_custom.csv')
df_adult_med12_test <- read_csv('test_preds_custom.csv')
# define ROC object
roc_adult_med12_val <- roc(df_adult_med12_val$Med.UnmedOCD, df_adult_med12_val$Med.UnmedOCD.1)
roc_adult_med12_test <- roc(df_adult_med12_test$Med.UnmedOCD, df_adult_med12_test$Med_UnmedOCD.1)

# Visualize
vis_roc_adult_med12 <- ggroc(list('Validation set = ROC AUC 60.22' = roc_adult_med01_val, 'Test set = ROC AUC 76.72' = roc_adult_med01_test)) +
  scale_color_manual(values=c('darkgreen', 'blue'), name = element_blank())  + 
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20)) +
  theme(legend.position = c(0.65, 0.2)) +
  theme(legend.text =element_text(size = 14)) + coord_fixed()




vis_roc_adult_dx
vis_roc_adult_med01
vis_roc_adult_med12


##### Pediatric Dx #####
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Dai result/After piras updated_Pediatric/Pedi_dx_scaled")
df_pedi_dx_val <- read_csv('h2oai_experiment_T.Dx_Pediatric_21.12.18_train_predictions.csv')
df_pedi_dx_test  <- read_csv('h2oai_experiment_T.Dx_Pediatric_21.12.18_test_predictions.csv')
# define ROC object
roc_pedi_dx_val <- roc(df_pedi_dx_val$Dx, df_pedi_dx_val$Dx.1)
roc_pedi_dx_test <- roc(df_pedi_dx_test$Dx, df_pedi_dx_test$Dx.1)
# Visualize
vis_roc_pedi_dx <- ggroc(list('Validation set = ROC AUC 69.54' = roc_pedi_dx_val, 'Test set = ROC AUC 59.8' = roc_pedi_dx_test)) +
  scale_color_manual(values=c('darkgreen', 'blue'), name = element_blank())  + 
  theme_bw() +
  theme(plot.title = My.plot.title) +
  theme(legend.position = My.legend.position) +
  theme(legend.text = My.legend.text)



##### Pediatric med 01 #####
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Dai result/After piras updated_Pediatric/Pedi_UnmedHC_scaled")
df_pedi_med01_val <- read_csv('h2oai_experiment_T.UnmedOCDHC_Pediatric_21.12.18_train_predictions.csv')
df_pedi_med01_test <- read_csv('h2oai_experiment_T.UnmedOCDHC_Pediatric_21.12.18_test_predictions.csv')

# define ROC object
roc_pedi_med01_val <- roc(df_pedi_med01_val$UnmedOCD.HC, df_pedi_med01_val$UnmedOCD_HC.1)
roc_pedi_med01_test <- roc(df_pedi_med01_test$UnmedOCD.HC, df_pedi_med01_test$UnmedOCD_HC.1)

# Visualize
vis_roc_pedi_med01 <- ggroc(list('Validation set = ROC AUC 65.96' = roc_pedi_med01_val, 'Test set = ROC AUC 48.51' = roc_pedi_med01_test)) +
  scale_color_manual(values=c('darkgreen', 'blue'), name = element_blank())  + 
  theme_bw() +
  theme(plot.title = My.plot.title) +
  theme(legend.position = My.legend.position) +
  theme(legend.text = My.legend.text)





##### Pediatric med 12 #####
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Dai result/After piras updated_Pediatric/Pedi_MedUnmed_scaled")
df_pedi_med12_val <- read_csv('h2oai_experiment_T.MedUnmedOCD_Pediatric_21.12.18_train_predictions.csv')
df_pedi_med12_test  <- read_csv('h2oai_experiment_T.MedUnmedOCD_Pediatric_21.12.18_test_predictions.csv')

# define ROC object
roc_pedi_med12_val <- roc(df_pedi_med12_val$Med.UnmedOCD, df_pedi_med12_val$Med_UnmedOCD.1)
roc_pedi_med12_test <- roc(df_pedi_med12_test$Med.UnmedOCD, df_pedi_med12_test$Med_UnmedOCD.1)

# Visualize
vis_roc_pedi_med12 <- ggroc(list('Validation set = ROC AUC 61.82' = roc_pedi_med12_val, 'Test set = ROC AUC 72.45' = roc_pedi_med12_test)) +
  scale_color_manual(values=c('darkgreen', 'blue'), name = element_blank())  + 
  theme_bw() +
  theme(plot.title = My.plot.title) +
  theme(legend.position = My.legend.position) +
  theme(legend.text = My.legend.text)


# save
vis_roc_pedi_dx
vis_roc_pedi_med01
vis_roc_pedi_med12



  
##### Val. Adult, Test. Pediatric ######
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Dai result/Dx_D.nonharmo_V.adult.T.ped_cv.LOSO11_737_F.age, sex_2020.10.09")
df_V.ad.T.pedi_val <- read_csv('pred_V.adult_T.ped_train.csv') 
df_V.ad.T.pedi_test <- read_csv('pred_V.adult_T.ped_test.csv')

# define ROC object
roc_V.ad.T.pedi_val <- roc(df_V.ad.T.pedi_val$Dx, df_V.ad.T.pedi_val$Dx.1)
roc_V.ad.T.pedi_test <- roc(df_V.ad.T.pedi_test$Dx, df_V.ad.T.pedi_test$Dx.1)

# Visualize 
ggroc(list('Validation set = ROC AUC 63.91 (52.4-72.3)' = roc_V.ad.T.pedi_val, 'Test set = ROC AUC 55.81 (49.5-62.2)' = roc_V.ad.T.pedi_test)) +
  scale_color_manual(values=c('darkgreen', 'blue'), name = element_blank())  + 
  theme_bw() +
  ggtitle("Adult trained model in pediatric OCD prediction") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20)) +
  theme(legend.position = c(0.65, 0.2)) +
  theme(legend.text =element_text(size = 14))









