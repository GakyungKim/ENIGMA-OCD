library(tidyverse)
library(naniar)
library(skimr)

# Load dataset
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/1_Siteº° merged")

Amsterdam_merged <- read.csv(file='Merged_Amsterdam_79_v.21.11.16.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()
Bangalore_merged <- read.csv(file='Merged_Bangalore_290_v.21.11.16.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()
Capetown_merged <- read.csv(file='Merged_Capetown_49_v.21.11.16.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()
Kyoto_merged <- read.csv(file='Merged_Kyoto_76_v.21.11.16.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()
Milan_merged <- read.csv(file='Merged_Milan_130_v.21.11.16.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()
Mountsinai_merged <- read.csv(file='Merged_Mountsinai_34_v.21.11.16.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()
Munich_merged <- read.csv(file='Merged_Munich_133_v.21.11.16.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()
Rome_merged <- read.csv(file='Merged_Rome_188_v.21.11.16.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()
Saopaulo_merged <- read.csv(file='Merged_Saopaulo_67_v.21.11.16.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()
Shanghai_merged <- read.csv(file='Merged_Shanghai_128_v.21.11.21.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()
Seoul_merged <- read.csv(file='Merged_Seoul_185_v.21.11.16.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()


# 2. Bind rows: because they have same shape
## 2.1. Check data type

# should be all character to combine
Milan_merged$subjectID <- as.character(Milan_merged$subjectID)
Mountsinai_merged$subjectID <- as.character(Mountsinai_merged$subjectID)

# Merge
df_11site_merged <- Amsterdam_merged %>% 
  bind_rows(Bangalore_merged) %>% 
  bind_rows(Capetown_merged) %>% 
  bind_rows(Kyoto_merged) %>% 
  bind_rows(Milan_merged) %>% 
  bind_rows(Mountsinai_merged) %>% 
  bind_rows(Munich_merged) %>% 
  bind_rows(Rome_merged) %>% 
  bind_rows(Saopaulo_merged) %>% 
  bind_rows(Shanghai_merged) %>% 
  bind_rows(Seoul_merged) 

df_11site_merged
tail(df_11site_merged)
table(df_11site_merged$Site)
vis_miss(df_11site_merged)
View(miss_var_summary(df_11site_merged))

# 3. Make 'subjectkey' variable

df_11site_merged$subjectkey <- paste0("sub_", 1:nrow(df_11site_merged))
df_11site_merged<- df_11site_merged %>%  relocate(subjectkey)



# 4. filtering data without mandatory covariates
df_11site_merged<- df_11site_merged %>% 
  filter(is.na(Age) == FALSE & is.na(Sex) == FALSE)

# 5. Filter out pediatric data
df_11site_merged_Adult <- df_11site_merged %>% 
  filter(Age >= 18) 

df_11site_merged_Pediatric<- df_11site_merged %>% 
  filter(Age <= 17) 

# 6. Make dataset for each task
# 6.0.1 check abnormal data
table(df_11site_merged_Adult$Dx)
df_11site_merged_Adult <- df_11site_merged_Adult %>% 
  filter(Dx == 0 | Dx ==1)

# 6.1. for Diagnosis
df_11site_merged_Adult

'''
# 6.2. for MedUnmed OCD (Med vs. unmed OCD)
df_11site_merged_Adult_Only.MedUnmedOCD <- df_11site_merged_Adult %>% 
  filter(Med == 1 | Med == 2)

df_11site_merged_Adult_Only.MedUnmedOCD <- df_11site_merged_Adult_Only.MedUnmedOCD %>% 
  mutate(Med.UnmedOCD = ifelse(Med == 2, 1, 
                               ifelse(Med == 1, 0, NA)))

table(df_11site_merged_Adult_Only.MedUnmedOCD$Med.UnmedOCD) # 0 = unmed OCD, 1 = med OCD

# 6.3. for UnmedOCD.HC (unmed OCD vs. HC)
df_11site_merged_Adult_Only.UnmedOCDHC <- df_11site_merged_Adult %>% 
  filter(Med == 0 | Med ==1)

df_11site_merged_Adult_Only.UnmedOCDHC <- df_11site_merged_Adult_Only.UnmedOCDHC %>% 
  mutate(UnmedOCD.HC  = ifelse(Med == 1, 1, 
                               ifelse(Med == 0, 0, NA)))

table(df_11site_merged_Adult_Only.UnmedOCDHC$UnmedOCD.HC)

nrow(df_11site_merged_Adult_Only.MedUnmedOCD)
nrow(df_11site_merged_Adult_Only.UnmedOCDHC)
'''



# 7. Relocate variable
df_11site_merged_Adult <- df_11site_merged_Adult %>% relocate(subjectkey, Dx, Site)
#df_11site_merged_Adult_Only.MedUnmedOCD <- df_11site_merged_Adult_Only.MedUnmedOCD %>% relocate(subjectkey, Med.UnmedOCD, Site)
#df_11site_merged_Adult_Only.UnmedOCDHC <- df_11site_merged_Adult_Only.UnmedOCDHC %>% relocate(subjectkey, UnmedOCD.HC, Site)

df_11site_merged_Adult

# 8. save dataset
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Analysis/1_Data split")

write.csv(df_11site_merged_Adult, file = 'T.Dx_Adult_1336_v.21.11.22.csv', row.names = FALSE)

#write.csv(df_11site_merged_Adult_Only.MedUnmedOCD, file = 'T.MedUnmedOCD_Adult_684_v.21.11.22.csv', row.names = FALSE)
#write.csv(df_11site_merged_Adult_Only.UnmedOCDHC, file = 'T.UnmedOCD.HC_Adult_1070_v.21.11.22.csv', row.names = FALSE)




























