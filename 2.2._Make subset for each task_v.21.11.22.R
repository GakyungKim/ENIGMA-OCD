
#### Load dataset ####
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Analysis/1_Data split")
df_11site_merged_Adult <- read.csv('T.Dx_Adult_1336_v.cleaned.21.11.22.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()

table(df_11site_merged_Adult$Dx, df_11site_merged_Adult$Med)

# 6.1. for Diagnosis
df_11site_merged_Adult

# 6.2. for MedUnmed OCD (Med vs. unmed OCD)
df_11site_merged_Adult_Only.MedUnmedOCD <- df_11site_merged_Adult %>% 
  filter(Med == 1 | Med == 2)

# check
df_11site_merged_Adult_Only.MedUnmedOCD %>% 
  xtabs(~ Dx + Med, data = ., addNA = TRUE)
# Recoding
df_11site_merged_Adult_Only.MedUnmedOCD <- df_11site_merged_Adult_Only.MedUnmedOCD %>% 
  mutate(Med.UnmedOCD = ifelse(Med == 2, 1, 
                               ifelse(Med == 1, 0, NA)))
#check
df_11site_merged_Adult_Only.MedUnmedOCD %>% 
  xtabs(~ Dx + Med.UnmedOCD, data = ., addNA = TRUE)
table(df_11site_merged_Adult_Only.MedUnmedOCD$Med.UnmedOCD) # 0 = unmed OCD, 1 = med OCD

# 6.3. for UnmedOCD.HC (unmed OCD vs. HC)
df_11site_merged_Adult_Only.UnmedOCDHC <- df_11site_merged_Adult %>% 
  filter((Dx == 0 & Med == 0) | (Dx == 1 & Med ==1))

# check
df_11site_merged_Adult_Only.UnmedOCDHC %>% 
  xtabs(~ Dx + Med, data = ., addNA = TRUE)
# make target outcome
df_11site_merged_Adult_Only.UnmedOCDHC <- df_11site_merged_Adult_Only.UnmedOCDHC %>% 
  mutate(UnmedOCD.HC  = ifelse(Med == 1, 1, 
                               ifelse(Med == 0, 0, NA)))
# check
df_11site_merged_Adult_Only.UnmedOCDHC %>% 
  xtabs( ~ Dx + UnmedOCD.HC, data = ., addNA = TRUE)
table(df_11site_merged_Adult_Only.UnmedOCDHC$UnmedOCD.HC)

nrow(df_11site_merged_Adult_Only.MedUnmedOCD)
nrow(df_11site_merged_Adult_Only.UnmedOCDHC)

# 7. Relocate variable
df_11site_merged_Adult <- df_11site_merged_Adult %>% relocate(subjectkey, Dx, Site)
df_11site_merged_Adult_Only.MedUnmedOCD <- df_11site_merged_Adult_Only.MedUnmedOCD %>% relocate(subjectkey, Med.UnmedOCD, Site)
df_11site_merged_Adult_Only.UnmedOCDHC <- df_11site_merged_Adult_Only.UnmedOCDHC %>% relocate(subjectkey, UnmedOCD.HC, Site)

# 8. save dataset
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Analysis/1_Data split")

#write.csv(df_11site_merged_Adult, file = 'T.Dx_Adult_1336_v.21.11.22.csv', row.names = FALSE)
write.csv(df_11site_merged_Adult_Only.MedUnmedOCD, file = 'T.MedUnmedOCD_Adult_684_v.cleaned.21.11.22.csv', row.names = FALSE)
write.csv(df_11site_merged_Adult_Only.UnmedOCDHC, file = 'T.UnmedOCD.HC_Adult_1069_v.cleaned.21.11.22.csv', row.names = FALSE)

