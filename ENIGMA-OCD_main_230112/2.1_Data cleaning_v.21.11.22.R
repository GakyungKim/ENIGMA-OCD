library(tidyverse)
library(naniar)
library(skimr)
library(rstatix)
library(tigerstats)


#### load dataset ####
# non imputed version
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Analysis/1_Data split")
df_adult <- read.csv('T.Dx_Adult_1336_v.21.11.22.csv', header = T, sep = ",", na.strings = c("NA", "")) %>% as_tibble()


names(df_adult[1:30])

#### check data type ####
str(df_adult[1:30])
df_adult$Dx <- as.factor(df_adult$Dx)
df_adult$Sex <- as.factor(df_adult$Sex)

df_adult$Med <- as.factor(df_adult$Med)
#subsymptom
df_adult$Agr_Check <- as.factor(df_adult$Agr_Check)
df_adult$Clean <- as.factor(df_adult$Clean)
df_adult$Sex_Rel <- as.factor(df_adult$Sex_Rel)
df_adult$Hoard <- as.factor(df_adult$Hoard)
df_adult$Ord <- as.factor(df_adult$Ord)
# comorbidity
df_adult$Anx <- as.factor(df_adult$Anx)
df_adult$CurAnx <- as.factor(df_adult$CurAnx)
df_adult$Dep <- as.factor(df_adult$Dep)
df_adult$CurDep <- as.factor(df_adult$CurDep)


#### Make subset for OCD ####
df_adult_ocd <- df_adult %>% 
  filter(Dx == 1)
df_adult_hc <- df_adult %>% 
  filter(Dx == 0)


#### Abnormal coding ####
# 1. Dx
df_adult %>% 
  xtabs(~ Dx, data = ., addNA = TRUE)

# 2. Age
hist(df_adult$Age)

# 3. Sex
df_adult %>% 
  xtabs(~ Sex, data = ., addNA = TRUE)

# 4. Med
df_adult %>% 
  xtabs(~ Med + Dx, data = ., addNA = TRUE)

table(df_adult$Dx, df_adult$Med)
# Recoded Med
#df_adult <- df_adult %>% 
#  mutate(Med_recoded = ifelse(Dx == 1 & Med == 0, 1, Med))


# 5. AO
hist(df_adult$AO)
df_adult %>% 
  xtabs(~ AO + Dx, data = ., addNA = TRUE)
# Recoded AO
df_adult <- df_adult %>% 
  mutate(AO = ifelse(Dx == 0 & AO == 0, NA, 
                     ifelse(Dx == 0 & AO == 999, NA,
                            ifelse(Dx == 1 & AO == 999, NA, AO))))


# 6. Dur
hist(df_adult$Dur)
df_adult %>% 
  xtabs(~ Dur + Dx, data = ., addNA = TRUE)
# Recoded Dur
df_adult <- df_adult %>% 
  mutate(Dur = ifelse(Dx == 0 & Dur == 0, NA, 
                      ifelse(Dx == 0 & Dur == 999, NA, 
                     ifelse(Dx == 1 & Dur == -32, NA,
                     ifelse(Dx == 1 & Dur == 999, NA, Dur)))))


# 7. Sev
hist(df_adult$Sev)
df_adult %>% 
  xtabs(~ Sev + Dx, data = ., addNA = TRUE)

df_adult <- df_adult %>% 
  mutate(Sev = ifelse(Dx == 0 & ( Sev == 0 | Sev ==2 | Sev == 999), NA, Sev))

# 8. Clean
df_adult %>% 
  xtabs(~ Clean + Dx, data = ., addNA = TRUE)

#df_adult <- df_adult %>% 
#  mutate(Clean = ifelse(Dx ==0 & (Clean == 0 | Clean == 999), NA,
#                        ifelse(Dx == 1 & Clean == 999, NA, Clean)))


# 9. Agr_Check
df_adult %>% 
  xtabs(~ Agr_Check + Dx, data = ., addNA = TRUE)


#df_adult <- df_adult %>% 
#  mutate(Agr_Check = ifelse(Dx == 0 & (Agr_Check == 0 | Agr_Check == 999), NA, 
#                            ifelse(Dx == 1 & Agr_Check == 999, NA, Agr_Check)))

# 10. Hoard
df_adult %>% 
  xtabs(~ Hoard +Dx, data = ., addNA = TRUE)

#df_adult <- df_adult %>% 
#  mutate(Hoard = ifelse(Dx == 0 & (Hoard == 0 | Hoard == 999), NA, 
#                                   ifelse(Dx == 1 & Hoard == 999, NA, Hoard)))


# 11. Ord 
df_adult %>% 
  xtabs(~ Ord + Dx, data = ., addNA = TRUE)

#df_adult <- df_adult %>% 
#  mutate(Ord = ifelse(Dx == 0 & (Ord == 0 | Ord == 999), NA, 
#                      ifelse(Dx ==1 & Ord == 999, NA, Ord)))

# 12. Anx
df_adult %>% 
  xtabs(~ Anx + Dx, data = ., addNA = TRUE)

#df_adult <- df_adult %>% 
#  mutate(Anx = ifelse(Dx == 1 & Anx == 0, NA, Anx))


# 13. CurAnx
df_adult %>% 
  xtabs(~ CurAnx + Dx, data = ., addNA = TRUE)

#df_adult <- df_adult %>% 
#  mutate(CurAnx = ifelse(Dx == 1 & CurAnx == 0, NA, CurAnx))

# 14. Dep
df_adult %>% 
  xtabs(~ Dep + Dx, data = ., addNA = TRUE)

# 15. CurDep
df_adult %>% 
  xtabs(~ CurDep + Dx, data = ., addNA = TRUE)



#### save file ####
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Analysis/1_Data split")
write.csv(df_adult, 'T.Dx_Adult_1336_v.cleaned.21.11.22.csv', row.names = FALSE)






