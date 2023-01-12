library(tidyverse)
library(magrittr)
# library(naniar)
# library(skimr)
# library(rstatix)
# library(tigerstats)


# Data: total (before merge and scaling)

setwd("~/Desktop/study_enigma-ocd/0.Data/Analysis/1_Data split/1.adult")
df_adult_total <- read_csv('T.Dx_Adult_1336_v.cleaned.21.11.22.csv') # 1,336 x 272

#### check target outcome - Dx ####
df_adult_total %>%  
  xtabs(~Dx, data = ., addNA = T) %>% addmargins() 

#### Make Dx_character variable ####
df_adult_total %<>% 
  mutate(Dx_charac = ifelse(Dx == 1, 'OCD', 'HC')) # 1,336 x 273

# set levels (순서 바꾸기 위해)
df_adult_total$Dx_charac %<>% 
  factor(x = ., levels = c('OCD','HC'))

#### Make Med_12 variable ####
df_adult_total %<>% 
  mutate(Med_12 = ifelse(Med == 1, 1, 
                         ifelse(Med == 2, 2, NA)))
df_adult_total %>% 
  xtabs(~ Med_12, data = ., addNA = TRUE)

# Male
df_adult_total %<>% 
  mutate(Male = ifelse(Sex == 1, 'Male', 'Female')) # 1,336 x 274

###### Make AO_binary variable #######
df_adult_total %<>% 
  mutate(AO_binary = ifelse(Dx == 1 & AO <18, 'childhood-onset', 
                            ifelse(Dx ==1 & AO >= 18, 'adult-onset', NA)), 
         AO_child = ifelse(AO_binary == 'childhood-onset', 1, 
                           ifelse(AO_binary == 'adult-onset', 0, NA)), 
         AO_child = as.factor(AO_child))

df_adult_total %>% xtabs(~AO_binary, data = . ,addNA = T)
df_adult_total %>% xtabs(~AO_child, data = . ,addNA = T)

#### Set datatype ####
df_adult_total$Sex <- as.factor(df_adult_total$Sex)
df_adult_total$Dx <- as.factor(df_adult_total$Dx) 

df_adult_total$Med <- as.factor(df_adult_total$Med) 
df_adult_total$Med_12 <- as.factor(df_adult_total$Med_12)

df_adult_total$Anx <- as.factor(df_adult_total$Anx) 
df_adult_total$Dep <- as.factor(df_adult_total$Dep) 
df_adult_total$CurAnx <- as.factor(df_adult_total$CurAnx) 
df_adult_total$CurDep <- as.factor(df_adult_total$CurDep) 

df_adult_total$Agr_Check <- as.factor(df_adult_total$Agr_Check) 
df_adult_total$Clean <- as.factor(df_adult_total$Clean) 
df_adult_total$Ord <- as.factor(df_adult_total$Ord) 
df_adult_total$Sex_Rel <- as.factor(df_adult_total$Sex_Rel) 
df_adult_total$Hoard <- as.factor(df_adult_total$Hoard) 



#### Make subset ####
df_adult_ocd <- df_adult_total %>% 
  filter(Dx_charac == 'OCD')


#### Descriptive analysis ####
# Age (years)
# OCD illness severity score
# Duration of illness

# Descriptive analysis
df_adult_total%>%
  group_by(Dx_charac) %>%
  rstatix::get_summary_stats(Age, Sev, AO, Dur , type = "mean_sd")

# t-test
t.test(data = df_adult_total, Age ~ Dx_charac)


# 
# Male
# sum(is.na(df_adult_total$Sex)) # 0

df_adult_total %<>% 
  mutate(Male = ifelse(Sex == 1, 'Male', 'Female')) # 1,336 x 274

ct.male <- df_adult_total %>% 
  xtabs(~Male + Dx_charac, data = ., addNA= TRUE) %>% 
  addmargins() 

ct.male
ct.male %>% tigerstats::rowPerc()
summary(ct.male)


# Medication use at time of scan
df_adult_ocd %>% 
  xtabs(~ Med_12, data = ., addNA= TRUE)
df_adult_ocd %>% 
  xtabs(~ Med_12, data = ., addNA = TRUE) %>% tigerstats::rowPerc()

# childhood onset 
ct.AO <- df_adult_ocd %>% 
  xtabs(~AO_child + Site, data = .) %>% 
  addmargins() 
ct.AO
summary(ct.AO)


# Lifetime diagnosis
# 1. Anxiety
df_adult_ocd %>% 
  mutate(Anx  = ifelse(Anx ==1, 1, 
                       ifelse(Anx == 2, 2, 
                              ifelse(Anx == 0, NA, NA)))) %>% 
  xtabs(~Anx, data = ., addNA = TRUE) # %>% tigerstats::rowPerc()


# 2. Major depression
df_adult_ocd %>% 
  mutate(Dep  = ifelse(Dep ==1, 1, 
                       ifelse(Dep == 2, 2, 
                              ifelse(Dep == 0, NA, NA)))) %>% 
  xtabs(~Dep, data = ., addNA = TRUE) # %>% tigerstats::rowPerc()



# Current comorbid disorders
# 1. Anxiety
df_adult_ocd %>% 
  mutate(CurAnx  = ifelse(CurAnx ==1, 1, 
                          ifelse(CurAnx == 2, 2, 
                                 ifelse(CurAnx == 0, NA, NA)))) %>% 
  xtabs(~CurAnx, data = ., addNA = TRUE) # %>% tigerstats::rowPerc()

# 2. Major depression
df_adult_ocd %>% 
  mutate(CurDep  = ifelse(CurDep ==1, 1, 
                          ifelse(CurDep == 2, 2, 
                                 ifelse(CurDep == 0, NA, NA)))) %>% 
  xtabs(~CurDep, data = ., addNA = TRUE) # %>% tigerstats::rowPerc()





# OCD symptom dimension
# Aggressive/checking
df_adult_ocd %>% 
  mutate(Agr_Check = ifelse(Agr_Check == 0, 0,
                            ifelse(Agr_Check == 1, 1, 
                                   ifelse(Agr_Check ==999, NA, NA)))) %>%   xtabs( ~ Agr_Check, data = ., addNA = TRUE) # %>% tigerstats::rowPerc()
ct.Agr_Check <- df_adult_ocd %>% 
  xtabs(~Agr_Check + Site, data = .) %>% 
  addmargins() 
ct.Agr_Check
summary(ct.Agr_Check)


# Contamination/cleaning
df_adult_ocd %>% 
  mutate(Clean = ifelse(Clean == 0, 0,
                        ifelse(Clean == 1, 1, 
                               ifelse(Clean ==999, NA, NA)))) %>%   xtabs( ~ Clean, data = ., addNA = TRUE) # %>% tigerstats::rowPerc()

ct.Clean <- df_adult_ocd %>% 
  xtabs(~Clean + Site, data = .) %>% 
  addmargins() 
ct.Clean  
summary(ct.Clean)

# Symmetry/ordering
df_adult_ocd %>% 
  mutate(Ord = ifelse(Ord == 0, 0,
                      ifelse(Ord == 1, 1, 
                             ifelse(Ord ==999, NA, NA)))) %>%   xtabs( ~ Ord, data = ., addNA = TRUE) # %>% tigerstats::rowPerc()

ct.Ord <- df_adult_ocd %>% 
  xtabs(~Ord + Site, data = .) %>% 
  addmargins() 
ct.Ord  
summary(ct.Ord)

# Sexual/religious
df_adult_ocd %>% 
  mutate(Sex_Rel = ifelse(Sex_Rel == 0, 0,
                          ifelse(Sex_Rel == 1, 1, 
                                 ifelse(Sex_Rel ==999, NA, NA)))) %>%   xtabs( ~ Sex_Rel, data = ., addNA = TRUE) # %>% tigerstats::rowPerc()
ct.Sex_Rel <- df_adult_ocd %>% 
  xtabs(~Sex_Rel + Site, data = .) %>% 
  addmargins() 
ct.Sex_Rel  
summary(ct.Sex_Rel)

# Hoarding
df_adult_ocd %>% 
  mutate(Hoard = ifelse(Hoard == 0, 0,
                        ifelse(Hoard == 1, 1, 
                               ifelse(Hoard ==999, NA, NA)))) %>%   xtabs( ~ Hoard, data = ., addNA = TRUE) # %>% tigerstats::rowPerc()

ct.Hoard <- df_adult_ocd %>% 
  xtabs(~Hoard + Site, data = .) %>% 
  addmargins() 
ct.Hoard  
summary(ct.Hoard)

####################################################################################
####################################################################################
####################################################################################

#### site effects on sample characteristics ####

###########################################################################
####################################################################################
####################################################################################


##########################################################################

#### 1. GLM ####
# covariate : 
# Demo: Age, Sex, Site 
# Dx (if using df_adult_total)
# Clinical variable
# Average DTI 

# Result- 

###########################################################################

#### Demographic ####
# 1. Age
glm.Age_Cov.Demo.Dx <- df_adult_total %>% lm(Age ~ Sex + Dx + Site, data = .)
summary(glm.Age_Cov.Demo.Dx)
anova(glm.Age_Cov.Demo.Dx)

glm.Age_Cov.Demo.Dx_wo.Site <- df_adult_total %>% lm(Age ~ Sex + Dx , data = .)
summary(glm.Age_Cov.Demo.Dx_wo.Site)
anova(glm.Age_Cov.Demo.Dx_wo.Site)


#### Clinical variable in OCD sample ####

# OCD illness severity score
glm.Sev_Cov.Demo <- df_adult_ocd %>% 
  lm(Sev ~ Age + Sex + Site, data = .) # with cov
summary(glm.Sev_Cov.Demo)
anova(glm.Sev_Cov.Demo)

#
glm.Sev_Cov.Demo_wo.Site <- df_adult_ocd %>% 
  lm(Sev ~ Age + Sex , data = .) # with cov


# Age at onset
#glm.AO_Cov.Demo <- df_adult_ocd %>% 
#  lm(AO ~ Age + Sex + Site, data = .) # with cov
#summary(glm.AO_Cov.Demo)
#anova(glm.AO_Cov.Demo)

#glm.AO_Cov.Demo_wo.Site <- df_adult_ocd %>% 
#  lm(AO ~ Age + Sex, data = .) # with cov



# Duration of illness
glm.Dur_Cov.Demo <- df_adult_ocd %>% 
  lm(Dur ~ Age + Sex + Site, data = .) # with cov
summary(glm.Dur_Cov.Demo)
anova(glm.Dur_Cov.Demo)

glm.Dur_Cov.Demo_wo.Site <- df_adult_ocd %>% 
  lm(Dur ~ Age + Sex , data = .) # with cov

# 
# Medication use at time of scan
# childhood onset

# Lifetime diagnosis
# Anxiety
# Major depression
# Current comorbid disorders
# Anxiety
# Major depression
# OCD symptom dimension
# Aggressive/checking
# Contamination/cleaning
# Symmetry/ordering
# Sexual/religious
# Hoarding

#### Average DTI in total sample (OCD + HC) ####

glm.AverageFA_Cov.Demo.Dx <- df_adult_total %>% 
  lm(AverageFA ~ Age + Sex + Site+  Dx, data = .)
glm.AverageFA_Cov.Demo.Dx_wo.Site <- df_adult_total %>% 
  lm(AverageFA ~ Age + Sex +  Dx, data = .)


glm.AverageMD_Cov.Demo.Dx <- df_adult_total %>% 
  lm(AverageMD ~ Age + Sex + Site+  Dx, data = .)
glm.AverageMD_Cov.Demo.Dx_wo.Site <- df_adult_total %>% 
  lm(AverageMD ~ Age + Sex + Site+  Dx, data = .)

glm.AverageRD_Cov.Demo.Dx <- df_adult_total %>% 
  lm(AverageRD ~ Age + Sex + Site+  Dx, data = .)
glm.AverageRD_Cov.Demo.Dx_wo.Site <- df_adult_total %>% 
  lm(AverageRD ~ Age + Sex +  Dx, data = .)

glm.AverageAD_Cov.Demo.Dx <- df_adult_total %>% 
  lm(AverageAD ~ Age + Sex + Site+  Dx, data = .)
glm.AverageAD_Cov.Demo.Dx_wo.Site <- df_adult_total %>% 
  lm(AverageAD ~ Age + Sex +  Dx, data = .)


#lm(data = proba_val, AverageFA ~ site + Age + Sex + Dx)
summary(glm.AverageFA_Cov.Demo.Dx)
anova(glm.AverageFA_Cov.Demo.Dx)

summary(glm.AverageMD_Cov.Demo.Dx)
anova(glm.AverageMD_Cov.Demo.Dx)

summary(glm.AverageRD_Cov.Demo.Dx)
anova(glm.AverageRD_Cov.Demo.Dx)

summary(glm.AverageAD_Cov.Demo.Dx)
anova(glm.AverageAD_Cov.Demo.Dx)




##########################################################################

#### 2. Hierarchical regression model ####

# Result: all significant except AverageMD

###########################################################################

# 2.1. Site effect on Demo and clinical variable

# Hierarchical - Site effect on Age
anova(glm.Age_Cov.Demo.Dx_wo.Site, glm.Age_Cov.Demo.Dx)
# Hierarchical - Site effect on Sev > significant
anova(glm.Sev_Cov.Demo_wo.Site, glm.Sev_Cov.Demo)
# Hierarchical - Site effect on AO > significant
anova(glm.AO_Cov.Demo_wo.Site, glm.AO_Cov.Demo)
# Hierarchical - Site effect on Dur > significant
anova(glm.Dur_Cov.Demo, glm.Dur_Cov.Demo_wo.Site)

# Site effect on Average DTI - Hierarchical analysis
anova(glm.AverageFA_Cov.Demo.Dx_wo.Site, glm.AverageFA_Cov.Demo.Dx)
anova(glm.AverageMD_Cov.Demo.Dx_wo.Site, glm.AverageMD_Cov.Demo.Dx)
anova(glm.AverageRD_Cov.Demo.Dx_wo.Site, glm.AverageRD_Cov.Demo.Dx)
anova(glm.AverageAD_Cov.Demo.Dx_wo.Site, glm.AverageAD_Cov.Demo.Dx)



