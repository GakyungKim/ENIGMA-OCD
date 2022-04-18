# violin plot
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(rstatix)
library(ggpubr)
library(magrittr)
rm(list = ls())
setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Analysis/1_Data split/1.adult")
#### Load dataset ####
df_adult_total <- read_csv('T.Dx_Adult_1336_v.cleaned.21.11.22.csv') 


#### check data type ####
str(df_adult_total)
df_adult_total$Dx <- as.factor(df_adult_total$Dx)
df_adult_total$Sex <- as.factor(df_adult_total$Sex)
df_adult_total$Med <- as.factor(df_adult_total$Med)
df_adult_total$Agr_Check <- as.factor(df_adult_total$Agr_Check)
df_adult_total$Clean <- as.factor(df_adult_total$Clean)
df_adult_total$Sex_Rel <- as.factor(df_adult_total$Sex_Rel)
df_adult_total$Hoard <- as.factor(df_adult_total$Hoard)
df_adult_total$Ord <- as.factor(df_adult_total$Ord)
df_adult_total$Anx <- as.factor(df_adult_total$Anx)
df_adult_total$CurAnx <- as.factor(df_adult_total$CurAnx)
df_adult_total$Dep <- as.factor(df_adult_total$Dep)
df_adult_total$CurDep <- as.factor(df_adult_total$CurDep)
# df_adult_total$Site_int <- as.factor(df_adult_total$Site_int)
# df_adult_total$Med_01.0.3. <- as.factor(df_adult_total$Med_01.0.3.)
# df_adult_total$Med_12 <- as.factor(df_adult_total$Med_12)

#### Trim column name for visualization ####
df_adult_total <- rename(df_adult_total, Severity = Sev, 'Age at onset' = AO, 'Duration of illness' = Dur, 'Average FA' = AverageFA, 'Average MD' = AverageMD, 'Average RD' = AverageRD, 'Average AD' = AverageAD)



############################################################################################

#### Make Site_int column ####
df_adult_total %<>% 
  mutate(Site_int = ifelse(Site == 'Amsterdam', 1, 
                           ifelse(Site == 'Bangalore', 2, 
                                  ifelse(Site == 'Capetown', 3,
                                         ifelse(Site == 'Kyoto', 4,
                                                ifelse(Site == 'Milan', 5,
                                                       ifelse(Site == 'Mountsinai', 6, 
                                                              ifelse(Site == 'Munich', 7, 
                                                                     ifelse(Site == 'Rome', 8, 
                                                                            ifelse(Site == 'Saopaulo', 9, 
                                                                                   ifelse(Site == 'Seoul', 10, 
                                                                                          ifelse(Site == 'Shanghai', 11, NA))))))))))))

## reorder
df_adult_total %<>% mutate(Site = fct_reorder(Site, desc(Site_int)))


#### 1. Age #### 
# Site <- df_adult_total$Site %>% table() %>% row.names()
# df_adult_total$Site <- factor(Site, levels = Site)
Age <- df_adult_total %>% 
  ggplot(.) +
  geom_violin(aes(x = Site, y = Age, fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y = 'Age') +
  theme_test() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c('#280a3d', '#66105E', '#3d0f5b',  '#51147a',  '#661999', '#7a1eb7', 
                               '#8e23d6',  '#9e3ede', '#ae5de4',  '#cc9aee', '#dbb7f3'))
Age



#### 2. Severity ####
Severity = df_adult_total %>% 
  mutate(Site = fct_reorder(Site, desc(Site_int))) %>%
  ggplot(.) +
  geom_violin(aes(x = Site, y = Severity, fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y = 'Severity') +
  theme_test() +  theme(legend.position = "none") +
  scale_fill_manual(values = c('#280a3d', '#66105E', '#3d0f5b',  '#51147a',  '#661999', '#7a1eb7', 
                               '#8e23d6',  '#9e3ede', '#ae5de4',  '#cc9aee', '#dbb7f3'))

Severity

#### 3. Age onset ####
'Age of onset' = df_adult_total %>% 
  mutate(Site = fct_reorder(Site, desc(Site_int))) %>%
  ggplot(.) +
  geom_violin(aes(x = Site, y = `Age at onset`, fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y = 'Age of onset') +
  theme_test() +  theme(legend.position = "none") +
  scale_fill_manual(values = c('#280a3d', '#66105E', '#3d0f5b',  '#51147a',  '#661999', '#7a1eb7', 
                               '#8e23d6',  '#9e3ede', '#ae5de4',  '#cc9aee', '#dbb7f3'))


`Age of onset`


#### 4. Duration of illness####

'Duration of illness' =  df_adult_total %>% 
  mutate(Site = fct_reorder(Site, desc(Site_int))) %>%
  ggplot(.) +
  geom_violin(aes(x = Site, y = `Duration of illness`, fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y = 'Duration of illness') +
  theme_test() +  theme(legend.position = "none") +
  scale_fill_manual(values = c('#280a3d', '#66105E', '#3d0f5b',  '#51147a',  '#661999', '#7a1eb7', 
                               '#8e23d6',  '#9e3ede', '#ae5de4',  '#cc9aee', '#dbb7f3'))

#### 5. Average FA ####
'Average FA' = df_adult_total %>% 
  mutate(Site = fct_reorder(Site, desc(Site_int))) %>%
  ggplot(.) +
  geom_violin(aes(x = Site, y = `Average FA`, fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y ='Average FA') +
  theme_test() +  theme(legend.position = "none") +
  scale_fill_manual(values = c('#280a3d', '#66105E', '#3d0f5b',  '#51147a',  '#661999', '#7a1eb7', 
                               '#8e23d6',  '#9e3ede', '#ae5de4',  '#cc9aee', '#dbb7f3'))


#### 6. Average MD ####
'Average MD' = df_adult_total %>% 
  mutate(Site = fct_reorder(Site, desc(Site_int))) %>%
  ggplot(.) +
  geom_violin(aes(x = Site, y = `Average MD`, fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y ='Average MD') +
  theme_test() +  theme(legend.position = "none") +
  scale_fill_manual(values = c('#280a3d', '#66105E', '#3d0f5b',  '#51147a',  '#661999', '#7a1eb7', 
                               '#8e23d6',  '#9e3ede', '#ae5de4',  '#cc9aee', '#dbb7f3'))

#### 7. Average RD ####
'Average RD' = df_adult_total %>% 
  mutate(Site = fct_reorder(Site, desc(Site_int))) %>%
  ggplot(.) +
  geom_violin(aes(x = Site, y = `Average RD`, fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y ='Average RD') +
  theme_test() +  theme(legend.position = "none") +
  scale_fill_manual(values = c('#280a3d', '#66105E', '#3d0f5b',  '#51147a',  '#661999', '#7a1eb7', 
                               '#8e23d6',  '#9e3ede', '#ae5de4',  '#cc9aee', '#dbb7f3'))
  

#### 8. Average AD ####
'Average AD' =df_adult_total %>% 
  mutate(Site = fct_reorder(Site, desc(Site_int))) %>%
  ggplot(.) +
  geom_violin(aes(x = Site, y = `Average FA`, fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y ='Average AD') +
  theme_test() +  theme(legend.position = "none") +
  scale_fill_manual(values = c('#280a3d', '#66105E', '#3d0f5b',  '#51147a',  '#661999', '#7a1eb7', 
                               '#8e23d6',  '#9e3ede', '#ae5de4',  '#cc9aee', '#dbb7f3'))


#### Add theme ####
# 1. Age
Age =Age +  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.title = element_text(size=8), legend.position = 'none' )
# 2. Severity
Severity =Severity +  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.title = element_text(size=8), legend.position = 'none' )
# 3.`Age at onset`
`Age of onset` =`Age of onset` +  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.title = element_text(size=8), legend.position = 'none' ) 
# 4. `Duration of illness`
`Duration of illness` = `Duration of illness` + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.title = element_text(size=8), legend.position = 'none')

# 5. `Average FA`
`Average FA` = `Average FA` + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.title = element_text(size=8) , legend.position = 'none')

# `Average MD`
`Average MD` = `Average MD` + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.title = element_text(size=8) , legend.position = 'none' )

# `Average RD`
`Average RD`  = `Average RD` + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.title = element_text(size=8), legend.position = 'none' )

# `Average AD`
`Average AD`  = `Average AD` + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.title = element_text(size=8))



ggarrange(Age, Severity, `Age of onset`, `Duration of illness`, `Average FA`, `Average MD`, `Average RD`, `Average AD`, nrow=1, ncol=8)



# legend

df_adult_total %>% 
  mutate(Site = fct_reorder(Site, desc(Site_int))) %>%
  ggplot(.) +
  geom_violin(aes(x = Site, y = Age, fill = Site)) +
  labs(x = 'Site', y = 'Age') +
  theme_test() +
  scale_fill_manual(values = c('#280a3d', '#66105E', '#3d0f5b',  '#51147a',  '#661999', '#7a1eb7', 
                               '#8e23d6',  '#9e3ede', '#ae5de4',  '#cc9aee', '#dbb7f3'))






