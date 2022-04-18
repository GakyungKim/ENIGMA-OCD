# violin plot
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(rstatix)
library(ggpubr)
library(magrittr)

rm(list = ls())

setwd("C:/Users/±èº¸°â/Desktop/Connectome/study-enigma ocd/ENIGMA-OCD/0.Data/Analysis/1_Data split/2.pediatric")


#### Load dataset ####
df_pedi_total <- read_csv('T.Dx_Pediatric_317_v.21.12.18.csv') 


#### check data type ####
str(df_pedi_total)
df_pedi_total$Dx <- as.factor(df_pedi_total$Dx)
df_pedi_total$Sex <- as.factor(df_pedi_total$Sex)
df_pedi_total$Med <- as.factor(df_pedi_total$Med)
df_pedi_total$Agr_Check <- as.factor(df_pedi_total$Agr_Check)
df_pedi_total$Clean <- as.factor(df_pedi_total$Clean)
df_pedi_total$Sex_Rel <- as.factor(df_pedi_total$Sex_Rel)
df_pedi_total$Hoard <- as.factor(df_pedi_total$Hoard)
df_pedi_total$Ord <- as.factor(df_pedi_total$Ord)
df_pedi_total$Anx <- as.factor(df_pedi_total$Anx)
df_pedi_total$CurAnx <- as.factor(df_pedi_total$CurAnx)
df_pedi_total$Dep <- as.factor(df_pedi_total$Dep)
df_pedi_total$CurDep <- as.factor(df_pedi_total$CurDep)
# df_pedi_total$Site_int <- as.factor(df_pedi_total$Site_int)
# df_pedi_total$Med_01.0.3. <- as.factor(df_pedi_total$Med_01.0.3.)
# df_pedi_total$Med_12 <- as.factor(df_pedi_total$Med_12)

#### Trim column name for visualization ####
df_pedi_total <- rename(df_pedi_total, Severity = Sev, 'Age at onset' = AO, 'Duration of illness' = Dur, 'Average FA' = AverageFA, 'Average MD' = AverageMD, 'Average RD' = AverageRD, 'Average AD' = AverageAD)



############################################################################################

#### Make Site_int column ####
df_pedi_total$Site %>% table()

df_pedi_total %<>% 
  mutate(Site_int = ifelse(Site == 'Bangalore', 1, 
                           ifelse(Site == 'Barcelona', 2, 
                                  ifelse(Site == 'British Columbia', 3,
                                         ifelse(Site == 'Calgary', 4,
                                                ifelse(Site == 'Chiba', 5,
                                                       ifelse(Site == 'Oxford', 6, 
                                                              ifelse(Site == 'Yale', 7, 
                                                                     ifelse(Site == 'Zurich', 8, NA)))))))))




#### 1. Age #### 
# Site <- df_pedi_total$Site %>% table() %>% row.names()
# df_pedi_total$Site <- factor(Site, levels = Site)

Age <- df_pedi_total %>% 
  mutate(Site = fct_reorder(Site, desc(Site_int))) %>%
  ggplot(.) +
  geom_violin(aes(x = Site, y = Age, fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y = 'Age') +
  theme_test() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c('#476a24', '#1c320c', '#217638', '#40731b', '#60ac29',
                               '#70c930', '#a0ce72', '#c4e0a7'))

Age
#### 2. Severity ####
Severity <- df_pedi_total %>% 
  mutate(Site = fct_reorder(Site, desc(Site_int))) %>%
  ggplot(.) +
  geom_violin(aes(x = Site, y = Severity, fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y = 'Severity') +
  theme_test() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c('#476a24', '#1c320c', '#217638', '#40731b', '#60ac29',
                               '#70c930', '#a0ce72', '#c4e0a7'))

Severity


#### 3. Age onset ####
'Age of onset'  <- df_pedi_total %>% 
  mutate(Site = fct_reorder(Site, desc(Site_int))) %>%
  ggplot(.) +
  geom_violin(aes(x = Site, y = `Age at onset` , fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y = 'Age of onset' ) +
  theme_test() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c('#476a24', '#1c320c', '#217638', '#40731b', '#60ac29',
                               '#70c930', '#a0ce72', '#c4e0a7'))

`Age of onset`

#### 4. Duration of illness####
'Duration of illness'  <- df_pedi_total %>% 
  mutate(Site = fct_reorder(Site, desc(Site_int))) %>%
  ggplot(.) +
  geom_violin(aes(x = Site, y = `Duration of illness` , fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y = 'Duration of illness' ) +
  theme_test() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c('#476a24', '#1c320c', '#217638', '#60ac29',
                               '#70c930', '#c4e0a7'))

`Duration of illness`


#### 5. Average FA ####
'Average FA' <-  df_pedi_total %>% 
  mutate(Site = fct_reorder(Site, desc(Site_int))) %>%
  ggplot(.) +
  geom_violin(aes(x = Site, y =`Average FA` , fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y = 'Average FA'  ) +
  theme_test() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c('#476a24', '#1c320c', '#217638', '#40731b', '#60ac29',
                               '#70c930', '#a0ce72', '#c4e0a7'))
`Average FA`

#### 6. Average MD ####
'Average MD' <-  df_pedi_total %>% 
  mutate(Site = fct_reorder(Site, desc(Site_int))) %>%
  filter(`Average MD` < 0.02) %>% 
  ggplot(.) +
  geom_violin(aes(x = Site, y =`Average MD` , fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y = 'Average MD'  ) +
  theme_test() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c('#476a24', '#1c320c', '#217638', '#40731b', '#60ac29',
                               '#70c930', '#a0ce72', '#c4e0a7'))
`Average MD`

`Average MD`
#### 7. Average RD ####
'Average RD' <-  df_pedi_total %>% 
  filter(`Average RD` < 0.02) %>% 
  mutate(Site = fct_reorder(Site, desc(Site_int))) %>%
  ggplot(.) +
  geom_violin(aes(x = Site, y =`Average RD` , fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y = 'Average RD'  ) +
  theme_test() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c('#476a24', '#1c320c', '#217638', '#40731b', '#60ac29',
                               '#70c930', '#a0ce72', '#c4e0a7'))
`Average RD` 


#### 8. Average AD ####
'Average AD' <-  df_pedi_total %>% 
  filter(`Average RD` < 0.03) %>% 
  mutate(Site = fct_reorder(Site, desc(Site_int))) %>%
  ggplot(.) +
  geom_violin(aes(x = Site, y =`Average AD` , fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y = 'Average AD'  ) +
  theme_test() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c('#476a24', '#1c320c', '#217638', '#40731b', '#60ac29',
                               '#70c930', '#a0ce72', '#c4e0a7'))
`Average AD`

#### Add theme ####
# 1. Age
Age = Age +  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.title = element_text(size=8), legend.position = 'none' )
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


# age

# left
df_pedi_total %>% 
  mutate(Site = fct_reorder(Site, desc(Site_int))) %>%
  filter(`Average MD` < 0.02) %>% 
  ggplot(.) +
  geom_violin(aes(x = Site, y =`Average MD` , fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y = 'Average MD'  ) +
  theme_test() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c('#476a24', '#1c320c', '#217638', '#40731b', '#60ac29',
                               '#70c930', '#a0ce72', '#c4e0a7'))

# right
df_pedi_total %>% 
  mutate(Site = fct_reorder(Site, Site_int)) %>%
  ggplot(.) +
  geom_violin(aes(x = Site, y = Age, fill = Site)) +
  coord_flip() +
  labs(x = 'Site', y = 'Age') +
  theme_test() +
  scale_fill_manual(values = c('#c4e0a7', '#a0ce72', '#70c930', 
                                '#60ac29', '#40731b', '#217638', '#1c320c', '#476a24' ))













