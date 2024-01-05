# Set directory ####

setwd("/Users/rparadela/Desktop/Análises R/Pós-doc/Biobanco/Myocardial hypertrophy and cognition - IC Heberti")

# Library packages ####

library(tidyverse)

# Read in the data ####

biobanco <- read.csv('BiobancoParaEstudosE-Hebertiregina_DATA_2023-04-25_1956.csv')

cardio <- read.csv('BiobancoParaEstudosE-Hebertiregina_DATA_2023-04-11_1916.csv') %>% rename(nsvo = nsvo_id)

# Merge

df_lvh <- merge(cardio, biobanco, by = 'nsvo', all.x = T)

# View the data

glimpse(df_lvh)

# Check the main variables ####

## left ventricular hypertrophy

#Check variable type
class(df_lvh$left_ventr_wall_thickness)

#Convert string into numeric
df_lvh$left_ventr_wall_thickness <- as.numeric(df_lvh$left_ventr_wall_thickness)
#max: 999?

#Get the summary 
summary(df_lvh$left_ventr_wall_thickness)

## Clinical dementia rating sum of boxes

#Check variable type
class(df_lvh$cdr_sb)

#Get the summary
summary(df_lvh$cdr_sb)

## Clinical dementia rating 
class(df_lvh$escore_cdr_final)

df_lvh %>% count(escore_cdr_final)

# Cleaning ####

## Main variables

backup <- df_lvh #1491

df_lvh <- df_lvh %>% filter(age >=50)#1386

df_lvh <- df_lvh %>% filter(frequency_interaction <=2)#1323

df_lvh <- df_lvh %>% filter(!is.na(left_ventr_wall_thickness) & left_ventr_wall_thickness <999)#1122

df_lvh <- df_lvh %>% filter(!is.na(cdr_sb))

df_lvh <- df_lvh %>% filter(!is.na(escore_cdr_final))#1121

## Other variables 

## Age
summary(df_lvh$age)

## Education 
class(df_lvh$education_deceased)

df_lvh$education_deceased <- as.numeric(df_lvh$education_deceased)

summary(df_lvh$education_deceased)

df_lvh <- df_lvh %>% filter(!is.na(df_lvh$education_deceased))#1120

## Sex
table(df_lvh$deceased_sex, useNA = 'ifany')

## Race
df_lvh %>% count(race) 

df_lvh <- df_lvh %>% filter(race <=4)#1115

## Hypertension 
df_lvh %>% count(hypertension_yn)## 99

df_lvh <- df_lvh %>% filter(hypertension_yn <=1)#1112

## Diabetes
df_lvh %>% count(diabetes_yn)#999 

df_lvh <- df_lvh %>% filter(diabetes_yn <=1)#1111

## Heart disease 
glimpse(df_lvh)
df_lvh %>% count(cad_yn)
df_lvh %>% count(cardiac_failure_yn) 
df_lvh %>% count(arrhythmia_yn)

df_lvh$heart_disease <- ifelse(df_lvh$cad_yn == 1 | 
                                 df_lvh$cardiac_failure_yn == 1 |
                                 df_lvh$arrhythmia_yn == 1, 1, 0)



table(df_lvh$heart_disease)

## Dyslipidemia
df_lvh %>% count(dyslipidemia_yn)
df_lvh <- df_lvh %>% filter(dyslipidemia_yn <=1)#1110

## BMI
summary(df_lvh$bmi)

df_lvh <- df_lvh %>% filter(!is.na(bmi))#1109

## Alcohol
df_lvh %>% count(alcohol_yn)

df_lvh <- df_lvh %>% filter(alcohol_yn <=3)#1072

## Smoking

df_lvh %>% count(smoking_yn)

df_lvh <- df_lvh %>% filter(smoking_yn <=2)#1067

## physical activity 

df_lvh$physical_activity <- ifelse(df_lvh$physical_activity___0 == 0, 1, 
                                   ifelse(df_lvh$physical_activity___1 == 1, 1, 
                                          ifelse(df_lvh$physical_activity___2 == 1, 1, 
                                                 ifelse(df_lvh$physical_activity___3 == 1, 1, 
                                                        ifelse(df_lvh$physical_activity___4 == 1, 1, 0)))))
  


table(df_lvh$physical_activity, useNA = 'ifany')

## CDR 

table(df_lvh$escore_cdr_final)

df_lvh$cdr_binary <- ifelse(df_lvh$escore_cdr_final >=0.5, 1, 0)

table(df_lvh$cdr_binary)

## CDR categorized into three levels: CDR = 0, CDR = 0.5 CDR ≥ 1

df_lvh <- 
  df_lvh %>% mutate(cdr_cat = factor(case_when(escore_cdr_final == 0 ~ "CDR = 0", 
                                               escore_cdr_final == 0.5 ~ "CDR = 0.5", 
                                               escore_cdr_final >= 1 ~ "CDR ≥ 1")))
table(df_lvh$cdr_cat)

# Download clean data #### 

write.table(df_lvh, file = "df_lvh_clean.csv", sep = ',')#1067

df_lvh <- read.csv("df_lvh_clean.csv")

library(tidyverse)

glimpse(df_lvh)

