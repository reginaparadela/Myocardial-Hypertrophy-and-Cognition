setwd("/Users/rparadela/Desktop/Análises R/Pós-doc/Biobanco/Myocardial hypertrophy and cognition - IC Heberti")

# Loading packages ####
library(tidyverse)

# Read in the data ####

df_lvh <- read.csv("df_lvh_clean_cid.csv")

glimpse(df_lvh)

# Setting the variables ####

df_lvh$deceased_sex <- factor(df_lvh$deceased_sex, levels = c(0,1), labels = c("M", "F"))
df_lvh$race <- factor(df_lvh$race, levels = c(1,2,3,4), labels = c("White", "Black", "Black", "Asian"))
df_lvh$hypertension_yn <- factor(df_lvh$hypertension_yn, levels = c(0,1), labels = c("No", "Yes"))
df_lvh$diabetes_yn <- factor(df_lvh$diabetes_yn, levels = c(0,1), labels = c("No", "Yes"))
df_lvh$heart_disease <- factor(df_lvh$heart_disease, levels = c(0,1), labels = c("No", "Yes"))
df_lvh$dyslipidemia_yn <- factor(df_lvh$dyslipidemia_yn, levels = c(0,1), labels = c("No", "Yes"))
df_lvh$stroke_yn <- factor(df_lvh$stroke_yn, levels = c(0,1), labels = c("No", "Yes"))
df_lvh$smoking_yn <- factor(df_lvh$smoking_yn, levels = c(0,1,2), labels = c('Never', 'Current', 'Past'))
df_lvh$alcohol_yn <- factor(df_lvh$alcohol_yn, levels = c(0,1,2,3), labels = c("Never", "Current", "Current", "Past"))
df_lvh$physical_activity <- factor(df_lvh$physical_activity, levels = c(0,1), labels = c("No", "Yes"))
df_lvh$cdr_binary <- factor(df_lvh$cdr_binary, levels = c(0,1), labels = c("CDR = 0", "CDR ≥ 0.5"))
df_lvh$cdr_cat <- factor(df_lvh$cdr_cat)
df_lvh$causa <- as.factor(df_lvh$causa)

glimpse(df_lvh)


# Stratify by cause death ####

#df_lvh$causa = causa da morte baseada no cid 

df_lvh <- df_lvh %>% mutate(cardio_death = 
                              ifelse(df_lvh$causa == 'I', 1, 0))

table(df_lvh$cardio_death)

#cardiovascular = 1 
#non-cardivascular = 0

cardio_death1 <- df_lvh %>% filter(cardio_death == 1)
cardio_death0 <- df_lvh %>% filter(cardio_death == 0)

# Regression analyses on cardio_death1 ####


# Regression analyses on cardio_death1 ####