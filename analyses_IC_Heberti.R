# Set directory ####

setwd("C:/Users/User/Desktop/Análises R/Pós-doc/Biobanco/Myocardial hypertrophy and cognition - IC Heberti")

# Library packages ####

library(tidyverse)
library(car)
library(table1)
library(RVAideMemoire)#shapiro by groups 
library(rstatix) #outliers
library(DescTools) #post-hoc anova
library(FSA) #post-hoc kruskal wallis

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

# Download and upload the clean data #### 

#write.table(df_lvh, file = "df_lvh_clean.csv", sep = ',')#1067

df_lvh <- read.csv("df_lvh_clean.csv")

# Recoding variables ####

glimpse(df_lvh)

## CDR 

table(df_lvh$escore_cdr_final)

df_lvh$cdr_binary <- ifelse(df_lvh$escore_cdr_final >=0.5, 1, 0)

table(df_lvh$cdr_binary)

df_lvh$cdr_binary <- factor(df_lvh$cdr_binary, 
                            levels = c(0,1), 
                            labels = c('CDR=0', 'CDR≥0.5'))

## Sex
df_lvh$deceased_sex <- factor(df_lvh$deceased_sex, 
                              levels = c(0,1), 
                              labels = c('Male', 'Female'))

table(df_lvh$deceased_sex)

## Race

table(df_lvh$race)

df_lvh$race <- factor(df_lvh$race, 
                      levels = c(1,2,3,4), 
                      labels = c('White', 'Black', 'Black', 'Asian'))


## Hypertension 

table(df_lvh$hypertension_yn)

df_lvh$hypertension_yn[df_lvh$hypertension_yn == 0] <- 'Absence'
df_lvh$hypertension_yn[df_lvh$hypertension_yn == 1] <- 'Presence'

df_lvh$hypertension_yn <- as.factor(df_lvh$hypertension_yn)

## Diabetes

df_lvh$diabetes_yn <- factor(df_lvh$diabetes_yn, 
                             levels = c(0,1), 
                             labels = c('Absence', 'Presence'))

table(df_lvh$diabetes_yn)


## Heart disease 

table(df_lvh$heart_disease)

df_lvh$heart_disease <- factor(df_lvh$heart_disease, 
                             levels = c(0,1), 
                             labels = c('Absence', 'Presence'))


## Dyslipidemia

table(df_lvh$dyslipidemia_yn)

df_lvh$dyslipidemia_yn <- factor(df_lvh$dyslipidemia_yn, 
                               levels = c(0,1), 
                               labels = c('Absence', 'Presence'))

## Alcohol

table(df_lvh$alcohol_yn)

df_lvh$alcohol_yn <- factor(df_lvh$alcohol_yn, 
                            levels = c(0:3), 
                            labels = c('Never', 'Current', 'Current', 'Past'))

## Smoking

table(df_lvh$smoking_yn)

df_lvh$smoking_yn <- factor(df_lvh$smoking_yn, 
                            levels = c(0:2), 
                            labels = c('Never', 'Current', 'Past'))

## physical activity

table(df_lvh$physical_activity)

df_lvh$physical_activity <- factor(df_lvh$physical_activity, 
                            levels = c(0,1), 
                            labels = c('No', 'Yes'))

# Descriptive analysis - Table 1 ####

glimpse(df_lvh)

label(df_lvh$age) <- 'Age (y), Mean±SD'

glimpse(df_lvh)

# Create a descriptive table split into the two CDR categories ####

table1::table1(~age + 
               education_deceased + 
               deceased_sex + 
               race + 
               hypertension_yn + 
               diabetes_yn+ 
               dyslipidemia_yn+ 
               heart_disease+
               bmi+ 
               physical_activity+ 
               smoking_yn+ 
               alcohol_yn+ 
               left_ventr_wall_thickness | cdr_binary, data = df_lvh, overall=c(left='Overall'))

# Calculate the p-values ####

## Age ####

### Distribution 

hist(df_lvh$age)

df_lvh %>% ggplot(aes(x=age))+
  geom_histogram(aes(fill = cdr_binary))

### Check the homogeneity of variances 

leveneTest(age ~ cdr_binary, data = df_lvh, center = mean) 

### t test

t.test(age ~ cdr_binary, data = df_lvh, var.equal=FALSE)

## Education ####

### Distribution 

hist(df_lvh$education_deceased)

df_lvh %>% ggplot(aes(x=education_deceased))+
  geom_histogram(aes(fill=cdr_binary))

### Check the homogeneity of variances

leveneTest(education_deceased ~ cdr_binary, data = df_lvh, center = mean)

### t test

t.test(education_deceased ~ cdr_binary, data = df_lvh, var.equal = FALSE)

### Mann-Whitney-Wilcoxon Test 

wilcox.test(education_deceased ~ cdr_binary, data = df_lvh)

### Median and interquartile range by groups 

df_lvh %>% group_by(cdr_binary) %>% 
  summarise(median = median(education_deceased),
            Q1 = quantile(education_deceased, 0.25),
            Q3 = quantile(education_deceased, 0.75))

## BMI ####

### Distribution 

hist(df_lvh$bmi)

df_lvh %>% ggplot(aes(x=bmi))+
  geom_histogram(aes(fill=cdr_binary))

### Check the homogeneity of variances

leveneTest(bmi ~ cdr_binary, data = df_lvh, center = mean)

### t test

t.test(bmi ~ cdr_binary, data = df_lvh, var.equal = TRUE)

## Sex ####

chisq.test(df_lvh$deceased_sex, df_lvh$cdr_binary)

## Race ####

fisher.test(df_lvh$race, df_lvh$cdr_binary)

## Hypertension ####

chisq.test(df_lvh$hypertension_yn, df_lvh$cdr_binary)

## Diabetes ####

chisq.test(df_lvh$diabetes_yn, df_lvh$cdr_binary)

## Dyslipidemia ####

chisq.test(df_lvh$dyslipidemia_yn, df_lvh$cdr_binary)

## Heart disease ####

chisq.test(df_lvh$heart_disease, df_lvh$cdr_binary)

## Physical activity ####

chisq.test(df_lvh$physical_activity, df_lvh$cdr_binary)

## Smoking ####

chisq.test(df_lvh$smoking_yn, df_lvh$cdr_binary)

## Alcohol ####

chisq.test(df_lvh$alcohol_yn, df_lvh$cdr_binary)

## Left ventricular wall thickness (cm) ####

### Distribution 

hist(df_lvh$left_ventr_wall_thickness)

df_lvh %>% ggplot(aes(x=left_ventr_wall_thickness))+
  geom_histogram(aes(fill=cdr_binary))

## Check the homogeneity of variances 

leveneTest(left_ventr_wall_thickness ~ cdr_binary, center = 'mean', data = df_lvh)

## t test

t.test(left_ventr_wall_thickness ~ cdr_binary, var.equal = F, data = df_lvh)

# Descriptive analysis - Table 2 ####

## CDR categorized into three levels: CDR = 0, CDR = 0.5 CDR ≥ 1

table(df_lvh$escore_cdr_final)

df_lvh <- 
  df_lvh %>% mutate(cdr_cat = factor(case_when(escore_cdr_final == 0 ~ "CDR = 0", 
                                             escore_cdr_final == 0.5 ~ "CDR = 0.5", 
                                             escore_cdr_final >= 1 ~ "CDR ≥ 1")))
table(df_lvh$cdr_cat)

# Create a descriptive table split into the three CDR categories ####

table1(~age + 
         education_deceased + 
         deceased_sex + 
         race + 
         hypertension_yn + 
         diabetes_yn+ 
         dyslipidemia_yn+ 
         heart_disease+
         bmi+ 
         physical_activity+ 
         smoking_yn+ 
         alcohol_yn+ 
         left_ventr_wall_thickness | cdr_cat, data = df_lvh, 
       overall=c(left='Overall'))

# Calculate the p-values ####

## Age ####

### Distribution 

df_lvh %>% ggplot(aes(x=age))+
  geom_histogram(aes(fill=cdr_cat))

byf.shapiro(age ~ cdr_cat, df_lvh)

### Check the homogeneity of variances

leveneTest(age ~ cdr_cat, center = 'mean', data = df_lvh)

### Check for outliers

df_lvh %>% group_by(cdr_cat) %>% 
  identify_outliers(age)

boxplot(age ~ cdr_cat, data = df_lvh)

### ANOVA

m1 <- aov(age ~ cdr_cat, data = df_lvh)
summary(m1)

### Post-hoc test 

PostHocTest(m1, method = "hsd", confi.level = 0.95)#TukeyHSD

## Education ####

### Distribution 

df_lvh %>% ggplot(aes(x=education_deceased))+
  geom_histogram(aes(fill=cdr_cat))

byf.shapiro(education_deceased ~ cdr_cat, df_lvh)

### Check the homogeneity of variances

leveneTest(education_deceased ~ cdr_cat, center = 'mean', data = df_lvh)

### Check for outliers

df_lvh %>% group_by(cdr_cat) %>% 
  identify_outliers(education_deceased)

boxplot(education_deceased ~ cdr_cat, data = df_lvh)

### Kruskall Wallis

kruskal.test(education_deceased ~ cdr_cat, data = df_lvh)

### Post-hoc test 

dunnTest(education_deceased ~ cdr_cat, data = df_lvh, method="bh")

dunn_test(education_deceased ~ cdr_cat, data = df_lvh, p.adjust.method = "bonferroni")#rstatix

## BMI ####

### Distribution 

df_lvh %>% ggplot(aes(x=bmi))+
  geom_histogram(aes(fill=cdr_cat))

byf.shapiro(bmi ~ cdr_cat, df_lvh)

### Check the homogeneity of variances

leveneTest(bmi ~ cdr_cat, center = 'mean', data = df_lvh)

### Check for outliers

df_lvh %>% group_by(cdr_cat) %>% 
  identify_outliers(bmi)

boxplot(bmi ~ cdr_cat, data = df_lvh)

### ANOVA

m1 <- aov(bmi ~ cdr_cat, data = df_lvh)
summary(m1)

### Post-hoc test 

PostHocTest(m1, method = "hsd", confi.level = 0.95)#TukeyHSD

## Left ventricular wall thickness (cm) ####

### Distribution 

df_lvh %>% ggplot(aes(x=left_ventr_wall_thickness))+
  geom_histogram(aes(fill=cdr_cat))

byf.shapiro(left_ventr_wall_thickness ~ cdr_cat, df_lvh)

### Check the homogeneity of variances

leveneTest(left_ventr_wall_thickness ~ cdr_cat, center = 'mean', data = df_lvh)

### Check for outliers

df_lvh %>% group_by(cdr_cat) %>% 
  identify_outliers(left_ventr_wall_thickness)

boxplot(left_ventr_wall_thickness ~ cdr_cat, data = df_lvh)

### ANOVA

m1 <- aov(left_ventr_wall_thickness ~ cdr_cat, data = df_lvh)
summary(m1)

### Kruskall Wallis

kruskal.test(left_ventr_wall_thickness ~ cdr_cat, data = df_lvh)

# Linear regressions ####

## Unadjusted model 

m1 <- lm(cdr_sb ~ left_ventr_wall_thickness, data = df_lvh)
summary(m1)
hist(m1$residuals)

## Model adjusted for sociodemographic factors 

m2 <- lm(cdr_sb ~ left_ventr_wall_thickness
         + age
         + education_deceased
         + deceased_sex
         + race, data = df_lvh)

summary(m2)
confint(m2)
hist(m2$residuals)

## Model adjusted for sociodemographic and clinical factors 

m3 <- lm(cdr_sb ~ left_ventr_wall_thickness
         + age
         + education_deceased
         + deceased_sex
         + race
         + diabetes_yn
         + heart_disease
         + dyslipidemia_yn
         + bmi
         + smoking_yn
         + alcohol_yn
         + physical_activity, data = df_lvh)
summary(m3)
hist(m3$residuals)
confint(m3)

#Test for multicollinearity

m <- lm(cdr_sb ~ left_ventr_wall_thickness
        + age
        + education_deceased
        + deceased_sex
        + race
        + diabetes_yn
        + heart_disease
        + dyslipidemia_yn
        + bmi
        + smoking_yn
        + alcohol_yn
        + physical_activity, data = df_lvh)

car::vif(m)#multicollinearity: vif >10

# Logistic regression analysis (CDR=0, CDR≥0.5) ####

levels(df_lvh$cdr_binary)
class(df_lvh$cdr_binary)
summary(df_lvh$left_ventr_wall_thickness)

## Unadjusted model 

m1 <- glm(cdr_binary ~ left_ventr_wall_thickness, family = "binomial", data = df_lvh)
summary(m1)
exp(coef(m1))
exp(confint(m1))

## Model adjusted for sociodemographic factors 

m2 <- glm(cdr_binary ~ left_ventr_wall_thickness
         + age
         + education_deceased
         + deceased_sex
         + race, family = "binomial", data = df_lvh)
summary(m2)
exp(coef(m2))
exp(confint(m2))

## Model adjusted for sociodemographic and clinical factors 

m3 <- glm(cdr_binary ~ left_ventr_wall_thickness
         + age
         + education_deceased
         + deceased_sex
         + race
         + diabetes_yn
         + heart_disease
         + dyslipidemia_yn
         + bmi
         + smoking_yn
         + alcohol_yn
         + physical_activity, family = "binomial" ,data = df_lvh)
summary(m3$coefficients[,])
m3$coefficients[4,2]
exp(coef(m3))
exp(confint(m3))

#Test for multicollinearity

m <- lm(as.numeric(cdr_binary) ~ left_ventr_wall_thickness
        + age
        + education_deceased
        + deceased_sex
        + race
        + diabetes_yn
        + heart_disease
        + dyslipidemia_yn
        + bmi
        + smoking_yn
        + alcohol_yn
        + physical_activity, data = df_lvh)

car::vif(m)#multicollinearity: vif >10


# Ordinal Logistic regression analysis (CDR=0, CDR=0.5, CDR≥1) ####

levels(df_lvh$cdr_cat)
class(df_lvh$cdr_cat)

## Unadjusted model 

m1 <- MASS::polr(cdr_cat ~ left_ventr_wall_thickness, data = df_lvh, Hess = T)
#overall results
car::Anova(m1, type = "II", test = "Wald")

#specific results
ctable <- coef(summary(m1))#coefficients 
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2#p-values for both the right and left tails combined.
ctable <- as.data.frame(round(cbind(ctable, "p value" = p), 4))
#or:
lmtest::coeftest(m1)

#odds ratio and 95% CI
exp(cbind(OR = coef(m1), confint(m1)))

#OR - 1: risk percentage (increase, if positive, or decreased, if negative)

## Model adjusted for sociodemographic factors

m2 <- MASS::polr(cdr_cat ~ left_ventr_wall_thickness
                 + age
                 + education_deceased
                 + deceased_sex
                 + race, data = df_lvh, Hess = T)
#overall results
car::Anova(m2, type = "II", test = "Wald")

#specific results
ctable <- coef(summary(m2))
p <- pnorm(ctable[, "t value"], lower.tail = FALSE)*2#p-values for both the right and left tails combined.
ctable <- as.data.frame(round(cbind(ctable, "p value" = p), 4))

#odds ratio and 95% CI
exp(cbind(OR = coef(m2), confint(m2)))

#OR - 1: risk percentage (increase, if positive, or decreased, if negative)

## Model adjusted for sociodemographic and clinical factors

m3 <- MASS::polr(cdr_cat ~ left_ventr_wall_thickness
                 + age
                 + education_deceased
                 + deceased_sex
                 + race
                 + diabetes_yn
                 + heart_disease
                 + dyslipidemia_yn
                 + bmi
                 + smoking_yn
                 + alcohol_yn
                 + physical_activity, data = df_lvh, Hess = T)
#overall results
car::Anova(m3, type = "II", test = "Wald")

#specific results
ctable <- coef(summary(m3))
p <- pnorm(ctable[, "t value"], lower.tail = FALSE)*2 #p-values for both the right and left tails combined.
ctable <- as.data.frame(round(cbind(ctable, "p value" = p), 4))
ctable

#odds ratio and 95% CI
exp(cbind(OR = coef(m3), confint(m3)))

#OR - 1: risk percentage (increase, if positive, or decreased, if negative)

# Ordinal Logistic regression analysis (CDR divided into 6 categories) ####

