setwd("/Users/rparadela/Desktop/Análises R/Pós-doc/Biobanco/Myocardial hypertrophy and cognition - IC Heberti")

# Loading packages ####
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("car")) install.packages("car")
if(!require("RVAideMemoire")) install.packages("RVAideMemoire")#shapiro by groups 
if(!require("rstatix")) install.packages("rstatix")#outliers
if(!require("DescTools")) install.packages("DescTools")#post-hoc anova
if(!require("(FSA")) install.packages("FSA")#post-hoc kruskal wallis

# Read in the data ####

df_lvh <- read.csv("df_lvh_clean.csv")

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

glimpse(df_lvh)

# Checking model assumptions ####

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

# Interaction analysis ####

## Model adjusted for sociodemographic and clinical factors 

# Age 

df_lvh$age_cat <-  ifelse(df_lvh$age >= 75, 1, 0)
df_lvh$age_cat <- as.factor(df_lvh$age_cat)


m3 <- lm(cdr_sb ~ left_ventr_wall_thickness*age_cat
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

# Sex 

m3 <- lm(cdr_sb ~ left_ventr_wall_thickness*deceased_sex
         + age
         + education_deceased
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

# Race  

m3 <- lm(cdr_sb ~ left_ventr_wall_thickness*race
         + age
         + education_deceased
         + deceased_sex
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

# Education 

df_lvh$education_deceased_cat <- ifelse(df_lvh$education_deceased >=4, 1,0) 

m3 <- lm(cdr_sb ~ left_ventr_wall_thickness*education_deceased_cat
         + age
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

# Analysis with standardized variables ####

#The scale() function can be used to scale variables in any arbitrary way, 
#but the default is to standardize them.

#each observation is subtracted from the mean and divided by the std deviation of the mean

scale(df_lvh$left_ventr_wall_thickness)

## Linear regressions ####

#The output shows that a one standard deviation increase in left_ventr_wall_thickness
#leads to a X increase (if positive) or decrease (if negative) in the CDR-SOB score 

## Unadjusted model 

m1 <- lm(cdr_sb ~ scale(left_ventr_wall_thickness), data = df_lvh)
summary(m1)
hist(m1$residuals)

## Model adjusted for sociodemographic factors 

m2 <- lm(cdr_sb ~ scale(left_ventr_wall_thickness)
         + age
         + education_deceased
         + deceased_sex
         + race, data = df_lvh)

summary(m2)
confint(m2)
hist(m2$residuals)

## Model adjusted for sociodemographic and clinical factors 

m3 <- lm(cdr_sb ~ scale(left_ventr_wall_thickness)
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

m <- lm(cdr_sb ~ scale(left_ventr_wall_thickness)
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

# Interaction analysis ####

## Model adjusted for sociodemographic and clinical factors 

# Age 

df_lvh$age_cat <-  ifelse(df_lvh$age >= 75, 1, 0)
df_lvh$age_cat <- as.factor(df_lvh$age_cat)


m3 <- lm(cdr_sb ~ left_ventr_wall_thickness*age_cat
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

# Sex 

m3 <- lm(cdr_sb ~ left_ventr_wall_thickness*deceased_sex
         + age
         + education_deceased
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

# Race  

m3 <- lm(cdr_sb ~ left_ventr_wall_thickness*race
         + age
         + education_deceased
         + deceased_sex
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

# Education 

df_lvh$education_deceased_cat <- ifelse(df_lvh$education_deceased >=4, 1,0) 

m3 <- lm(cdr_sb ~ left_ventr_wall_thickness*education_deceased_cat
         + age
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

# Analysis with standardized variables ####

#The scale() function can be used to scale variables in any arbitrary way, 
#but the default is to standardize them.

#each observation is subtracted from the mean and divided by the std deviation of the mean

scale(df_lvh$left_ventr_wall_thickness)

## Linear regressions ####

#The output shows that a one standard deviation increase in left_ventr_wall_thickness
#leads to a X increase (if positive) or decrease (if negative) in the CDR-SOB score 

## Unadjusted model 

m1 <- lm(cdr_sb ~ scale(left_ventr_wall_thickness), data = df_lvh)
summary(m1)
hist(m1$residuals)

## Model adjusted for sociodemographic factors 

m2 <- lm(cdr_sb ~ scale(left_ventr_wall_thickness)
         + age
         + education_deceased
         + deceased_sex
         + race, data = df_lvh)

summary(m2)
confint(m2)
hist(m2$residuals)

## Model adjusted for sociodemographic and clinical factors 

m3 <- lm(cdr_sb ~ scale(left_ventr_wall_thickness)
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

m <- lm(cdr_sb ~ scale(left_ventr_wall_thickness)
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
