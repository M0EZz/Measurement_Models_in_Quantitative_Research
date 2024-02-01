######### RMCW Exercises - WEEK 2

## Set working directory
setwd("/Users/YOUR_DIRECTORY_HERE")

## Loading packages
library(tidyverse)
library(ggplot2)
library(QuantPsyc)
library(car)
library(lm.beta)
library(haven)
library(psych)
library(gmodels)
library(broom)
library(jtools)

# Load the dataset
ong1 <- read_sav("NAME_OF_DOWNLOADED_DATASET_HERE.sav")

##### 0) Run "hierarchical" models with multiple independent variables

# You can include multiple independent variables (IVs) in the regression by using '+'

# Block A: Age and Gender 
reg1 <- lm(FB_Status ~ Age + Gender, data = ong1)
summary(reg1)

# Block B: Age, Gender and NEO_FFI
reg2 <- lm(FB_Status ~ Age + Gender + NEO_FFI, data = ong1)
summary(reg2)

# Block C: Age, Gender, NEO_FFI and NPQC_R 
reg3 <- lm(FB_Status ~ Age + Gender + NEO_FFI + NPQC_R, data = ong1)
summary(reg3)


##### 1) Checking model assumptions

# We use the plot() function with some handy default plot settings, which can be accessed by typing plot(model_name, 1), plot(model_name, 2), plot(model_name, 3) etc.

# A) Linearity: Plot residuals vs fitted plot
plot(reg3, 1) # A horizontal line, without distinct patterns is an indication for a linear relationship

# B) Homoscedasticity: Scale-Location plot (standardized residuals vs. fitted (predicted) values)
plot(reg3, 3) # Horizontal line with equally spread points is a good indication of homoscedasticity

# # Make the SPSS plot instead: Plot standardized residuals against standardized fitted values
# ong1_filtered = filter(ong1, !is.na(FB_Status) & !is.na(Age) & !is.na(Gender) & !is.na(NEO_FFI) & !is.na(NPQC_R))
# ong1_filtered$res = reg3$residuals
# ong1_filtered$res_std = scale(ong1_filtered$res)
# ong1_filtered$fitted_values = reg3$fitted.values
# ong1_filtered$fitted_values_std = scale(onsg1_filtered$fitted_values)
# plot(ong1_filtered$res_std ~ ong1_filtered$fitted_values_std)

# C) Independence of the residuals: Durbin Watson (from car package)
durbinWatsonTest(reg3)

# D) Normal distribution of the residuals: Normal Q-Q plot
hist(reg3$residuals) 
plot(reg3, 2) # Residuals are normally distributed if the points follow the straight dashed line

# E) No multicollinearity: Check variance inflation factor
vif(reg3)

# F) No influential observations: Check Cooks distance (reflects leverage + residual size)
plot(reg3, 4)


##### 2) Examine models further

# Check the F value [ F(df0,df1) = test value ] for reg2
summary(reg2) # where df0 = 3, df1 = 247, F = 4.308 and p = 0.006

### Model comparison

# Check R2 across models
summary(reg1) # R2 = 0.035
summary(reg2) # R2 = 0.050
summary(reg3) # R2 = 0.083

# How much more explained variance in Block B than Block A?
5 - 3.5

# How much more explained variance in Block C than Block B?
8.3 - 5

# Use anova() function to compare models and see whether adding the additional variable significantly improves the model fit
anova(reg1, reg2) # p = 0.050
anova(reg2, reg3) # p = 0.003

# Reg1: Who changes their Facebook status more often, a boy or a girl?
summary(reg1)

# Reg1: Predicted number of FB_Status updates for a 14 year old girl
6.780 + 14 * (-0.342) + 0 * (-0.687)

# Effect of gender in Block A and Block B
summary(reg1)
summary(reg2)

# Reg3: Which variable has the greatest effect on Y?
lm.beta(reg3) # To compare the sizes of the effects of different variables, we need to check the standardized coefficients


### Practice with categorical variables: Grade

# R does not know that Grade is a categorical variable, because it is coded as numeric. To make it categorical, you have two options:

# a) Use as.factor() to tell R this is a categorical variable
ong1$Grade_categ <- as.factor(ong1$Grade)

# b) Alternatively, you can make a new variable with ifelse() and use text instead of numbers as level names. This way R cannot confuse the variable for a numeric, non-categorical variable
ong1$Grade_categ2 <- ifelse(ong1$Grade == 1, "Sec 1", 
                            ifelse(ong1$Grade == 2, "Sec 2", "Sec 3"))

# Not necessary here, but you may need to manually give R the order of categories to get the right reference level (the default in R is the alphabetically first level)
ong1$Grade_categ2 <- factor(ong1$Grade_categ2, levels = c("Sec 1", "Sec 2", "Sec 3"))

# Note that R uses dummy coding by default if there is a categorical variable

# Re-run the model from Block C including categorical Grade
reg4 <- lm(FB_Status ~ Age + Gender + Grade_categ2 + NEO_FFI + NPQC_R, data = ong1)
summary(reg4)

# Does Grade make the model better? Use anova()
anova(reg3, reg4)


### Practice with categorical variables: Age

# Calculate a new categorical variable for age (from continuous Age variable) and run model analogous to reg1 with this variable. 
# Compare this model with a model that uses the continuous age measure

ong1$Age_categories <- ifelse(ong1$Age < 15, "early teens", 
                             ifelse(ong1$Age > 15, "late teens", "mid teens"))

reg5_age_cont <- lm(FB_Status ~ Age, data = ong1)
summary(reg5_age_cont)

reg5_age_categ <- lm(FB_Status ~ Age_categories, data = ong1)
summary(reg5_age_categ)

