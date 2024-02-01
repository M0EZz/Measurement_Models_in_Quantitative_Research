
####################################################

rm(list = ls())

# -  Packages - #

list.of.packages <- c("lavaan", "semPlot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(lavaan)
library(semPlot)

# ------------- #

# -- Relevant information -- # 

cors <- '
0.377
0.158 0.245
-0.012 0.297 0.143
0.077 0.005 0.051 0.248
-0.048 0.064 0.039 0.375 0.188
-0.096 -0.008 0.000 0.391 0.218 0.760
-0.009 0.024 -0.007 0.263 0.148 0.753 0.707
-0.146 -0.051 0.008 0.402 0.195 0.680 0.762 0.600
0.390 0.242 0.108 -0.065 -0.069 -0.218 -0.271 -0.187 -0.316
0.264 0.192 0.104 -0.182 -0.128 -0.384 -0.437 -0.315 -0.417 0.587
'

sds <- '3.20 4.96 3.71 3.04 4.63 5.27 5.03 4.36 5.80 5.47 5.05'

covs <- getCov(cors, diagonal = FALSE, sds = sds, names = c("DA", "CM", "PE", "PS", "OR", "VI", "DE", "AB", "EF", "EX", "CY"))
n <- 482

# Procedures 

# 1) Descriptive Statistics --> We don't have to do this since we don't have the proper data
# 2) Confirmatory Factor Analysis 
# 3) Structural Equation Model

# -- Confirmatory Factor Analysis -- #

# Model 1: assumed a single factor solution of the two measures; study related well-being

# We expect to have Chi2 = 146.59, df = 9, GFI = 0.91, AGFI = 0.78, CFI = 0.89, RMSEA = 0.18, AIC = 170.59, CAIC = 232.72

fit.indices <- c("chisq", "df", "gfi", "agfi", "cfi", "rmsea", "AIC")

Model_1 <- '
           WB =~ VI + DE + AB + EF + EX + CY
'

Model_1.fit <- cfa(Model_1, sample.cov = covs, sample.nobs = n, std.lv = TRUE)

summary(Model_1.fit, rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)
fitMeasures(Model_1.fit, fit.indices)

# Model 2: Exhaustion, Cynicism, and Efficiency load on burnout and Vigor, Dedication, and Absorption load on engagement.

# We expect to have Chi2 = 129.69, df = 8, GFI = 0.92, AGFI = 0.80, CFI = 0.91, RMSEA = 0.17, AIC = 148.00, CAIC = 215.32

Model_2 <- '
           BNT =~ EX + CY + EF 
           ENG =~ VI + DE + AB
           
           BNT ~~ ENG
'

Model_2.fit <- cfa(Model_2, sample.cov = covs, sample.nobs = n, std.lv = TRUE)
summary(Model_2.fit, rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)
fitMeasures(Model_2.fit, fit.indices)

# Model 3: Efficiency loading on engagement instead of burnout

# We expect to have Chi2 = 46.25, df = 8, GFI = 0.97, AGFI = 0.92, CFI = 0.97, RMSEA = 0.08, AIC = 72.25, CAIC = 139.56

Model_3 <- '
           BNT =~  EX + CY 
           ENG =~ VI + DE + AB + EF 
           
           BNT ~~ ENG
'

Model_3.fit <- cfa(Model_3, sample.cov = covs, sample.nobs = n, std.lv = TRUE)
summary(Model_3.fit, rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)
fitMeasures(Model_3.fit, fit.indices)

# -- Structural Equation Models -- #

# M1: MP linked to BNT and AP linked to ENG

# We expect to have Chi2 = 193.81, df = 40, GFI = 0.93, AGFI = 0.81, NNCFI = 0.91, CFI = 0.93, RMSEA = 0.078
fit.indices2 <- c("chisq", "df", "gfi", "agfi", "nnfi", "cfi", "rmsea")


M1 <- '
      # Measurement Part
      BNT =~ NA*EX + CY
      ENG =~ NA*VI + DE + AB + EF
      MP =~ NA*DA + CM + PE
      AP =~ NA*PS + OR
      
      BNT ~~ 1*BNT
      ENG ~~ 1*ENG
      MP ~~ 1*MP
      AP ~~ 1*AP
      BNT ~~ ENG
      MP ~~ AP
      
      # Structural Part
      BNT ~ MP
      ENG ~ AP
'

M1.fit <- sem(M1, sample.cov = covs, sample.nobs = n)
summary(M1.fit, rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)
fitMeasures(M1.fit, fit.indices2)

# M2: MP linked to BNT and AP linked to ENG. Additional link between AP and BNT

# We expect to have Chi2 = 193.81, df = 40, GFI = 0.93, AGFI = 0.81, NNCFI = 0.91, CFI = 0.93, RMSEA = 0.078

M2 <- '
      # Measurement Part
      BNT =~ NA*EX + CY
      ENG =~ NA*VI + DE + AB + EF
      MP =~ NA*DA + CM + PE
      AP =~ NA*PS + OR
      
      BNT ~~ 1*BNT
      ENG ~~ 1*ENG
      MP ~~ 1*MP
      AP ~~ 1*AP
      BNT ~~ ENG
      MP ~~ AP
      
      # Structural Part
      BNT ~ MP + AP
      ENG ~ AP
'

M2.fit <- sem(M2, sample.cov = covs, sample.nobs = n)
summary(M2.fit, rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)
fitMeasures(M2.fit, fit.indices2)

# M3: MP linked to BNT and AP linked to ENG. Additional link between AP and BNT. Additional link MP and ENG

# We expect to have Chi2 = 193.81, df = 40, GFI = 0.93, AGFI = 0.81, NNCFI = 0.91, CFI = 0.93, RMSEA = 0.078

M3 <- '
      # Measurement Part
      BNT =~ NA*EX + CY
      ENG =~ NA*VI + DE + AB + EF
      MP =~ NA*DA + CM + PE
      AP =~ NA*PS + OR
      
      BNT ~~ 1*BNT
      ENG ~~ 1*ENG
      MP ~~ 1*MP
      AP ~~ 1*AP
      BNT ~~ ENG
      MP ~~ AP
      
      # Structural Part
      BNT ~ MP + AP
      ENG ~ AP + MP
'

M3.fit <- sem(M3, sample.cov = covs, sample.nobs = n)
summary(M3.fit, rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)
fitMeasures(M3.fit, fit.indices2)

# Visualize the path model
semPaths(M3.fit,
         rotation = 2,
         layout = "tree2",
         what = "std",
         posCol = "black",
         edge.width = 0.5,
         style = "Lisrel",
         fade = T,
         edge.label.position = 0.7)

# -- Additional analysis -- #

## we allow exhausion and vigor to covary as well as cynicsm and dedication

Add_M1 <- '
      # Measurement Part
      BNT =~ NA*EX + CY
      ENG =~ NA*VI + DE + AB + EF
      MP =~ NA*DA + CM + PE
      AP =~ NA*PS + OR
      
      BNT ~~ 1*BNT
      ENG ~~ 1*ENG
      MP ~~ 1*MP
      AP ~~ 1*AP
      BNT ~~ ENG
      MP ~~ AP
      
      EX ~~ VI
      CY ~~ DE
      
      # Structural Part
      BNT ~ MP + AP
      ENG ~ AP + MP
'

Add_M1.fit <- sem(Add_M1, sample.cov = covs, sample.nobs = n)
summary(Add_M1.fit, rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)
fitMeasures(Add_M1.fit, fit.indices2)

# measurement model perfectionism

Add_M2 <- '
      # Measurement Part
      MP =~ NA*DA + CM + PE
      AP =~ NA*PS + OR
      

      MP ~~ 1*MP
      AP ~~ 1*AP
      MP ~~ AP
'

Add_M2.fit <- sem(Add_M2, sample.cov = covs, sample.nobs = n)
summary(Add_M2.fit, rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)
fitMeasures(Add_M2.fit, fit.indices2)