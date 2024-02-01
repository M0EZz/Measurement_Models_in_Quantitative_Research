


rm(list = ls())
library(psych)
library(lavaan)
library(semPlot)

big5 <- bfi
fit_indices <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea","rmsea.ci.lower","rmsea.ci.upper", "srmr")

# Model 1 - One unique factor

m1 <- 'Per =~ A1 + A2 + A3 + A4 + A5 + C1 + C2 + C3 + C4 + C5 + E1 + E2 + E3 + E4 + E5 + N1 + N2 + N3 + N4 + N5 + O1 + O2 + O3 + O4 + O5'
est_m1 <- cfa(model = m1, data = big5[,1:25])
summary(est_m1)
fit_m1 <- fitMeasures(est_m1, fit.measures = fit_indices)

# Model 2 - Means FA=0
m2 <- 'Ag =~ A1 + A2 + A3 + A4 + A5 
       Con =~ C1 + C2 + C3 + C4 + C5 
       Ext =~ E1 + E2 + E3 + E4 + E5 
       Neu =~ N1 + N2 + N3 + N4 + N5
       Opn =~ O1 + O2 + O3 + O4 + O5
'

est_m2 <- cfa(model = m2, data = big5[,1:25])
summary(est_m2)
fit_m2 <- fitMeasures(est_m2, fit.measures = fit_indices)

# Model 3 - Marker variables

m3 <- 'Ag =~ 1*A1 + A2 + A3 + A4 + A5 
       Con =~ 1*C1 + C2 + C3 + C4 + C5 
       Ext =~ 1*E1 + E2 + E3 + E4 + E5 
       Neu =~ 1*N1 + N2 + N3 + N4 + N5
       Opn =~ 1*O1 + O2 + O3 + O4 + O5
'
est_m3 <- cfa(model = m3, data = big5[,1:25])
summary(est_m3)
fit_m3 <- fitMeasures(est_m3, fit.measures = fit_indices)

# Model 4 - Variance =1

m4 <- '
       # Measurement model
       Ag =~ A1 + A2 + A3 + A4 + A5 
       Con =~ C1 + C2 + C3 + C4 + C5 
       Ext =~ E1 + E2 + E3 + E4 + E5 
       Neu =~ N1 + N2 + N3 + N4 + N5
       Opn =~ O1 + O2 + O3 + O4 + O5
       
       # Variances
       Ag ~~1*Ag
       Con ~~1*Con
       Ext ~~1*Ext
       Neu ~~1*Neu
       Opn ~~1*Opn
'
est_m4 <- cfa(model = m4, data = big5[,1:25])
summary(est_m4)
fit_m4 <- fitMeasures(est_m4, fit.measures = fit_indices)
modindices(est_m2)
# We can just simply add the effect.coding option to turn into effect coding.

# Model 5 - Modifications

m5 <- '
       # Measurement model
       Ag =~ A1 + A2 + A3 + A4 + A5 
       Con =~ C1 + C2 + C3 + C4 + C5 
       Ext =~ E1 + E2 + E3 + E4 + E5 
       Neu =~ N1 + N2 + N3 + N4 + N5
       Opn =~ O1 + O2 + O3 + O4 + O5
       
       # Covariances
       Ag ~~ Con + Ext + Neu + Opn
       Con ~~ Ext + Neu + Opn
       Ext ~~ Neu + Opn
       Neu ~~ Opn
'
est_m5 <- cfa(model = m5, data = big5[,1:25])
summary(est_m5)
fit_m5 <- fitMeasures(est_m5, fit.measures = fit_indices)
semPaths(est_m5, "std", "hide", intAtSide = TRUE, rotation = 2)

# Finally, let us check the fit of all the models

Fit <- cbind(fit_m1, fit_m2, fit_m3, fit_m4, fit_m5)
Fit

