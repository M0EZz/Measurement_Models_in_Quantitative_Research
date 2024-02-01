# Script Alpha

# The idea of this script is to manually compute the alpha given the correlation
# matrix presented as an example in Sijtsma (2015). Additionaly, you can see at 
# the bottom of the script how to do this procedure with the alpha() function
# from the psych package.

rm(list = ls())

# Example covariance matrix from Sijtsma (2015)
cov <- matrix(data = c(0.25, 0.12, 0.16, 0.10,
                       0.12, 0.24, 0.08, 0.09,
                       0.16, 0.08, 0.25, 0.12,
                       0.10, 0.09, 0.12, 0.21), nrow = 4, ncol = 4)
J <- nrow(cov) # Number of items considered

# Three different ways of constructing the Cronbach's Alpha

# 1. Variances
alpha_1 <- (J/(J-1))*(1 - sum(diag(cov))/sum(cov))
alpha_1

# 2. Covariances
# This one is a bit more 'tricky' since we don't have an off-diagonal function
# We can use the upper and lower triangular functions though 
alpha_2 <- (J/(J-1))*(sum(c(cov[upper.tri(cov)], cov[lower.tri(cov)]))/sum(cov))
alpha_2

# 3. Inter-item correlation
alpha_3 <- (J^2)*mean(c(cov[upper.tri(cov)], cov[lower.tri(cov)]))/sum(cov)
alpha_3


# install.packages("psych")
library(psych)
alpha(cov)
