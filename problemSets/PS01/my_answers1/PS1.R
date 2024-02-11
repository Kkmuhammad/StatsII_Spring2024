#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################
set.seed(123)
n <- 1000
emp <- rcauchy(n, location = 0, scale = 1)
# create a function that takes observed data as input
ks <- function(data) {
  # create empirical distribution of observed data
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  
  # generate test statistic 
  D <- max(abs(empiricalCDF - pnorm(data)))
  
  addition <- 0 
  for(a in 1:n){
    addition <- c(addition, exp((- (2 * a - 1)^2 * pi^2) / ((8 * D)^2)))
  }
  
  p <- sqrt(2 * pi) / D * sum(addition)
  
  print(paste("D =", D))
  print(paste("p_value =", p))
}

print(ks(emp))

ks.test(emp, "pnorm")

#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)


# Log-likelihood function for OLS regression
log_lkhd_ols <- function(beta, x, y) {
  y_pred <- beta[1] + beta[2] * x
  residuals <- y - y_pred
  log_lkhd_values <- dnorm(residuals, mean = 0, sd = 1.5, log = TRUE)
  return(-sum(log_lkhd_values))
}

# Use BFGS optimization to estimate OLS coefficients
initial <- c(0, 1)  
result <- optim(par = initial, log_lkhd_ols, x = data$x, y = data$y, method = "BFGS")

ols_coef_log_lkhd <- result$par

print(paste("Intercept:", ols_coef_log_lkhd[1]))
print(paste("Slope:", ols_coef_log_lkhd[2]))

# Compare with lm function
lm_result <- lm(y ~ x, data = data)

ols_coef_lm <- coef(lm_result)

print(paste("Intercept:", ols_coef_lm[1]))
print(paste("Slope:", ols_coef_lm[2]))

