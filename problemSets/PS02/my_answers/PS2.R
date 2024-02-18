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

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))

# Creating an unordered list of 'countries' variable 
countries <- factor(climateSupport$countries, levels = c("20 of 192", "80 of 192", "160 of 192"), ordered = FALSE)

# Creating an unordered list of 'sanctions' variable 
sanctions <- factor(climateSupport$sanctions, levels = c("None", "5%", "15%", "20%"), ordered = FALSE)

# Change baseline category for the "countries" variable
countries <- relevel(climateSupport$countries, ref = "20 of 192")

# Change baseline category for the "sanctions" variable
sanctions <- relevel(climateSupport$sanctions, ref = "None")

# Fit logistic regression model
model <- glm(choice ~ countries + sanctions, data = climateSupport, family = binomial(link = "logit"))

# Summary output
summary(model)

# creating a baseline model using intercept
null_model <- glm(choice~1, data = climateSupport, family = binomial(link = "logit"))

# comparing both the models using likelihood ratio
anova(null_model, model, test = "LRT")

#2(a)
# Coefficient for sanctions at 15%
sanctions_15 <- -0.32510

# Calculate the odds ratio
odds_15 <- exp(sanctions_15)

# Print the odds ratio
print(odds_15)

# 2(b)
# Coefficients from our model
intercept <- -0.08081
countries_80 <- 0.33636
sanctions_none <- -0.19186

# Calculate the log odds for the specific condition: 80 of 192 countries with no sanctions
odds_80 <- intercept + countries_80*1 + sanctions_none*1

# Convert log odds to probability
probability <- 1 / (1 + exp(-odds_80))

# Print the estimated probability
print(probability)

# 2(c)
# Fit model with interaction term
model_interaction <- glm(choice ~ countries + sanctions + countries:sanctions, 
                         data = climateSupport, 
                         family = binomial(link = "logit"))

# Use ANOVA to compare the models
anova_result <- anova(model, model_interaction, test = "LRT")
print(anova_result)

