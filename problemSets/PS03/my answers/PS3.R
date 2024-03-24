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

lapply(c("nnet", "MASS"),  pkgTest)
install.packages("stargazer")
library(nnet)
library(stargazer)
library(MASS)
# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# 1.1

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

# creating a factor variable
gdp_data$category <- ifelse(gdp_data$GDPWdiff == 0, "no change",
                            ifelse(gdp_data$GDPWdiff > 0, "positive change", "negative change"))


# conevrting category variable into a factor, keeping "no change" as reference level
gdp_data$category <- relevel(as.factor(gdp_data$category), ref="no change")

# using ordinal variables from the dataset to fit an unordered multinomial logit model
un_logit <- multinom(gdp_data$category ~ gdp_data$OIL+gdp_data$REG)
summary(un_logit)


# 1.2

# create factor through relevel for increasing order level creation
gdp_data$category <- relevel(gdp_data$category, ref = "negative change")

# using ordinal variables from the dataset to fit an ordered multinomial logit model
ord_logit <- polr(gdp_data$category ~ gdp_data$OIL+gdp_data$REG)

summary(ord_logit)
stargazer(ord_logit)

#####################
# Problem 2
#####################

# 2.1

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")



# Fit Poisson regression model
poisson <- glm(mexico_elections$PAN.visits.06 ~ 
                 mexico_elections$competitive.district + 
                 mexico_elections$marginality.06 + 
                 mexico_elections$PAN.governor.06,
               family = poisson())

# Summarize the model
summary(poisson)
stargazer(poisson)

# 2.3

# create a new data frame with specific characteristics
hyp_data <- data.frame(competitive.district = 1, 
                       marginality.06 = 0, 
                       PAN.governor.06 = 1)

# predict mean number of visits for hypothetical data
predicted_mean <- predict(poisson, data = hyp_data, type = "response")

estimated_mean <- mean(predicted_mean)

# print the predicted mean
print(estimated_mean)
