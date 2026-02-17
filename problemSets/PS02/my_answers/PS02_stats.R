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
install.packages("stargazer")
library(stargazer)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))

climateSupport$countries <- factor(climateSupport$countries, ordered = FALSE)
climateSupport$sanctions <- factor(climateSupport$sanctions, ordered = FALSE)

                                   
#1 
add_model <- glm(
  data = climateSupport,
  choice ~ countries + sanctions, 
  family = binomial(link = "logit"))
summary(add_model)

stargazer(add_model, 
          type = "latex",
          title = "Additive Model",
          column.labels = "Coefficients")

null_model <- glm(choice ~ 1,
                  data = climateSupport,
                  family = binomial)
summary(null_model)

stargazer(null_model, 
          type = "latex",
          title = "Null Model",
          column.labels = "Coefficients")

anova(null_model, add_model, test = "LRT")
#Analysis of Deviance Table
#Model 1: choice ~ 1
#Model 2: choice ~ countries + sanctions
#Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#1      8499      11783                          
#2      8494      11568  5   215.15 < 2.2e-16 ***

#2c
p_80 <- exp(0.063)/(1 +exp(0.063)) 
p_80
#[1] 0.5157448

#3
inter_model <- glm(
  data = climateSupport,
  choice ~ countries * sanctions,
  family = binomial
)
summary(inter_model)

stargazer(inter_model, 
          type = "latex",
          title = "Interaction Model",
          column.labels = "Coefficients")


anova(add_model, inter_model, test = "LRT")
#Analysis of Deviance Table
#Model 1: choice ~ countries + sanctions
#Model 2: choice ~ countries * sanctions
#Resid. Df Resid. Dev Df Deviance Pr(>Chi)
#1      8494      11568                     
#2      8488      11562  6   6.2928   0.3912
