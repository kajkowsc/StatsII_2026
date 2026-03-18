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

lapply(c("nnet", "MASS", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)

#manipulation 
gdp_data$GDPWdiff_cat <- ifelse(gdp_data$GDPWdiff > 0, "positive",
                            ifelse(gdp_data$GDPWdiff <0, "negative", "no change"))

gdp_data$GDPWdiff_cat <- factor(gdp_data$GDPWdiff_cat,
                            levels = c("no change", "negative", "positive"))
#part 1
gdp_data$GDPWdiff_cat <- relevel(gdp_data$GDPWdiff_cat, ref = "no change")

unorder_gdp <- multinom(
  GDPWdiff_cat ~ REG + OIL,
  data = gdp_data,
  MaxNWts = 10000
)
summary(unorder_gdp)

stargazer(unorder_gdp, type = "latex")

exp(coef(unorder_gdp))
z <- summary(unorder_gdp)$coefficients/summary(unorder_gdp)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)

#part 2
order_gdp <- polr(GDPWdiff_cat ~ REG + OIL, data = gdp_data, Hess = T)
summary(order_gdp)

stargazer(order_gdp, type = "latex")

order_gdp$zeta
ctable <- coef(summary(order_gdp))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")

#a. run a Poisson regression
PAN_pois <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
                data = mexico_elections,
                family = poisson(link = "log"))
summary(PAN_pois)
stargazer(PAN_pois, type = "latex")

coefs <- summary(PAN_pois)$coefficients
z <- coefs[, "Estimate"] / coefs[, "Std. Error"]
p <- 2 * (1 - pnorm(abs(z)))
cbind(coefs, z_value = z, p_value = p)

#c. estimated means
scenerio_values <- data.frame(
  competitive.district = 1,
  marginality.06 = 0,
  PAN.governor.06 = 1
)
predict(PAN_pois, scenerio_values, type = "response")
#1 
#0.01494818 
