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
data <- (rcauchy(1000, location = 0, scale = 1)) 
# create empirical distribution of observed data
# generate test statistic
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
d_value <- max(abs(empiricalCDF - pnorm(data)))

KolSmi_test <- function(data) {
  i <- 1:length(data)
  expo <- exp(-((2*i - 1)^2) * pi^2)/(8 * (d_value^2))
  p_value <- (sqrt(2 * pi)/d_value) * sum(expo)
  return(p_value)
}

KolSmi_test(data)
#[1] 0.006626909
ks.test(data, "pnorm")
#D = 0.13573, p-value < 2.2e-16

#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

new_raph <- function(outcome, input, parameter) {
  residu <- data$y - (parameter[1] + parameter[2]*input)
  sum_resud <- sum(residu^2)
}
results_nr <- optim(fn=new_raph, outcome=data$y, input = data$x, par=c(0,1),method="BFGS")
results_nr$par
#  results_nr$par
#[1] 0.139187 2.726699

coef(lm(data$y~data$x))
#(Intercept)      data$x 
#0.1391874   2.7266985 
