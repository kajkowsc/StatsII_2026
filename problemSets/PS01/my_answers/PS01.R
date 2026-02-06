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
  k <- length(data)
  i <- 1:k #creating the iteration through terms for the expo calculation
  expo <- exp(-((2*i - 1)^2) * pi^2)/(8 * (d_value^2))
  d_obs <- sqrt(2 * pi)/d_value * sum(expo)
  return(d_obs)
}

KolSmi_test(data)

  
#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

