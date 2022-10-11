#### 
## model_fitting_functions.R
##
## Author: Paul Carvalho (paul.carvalho@noaa.gov, pcarvalh@ucsc.edu)
## Last update: 9/21/2022
##
## Description: Functions used in the in the optim.simulation() function in model_fitting.R
####


# Script notes ----------------------------------------------------------------------------------------------------
# 1. Use docstring("insert function name") to view function documentation and information.


# Stock recruitment relationship ----------------------------------------------------------------------------------
srr <- function(theta, g, x){
  #' srr
  #' @description Beverton-Holt stock recruitment relationship (Beverton and Holt 1957)
  #' 
  #' @param theta vector with two values: (1) maximum egg-to-fry survival and (2) strength of density-dependence
  #' @param g fecundity
  #' @param x number of female spawners
  #' 
  #' @return fry produced per female spawner
  
  return((theta[1] * g) / (1 + theta[2] * g * x))  
}


# Fishery harvest -------------------------------------------------------------------------------------------------
fishery.impact <- function(O, i, nu){
  #' fishery.impact
  #' @description Calculate fishery harvest
  #' 
  #' @param O ocean abundance
  #' @param i harvest rate
  #' @param nu relative fishing mortality for difference ages
  #' 
  #' @return Number of fish harvested
  
  # Only used in parameter calibration
  tmp.nu <- c(nu, nu)
  I.out  <- rep(NA, length(O))
  for(x in 1:length(I.out)){
    if(((1 - (1 - i)) * tmp.nu[x]) > 1){
      print('Harvest rate greater than 1')
    } else if (((1 - (1 - i)) * tmp.nu[x]) < 0){
      print('Harvest rate less than 1')
    }
    I.out[x] <- O[x] * i * tmp.nu[x]
  }
  return(I.out)
}


# Flow-dependent juvenile survival --------------------------------------------------------------------------------
juv.survival <- function(w){
  #' juv.survival
  #' @description Calculate juvenile survival based on step function for river flow in Michel et al. (2022)
  #' 
  #' @param w river flow (cfs)
  #' 
  #' @return Juvenile survival rate
  
  if(is.na(w)){
    return(NA)
  }
  x.perc <- 0.9 # percentile of each step to keep flat
  step1 <- round(4258 - (4258 * ((1 - x.perc) / 2))) #c(0, 4258)
  step2 <- round(c(4259 + (10711 * ((1 - x.perc) / 2)), 10711 - (10711 * ((1 - x.perc) / 2))))   #c(4259, 10711)
  step3 <- round(c(10712 + (22871 * ((1 - x.perc) / 2)), 22871 - (22871 * ((1 - x.perc) / 2)))) #c(10712, 22871)
  step4 <- round(22872 + (22872 * ((1 - x.perc) / 2))) #22872
  if(w < step1){ #1
    surv <- 0.03 # flat step from Michel et al. 2021
    se   <- 0.276
  } else if(w >= step1 & w < step2[1]) { #2
    surv <- (((0.189 - 0.03) / (step2[1] - step1)) * w) + (0.189 - (((0.189 - 0.03) / (step2[1] - step1)) * step2[1])) # linear model to interpolate steps
    se   <- sum(c(0.276, 0.094))
  } else if(w >= step2[1] & w < step2[2]) { #3
    surv <- 0.189 # flat step from Michel et al. 2021
    se   <- 0.094
  } else if(w >= step2[2] & w < step3[1]) { #4 (0.508 - 0.189)
    surv <- (((0.508 - 0.189) / (step3[1] - step2[2])) * w) + (0.508 - (((0.508 - 0.189) / (step3[1] - step2[2])) * step3[1])) # linear model to interpolate steps
    se   <- sum(c(0.094, 0.082))
  } else if(w >= step3[1] & w < step3[2]) { #5
    surv <- 0.508 # flat step from Michel et al. 2021
    se   <- 0.082
  } else if(w >= step3[2] & w < step4){
    surv <- (((0.353 - 0.508) / (step4 - step3[2])) * w) + (0.353 - (((0.353 - 0.508) / (step4 - step3[2])) * step4)) # linear model to interpolate steps
    se   <- sum(c(0.082, 0.094))
  } else if(w >= step4){
    surv <- 0.353 # flat step from Michel et al. 2021 0.353
    se   <- 0.088 # 0.088
  }
  surv <- surv
  river.surv <- inv.logit(rnorm(n = 1, mean = logit(surv), sd = se))
  return(river.surv)
}


# Smart rounding --------------------------------------------------------------------------------------------------
smart.round <- function(x){
  #' smart.round
  #' @description Round vector of numbers but preserve their sum
  #' 
  #' @param x vector of numbers
  #' 
  #' @return Vector of integers with sum equal to the sum of the input vector
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y)
}
