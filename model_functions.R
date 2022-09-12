## ----------------------------------------------------------------------------------------------------------------------
## model_functions.R
##
## Author: Paul Carvalho (paul.carvalho@noaa.gov, pcarvalh@ucsc.edu)
## Last update: May 25, 2022
##
## Description: Support functions for main_script.R
## ----------------------------------------------------------------------------------------------------------------------

## General notes --------------------------------------------------------------------------------------------------------
# 1. Use docstring('insert function name') to view function documentation and information.

## Load libraries -------------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)

## Functions ------------------------------------------------------------------------------------------------------------
model.summary <- function(x){
  #' model.summary
  #' @description Summarize output from n model simulations
  #' 
  #' @param x dataframe with model outputs from x simulations
  #' 
  #' @return Dataframe with model summary values 
  
  tmp    <- x %>% dplyr::filter(year >= 30) %>% dplyr::select(year, Spawn.est, harvest, sim)
  tmp.cv <- tmp %>% dplyr::group_by(sim) %>% dplyr::summarise(spawn.cv = sd(Spawn.est, na.rm=TRUE)/mean(Spawn.est, na.rm=TRUE), harvest.cv = sd(harvest)/mean(harvest))
  tmp.df <- data.frame(spawn.mean = mean(tmp$Spawn.est, na.rm = TRUE),
                       spawn.median = median(tmp$Spawn.est, na.rm = TRUE),
                       spawn.pi.lo = quantile(tmp$Spawn.est, probs = 0.025, na.rm = TRUE),
                       spawn.pi.up = quantile(tmp$Spawn.est, probs = 0.975, na.rm = TRUE),
                       spawn.cv = mean(tmp.cv$spawn.cv),
                       spawn.cv.lo = quantile(tmp.cv$spawn.cv, probs = 0.025),
                       spawn.cv.up = quantile(tmp.cv$spawn.cv, probs = 0.975),
                       harvest.mean = mean(tmp$harvest),
                       harvest.median = median(tmp$harvest),
                       harvest.pi.lo = quantile(tmp$harvest, probs = 0.025),
                       harvest.pi.up = quantile(tmp$harvest, probs = 0.975),
                       harvest.cv = mean(tmp.cv$harvest.cv),
                       harvest.cv.lo = quantile(tmp.cv$harvest.cv, probs = 0.025),
                       harvest.cv.up = quantile(tmp.cv$harvest.cv, probs = 0.975))
  row.names(tmp.df) <- NULL
  return(tmp.df)
}

set.vars <- function(n.yr, scenario){
  #' set.vars
  #' @description Initialize main model variables and save as dataframe to be used in simulation runs
  #' 
  #' @param n.yr number of years simulated in the model
  #' @param scenario drought scenario to be modeled ('base', 'longer duration', 'more frequent', or 'more intense')
  #' 
  #' @return dataframe with simulated variables for main model simulations.
  
  load('srfc_data.RData') # load data
  library(mgcv)
  
  # Simulate climate/flow scenario
  w <- flow.sim(n.yr, scenario, flow.full) # flow
  
  # Hatchery releases - nonparametric bootstrap
  ht <- rep(mean(hat.release$total.release), n.yr)
  
  # Hatchery release site distance - function of flow
  newx <- data.frame(log.flow = log(w))
  sig2 <- weighted.gam$sig2 # estimated residual variance from the model
  newx <- transform(newx, newy = predict(weighted.gam, newx, type = 'response')) # predict distance
  newx <- transform(newx, ysim = rnorm(n.yr, mean = newy, sd = sqrt(sig2))) # simulate values
  hd   <- newx$ysim
  hd <- 2 + 7 * (hd/445) # survival increase with trucking, maintain the same maximum value (ie slope), max represents distance from Coleman to the bay (Sturrock et al. 2019)
  
  # Hatchery escapement proportion - nonparametric bootstrap
  xt <- sample(catch.esc$hatchery/catch.esc$total.esc, n.yr, replace = TRUE)
  
  # NPGO - concatenate years
  # npgo <- rep(npgo$npgo, times = ceiling(n.yr/length(npgo$npgo)))[1:n.yr] # concatenate npgo times series
  npgo <- rep(npgo.full$npgo, times = 2)
  # npgo <- sample(npgo$npgo, n.yr, replace = TRUE)
  
  return(data.frame(w, ht, hd, xt, npgo))
}

flow.sim <- function(n.yr, scenario, flow.full){
  #' flow.sim
  #' @description Simulate flow values for different drought scenarios. Base drought represents contemporary conditions. Longer duration droughts are simulated by increasing the range of duration from 3-5 years to 3-7 years. More frequent droughts are simulated by decreasing the mean time between droughts from the Poisson distribution (lambda = 12 to lambda = 6). More intense droughts are simulated by increasing the probability of drawing the lowest flow value during drought events.
  #'
  #' @param n.yr number of years simulated
  #' @param scenario drought scenario to simulate ('base', 'longer duration', 'more frequent', 'more intense')
  #' @param flow.full empirical flow data used to simulate drought scenarios
  #'
  #' @return vector with simulated flow values (cfs)
  
  # Save non-drought and drought values to sample with replacement
  nondrought.vals <- flow.full %>% filter((year>1992 & year<2007) | (year>2009 & year<2012) | (year>2016 & year<2020)) %>% mutate(flow = round(discharge)) %>% pull(flow)
  drought.vals    <- flow.full %>% filter((year<1993) | (year>2006 & year<2010) | (year>2011 & year<2017)) %>% mutate(flow = round(discharge)) %>% pull(flow)
  drought.dur     <- function(){return(sample(2:4, 1))} # draw number for drought duration (2-4 represents 3-5year droughts)
  if(scenario == 'base'){
    drought.fre <- 12 # frequency; interval between initial drought years Pois(lambda = 12 years)
    drought.pro <- rep(1, length(drought.vals))/length(drought.vals) # same probability of drought values
  } else if(scenario == 'longer duration'){
    drought.dur <- function(){return(sample(2:6, 1))} # overwrite drought duration function to select droughts 3-7 years
    drought.fre <- 12 # same as base
    drought.pro <- rep(1, length(drought.vals))/length(drought.vals) # same as base
  } else if(scenario == 'more frequent'){
    drought.fre <- 6 # Pois(lambda = 6 years)
    drought.pro <- rep(1, length(drought.vals))/length(drought.vals) # same as base
  } else if(scenario == 'more intense'){
    drought.fre <- 12 # same as base
    drought.pro <- c(rep(0.0705, 11), 0.154, 0.0705) # higher probability of drawing lowest value
  }
  # Define first drought event - draw number from a uniform distribution over the time interval (1, 12)
  tmp.drought     <- round(runif(1, min=1, max=12))
  flow.array      <- array(data = 0, dim = c(100, 2))
  tmp.drought.dur <- drought.dur()
  drought.record  <- tmp.drought.dur
  flow.array[(tmp.drought:(tmp.drought + tmp.drought.dur)), 2] <- 1
  
  # Simulate climate scenario
  for(i in 1:100){
    if((flow.array[i, 2] == 1 && i == 1) | (i > 1 && flow.array[i, 2] == 1 && flow.array[i-1, 2] == 0)){ # draw from Poisson dist for next drought and draw drought value
      tmp.drought     <- rpois(1, lambda = drought.fre) # draw from Poisson distribution
      tmp.drought.dur <- drought.dur() # draw duration of next drought
      drought.record  <- c(drought.record, tmp.drought.dur)
      
      while(tmp.drought < drought.record[length(drought.record)-1]+2){
        tmp.drought <- rpois(1, lambda = drought.fre)
      }
      if(between(i + tmp.drought, 0, 100)){ # check to make sure the next drought is within the bounds
        if(!between(i + tmp.drought + tmp.drought.dur, 0, 100)){ # truncate drought length to be within bounds
          flow.array[(i + tmp.drought):100, 2] <- 1
        } else {
          flow.array[((i + tmp.drought):(i + tmp.drought + tmp.drought.dur)), 2] <- 1
        }
      }
      flow.array[i, 1] <- sample(drought.vals, size = 1, prob = drought.pro) # probability of drawing drought values
    } else if(flow.array[i, 2] == 1) { # drought
      flow.array[i, 1] <- sample(drought.vals, size = 1, prob = drought.pro) # probability of drawing drought values
    } else {
      # non drought
      flow.array[i, 1] <- sample(nondrought.vals, size = 1)
      # make sure not more than two consecutive years below historic mean threshold
      if(i > 2 && (flow.array[i-1, 1] < 10712 & flow.array[i-1, 2] == 0) && (flow.array[i-2, 1] < 10712 & flow.array[i-2, 2] == 0)){
        while(flow.array[i, 1] < 10712){
          flow.array[i, 1] <- sample(nondrought.vals, size = 1)
        }
      } else if((i < 100 && flow.array[i+1, 2] == 1) | (i > 1 && flow.array[i-1, 2] == 1)) {
        while(flow.array[i, 1] < 10712){
          flow.array[i, 1] <- sample(nondrought.vals, size = 1)
        }
      }
    }
  }
  return(flow.array[,1])
  
}

# # Code to look at difference between longer duration and more frequent droughts
# ld <- NULL
# mf <- NULL
# for(i in 1:500){
#   ld.df <- data.frame(year=1:100, flow=flow.sim(n.yr,scenario='longer duration',flow.full), sim=i)
#   ld <- rbind(ld, ld.df)
#   mf.df <- data.frame(year=1:100, flow=flow.sim(n.yr,scenario='more frequent',flow.full), sim=i)
#   mf <- rbind(mf, mf.df)
# }
# # 1. look at the percentage of time below 10712
# library(dplyr)
# test1 <- function(x){
#   x.below <- sum(x < 10712)
# }
# ld.flow <- mean(unlist(ld %>% group_by(sim) %>% group_map(~test1(.x$flow))))
# mf.flow <- mean(unlist(mf %>% group_by(sim) %>% group_map(~test1(.x$flow))))
# # end of code for looking at difference between longer duration and more frequent droughts


srr <- function(theta, g, x){
  return((theta[1] * g) / (1 + theta[2] * g * x))  
}

fishery.impact <- function(O, i, nu){
  # Only used in parameter optimization
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

juv.survival <- function(w){
  if(is.na(w)){
    return(NA)
  }
  if(w < 4045){ #1
    surv <- 0.03 # flat step from Michel et al. 2021
    se   <- 0.276
  } else if(w >= 4045 & w < 4795) { #2
    surv <- (((0.189 - 0.03) / (4795 - 4045)) * w) + (0.189 - (((0.189 - 0.03) / (4795 - 4045)) * 4795)) # linear model to interpolate steps
    se   <- sum(c(0.276, 0.094))
  } else if(w >= 4795 & w < 10175) { #3
    surv <- 0.189 # flat step from Michel et al. 2021
    se   <- 0.094
  } else if(w >= 10175 & w < 11856) { #4 (0.508 - 0.189)
    surv <- (((0.508 - 0.189) / (11856 - 10175)) * w) + (0.508 - (((0.508 - 0.189) / (11856 - 10175)) * 11856)) # linear model to interpolate steps
    se   <- sum(c(0.094, 0.082))
  } else if(w >= 11856 & w < 21727) { #5
    surv <- 0.508 # flat step from Michel et al. 2021
    se   <- 0.082
  } else if(w >= 21727 & w < 24016){
    surv <- (((0.353 - 0.508) / (24016 - 21727)) * w) + (0.353 - (((0.353 - 0.508) / (24016 - 21727)) * 24016)) # linear model to interpolate steps
    se   <- sum(c(0.082, 0.094))
  } else if(w >= 24016){
    surv <- 0.353 # flat step from Michel et al. 2021 0.353
    se   <- 0.088 # 0.088
  }
  river.surv <- rnorm(n = 1, mean = log(surv/(1-surv)), sd = se) # logit function for mean
  return(exp(river.surv)/(1 + exp(river.surv))) # return inverse logit
}

control.rule <- function(SI){
  # fixed
  Fabc <- 0.70
  Smsy <- 122000
  MSST <- 91500
  A <- MSST / 2
  B <- (MSST + Smsy) / 2
  C <- Smsy / (1 - 0.25)
  D <- Smsy / (1 - Fabc)
  
  if(SI >= 0 & SI <= A){ 
    ER <- 0.10 * (SI / A)
  } else if(SI > A & SI <= MSST){
    ER <- 0.10
  } else if(SI > MSST & SI <= B){
    ER <- 0.10 + (0.15*((SI - MSST)/(B - MSST)))
  } else if(SI > B & SI <= C){
    ER <- 0.25 
  } else if(SI > C & SI <= D){
    ER <- (SI - Smsy) / SI
  } else if(SI > D){
    ER <- Fabc
  }
  return(ER)
}

# # Extra code for plotting the harvest control rule
# library(ggplot2)
# library(ggpubr)
# tmp.si <- seq(0, 500000, length.out = 1000)
# tmp.er <- sapply(tmp.si, control.rule)
# plot1 <- ggplot() +
#   geom_line(aes(x = tmp.si/1000, y = tmp.er), size = 1) +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 0.8)) +
#   labs(x = 'Sacramento Index (thousands)', y = 'Allowable exploitation rate') +
#   theme_classic() +
#   theme(text = element_text(size = 13), plot.margin = unit(c(0.5,1,0.5,0.5), 'cm'))
# 
# plot2 <- ggplot() +
#   geom_line(aes(x = tmp.si/1000, y = (tmp.si/1000) - ((tmp.si/1000)* tmp.er)), size = 1) +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   labs(x = 'Sacramento Index (thousands)', y = 'Expected spawners after exploitation') +
#   theme_classic() +
#   theme(text = element_text(size = 13), plot.margin = unit(c(0.5,1,0.5,0.5), 'cm'))
# # 
# # ggarrange(plot1, plot2, nrow=2)
# # 
# tmp.si2 <- seq(122000, 500000, length.out = 1000)
# constant_esc_er <- (tmp.si2 - 122000) / tmp.si2
# plot(tmp.si2, constant_esc_er)
