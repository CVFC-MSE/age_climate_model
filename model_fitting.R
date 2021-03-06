## ----------------------------------------------------------------------------------------------------------------------
## model_fitting.R
##
## Author: Paul Carvalho (paul.carvalho@noaa.gov, pcarvalh@ucsc.edu)
## Last update: May 25, 2022
##
## Description: Fit Sacramento River fall Chinook population dynamics model to escapement and harvest observations for 
##              parameter calibration and model validation. The calibrated model is used for age structure and climate modeling.
## ----------------------------------------------------------------------------------------------------------------------

## General notes --------------------------------------------------------------------------------------------------------
# 1. Use docstring('insert function name') to view function documentation and information.

## Set up workspace -----------------------------------------------------------------------------------------------------
rm(list = ls()) # clear workspace
load('srfc_data.RData')
library(ggplot2)
library(ggpubr)
library(optimParallel)
library(docstring)

## Functions ------------------------------------------------------------------------------------------------------------
optim.simulation <- function(pars, calibrate, reset.esc){
  #' optim.simulation
  #' @description Run simulation model to calibrate parameters (if calibrate == TRUE) and model validation.
  #' 
  #' @param pars vector of four numerical parameters that are fitted if calibrate == TRUE: (1) residual juvenile survival, (2) coefficient of variation of recruitment stochasticity, (3) mean effect of NPGO on juvenile survival, (4) variance of NPGO effect on juvenile survival.
  #' @param calibrate boolean value that will make the function return the sum of squared error between empirical and model outputs for fitting pars if TRUE, or returns simulation output variables for validation if FALSE
  #' @param reset.esc boolean value that will reset model escapement to the empirical value at the beginning of each time step for model calibration if TRUE, or keep model escapement throughout if FALSE.
  #' 
  #' @return If calibrate == TRUE, return sum of squared error between empirical and model escapement; If calibrate == FALSE, return a list of vectors with model output variables.

  # Need to load libraries and data here for parallel computing to work
  library(gtools)
  library(dplyr)
  library(tidyr)
  load('srfc_data.RData') # load data
  
  # Need to define functions here for parallel computing
  srr <- function(theta, g, x){
    return((theta[1] * g) / (1 + theta[2] * g * x))  
  }
  
  fishery.impact <- function(O, i, nu){
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
  
  juv.survival <- function(w){
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
  
  # Calibrated parameters
  alpha   <- pars[1] # residual juvenile survival
  cv.j    <- pars[2] # coefficient of variation of recruitment stochasticity
  phi     <- pars[3] # mean NPGO effect on survival
  sd      <- pars[4] # variance of NPGO effect on survival
  
  # Set model parameters
  n.yr        <- 26   # number of years 26 (1988 - 2013)
  n.sim       <- 1000 # number of simulations per optimization run
  n.age.stage <- 17   # number of age/stage classes; fry/pre-smolts, immature males (ages 2:A), mature males (ages 2:A), immature females (ages 2:A), mature females (ages 2:A)
  n.pops      <- 1    # number of populations to simulate
  A           <- 5    # maximum age
  
  # Set population dynamics parameters (see Table 2 in manuscript)
  theta.1       <- 0.3 # 0.35
  theta.2       <- 1.5e-8 # 1e-8
  m.maturity    <- c(0.038, 0.5, 0.999, 1) # maturation rates that achieve reasonable escapement age composition.
  f.maturity    <- m.maturity # option to define separate female and male maturation rates
  nat.maturity  <- m.maturity * c(1, 1, 1, 1) # natural-origin maturation rates in relation to hatchery derived maturation rates (fewer male fish mature at ages 2 and 3 for natural-origin)
  m.hat         <- t(array(c(m.maturity,f.maturity), dim = c(A-1, 2), dimnames <- list(c(paste('age.', 2:A, sep = "")), c('male' ,'female')))) # hatchery maturation array; NOTE - the array will automatically adjust for age/stage structure specified in above parameters
  m.nat         <- t(array(c(nat.maturity,f.maturity), dim = c(A-1, 2), dimnames <- list(c(paste('age.', 2:A, sep = "")), c('male' ,'female')))) # natural maturation array; NOTE - the array will automatically adjust for age/stage structure specified in above parameters
  cv.spawn.est  <- 0.2  # coefficient of variation of spawner abundance estimates - value results in plausible levels of abundance forecast error (https://s3.amazonaws.com/media.fisheries.noaa.gov/2020-10/SRFC-RP_finalEA-FONSI.pdf?null=)
  g             <- c(4325, 5407, 5407, 6488) #5401 # alternative 5407 for all ages - https://www.fws.gov/redbluff/MSJM Reports/RST/Juvenile Anadromous Fish Monitoring Compendium Report (2002-2012).pdf # c(4185, 5838, 5994, 7403) eggs-per-female by age (Kaufman et al. 2009)
  n.surv        <- c(0.5, 0.8, 0.8, 0.8) #c(0.5|0.35, 0.8, 0.8, 0.8) survival rate of fish aged 2 to 5; values from MO.
  nu            <- c(0.05, 1, 1, 1) # relative fishing mortality rate c(2, 0.25, 0.25, 0.25) 2.1, 0.3, 0.1, 0.1
  
  # Observation parameters
  sigma.log.Spawn.est <- sqrt(log(cv.spawn.est ^ 2 + 1)) # SD of log R.sum.est
  cor.log.Spawn.est   <- -0.5 * (sigma.log.Spawn.est ^ 2) # Bias correction for mean of log R.sum.est
  
  # Natural reproduction parameters
  sigma.log.j <- sqrt(log(cv.j ^ 2 + 1)) # SD of log of recruitment deviations
  cor.log.j   <- -0.5 * (sigma.log.j ^ 2) # Bias correction for mean of log of recruitment deviations
  
  # Annual biological and environmental observations
  y.params <- data.frame(years = seq(1988, 2013),
                         w = flow$discharge,
                         ht = c(hat.release$total.release, NA),
                         h = 2 + 7 * (hat.distance$mean.hat.to.release/445), #hat.distance$mean.hat.to.release * (5/mean(hat.distance$mean.hat.to.release)),
                         xt = catch.esc$hatchery / catch.esc$total.esc,
                         npgo = npgo$npgo,
                         i = catch.esc$exploitation.rate/100, # annual exploitation rate (PFMC 2020 Preseason report I)
                         Nt = catch.esc$natural,
                         Ht = catch.esc$hatchery,
                         cv.Nt = rep(cv.spawn.est, n.yr), 
                         total.harvest = catch.esc$total.ocean.harvest + catch.esc$river.harvest)
  
  # Simulation indices
  Z.nat.r.ind      <- as.matrix(expand.grid(1, (n.age.stage - (A - 2)):n.age.stage)) # Indices of Leslie transition matrix that correspond to recruitment rate
  N.H.O.ind        <- c(2:A, (2 * A):(3 * A - 2)) # Indices of natural- and hatchery-origin population vectors that correspond to ocean fish (immature fish age 2 or greater)
  N.H.S.female.ind <- (n.age.stage - (A - 2)):n.age.stage # # Indices of natural- and hatchery-origin population vectors that correspond to female spawners
  N.H.S.ind        <- c((A + 1):(2 * A - 1), (n.age.stage - (A - 2)):n.age.stage) # Indices of natural- and hatchery-origin population vectors that correspond to spawners
  
  # State variables
  N <- H <- I.N <- I.H <- array(0, dim = c(n.age.stage, n.yr, n.pops, n.sim)) # Numbers of natural-origin individuals (N); # numbers of hatchery-origin individuals (H); # natural-origin impacts (I.N, during period following time t); hatchery-origin impacts; rows are fry/pre-smolts, immature males (ages 2:A), mature males (ages 2:A), immature females (ages 2:A), mature females (ages 2:A); columns are times
  z <- B.m.h <- B.m.n <- B.f.h <- B.f.n <- R.spawn.fem.a <-  array(0, dim = c(A-1, n.yr, n.pops, n.sim)) # number of spawners that return to hatcheries
  j.surv <- jack <- harvest <- R.spawn.tmp <- Spawn <- B.total <- R.spawn <- R.spawn.fem <- Spawn.est <- R.spawn.est <- mu.c <- c <- Pop <- array(NA, dim = c(n.yr, n.pops, n.sim)) # Number of returning spawners (at time t), number of spawners in natural area (at time t), broodstock (per sex) removed at time t, number of female spawners in natural area (at time t), estimated number of returning spawners (at time t), estimated number of spawners in natural area (at time t), impact rate specified by the management strategy at time t, realized impact rate south of Point Arena following time t, and population size at time t (sum of spawners at time t and in previous two years)
  n <- array(NA, dim = c(A, n.yr, n.pops, n.sim)) # natural survival rate (probability of surviving to next age (based on flow for juveniles) after harvest during the current age for ages >= 2); columns are times; Note: impact rates and natural survival rates are not sex- or origin-specific
  
  # Population dynamics parameters/variables
  n[2:A, , , ]   <- n.surv # survival rate of fish aged 2 and older
  harvest.scalar <- 0.75 # need to scale back exploitation rate because not all ocean ages are exploited at the same rate, and this scales harvest to match empirical values
  
  # Initialize population
  # Initial number of hatchery escapement
  B.m.h[, 1, , ] <- B.m.n[, 1, , ] <- round(c(0.234, 0.583, 0.180, 0.003) * (y.params$Ht[1]/4))
  B.f.h[, 1, , ] <- B.f.n[, 1, , ] <- round(c(0.037, 0.665, 0.294, 0.004) * (y.params$Ht[1]/4))
  B.total[1, , ] <- sum(B.f.h[, 1, , 1], B.f.n[, 1, , 1], B.m.h[, 1, , 1], B.m.n[, 1, , 1])
  # Initial number of total escapement
  N[N.H.S.ind, 1, , ] <- round(c(0.234, 0.583, 0.18, 0.003, 0.037, 0.665, 0.294, 0.004) * ((y.params$Nt[1] + y.params$Ht[1])/4)) # initial number of natural-origin spawners
  H[N.H.S.ind, 1, , ] <- round(c(0.234, 0.583, 0.18, 0.003, 0.037, 0.665, 0.294, 0.004) * ((y.params$Nt[1] + y.params$Ht[1])/4)) # initial number of hatchery-origin spawners
  Spawn[1, , ]        <- sum(N[N.H.S.ind, 1, , 1], H[N.H.S.ind, 1, , 1]) # number of total returning spawners at time t
  # Initial juvenile production
  fem.spawn.i <- (175200/2) * c(0.037, 0.665, 0.294, 0.004)
  N[1, 1, , ] <- round(sum(fem.spawn.i * srr(c(theta.1, theta.2), g, sum(fem.spawn.i)))) # natural production at time t = 1 is a function of half the natural-area escapement observed in 1986 (t = 0)
  H[1, 1, , ] <- y.params$ht[1] # hatchery juvenile production in year t = 1
  n[1, 1, , ] <- juv.survival(y.params$w[1]) * alpha # juvenile survival as a function of flow (outmigration) and average survival through the bay (alpha). Flow data not available for 1987, thus 1988 was used for initialization.
  # Initial ocean age 2 - abundance of ocean fish estimated from harvest, exploitation rate, and resonable age-composition of harvest
  init.harvest <- ((1 - (1 - (y.params$i[2]))) * nu)
  N[c(2, 2*A), 1, ,] <- round((y.params$total.harvest[2] * 0.025 * 0.25) / (init.harvest[1] * harvest.scalar))
  H[c(2, 2*A), 1, ,] <- round((y.params$total.harvest[2] * 0.025 * 0.25) / (init.harvest[1] * harvest.scalar))
  # Initial ocean age 3
  N[c(3, (2*A)+1), 1, ,] <- round((y.params$total.harvest[2] * 0.715 * 0.25) / (init.harvest[2] * harvest.scalar) )
  H[c(3, (2*A)+1), 1, ,] <- round((y.params$total.harvest[2] * 0.715 * 0.25) / (init.harvest[2] * harvest.scalar))
  # Initial ocean age 4
  N[c(4, (2*A)+2), 1, ,] <- round((y.params$total.harvest[2] * 0.255 * 0.25) / (init.harvest[3] * harvest.scalar))
  H[c(4, (2*A)+2), 1, ,] <- round((y.params$total.harvest[2] * 0.255 * 0.25) / (init.harvest[3] * harvest.scalar))
  # Initial ocean age 5
  N[c(5, (2*A)+3), 1, ,] <- round((y.params$total.harvest[2] * 0.005 * 0.25) / (init.harvest[4] * harvest.scalar))
  H[c(5, (2*A)+3), 1, ,] <- round((y.params$total.harvest[2] * 0.005 * 0.25) / (init.harvest[4] * harvest.scalar))
  # Initial harvest
  I.N[N.H.O.ind, 1, , ] <- round(y.params$total.harvest[1] * 0.25 * c(0.025, 0.715, 0.255, 0.005, 0.025, 0.715, 0.255, 0.005))
  I.H[N.H.O.ind, 1, , ] <- round(y.params$total.harvest[1] * 0.25 * c(0.025, 0.715, 0.255, 0.005, 0.025, 0.715, 0.255, 0.005))
  
  # Run simulation
  for(sim in 1:n.sim){
    for(t in 2:n.yr){
      # Natural area female spawners and total (female + male) spawners
      R.spawn.fem.a[, t - 1, , sim] <- (H[N.H.S.female.ind, t - 1, , sim] - B.f.h[, t - 1, , sim]) + (N[N.H.S.female.ind, t - 1, , sim] - B.f.n[, t - 1, , sim])
      
      # Natural area spawner escapement
      R.spawn[t - 1, , sim] <- Spawn[t - 1, , sim] - B.total[t - 1, , sim]
      
      # Observation model (natural area spawners and total number of spawners)
      if(t > 2){
        R.spawn.est[t - 1, , sim] <- round(rlnorm(n = 1, meanlog = log(R.spawn[t - 1, , sim]) + cor.log.Spawn.est, sdlog = sigma.log.Spawn.est)) # estimated number of spawners in natural area at pervious time
        Spawn.est[t - 1, , sim]   <- R.spawn.est[t - 1, , sim] + B.total[t - 1, , sim] # estimated total number of returning (to hatcheries and natural area) spawners at previous time
      } else {
        R.spawn.est[t - 1, , sim] <- R.spawn[t - 1, , sim] # use empirical estimate for t = 1
        Spawn.est[t - 1, , sim]   <- R.spawn[t - 1, , sim] + 26800 # use empirical estimate for t = 1
      }      
      
      # Fishery impact
      harvest[t, , sim] <- y.params$i[t] * harvest.scalar # Empirical exploitation rate modified by scalar to account for fish that remain in the ocean and delay spawning
      I.H[N.H.O.ind, t, , sim] <- fishery.impact(H[N.H.O.ind, t - 1, , sim], harvest[t, , sim], nu) # natural mortality occurs first, so harvest is limited to fish that survive
      I.N[N.H.O.ind, t, , sim] <- fishery.impact(N[N.H.O.ind, t - 1, , sim], harvest[t, , sim], nu) # natural mortality occurs first, so harvest is limited to fish that survive
      
      # Survival
      z[, t, , sim] <- (1 - (harvest[t, , sim] * nu)) * n.surv
      
      # Juvenile production
      H[1, t, , sim] <- y.params$ht[t] # hatchery production at time t
      if(reset.esc == TRUE){
        Fnt.tmp <- y.params$Nt[t - 1]/2 # reset natural-area female spawners to half of the empirically observed spawner escapement
        Fnt <- Fnt.tmp * (R.spawn.fem.a[, t - 1, , sim]/sum(R.spawn.fem.a[, t - 1, , sim])) # maintain the model age-composition of spawner escapement
      } else {
        Fnt <- R.spawn.fem.a[, t - 1, , sim] # use model natural-area female spawners
      }
      N[1, t, , sim] <- rlnorm(n = 1, meanlog = log(sum(Fnt * srr(c(theta.1, theta.2), g, sum(Fnt)))) + cor.log.j, sdlog = sigma.log.j)
      n[1, t, , sim] <- juv.survival(y.params$w[t]) * alpha # juvenile survival at time t
      
      # Ocean age-2 fish
      phi.1 <- rlnorm(n = 1, meanlog = log(phi), sd = sd)
      phi.2 <- inv.logit(y.params$npgo[t] * phi.1)
      H[c(2, 2*A), t, , sim] <- round((0.5 * H[1, t, , sim]) * n[1, t, , sim] * phi.2 * y.params$h[t]) # numbers of hatchery-origin fish that are immature, age-2 male or female, at the current time
      N[c(2, 2*A), t, , sim] <- round((0.5 * N[1, t, , sim]) * n[1, t, , sim] * phi.2)     # numbers of natural-origin fish that are immature, age-2 male or female, at the current time
      
      # Loop over ages
      for(a in 2:A){
        if(a < A){
          f.i <- a + 2*A - 2; f.o <- a + 2*A - 1; f.s <- a + 3*A - 3 # f.i source ocean age; f.o ocean age destination; f.s spawner age destination
          m.o <- a + 1; m.s <- a + A - 1 # m.o ocean age destination; m.s spawner age destination
          # Female
          H[c(f.o, f.s), t, , sim] <- c(H[f.i, t - 1, , sim] * z[a - 1, t, , sim] * (1 - m.hat[2, a - 1]), H[f.i, t - 1, , sim] * z[a - 1, t, , sim] * m.hat[2, a - 1]) # numbers of hatchery-origin female fish in the ocean at age = a + 1, and fish that mature as spawners at age = a at current time step
          N[c(f.o, f.s), t, , sim] <- c(N[f.i, t - 1, , sim] * z[a - 1, t, , sim] * (1 - m.nat[2, a - 1]), N[f.i, t - 1, , sim] * z[a - 1, t, , sim] * m.nat[2, a - 1]) # numbers of natural-origin female fish ...
          # Male
          H[c(m.o, m.s), t, , sim] <- c(H[a, t - 1, , sim] * z[a - 1, t, , sim] * (1 - m.hat[1, a - 1]), H[a, t - 1, , sim] * z[a - 1, t, , sim] * m.hat[1, a - 1]) # numbers of hatchery-origin male fish in the ocean at age = a + 1, and fish that mature as spawners at age = a at current time steps
          N[c(m.o, m.s), t, , sim] <- c(N[a, t - 1, , sim] * z[a - 1, t, , sim] * (1 - m.nat[1, a - 1]), N[a, t - 1, , sim] * z[a - 1, t, , sim] * m.nat[1, a - 1]) # numbers of natural-origin male fish ...
        } else {
          # Female
          H[n.age.stage, t, , sim] <- H[n.age.stage - A + 1, t - 1, , sim] * z[A - 1, t, , sim] # numbers of hatchery-origin female spawners of age 5 at current time
          N[n.age.stage, t, , sim] <- N[n.age.stage - A + 1, t - 1, , sim] * z[A - 1, t, , sim] # numbers of natural-origin female spawners ... 
          # Male
          H[2*A - 1, t, , sim] <- H[A, t - 1, , sim] * z[A - 1, t, , sim] # numbers of hatchery-origin male spawners of age 5 at current time
          N[2*A - 1, t, , sim] <- N[A, t - 1, , sim] * z[A - 1, t, , sim] # numbers of natural-origin male spawners ...
        }
      }
      
      jack[t, , sim]    <- rlnorm(n = 1, meanlog = log(N[A + 1, t, , sim] + H[A + 1, t, , sim]) + cor.log.Spawn.est, sdlog = sigma.log.Spawn.est) # jack escapement
      Spawn[t, , sim]   <- sum(N[N.H.S.ind, t, , sim], H[N.H.S.ind, t, , sim], na.rm = TRUE) # number of total returning spawners at time t
      B.f.h[, t, , sim] <- H[N.H.S.female.ind, t, , sim] * y.params$xt[t] # number of female hatchery-origin spawners that return to hatcheries
      B.f.n[, t, , sim] <- N[N.H.S.female.ind, t, , sim] * y.params$xt[t] # number of female natural-origin spawners ...
      B.m.h[, t, , sim] <- H[N.H.S.ind[1:A-1], t, , sim] * y.params$xt[t] # number of male hatchery-origin spawners ...
      B.m.n[, t, , sim] <- N[N.H.S.ind[1:A-1], t, , sim] * y.params$xt[t] # number of male natural-origin spawners ...
      B.total[t, , sim] <- sum(B.f.h[, t, , sim], B.f.n[, t, , sim], B.m.h[, t, , sim], B.m.n[, t, , sim], na.rm = TRUE)
      j.surv[t, , sim] <- n[1, t, , sim] * phi.2
    }
  }
  
  # Calculate model and empirical estimated total number of spawners
  B.total[1, ,] <- catch.esc$hatchery[2]
  est.spawn.df <- gather(as.data.frame(cbind((R.spawn.est[1:25, , ] + B.total[1:25, , ]), seq(1988, 2012))), 'sim', 'est.spawners', -(eval(paste0('V', n.sim + 1)))) %>% 
    dplyr::mutate(year = as.integer(eval(parse(text = paste0('V', n.sim + 1))))) %>% 
    dplyr::select(-(eval(paste0('V', n.sim + 1))))
  emp.spawn.df <- data.frame(year = as.integer(catch.esc$year[2:26]), spawners = (catch.esc$total.esc[2:26]))
  med.est.spawn.df <- est.spawn.df %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarise(median.est = (round(median(est.spawners)))) %>% 
    dplyr::mutate(year = as.integer(year))
  
  if(calibrate == TRUE){
    # calculate the sum of squared error between model simulations and empirical observations
    spawner.sse    <- sum(((emp.spawn.df$spawners) - (med.est.spawn.df$median.est)) ^ 2, na.rm = TRUE)
    return(log(spawner.sse))
  } else {
    return(list(y.params, R.spawn.est, Spawn.est, H, N, I.H, I.N, z, jack, harvest, B.total, j.surv, sum((emp.spawn.df$spawners - med.est.spawn.df$median.est) ^ 2, na.rm = TRUE)))
  }
}

## Initialize parameters to calibrate -----------------------------------------------------------------------------------
alpha.i <- 0.04 # residual juvenile survival
cv.j.i  <- 0.26 # coefficient of variation of recruitment stochasticity
phi.i   <- 0.86 # mean NPGO effect on survival
sd.i    <- 0.26 # variance of NPGO effect on survival
pars    <- c(alpha.i, cv.j.i, phi.i, sd.i)

## Optimization function ------------------------------------------------------------------------------------------------
# NOTE: uncomment the code below to run optimization function.

# cluster <- makeCluster(detectCores() - 1); setDefaultCluster(cl = cluster)
# ptm     <- proc.time()
# result  <- optimParallel(par = pars, fn = optim.simulation, method = 'L-BFGS-B', control = list(maxit = 7500), lower = c(0.001, 0.001, 0.1, 0.001), upper = c(0.1, 0.5, 1, 0.5), calibrate = TRUE, reset.esc = TRUE)
# proc.time() - ptm; setDefaultCluster(cl = NULL); stopCluster(cl = cluster)

## Run simulation model -------------------------------------------------------------------------------------------------
tmp.par <- c(0.068, 0.30, 0.86, 0.20) # Iteratively adjusted calibrated parameters to fine tune model fit (0.04, 0.26, 0.86, 0.26)
sim.results <- optim.simulation(pars = tmp.par, calibrate = FALSE, reset.esc = FALSE)
sims <- 1000; n.age.stage <- 17; A <- 5 # Model setup
N.H.O.ind <- c(2:A, (2 * A):(3 * A - 2)) # Indices of natural- and hatchery-origin population vectors that correspond to ocean fish (immature fish age 2 or greater)
N.H.S.female.ind <- (n.age.stage - (A - 2)):n.age.stage # # Indices of natural- and hatchery-origin population vectors that correspond to female spawners
N.H.S.ind <- c((A + 1):(2 * A - 1), (n.age.stage - (A - 2)):n.age.stage) # Indices of natural- and hatchery-origin population vectors that correspond to spawners
R.spawn.est <- sim.results[[2]]; Spawn.est <- sim.results[[3]]; H <- sim.results[[4]]; N <- sim.results[[5]]; I.H <- sim.results[[6]]; I.N <- sim.results[[7]]; z <- sim.results[[8]]; jack <- sim.results[[9]]; harvest <- sim.results[[10]]; B.total <- sim.results[[11]]; j.surv <- sim.results[[12]]

## Model diagnostics ----------------------------------------------------------------------------------------------------
# Juvenile survival
mean(j.surv, na.rm = TRUE)

# Hatchery-origin and natural-origin prop
test.a = 3
median((N[test.a, , 1, 1] / (N[test.a, , 1, 1] + H[test.a, , 1, 1])) * 100, na.rm = TRUE)

# Natural-area spawners
mod.nspawn.df  <- gather(as.data.frame(cbind(R.spawn.est[1:25, , ], seq(1988,2012))), 'sim', 'n.escapement', -(eval(paste0('V', sims+1)))) %>% dplyr::mutate(year = as.integer(eval(parse(text = paste0('V', sims+1))))) %>% dplyr::select(-(eval(paste0('V', sims+1))))
mod.nspawn.med <- mod.nspawn.df %>% dplyr::group_by(year) %>% dplyr::summarise(median.esc = round(median(n.escapement))) %>% dplyr::mutate(year = as.integer(year))
emp.nspawn.df  <- data.frame(year = as.integer(catch.esc$year[1:25]), spawners = catch.esc$natural[1:25])
n.spawn.plot   <- ggplot() +
  geom_line(data = mod.nspawn.df, aes(x = year, y = n.escapement, group = sim, color = 'grey70')) +
  geom_line(data = mod.nspawn.med, aes(x = year, y = median.esc, color = 'black')) +
  geom_line(data = emp.nspawn.df, aes(x = year, y = spawners, color = 'red')) +
  scale_color_manual(values = c('grey70'='grey70', 'black' = 'black', 'red' = 'red'), labels = c('median', 'simulations', 'empirical')) +
  geom_line(aes(x = mod.nspawn.med$year, y = median(mod.nspawn.med$median.esc)), linetype = 'dashed', alpha = 0.5) +
  geom_line(aes(x = emp.nspawn.df$year, y = median(emp.nspawn.df$spawners)), linetype = 'dashed', col = 'red', alpha = 0.5) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1988, 2012), breaks = seq(1988, 2012)) +
  labs(x = 'Year', y = 'Natural-area spawners') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(), legend.position = 'none')

# Total spawners
B.total[1, ,]  <- catch.esc$hatchery[2]
mod.tspawn.df  <- gather(as.data.frame(cbind((R.spawn.est[1:25, , ] + B.total[1:25, , ]), seq(1988,2012))), 'sim', 't.escapement', -(eval(paste0('V', sims+1)))) %>% dplyr::mutate(year = as.integer(eval(parse(text = paste0('V', sims+1))))) %>% dplyr::select(-(eval(paste0('V', sims+1)))) 
mod.tspawn.med <- mod.tspawn.df %>% dplyr::group_by(year) %>% dplyr::summarise(median.esc = round(median(t.escapement))) %>% dplyr::mutate(year = as.integer(year)) 
emp.tspawn.df  <- data.frame(year = as.integer(catch.esc$year[1:25]), spawners = catch.esc$total.esc[1:25])
t.spawn.plot   <- ggplot() +
  geom_line(data = mod.tspawn.df, aes(x = year, y = t.escapement/1000, group = sim, color = 'grey70')) +
  geom_line(data = mod.tspawn.med, aes(x = year, y = median.esc/1000, color = 'black')) +
  geom_line(data = emp.tspawn.df, aes(x = year, y = spawners/1000, color = 'red')) +
  scale_color_manual(values = c('grey70'='grey70', 'black' = 'black', 'red' = 'red'), labels = c('Simulations', 'Median (simulations)', 'Observed')) +
  # geom_line(aes(x = mod.tspawn.med$year, y = median(mod.tspawn.med$median.esc)/1000), linetype = 'dashed', alpha = 0.5, size = 1) +
  # geom_line(aes(x = emp.tspawn.df$year, y = median(emp.tspawn.df$spawners)/1000), linetype = 'dashed', col = 'red', alpha = 0.5, size = 1) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1988, 2012), breaks = seq(1988, 2012, by = 2)) +
  labs(x = 'Year', y = 'Total escapement (thousands)') +
  theme_classic() +
  theme(legend.title = element_blank(), legend.position = c(0.25, 0.9), plot.margin = unit(c(0.5,0.75,0.5,0.75), 'cm'), text = element_text(size = 13))

# Harvest
mod.harvest <- NULL
for(i in 1:sims){
  years <- seq(1989, 2012); 
  i.tmp <- colSums(I.H[, 2:25, , i]) + colSums(I.N[, 2:25, , i]); 
  mod.harvest <- rbind(mod.harvest, data.frame(year = years, harvest = i.tmp, sim = paste0('V',i))) 
}
med.harvest  <- mod.harvest %>% dplyr::group_by(year) %>% dplyr::summarise(med.harvest = round(median(harvest, na.rm = TRUE)))
harvest.plot <- ggplot() +
  geom_line(data = mod.harvest, aes(x = year, y = harvest, group = sim, color = 'grey70'), alpha = 0.5) +
  geom_line(data = med.harvest, aes(x = year, y = med.harvest, color = 'black')) +
  geom_line(aes(x = catch.esc$year[2:25], y = (catch.esc$total.ocean.harvest[2:25] + catch.esc$river.harvest[2:25]), color = 'red')) +
  scale_color_manual(values = c('grey70'='grey70', 'black' = 'black', 'red' = 'red'), labels = c('median', 'simulation', 'empirical')) +
  geom_line(aes(x = mod.harvest$year, y = median(med.harvest$med.harvest, na.rm = TRUE)), linetype= 'dashed', alpha = 0.5, size = 1) +
  geom_line(aes(x = mod.harvest$year, y = median((catch.esc$total.ocean.harvest[2:25] + catch.esc$river.harvest[2:25]))), linetype = 'dashed', col = 'red', alpha = 0.5, size = 1) +
  scale_x_continuous(expand = c(0, 0), limits = c(1989, 2012), breaks = seq(1989, 2012)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(), legend.position = 'none') +
  labs(x = 'Year', y = 'Harvest')
ggarrange(t.spawn.plot, harvest.plot, ncol = 2, nrow = 1, labels = c('', ''))

# Sacramento Index
mod.SI <- mod.tspawn.df %>% merge(mod.harvest, by = c('year', 'sim')) %>% mutate(mod.si = t.escapement + harvest)
med.SI <- mod.SI %>% group_by(year) %>% summarise(median.si = median(mod.si)) 
SI.plot <- ggplot() +
  geom_line(data = mod.SI, aes(x = year, y = mod.si, group = sim, color = 'a'), alpha = 0.5) +
  geom_line(data = med.SI, aes(x = year, y = median.si, color = 'b')) +
  geom_line(aes(x = catch.esc$year[3:26], y = catch.esc$total.ocean.harvest[3:26] + catch.esc$river.harvest[3:26] + catch.esc$total.esc[3:26], color = 'c')) +
  scale_color_manual(values = c('a' = 'grey70', 'b' = 'black', 'c' = 'red'), labels = c('simulations', 'median (simulations)', 'empirical')) + 
  geom_line(aes(x = med.SI$year, y = median(med.SI$median.si, na.rm = TRUE)), color = 'black', linetype = 'dashed') +
  geom_line(aes(x = med.SI$year, y = median(catch.esc$si[3:26])), color = 'red', linetype = 'dashed') +
  scale_x_continuous(expand = c(0, 0), limits = c(1989, 2012), breaks = seq(1989, 2012)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(), legend.position = c(1.5, 0.5)) +
  labs(x = 'Year', y = 'SI')

# Age-composition of ocean abundance, escapement and harvest
year <- seq(1989, 2012)
for(a in 1:(A-1)){
  # assign empty dataframes
  assign(eval(paste0('o', a + 1)), NULL)
  assign(eval(paste0('a', a + 1, '.impact')), NULL)
  assign(eval(paste0('s', a + 1)), NULL)
  tmp.ind.o <- N.H.O.ind[c(a, a + (A - 1))]
  tmp.ind.s <- N.H.S.ind[c(a, a + (A - 1))]
  # ocean abundance
  tmp.No  <- N[tmp.ind.o, 2:25, ,]
  tmp.Ho  <- H[tmp.ind.o, 2:25, ,]
  # spawner escapement
  tmp.Ns  <- N[tmp.ind.s, 2:25, ,]
  tmp.Hs  <- H[tmp.ind.s, 2:25, ,]
  # harvest
  tmp.IN  <- I.N[tmp.ind.o, 2:25, ,]
  tmp.IH  <- I.H[tmp.ind.o, 2:25, ,]
  for(i in 1:sims){
    # ocean abundance
    tmp.o    <- colSums(tmp.No[, , i]) + colSums(tmp.Ho[, , i])
    tmp.o.df <- data.frame(year = year, sim = paste0('V', i), abundance = tmp.o)
    assign(eval(paste0('o', a + 1)), rbind(get(eval(paste0('o', a + 1))), tmp.o.df))  
    # spawner escapement
    tmp.s    <- colSums(tmp.Ns[, , i]) + colSums(tmp.Hs[, , i])
    tmp.s.df <- data.frame(year = year, sim = paste0('V', i), abundance = tmp.s)
    assign(eval(paste0('s', a + 1)), rbind(get(eval(paste0('s', a + 1))), tmp.s.df))
    # harvest
    tmp.I    <- colSums(tmp.IN[, , i]) + colSums(tmp.IH[, , i])
    tmp.I.df <- data.frame(year = year, sim = paste0('V', i), harvest = tmp.I)
    assign(eval(paste0('a', a + 1, '.impact')), rbind(get(eval(paste0('a', a + 1, '.impact'))), tmp.I.df))
  }
  # ocean abundance
  assign(eval(paste0('o', a + 1, '.med')), (get(eval(paste0('o', a + 1))) %>% group_by(year) %>% dplyr::summarise(med = round(median(abundance))) %>% dplyr::mutate(a = a+1)))
  assign(eval(paste0('o', a + 1, '.plot')), 
         ggplot() +
           geom_line(data = eval(parse(text = paste0('o', a + 1))), aes(x = year, y = abundance, group = sim, color = 'grey70'), alpha = 0.5) +
           geom_line(data = eval(parse(text = paste0('o', a + 1, '.med'))), aes(x = year, y = med, color = 'black')) +
           scale_color_manual(values = c('grey70' = 'grey70', 'black' = 'black'), labels = c('median', 'simulations')) +
           scale_x_continuous(expand = c(0, 0), limits = c(1989, 2012), breaks = seq(1989, 2012)) +
           scale_y_continuous(expand = c(0, 0)) +
           theme_classic() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(), legend.position = c(0.75, 0.95)) +
           labs(x = 'Year', y = paste('Ocean age-', a + 1, " abundance")))
  # spawner escapement
  assign(eval(paste0('s', a + 1, '.med')), (get(eval(paste0('s', a + 1))) %>% group_by(year) %>% dplyr::summarise(med = round(median(abundance))) %>% dplyr::mutate(a = a+1)))
  assign(eval(paste0('s', a + 1, '.plot')), 
         ggplot() +
           geom_line(data = eval(parse(text = paste0('s', a + 1))), aes(x = year, y = abundance, group = sim, color = 'grey70'), alpha = 0.5) +
           geom_line(data = eval(parse(text = paste0('s', a + 1, '.med'))), aes(x = year, y = med, color = 'black')) +
           scale_color_manual(values = c('grey70' = 'grey70', 'black' = 'black'), labels = c('median', 'simulations')) +
           scale_x_continuous(expand = c(0, 0), limits = c(1989, 2012), breaks = seq(1989, 2012)) +
           scale_y_continuous(expand = c(0, 0)) +
           theme_classic() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(), legend.position = c(0.75, 0.95)) +
           labs(x = 'Year', y = paste('Spawner age-', a + 1, " escapement")))
  # harvest
  assign(eval(paste0('a', a + 1, '.med')), get(eval(paste0('a', a + 1, '.impact'))) %>% group_by(year) %>% dplyr::summarise(med = round(median(harvest))) %>% dplyr::mutate(a = a+1))
  assign(eval(paste0('a', a + 1, '.harvest.plot')), 
         ggplot() +
           geom_line(data = eval(parse(text = paste0('a', a + 1, '.impact'))), aes(x = year, y = harvest, group = sim, color = 'grey70'), alpha = 0.5) +
           geom_line(data = eval(parse(text = paste0('a', a + 1, '.med'))), aes(x = year, y = med, color = 'black')) +
           scale_color_manual(values = c('grey70' = 'grey70', 'black' = 'black'), labels = c('median', 'simulations')) +
           scale_x_continuous(expand = c(0, 0), limits = c(1989, 2012), breaks = seq(1989, 2012)) +
           scale_y_continuous(expand = c(0, 0)) +
           theme_classic() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(), legend.position = c(0.75, 0.95)) +
           labs(x = 'Year', y = paste('Ocean age-', a + 1, " harvest")))
}
o.abundance  <- rbind(o2.med, o3.med, o4.med, o5.med) %>% group_by(year) %>% mutate(prop = med / sum(med))
s.escapement <- rbind(s2.med, s3.med, s4.med, s5.med) %>% group_by(year) %>% mutate(prop = med / sum(med))
a.impact     <- rbind(a2.med, a3.med, a4.med, a5.med) %>% group_by(year) %>% mutate(prop = med / sum(med))
a.impact.plot <- ggplot() +
  geom_area(data = a.impact, aes(x = year, y = prop, fill = as.factor(a))) +
  labs(x = 'Year', y = 'Proportion of harvest') +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill=guide_legend(title="Ocean age")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1989, 2012), breaks = seq(1989, 2012)) 
s.escapement.plot <- ggplot() +
  geom_area(data = s.escapement, aes(x = year, y = prop, fill = as.factor(a))) +
  labs(x = 'Year', y = 'Proportion of spawner escapement') +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill=guide_legend(title="Age")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limits = c(1989, 2012), breaks = seq(1989, 2012)) 
o.abundance.plot <- ggplot() +
  geom_area(data = o.abundance, aes(x = year, y = prop, fill = as.factor(a))) +
  labs(x = 'Year', y = 'Proportion of ocean abundance') +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill=guide_legend(title="Age")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limits = c(1988, 2012), breaks = seq(1988, 2012)) 
ggarrange(o2.plot, o3.plot, o4.plot, o5.plot, o.abundance.plot, ncol = 2, nrow = 3)
ggarrange(a2.harvest.plot, a3.harvest.plot, a4.harvest.plot, a5.harvest.plot, a.impact.plot, ncol = 2, nrow = 3)

# Spawners as a function of flow and NPGO
env.df <- data.frame(year = mod.tspawn.med$year, mod.spawn = mod.tspawn.med$median.esc, emp.spawn = emp.tspawn.df$spawners, flow = flow$discharge[1:25], npgo = npgo$npgo[1:25])
env.df <- cbind(env.df, as.data.frame(embed(c(rep(NA, 6), flow$discharge[1:25]), 6)[-1,-1]) %>% dplyr::rename(flow.t1 = V1, flow.t2 = V2, flow.t3 = V3, flow.t4 = V4, flow.t5 = V5), as.data.frame(embed(c(rep(NA, 6), npgo$npgo[1:25]), 6)[-1,-(1)]) %>% dplyr::rename(npgo.t1 = V1, npgo.t2 = V2, npgo.t3 = V3, npgo.t4 = V4, npgo.t5 = V5))
# GAM to test consistency between relationships of flow and npgo
flow.gam.emp <- mgcv::gam(env.df$emp.spawn ~ s(env.df$flow.t2, k = 3), family = poisson(link = 'log'))
summary(flow.gam.emp)
flow.emp.plot <- gratia::draw(flow.gam.emp) +
  scale_y_continuous(expand=c(0,0), limits = c(-0.65,0.65)) +
  theme_classic() +
  labs(x = expression(paste('Flow (', italic('t'), ' - 2)')), title = 'Observed') +
  theme(text = element_text(size = 13)) +
  annotate('text', x = 15000, y = -0.25, label = 'Deviance explained = 22%')
flow.gam.mod <- mgcv::gam(env.df$mod.spawn ~ s(env.df$flow.t2, k = 3), family = poisson(link = 'log'))
summary(flow.gam.mod)
flow.mod.plot <- gratia::draw(flow.gam.mod) +
  scale_y_continuous(expand=c(0,0), limits = c(-0.65,0.65)) +
  theme_classic() +
  labs(x = expression(paste('Flow (', italic('t'), ' - 2)')), y = "", title = 'Simulated') +
  theme(text = element_text(size = 13)) +
  annotate('text', x = 15000, y = -0.25, label = 'Deviance explained = 22.8%')
flow.plots <- ggarrange(flow.emp.plot, flow.mod.plot, labels=c('b','c'))
ggarrange(t.spawn.plot, flow.plots, nrow = 2, ncol = 1, labels = c('a','')) # FIGURE 1


flow.gam.df <- data.frame(year = env.df$year[4:25], emp.s = env.df$emp.spawn[4:25], mod.s = env.df$mod.spawn[4:25], flow = env.df$flow.t3[4:25])
ggplot() + 
  geom_point(data = flow.gam.df, aes(x = flow, y = emp.s)) + 
  geom_smooth(data = flow.gam.df, aes(x = flow, y = emp.s), color = 'black') +
  geom_point(data = flow.gam.df, aes(x = flow, y = mod.s), color = 'red') +
  geom_smooth(data = flow.gam.df, aes(x = flow, y = mod.s), color = 'red')

npgo.gam.emp <- mgcv::gam(env.df$emp.spawn ~ s(env.df$npgo.t1, k = 3), family = poisson(link = 'log'))
summary(npgo.gam.emp)
plot(npgo.gam.emp)

npgo.gam.mod <- mgcv::gam(env.df$mod.spawn ~ s(env.df$npgo.t1, k = 3), family = poisson(link = 'log'))
summary(npgo.gam.mod)
plot(npgo.gam.mod)


# Temporal autocorrelation
ac.esc.emp <- acf(log(catch.esc$total.esc), lag.max = A, plot = TRUE)
ac.esc.mod <- acf(log(mod.tspawn.med$median.esc), lag.max = A, plot = TRUE)
par(mfrow = c(1,2))
plot(ac.esc.emp, main = "Observed total escapement")
plot(ac.esc.mod, main = "Simulated total escapement")

ac.harvest.emp <- acf((catch.esc$total.ocean.harvest + catch.esc$river.harvest), lag.max = A, plot = TRUE)
ac.harvest.mod <- acf(med.harvest$med.harvest, lag.max = A, plot = TRUE)

ac.si.emp <- acf(log(catch.esc$si), lag.max = A, plot = TRUE) # For SRFC, there is substantial evidence for positive lag-1 autocorrelation in log-transformed values of the Sacramento Index.
ac.si.mod <- acf(log(med.SI$median.si), lag.max = A, plot = TRUE)

par(mfrow = c(3, 2))
plot(ac.esc.emp, main = 'Empirical escapement')
plot(ac.esc.mod, main = 'Model escapement')
plot(ac.harvest.emp, main = 'Empirical harvest')
plot(ac.harvest.mod, main = 'Model harvest')
plot(ac.si.emp, main = 'Empirical SI')
plot(ac.si.mod, main = 'Model SI')



