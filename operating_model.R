operating.model <- function(pars, years = 100, sims = 1000, m.maturity = NULL, 
                            n.surv = NULL, scenario = 'base'){
  # Load libraries for parallel computing
  library(foreach)
  library(doParallel)
  
  # Load data
  load('srfc_data.RData')
  
  # Calibrated parameters
  alpha <- pars[1] # bay/delta survival
  cv.j  <- pars[2] # coefficient of variation of recruitment stochasticity
  phi   <- pars[3] # NPGO dependent survival
  sd    <- pars[4] # NPGO dependent survival
  
  # Set model parameters
  n.yr        <- years   # number of years
  n.sim       <- sims # number of simulations per optimization run
  n.age.stage <- 17   # number of age/stage classes; fry/pre-smolts, immature males (ages 2:A), mature males (ages 2:A), immature females (ages 2:A), mature females (ages 2:A)
  n.pops      <- 1    # number of populations to simulate
  A           <- 5    # maximum age
  
  # Set population dynamics parameters
  theta.1       <- 0.3    # Maximum egg-to-fry survival rate: theta.1=0.3 and theta.2=1.5e-8 generates reasonable N:H
  theta.2       <- 1.5e-8 # Strength of density dependence
  f.maturity    <- m.maturity # option to define separate female and male maturation rates
  nat.maturity  <- m.maturity * c(1, 1, 1, 1) # natural-origin maturation rates in relation to hatchery derived maturation rates (fewer male fish mature at ages 2 and 3 for natural-origin)
  m.hat         <- t(array(c(m.maturity,f.maturity), dim = c(A-1, 2), dimnames <- list(c(paste('age.', 2:A, sep = "")), c('male' ,'female')))) # hatchery maturation array; NOTE - the array will automatically adjust for age/stage structure specified in above parameters
  m.nat         <- t(array(c(nat.maturity,f.maturity), dim = c(A-1, 2), dimnames <- list(c(paste('age.', 2:A, sep = "")), c('male' ,'female')))) # natural maturation array; NOTE - the array will automatically adjust for age/stage structure specified in above parameters
  cv.spawn.est  <- 0.2  # coefficient of variation of spawner abundance estimates - value results in plausible levels of abundance forecast error (https://s3.amazonaws.com/media.fisheries.noaa.gov/2020-10/SRFC-RP_finalEA-FONSI.pdf?null=)
  g             <- c(4325, 5407, 5407, 6488) # age-3 and -4 spawners expected to have similar fecundity, ages-2 and -5 spawners have 20% increase and decrease in fecundity, respoectively.
  # n.surv        <- c(0.5, 0.8, 0.8, 0.8) #c(0.5|0.35, 0.8, 0.8, 0.8) survival rate of fish aged 2 to 5; values from MO.
  nu            <- c(0.02, 1, 1, 1) # relative fishing mortality rate c(2, 0.25, 0.25, 0.25) 2.1, 0.3, 0.1, 0.1
  
  # Natural reproduction parameters
  sigma.log.j <- sqrt(log(cv.j ^ 2 + 1)) # SD of log of recruitment deviations
  cor.log.j   <- -0.5 * (sigma.log.j ^ 2) # Bias correction for mean of log of recruitment deviations
  
  # Observation parameters
  sigma.log.Spawn.est <- sqrt(log(cv.spawn.est ^ 2 + 1)) # SD of log R.sum.est
  cor.log.Spawn.est   <- -0.5 * (sigma.log.Spawn.est ^ 2) # Bias correction for mean of log R.sum.est
  
  # SI forecast parameters
  beta.0   <- 7.319135  # intercept
  beta.1   <- 0.5696732 # slope
  rho      <- 0.7529011 # autocorrelation coefficient
  sigma.si <- 0.1422386 # variance of the normally distributed error component of the fitted model 
  
  # Realized harvest parameters
  cv.er <- 0.32 # cv.er = 0.32 based on CV of exploitation rates Winship et al. (2013); 0.05 sensitivity analysis
  
  # Simulation indices
  Z.nat.r.ind      <- as.matrix(expand.grid(1, (n.age.stage - (A - 2)):n.age.stage)) # Indices of Leslie transition matrix that correspond to recruitment rate
  N.H.O.ind        <- c(2:A, (2 * A):(3 * A - 2)) # Indices of natural- and hatchery-origin population vectors that correspond to ocean fish (immature fish age 2 or greater)
  N.H.S.female.ind <- (n.age.stage - (A - 2)):n.age.stage # # Indices of natural- and hatchery-origin population vectors that correspond to female spawners
  N.H.S.ind        <- c((A + 1):(2 * A - 1), (n.age.stage - (A - 2)):n.age.stage) # Indices of natural- and hatchery-origin population vectors that correspond to spawners
  
  # State variables
  N <- H <- I.N <- I.H <- array(0, dim = c(n.age.stage, n.yr, n.pops)) # Numbers of natural-origin individuals (N); # numbers of hatchery-origin individuals (H); # natural-origin impacts (I.N, during period following time t); hatchery-origin impacts; rows are fry/pre-smolts, immature males (ages 2:A), mature males (ages 2:A), immature females (ages 2:A), mature females (ages 2:A); columns are times
  age.c <- B.m.h <- B.m.n <- B.f.h <- B.f.n <- R.spawn.fem.a <-  array(0, dim = c(A-1, n.yr, n.pops)) # number of spawners that return to hatcheries
  j.surv <- NH.ratio <- harvest <- real.c <- mu.c <- SI.error <- SI.observed <- SI.forecast <- jack <- Spawn <- B.total <- R.spawn <- Spawn.est <- R.spawn.est <- spawn.2 <- spawn.3 <- spawn.4 <- spawn.5 <- harvest.2 <- harvest.3 <- harvest.4 <- harvest.5 <- ocean.2 <- ocean.3 <- ocean.4 <- ocean.5 <- ocean <- array(NA, dim = c(n.yr, n.pops)) # Number of returning spawners (at time t), number of spawners in natural area (at time t), broodstock (per sex) removed at time t, number of female spawners in natural area (at time t), estimated number of returning spawners (at time t), estimated number of spawners in natural area (at time t), impact rate specified by the management strategy at time t, realized impact rate south of Point Arena following time t, and population size at time t (sum of spawners at time t and in previous two years)
  n <- array(NA, dim = c(A, n.yr, n.pops)) # natural survival rate (probability of surviving to next age (based on flow for juveniles) after harvest during the current age for ages >= 2); columns are times; Note: impact rates and natural survival rates are not sex- or origin-specific
  
  # Survival/harvest parameters
  n[2:A, , ]     <- n.surv # survival rate of fish aged 2 and older
  harvest.scalar <- 1.0
  
  # Initialize population
  # Initial number of hatchery escapement
  B.m.h[, 1, ] <- B.m.n[, 1, ] <- round(c(0.234, 0.583, 0.180, 0.003) * (catch.esc$hatchery[1]/4))
  B.f.h[, 1, ] <- B.f.n[, 1, ] <- round(c(0.037, 0.665, 0.294, 0.004) * (catch.esc$hatchery[1]/4))
  B.total[1, ] <- sum(B.f.h[, 1, ], B.f.n[, 1, ], B.m.h[, 1, ], B.m.n[, 1, ])
  # Initial number of total escapement
  N[N.H.S.ind, 1, ] <- round(c(0.234, 0.583, 0.18, 0.003, 0.037, 0.665, 0.294, 0.004) * ((catch.esc$total.esc[1])/4)) # initial number of natural-origin spawners
  H[N.H.S.ind, 1, ] <- round(c(0.234, 0.583, 0.18, 0.003, 0.037, 0.665, 0.294, 0.004) * ((catch.esc$total.esc[1])/4)) # initial number of hatchery-origin spawners
  Spawn[1, ]        <- sum(N[N.H.S.ind, 1, ], H[N.H.S.ind, 1, ]) # number of total returning spawners at time t
  # Initial juvenile production
  fem.spawn.i <- (175200/2) * c(0.037, 0.665, 0.294, 0.004)
  N[1, 1, ] <- round(sum(fem.spawn.i * srr(c(theta.1, theta.2), g, sum(fem.spawn.i)))) # natural production at time t = 1 is a function of half the natural-area escapement observed in 1986 (t = 0)
  H[1, 1, ] <- hat.release$total.release[1] # hatchery juvenile production in year t = 1
  n[1, 1, ] <- juv.survival(flow$discharge[1]) * alpha # juvenile survival as a function of flow (outmigration) and average survival through the bay (alpha). Flow data not available for 1987, thus 1988 was used for initialization.
  # Initial ocean age 2 - abundance of ocean fish estimated from harvest, exploitation rate, and resonable age-composition of harvest
  init.harvest <- ((1 - (1 - ((catch.esc$exploitation.rate/100)[2]))) * nu)
  init.total.harvest.1 <- catch.esc$total.ocean.harvest[1] + catch.esc$river.harvest[1]
  init.total.harvest.2 <- catch.esc$total.ocean.harvest[2] + catch.esc$river.harvest[2]
  N[c(2, 2*A), 1, ] <- round((init.total.harvest.2 * 0.025 * 0.25) / (init.harvest[1] * harvest.scalar))
  H[c(2, 2*A), 1, ] <- round((init.total.harvest.2 * 0.025 * 0.25) / (init.harvest[1] * harvest.scalar))
  # Initial ocean age 3
  N[c(3, (2*A)+1), 1, ] <- round((init.total.harvest.2 * 0.715 * 0.25) / (init.harvest[2] * harvest.scalar) )
  H[c(3, (2*A)+1), 1, ] <- round((init.total.harvest.2 * 0.715 * 0.25) / (init.harvest[2] * harvest.scalar))
  # Initial ocean age 4
  N[c(4, (2*A)+2), 1, ] <- round((init.total.harvest.2 * 0.255 * 0.25) / (init.harvest[3] * harvest.scalar))
  H[c(4, (2*A)+2), 1, ] <- round((init.total.harvest.2 * 0.255 * 0.25) / (init.harvest[3] * harvest.scalar))
  # Initial ocean age 5
  N[c(5, (2*A)+3), 1, ] <- round((init.total.harvest.2 * 0.005 * 0.25) / (init.harvest[4] * harvest.scalar))
  H[c(5, (2*A)+3), 1, ] <- round((init.total.harvest.2 * 0.005 * 0.25) / (init.harvest[4] * harvest.scalar))
  # Initial harvest
  I.N[N.H.O.ind, 1, ] <- round(init.total.harvest.1 * 0.25 * c(0.025, 0.715, 0.255, 0.005, 0.025, 0.715, 0.255, 0.005))
  I.H[N.H.O.ind, 1, ] <- round(init.total.harvest.1 * 0.25 * c(0.025, 0.715, 0.255, 0.005, 0.025, 0.715, 0.255, 0.005))
  harvest[1, ] <- init.total.harvest.1
  
  # Set up parallel backend to run multiple simulations simultaneously
  cores <- detectCores() # CPU cores
  cl <- makeCluster(cores-1) # Create R copies and reduce by 1 to avoid overloading computer
  registerDoParallel(cl) # register parallel backend with foreach package

  # Run simulation
  mse.out <- foreach(sim = 1:n.sim) %dopar% {
    # load libraries and functions for each parallel for loop
    source('mse_functions.R') # load functions

    # Simulate annual biological and environmental observations
    vars <- set.vars(n.yr = n.yr, scenario = scenario) # simulate variables
    w    <- vars$w
    ht   <- vars$ht # hatchery smolts released each year
    hd   <- vars$hd # distance between hatchery and release site
    xt   <- vars$xt # proportion of total escapement that returns to hatcheries
    npgo <- vars$npgo # North Pacific Gyre oscillation

    for(t in 2:n.yr){
      # Natural area female spawners
      R.spawn.fem.a[, t - 1, ] <- (H[N.H.S.female.ind, t - 1, ] - B.f.h[, t - 1, ]) + (N[N.H.S.female.ind, t - 1, ] - B.f.n[, t - 1, ])

      # Natural area spawner escapement (female + male)
      R.spawn[t - 1, ] <- Spawn[t - 1, ] - B.total[t - 1, ]

      # Observation model (natural area spawners [R.spawn.est] and total number of spawners[Spawn.est])
      R.spawn.est[t - 1, ] <- round(rlnorm(n = 1, meanlog = log(R.spawn[t - 1, ]) + cor.log.Spawn.est, sdlog = sigma.log.Spawn.est)) # estimated number of spawners in natural area at previous time
      Spawn.est[t - 1, ]   <- R.spawn.est[t - 1, ] + B.total[t - 1, ] # estimated total number of returning (to hatcheries and natural area) spawners at previous time
      SI.observed[t - 1, ] <- Spawn.est[t - 1, ] + sum(I.N[, t - 1, ]) + sum(I.H[, t - 1, ])
      
      if(t == 2){ # Keep empirical estimate for t = 1
        R.spawn.est[t - 1, ] <- R.spawn[t - 1, ] # use empirical estimate for t = 1
        Spawn.est[t - 1, ]   <- R.spawn[t - 1, ] + 26800 # use empirical estimate for t = 1
      }

      # Juvenile production
      H[1, t, ] <- ht[t] # hatchery production at time t
      Fnt <- R.spawn.fem.a[, t - 1, ] # use model natural-area female spawners at time t - 1
      N[1, t, ] <- rlnorm(n = 1, meanlog = log(sum(Fnt * srr(c(theta.1, theta.2), g, sum(Fnt)))) + cor.log.j, sdlog = sigma.log.j)
      
      # Natural mortality
      for(a in 1:(A-1)){ # loop over ocean ages
         H[c(N.H.O.ind[a], N.H.O.ind[a+A-1]), t, ] <- rbinom(2, H[c(N.H.O.ind[a], N.H.O.ind[a+A-1]), t-1, ], n[a+1, t, ])
         N[c(N.H.O.ind[a], N.H.O.ind[a+A-1]), t, ] <- rbinom(2, N[c(N.H.O.ind[a], N.H.O.ind[a+A-1]), t-1, ], n[a+1, t, ])
      }   
      
      # Forecast model
      # SI.error[t - 1, ]    <- ifelse(t > 2, log(SI.observed[t - 1, ]) - log(SI.forecast[t - 1, ]), 0)
      # jack[t - 1, ]        <- round(rlnorm(n = 1, meanlog = log(N[A + 1, t - 1, ] + H[A + 1, t - 1, ]) + cor.log.Spawn.est, sdlog = sigma.log.Spawn.est)) # observe jack escapement
      # SI.log.forecast      <- beta.0 + (beta.1 * log(jack[t - 1, ])) + (rho * SI.error[t - 1, ])
      # SI.forecast[t, ]     <- round(exp(SI.log.forecast + (0.5 * sigma.si)))
      # SI.forecast[t, ] <- sum(N[N.H.O.ind, t - 1, ], H[N.H.O.ind, t - 1, ]) * exp(forecast.error)
      forecast.error       <- rnorm(1, mean = 0.1317641, sd = 0.4858035) # mean = 0.1317641, sd = 0.4858035
      SI.forecast[t, ]     <- sum(N[c(3:5,11:13), t, ], H[c(3:5,11:13), t, ]) * exp(forecast.error) # use adult ocean abundance for forecast
      
      # Harvest control rule
      mu.c[t, ] <- control.rule(SI.forecast[t, ]) # harvest rate determined from control rule
      tmp.c <- mu.c[t, ] * harvest.scalar # modulate specified harvest rate to account for age-3 and -4 fish that delay spawning and remain in the ocean
      
      # Realized exploitation rate
      c.alpha <- (1 - (tmp.c * (1 + (cv.er^2)))) / (cv.er^2)
      c.beta  <- ((1/tmp.c) - 2 + tmp.c + (tmp.c - 1) * (cv.er^2)) / (cv.er^2)
      real.c[t, ] <- rbeta(1, c.alpha, c.alpha)
      
      # Fishery impact
      age.c[, t, ] <- real.c[t, ] * nu
      I.H[N.H.O.ind, t, ] <- rbinom(rep(1, length(N.H.O.ind)), H[N.H.O.ind, t, ], rep(age.c[, t, ], 2))
      I.N[N.H.O.ind, t, ] <- rbinom(rep(1, length(N.H.O.ind)), N[N.H.O.ind, t, ], rep(age.c[, t, ], 2))
      harvest[t, ] <- sum(I.H[N.H.O.ind, t, ] + I.N[N.H.O.ind, t, ])
      
      # Maturation and age transition
      H.tmp <- N.tmp <- array(0, dim = c(n.age.stage, n.pops)) # create empty shells for storing age/stage transitions
      for(a in 2:A){ # loop over ocean ages
         f.i <- a + 2*A - 2; f.o <- a + 2*A - 1; f.s <- a + 3*A - 3 # f.i source ocean age; f.o ocean age destination; f.s spawner age destination   
         m.o <- a + 1; m.s <- a + A - 1                             # a is source age; m.o ocean age destination; m.s spawner age destination
         if(a < A){
            # female
            H.tmp[c(f.o, f.s), ] <- rmultinom(1, H[f.i, t, ] - I.H[f.i, t, ], c((1 - m.hat[2, a-1]), m.hat[2, a-1]))
            N.tmp[c(f.o, f.s), ] <- rmultinom(1, N[f.i, t, ] - I.N[f.i, t, ], c((1 - m.nat[2, a-1]), m.nat[2, a-1]))
            # male
            H.tmp[c(m.o, m.s), ] <- rmultinom(1, H[a, t, ] - I.H[a, t, ], c((1 - m.hat[1, a-1]), m.hat[1, a-1]))
            N.tmp[c(m.o, m.s), ] <- rmultinom(1, N[a, t, ] - I.N[a, t, ], c((1 - m.nat[1, a-1]), m.nat[1, a-1]))
         } else {
            # female
            H.tmp[n.age.stage, ] <- (H[n.age.stage-A+1, t, ] - I.H[n.age.stage-A+1, t, ]) * m.hat[2, A-1]
            N.tmp[n.age.stage, ] <- (N[n.age.stage-A+1, t, ] - I.N[n.age.stage-A+1, t, ]) * m.nat[2, A-1]
            # male
            H.tmp[2*A-1, ] <- (H[A, t, ] - I.H[A, t, ]) * m.hat[1, A-1]
            N.tmp[2*A-1, ] <- (N[A, t, ] - I.N[A, t, ]) * m.nat[1, A-1]
         }
      }
      
      # Juvenile flow, npgo mortality and transition to ocean age 2
      n[1, t, ] <- juv.survival(w[t]) * alpha # juvenile survival at time t
      phi.1 <- rlnorm(n = 1, meanlog = log(phi), sd = sd)
      phi.2 <- exp(npgo[t] * phi.1) / (1 + exp(npgo[t] * phi.1)) # inverse logit
      H.tmp[c(2, 2*A), ] <- rmultinom(1, H[1, t, ], c((0.5 * n[1, t, ] * phi.2 * hd[t]), 
                                                      (0.5 * n[1, t, ] * phi.2 * hd[t]), 
                                                      (1 - n[1, t, ] * phi.2 * hd[t])))[1:2]
      N.tmp[c(2, 2*A), ] <- rmultinom(1, N[1, t, ], c((0.5 * n[1, t, ] * phi.2), 
                                                      (0.5 * n[1, t, ] * phi.2), 
                                                      (1 - n[1, t, ] * phi.2)))[1:2]
      
      # Set population at the end of the year
      H[2:n.age.stage, t, ] <- H.tmp[2:n.age.stage]
      N[2:n.age.stage, t, ] <- N.tmp[2:n.age.stage]

      # Save variables
      NH.ratio[t,] <- sum(N[N.H.S.ind, t, ]) / sum(H[N.H.S.ind, t, ])
      ocean[t, ]   <- sum(N[N.H.O.ind[c(2:4,(A+1):(A+3))], t, ], H[N.H.O.ind[c(2:4,(A+1):(A+3))], t, ]) # total ocean abundance
      ocean.2[t, ] <- sum(N[N.H.O.ind[c(1,A)], t, ], H[N.H.O.ind[c(1,A)], t, ]) # number of age-2 ocean
      ocean.3[t, ] <- sum(N[N.H.O.ind[c(2,A+1)], t, ], H[N.H.O.ind[c(2,A+1)], t, ]) # number of age-3 ocean
      ocean.4[t, ] <- sum(N[N.H.O.ind[c(3,A+2)], t, ], H[N.H.O.ind[c(3,A+2)], t, ]) # number of age-4 ocean
      ocean.5[t, ] <- sum(N[N.H.O.ind[c(4,A+3)], t, ], H[N.H.O.ind[c(4,A+3)], t, ]) # number of age-5 ocean
      Spawn[t, ]   <- sum(N[N.H.S.ind, t, ], H[N.H.S.ind, t, ]) # number of total returning spawners at time t
      spawn.2[t, ] <- sum(N[N.H.S.ind[c(1,A)], t, ], H[N.H.S.ind[c(1,A)], t, ]) # number of age-2 spawners
      spawn.3[t, ] <- sum(N[N.H.S.ind[c(2,A+1)], t, ], H[N.H.S.ind[c(2,A+1)], t, ]) # number of age-3 spawners
      spawn.4[t, ] <- sum(N[N.H.S.ind[c(3,A+2)], t, ], H[N.H.S.ind[c(3,A+2)], t, ]) # number of age-4 spawners
      spawn.5[t, ] <- sum(N[N.H.S.ind[c(4,A+3)], t, ], H[N.H.S.ind[c(4,A+3)], t, ]) # number of age-5 spawners
      harvest.2[t, ] <- sum(I.N[N.H.O.ind[c(1,A)], t, ], I.H[N.H.O.ind[c(1,A)], t, ]) # number of age-2 harvest
      harvest.3[t, ] <- sum(I.N[N.H.O.ind[c(2,A+1)], t, ], I.H[N.H.O.ind[c(2,A+1)], t, ]) # number of age-3 harvest
      harvest.4[t, ] <- sum(I.N[N.H.O.ind[c(3,A+2)], t, ], I.H[N.H.O.ind[c(3,A+2)], t, ]) # number of age-4 harvest
      harvest.5[t, ] <- sum(I.N[N.H.O.ind[c(4,A+3)], t, ], I.H[N.H.O.ind[c(4,A+3)], t, ]) # number of age-5 harvest
      B.f.h[, t, ] <- round(H[N.H.S.female.ind, t, ] * xt[t]) # number of female hatchery-origin spawners that return to hatcheries
      B.f.n[, t, ] <- round(N[N.H.S.female.ind, t, ] * xt[t]) # number of female natural-origin spawners ...
      B.m.h[, t, ] <- round(H[N.H.S.ind[1:A-1], t, ] * xt[t]) # number of male hatchery-origin spawners ...
      B.m.n[, t, ] <- round(N[N.H.S.ind[1:A-1], t, ] * xt[t]) # number of male natural-origin spawners ...
      B.total[t, ] <- sum(B.f.h[, t, ], B.f.n[, t, ], B.m.h[, t, ], B.m.n[, t, ])
      j.surv[t, ]  <- n[1, t, 1] * phi.2

    } # end t
    out <- data.frame(year = 1:n.yr, Spawn.est, harvest, NH.ratio, j.surv, SI.observed, SI.forecast, SI.error, Spawn, jack, mu.c, real.c, spawn.2, spawn.3, spawn.4, spawn.5, harvest.2, harvest.3, harvest.4, harvest.5, ocean.2, ocean.3, ocean.4, ocean.5, ocean)
  
  } # end parfor

  stopCluster(cl = cl)

  # Unpackage parallel for loop output
  for(i in seq_len(n.sim)){mse.out[[i]] <- cbind(mse.out[[i]], sim = paste0('s', i))}
  return(do.call('rbind', mse.out))
}






