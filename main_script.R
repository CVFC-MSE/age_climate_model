## ----------------------------------------------------------------------------------------------------------------------
## model_fitting.R
##
## Author: Paul Carvalho (paul.carvalho@noaa.gov, pcarvalh@ucsc.edu)
## Last update: May 25, 2022
##
## Description: Run simulation models to test the effects of drought on Sacramento River fall run Chinook salmon under
##              different age structure scenarios. Two mechanisms that affect age structure were tested separately - 
##              mortality and maturation.
## ----------------------------------------------------------------------------------------------------------------------

## General notes --------------------------------------------------------------------------------------------------------
# 1. Use docstring('insert function name') to view function documentation and information.

# Setup workspace -------------------------------------------------------------------------------------------------------
# Clear workspace
rm(list = ls())
# Load data
load('srfc_data.RData')
# Load functions stored in other scripts
source('model_functions.r')
source('mse_simulation.r')

# Load libraries
# library(docstring) # NOTE: Use docstring("function name") to display R documentation for functions defined in this script
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(foreach)
library(doParallel)


# Functions ------------------------------------------------------------------------------------------------------------
age_struct_plot <- function(df.list, spawn1_harvest0, labels){
  if(spawn1_harvest0 == 1){
    mean1 <- df.list[[1]]$spawn.mean/1000; mean2 <- df.list[[2]]$spawn.mean/1000; mean3 <- df.list[[3]]$spawn.mean/1000; mean4 <- df.list[[4]]$spawn.mean/1000; mean5 <- df.list[[5]]$spawn.mean/1000
    pilo1 <- df.list[[1]]$spawn.pi.lo/1000; pilo2 <- df.list[[2]]$spawn.pi.lo/1000; pilo3 <- df.list[[3]]$spawn.pi.lo/1000; pilo4 <- df.list[[4]]$spawn.pi.lo/1000; pilo5 <- df.list[[5]]$spawn.pi.lo/1000
    piup1 <- df.list[[1]]$spawn.pi.up/1000; piup2 <- df.list[[2]]$spawn.pi.up/1000; piup3 <- df.list[[3]]$spawn.pi.up/1000; piup4 <- df.list[[4]]$spawn.pi.up/1000; piup5 <- df.list[[5]]$spawn.pi.up/1000
  } else if(spawn1_harvest0 == 0){
    mean1 <- df.list[[1]]$harvest.mean/1000; mean2 <- df.list[[2]]$harvest.mean/1000; mean3 <- df.list[[3]]$harvest.mean/1000; mean4 <- df.list[[4]]$harvest.mean/1000; mean5 <- df.list[[5]]$harvest.mean/1000
    pilo1 <- df.list[[1]]$harvest.pi.lo/1000; pilo2 <- df.list[[2]]$harvest.pi.lo/1000; pilo3 <- df.list[[3]]$harvest.pi.lo/1000; pilo4 <- df.list[[4]]$harvest.pi.lo/1000; pilo5 <- df.list[[5]]$harvest.pi.lo/1000
    piup1 <- df.list[[1]]$harvest.pi.up/1000; piup2 <- df.list[[2]]$harvest.pi.up/1000; piup3 <- df.list[[3]]$harvest.pi.up/1000; piup4 <- df.list[[4]]$harvest.pi.up/1000; piup5 <- df.list[[5]]$harvest.pi.up/1000
  }
  
  out <- ggplot() +
    geom_point(aes(x = c('1', '2', '3', '4', '5'), y = c(mean1, mean2, mean3, mean4, mean5)), size = 3) +
    geom_errorbar(aes(x = c('1', '2', '3', '4', '5'), ymin = c(pilo1, pilo2, pilo3, pilo4, pilo5), ymax = c(piup1, piup2, piup3, piup4, piup5)), width = 0) +
    scale_y_continuous(limits = c(0, max(c(piup1, piup2, piup3, piup4, piup5))+100 )) +
    labs(x = labels[1], y = labels[2], title = labels[3]) +
    theme_classic() +
    theme(text = element_text(size = 13))
  
  return(out)
}

cv_plot <- function(df, scen, spawn1_harvest0, lims, labels){
  tmp.df <- df %>% filter(climate_scenario == scen)
  if(spawn1_harvest0 == 1) tmp.y = 'spawn_cv' else tmp.y = 'harvest_cv'
  out <- ggplot() +
    geom_point(data = tmp.df, aes_string(x = 'age_struct', y = tmp.y), shape = 1, size = 2) +
    theme_classic() +
    scale_y_continuous(expand = c(0, 0), limits = lims) +
    labs(x = labels[1],y = labels[2], title = labels[3]) + 
    theme(text = element_text(size = 13))
  return(out)
}

calc_overfished <- function(mod, n.sim, n.yr){
  # Set up parallel backend to run multiple simulations simultaneously
  cores <- detectCores() # CPU cores
  cl <- makeCluster(cores-1) # Create R copies and reduce by 1 to avoid overloading computer
  registerDoParallel(cl) # register parallel backend with foreach package
  overfished <- foreach(i = 1:n.sim) %dopar%{
    library(dplyr)
    tmp.mod <- mod %>% filter(sim == paste0('s', i)) # get data for a single simulation
    
    # calculate proportion of years when total escapement < MSST (i.e., 91500)
    prop.under.MSST <- sum(tmp.mod$Spawn.est[30:n.yr-1]<91500) / length(c(30:n.yr-1))
    
    n.overfished <- 0
    for(j in 32:n.yr-1){ # use years 30-99
      gm <- exp(mean(log(tmp.mod$Spawn.est[c(j-2, j-1, j)]))) # calculate 3-year geometric mean
      if(gm < 91500){
        n.overfished <- n.overfished + 1 
      }
    }
    prop.overfished <- n.overfished / (n.yr - 1 - 32)
    prop.70 <- sum(tmp.mod$mu.c[30:100]==0.7)/length(tmp.mod$mu.c[30:100])
    prop.25 <- sum(tmp.mod$mu.c[30:100]==0.25)/length(tmp.mod$mu.c[30:100])
    prop.10 <- sum(tmp.mod$mu.c[30:100]==0.10)/length(tmp.mod$mu.c[30:100])
    return(c(prop.overfished, prop.under.MSST ,prop.70, prop.25, prop.10))
  }
  stopCluster(cl = cl)
  
  unlist_overfished <- function(x){
    return(data.frame(prop.overfished = x[1], prop.under.MSST = x[2] ,prop.70 = x[3], prop.25 = x[4], prop.10 = x[5]))
  }
  out <- bind_rows(lapply(overfished, unlist_overfished))
  return(out)
}

overfished_plot <- function(dflist, data, labels, lims){
  mean1 <- mean(dflist[[1]][, data]); pilo1 <- quantile(dflist[[1]][, data], probs = 0.025); piup1 <- quantile(dflist[[1]][, data], probs = 0.975)
  mean2 <- mean(dflist[[2]][, data]); pilo2 <- quantile(dflist[[2]][, data], probs = 0.025); piup2 <- quantile(dflist[[2]][, data], probs = 0.975)
  mean3 <- mean(dflist[[3]][, data]); pilo3 <- quantile(dflist[[3]][, data], probs = 0.025); piup3 <- quantile(dflist[[3]][, data], probs = 0.975)
  mean4 <- mean(dflist[[4]][, data]); pilo4 <- quantile(dflist[[4]][, data], probs = 0.025); piup4 <- quantile(dflist[[4]][, data], probs = 0.975)
  mean5 <- mean(dflist[[5]][, data]); pilo5 <- quantile(dflist[[5]][, data], probs = 0.025); piup5 <- quantile(dflist[[5]][, data], probs = 0.975)
  
  out <- ggplot() +
    geom_point(aes(x = c('1','2','3','4','5'), y = c(mean1*100, mean2*100, mean3*100, mean4*100, mean5*100)), size = 2) +
    geom_errorbar(aes(x = c('1','2','3','4','5'), ymin = c(pilo1*100, pilo2*100, pilo3*100, pilo4*100, pilo5*100), ymax = c(piup1*100, piup2*100, piup3*100, piup4*100, piup5*100)), width= 0) +
    theme_classic() +
    labs(x = labels[1], y = labels[2], title = labels[3]) +
    scale_y_continuous(limits = lims) +
    theme(text = element_text(size = 13))
  return(out)
}

cal_prop_diff <- function(val1, val2){ return((val1 - val2)/val1) }

se <- function(x){return(sd(x)/sqrt(length(x)))}


# Set model parameters and variables ----------------------------------------------------------------------------------
n.yr  <- 100 # number of years to simulate
n.sim <- 500 # number of simulations to run 20000
pars  <- c(0.068, 0.30, 0.86, 0.20) # See 'fall_model_fit.r' for calibration process: (1) residual juvenile mortality, (2) CV in recruitment stochasticity, (3) NPGO-dependent mortality coefficient, (4) Variance of NPGO-dependent mortality 

# Sensitivity to CV of realized harvest rate ---------------------------------------------------------------------------
n.yr  <- 100
n.sim <- 500 # number of simulations
pars  <- c(0.068, 0.30, 0.86, 0.20)
# 1.  Base flow,       maturation = 0.99
cver.sa.01 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')
# 2.  Base flow,       survival = 0.01            
cver.sa.02 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'base')
# 3.  Base flow,       base maturity and survival
cver.sa.03 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')
# 4.  Base flow,       survival = 0.99            
cver.sa.04 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'base')
# 5.  Base flow,       maturation = 0.25           
cver.sa.05 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')

# 6.  Longer duration, maturation = 0.99            
cver.sa.06 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')
# 7.  Longer duration, survival = 0.01            
cver.sa.07 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'longer duration')
# 8.  Longer duration, base maturity and survival
cver.sa.08 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')
# 9.  Longer duration, survival = 0.99            
cver.sa.09 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'longer duration')
# 10. Longer duration, maturation = 0.25           
cver.sa.10 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')

# 11. More frequent,   maturation = 0.99       
cver.sa.11 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')
# 12. More frequent,   survival = 0.01            
cver.sa.12 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more frequent')
# 13. More frequent,   base maturity and survival
cver.sa.13 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')
# 14. More frequent,   survival = 0.99            
cver.sa.14 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more frequent')
# 15. More frequent,   maturation = 0.25           
cver.sa.15 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')

# 16. More intense,    maturation = 0.99      
cver.sa.16 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')
# 17. More intense,    survival = 0.01            
cver.sa.17 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more intense')
# 18. More intense,    base maturity and survival
cver.sa.18 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')
# 19. More intense,    survival = 0.99            
cver.sa.19 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more intense')
# 20. More intense,    maturation = 0.25           
cver.sa.20 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')

save(cver.sa.01, cver.sa.02, cver.sa.03, cver.sa.04, cver.sa.05,
     cver.sa.06, cver.sa.07, cver.sa.08, cver.sa.09, cver.sa.10,
     cver.sa.11, cver.sa.12, cver.sa.13, cver.sa.14, cver.sa.15,
     cver.sa.16, cver.sa.17, cver.sa.18, cver.sa.19, cver.sa.20,
     file = 'cv_er_sa.RData')

cver.sa.01.df <- mse.summary(cver.sa.01)
cver.sa.02.df <- mse.summary(cver.sa.02)
cver.sa.03.df <- mse.summary(cver.sa.03)
cver.sa.04.df <- mse.summary(cver.sa.04)
cver.sa.05.df <- mse.summary(cver.sa.05)
cver.sa.06.df <- mse.summary(cver.sa.06)
cver.sa.07.df <- mse.summary(cver.sa.07)
cver.sa.08.df <- mse.summary(cver.sa.08)
cver.sa.09.df <- mse.summary(cver.sa.09)
cver.sa.10.df <- mse.summary(cver.sa.10)
cver.sa.11.df <- mse.summary(cver.sa.11)
cver.sa.12.df <- mse.summary(cver.sa.12)
cver.sa.13.df <- mse.summary(cver.sa.13)
cver.sa.14.df <- mse.summary(cver.sa.14)
cver.sa.15.df <- mse.summary(cver.sa.15)
cver.sa.16.df <- mse.summary(cver.sa.16)
cver.sa.17.df <- mse.summary(cver.sa.17)
cver.sa.18.df <- mse.summary(cver.sa.18)
cver.sa.19.df <- mse.summary(cver.sa.19)
cver.sa.20.df <- mse.summary(cver.sa.20)

cver.sa.tau.df <- rbind(cver.sa.01.df %>% mutate(climate='Contemporary', age_scen = 0.7),
                        cver.sa.03.df %>% mutate(climate='Contemporary', age_scen = 1.7),
                        cver.sa.05.df %>% mutate(climate='Contemporary', age_scen = 2.7),
                        cver.sa.06.df %>% mutate(climate='Longer duration', age_scen = 0.9),
                        cver.sa.08.df %>% mutate(climate='Longer duration', age_scen = 1.9),
                        cver.sa.10.df %>% mutate(climate='Longer duration', age_scen = 2.9),
                        cver.sa.11.df %>% mutate(climate='More frequent', age_scen = 1.1),
                        cver.sa.13.df %>% mutate(climate='More frequent', age_scen = 2.1),
                        cver.sa.15.df %>% mutate(climate='More frequent', age_scen = 3.1),
                        cver.sa.16.df %>% mutate(climate='More intense', age_scen = 1.3),
                        cver.sa.18.df %>% mutate(climate='More intense', age_scen = 2.3),
                        cver.sa.20.df %>% mutate(climate='More intense', age_scen = 3.3))

cver.sa.tau.cv.df <- data.frame(climate_scenario = rep(c('Contemporary','Duration','Frequency','Intensity'), each=3),
                                age_struct = c(seq(0.7,2.7,by=1),seq(0.9,2.9,by=1),seq(1.1,3.1,by=1),seq(1.3,3.3,by=1)),
                                spawn_cv = c(cver.sa.01.df$spawn.cv, cver.sa.03.df$spawn.cv, cver.sa.05.df$spawn.cv, cver.sa.06.df$spawn.cv, cver.sa.08.df$spawn.cv, cver.sa.10.df$spawn.cv, cver.sa.11.df$spawn.cv, cver.sa.13.df$spawn.cv, cver.sa.15.df$spawn.cv, cver.sa.16.df$spawn.cv, cver.sa.18.df$spawn.cv, cver.sa.20.df$spawn.cv),
                                harvest_cv = c(cver.sa.01.df$harvest.cv, cver.sa.03.df$harvest.cv, cver.sa.05.df$harvest.cv, cver.sa.06.df$harvest.cv, cver.sa.08.df$harvest.cv, cver.sa.10.df$harvest.cv, cver.sa.11.df$harvest.cv, cver.sa.13.df$harvest.cv, cver.sa.15.df$harvest.cv, cver.sa.16.df$harvest.cv, cver.sa.18.df$harvest.cv, cver.sa.20.df$harvest.cv))

cver.sa.eta.df <- rbind(cver.sa.02.df %>% mutate(climate='Contemporary', age_scen = 0.7),
                        cver.sa.03.df %>% mutate(climate='Contemporary', age_scen = 1.7),
                        cver.sa.04.df %>% mutate(climate='Contemporary', age_scen = 2.7),
                        cver.sa.07.df %>% mutate(climate='Longer duration', age_scen=0.9),
                        cver.sa.08.df %>% mutate(climate='Longer duration', age_scen=1.9),
                        cver.sa.09.df %>% mutate(climate='Longer duration', age_scen=2.9),
                        cver.sa.12.df %>% mutate(climate='More frequent', age_scen=1.1),
                        cver.sa.13.df %>% mutate(climate='More frequent', age_scen=2.1),
                        cver.sa.14.df %>% mutate(climate='More frequent', age_scen=3.1),
                        cver.sa.17.df %>% mutate(climate='More intense', age_scen=1.3),
                        cver.sa.18.df %>% mutate(climate='More intense', age_scen=2.3),
                        cver.sa.19.df %>% mutate(climate='More intense', age_scen=3.3))

cver.sa.eta.cv.df <- data.frame(climate_scenario = rep(c('Contemporary','Duration','Frequency','Intensity'), each=3),
                                age_struct = c(seq(0.7,2.7,by=1),seq(0.9,2.9,by=1),seq(1.1,3.1,by=1),seq(1.3,3.3,by=1)),
                                spawn_cv = c(cver.sa.02.df$spawn.cv, cver.sa.03.df$spawn.cv, cver.sa.04.df$spawn.cv, cver.sa.07.df$spawn.cv, cver.sa.08.df$spawn.cv, cver.sa.09.df$spawn.cv, cver.sa.12.df$spawn.cv, cver.sa.13.df$spawn.cv, cver.sa.14.df$spawn.cv, cver.sa.17.df$spawn.cv, cver.sa.18.df$spawn.cv, cver.sa.19.df$spawn.cv),
                                harvest_cv = c(cver.sa.02.df$harvest.cv, cver.sa.03.df$harvest.cv, cver.sa.04.df$harvest.cv, cver.sa.07.df$harvest.cv, cver.sa.08.df$harvest.cv, cver.sa.09.df$harvest.cv, cver.sa.12.df$harvest.cv, cver.sa.13.df$harvest.cv, cver.sa.14.df$harvest.cv, cver.sa.17.df$harvest.cv, cver.sa.18.df$harvest.cv, cver.sa.19.df$harvest.cv))

## SPAWN PLOTS
spawn.tau.plot <- ggplot(data = cver.sa.tau.df) +
  geom_point(aes(x = age_scen, y = spawn.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = spawn.pi.lo/1000, ymax = spawn.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = 'Maturation') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 400)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        panel.background = element_rect(fill = 'gray90', color = 'gray90'), plot.background = element_rect(fill = 'gray90', color = 'gray90'),
        plot.title = element_text(hjust = 0.5))
spawnCV.tau.plot <- ggplot(data = cver.sa.tau.cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(expression(tau[3]~"= 0.99"), 'Base case', expression(tau[3]~"= 0.25"))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.5, 0.7)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'),
        panel.background = element_rect(fill = 'gray90', color = 'gray90'), plot.background = element_rect(fill = 'gray90', color = 'gray90'))
spawn.tau <- ggarrange(spawn.tau.plot, spawnCV.tau.plot, nrow=2, labels = c('b', 'd'))

spawn.eta.plot <- ggplot(data = cver.sa.eta.df) +
  geom_point(aes(x = age_scen, y = spawn.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = spawn.pi.lo/1000, ymax = spawn.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Spawner escapement (thousands)', title = 'Natural mortality') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 400)) +
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.25),
        text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))
spawnCV.eta.plot <- ggplot(data = cver.sa.eta.cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'CV of spawner escapement') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(~paste(eta['4,5'], " = 0.01"), 'Base case', expression(~paste(eta['4,5'], ' = 0.99')))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.5, 0.7)) + 
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'))
spawn.eta <- ggarrange(spawn.eta.plot, spawnCV.eta.plot, nrow=2, labels = c('a', 'c'))

spawn.final <- ggarrange(spawn.eta, spawn.tau, ncol=2)

## HARVEST PLOTS
harvest.tau.plot <- ggplot(data = cver.sa.tau.df) +
  geom_point(aes(x = age_scen, y = harvest.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = harvest.pi.lo/1000, ymax = harvest.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = '') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        plot.background = element_rect(fill = 'gray90', color = 'gray90'), panel.background = element_rect(fill = 'gray90', color = 'gray90'),
        plot.title = element_text(hjust = 0.5))
harvestCV.tau.plot <- ggplot(data = cver.sa.tau.cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(expression(tau[3]~"= 0.99"), 'Base case', expression(tau[3]~"= 0.25"))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits = c(0.5, 0.7)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'),
        panel.background = element_rect(fill = 'gray90', color = 'gray90'), plot.background = element_rect(fill = 'gray90', color = 'gray90'))
harvest.tau <- ggarrange(harvest.tau.plot, harvestCV.tau.plot, nrow=2, labels = c('f', 'h'))

harvest.eta.plot <- ggplot(data = cver.sa.eta.df) +
  geom_point(aes(x = age_scen, y = harvest.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = harvest.pi.lo/1000, ymax = harvest.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Harvest (thousands)', title = '') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))
harvestCV.eta.plot <- ggplot(data = cver.sa.eta.cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of harvest') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(~paste(eta['4,5'], " = 0.01"), 'Base case', expression(~paste(eta['4,5'], ' = 0.99')))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits = c(0.5, 0.7)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'))
harvest.eta <- ggarrange(harvest.eta.plot, harvestCV.eta.plot, nrow=2, labels = c('e', 'g'))

harvest.final <- ggarrange(harvest.eta, harvest.tau, ncol=2)

cv.er.final <- ggarrange(spawn.final, harvest.final, nrow = 2)

# Sensitivity to CV of recruitment stochasticity ----------------------------------------------------------------------------
n.yr  <- 100
n.sim <- 500 # number of simulations
pars  <- c(0.068, 0.1, 0.86, 0.20)
# 1.  Base flow,       maturation = 0.99
cv.j.01 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')
# 2.  Base flow,       survival = 0.01            
cv.j.02 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'base')
# 3.  Base flow,       base maturity and survival
cv.j.03 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')
# 4.  Base flow,       survival = 0.99            
cv.j.04 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'base')
# 5.  Base flow,       maturation = 0.25           
cv.j.05 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')

# 6.  Longer duration, maturation = 0.99            
cv.j.06 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')
# 7.  Longer duration, survival = 0.01            
cv.j.07 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'longer duration')
# 8.  Longer duration, base maturity and survival
cv.j.08 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')
# 9.  Longer duration, survival = 0.99            
cv.j.09 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'longer duration')
# 10. Longer duration, maturation = 0.25           
cv.j.10 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')

# 11. More frequent,   maturation = 0.99       
cv.j.11 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')
# 12. More frequent,   survival = 0.01            
cv.j.12 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more frequent')
# 13. More frequent,   base maturity and survival
cv.j.13 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')
# 14. More frequent,   survival = 0.99            
cv.j.14 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more frequent')
# 15. More frequent,   maturation = 0.25           
cv.j.15 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')

# 16. More intense,    maturation = 0.99      
cv.j.16 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')
# 17. More intense,    survival = 0.01            
cv.j.17 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more intense')
# 18. More intense,    base maturity and survival
cv.j.18 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')
# 19. More intense,    survival = 0.99            
cv.j.19 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more intense')
# 20. More intense,    maturation = 0.25           
cv.j.20 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')

save(cv.j.01, cv.j.02, cv.j.03, cv.j.04, cv.j.05,
     cv.j.06, cv.j.07, cv.j.08, cv.j.09, cv.j.10,
     cv.j.11, cv.j.12, cv.j.13, cv.j.14, cv.j.15,
     cv.j.16, cv.j.17, cv.j.18, cv.j.19, cv.j.20,
     file = 'cv_j_sa.RData')

cv.j.01.df <- mse.summary(cv.j.01)
cv.j.02.df <- mse.summary(cv.j.02)
cv.j.03.df <- mse.summary(cv.j.03)
cv.j.04.df <- mse.summary(cv.j.04)
cv.j.05.df <- mse.summary(cv.j.05)
cv.j.06.df <- mse.summary(cv.j.06)
cv.j.07.df <- mse.summary(cv.j.07)
cv.j.08.df <- mse.summary(cv.j.08)
cv.j.09.df <- mse.summary(cv.j.09)
cv.j.10.df <- mse.summary(cv.j.10)
cv.j.11.df <- mse.summary(cv.j.11)
cv.j.12.df <- mse.summary(cv.j.12)
cv.j.13.df <- mse.summary(cv.j.13)
cv.j.14.df <- mse.summary(cv.j.14)
cv.j.15.df <- mse.summary(cv.j.15)
cv.j.16.df <- mse.summary(cv.j.16)
cv.j.17.df <- mse.summary(cv.j.17)
cv.j.18.df <- mse.summary(cv.j.18)
cv.j.19.df <- mse.summary(cv.j.19)
cv.j.20.df <- mse.summary(cv.j.20)

cv.j.tau.df <- rbind(cv.j.01.df %>% mutate(climate='Contemporary', age_scen = 0.7),
                     cv.j.03.df %>% mutate(climate='Contemporary', age_scen = 1.7),
                     cv.j.05.df %>% mutate(climate='Contemporary', age_scen = 2.7),
                     cv.j.06.df %>% mutate(climate='Longer duration', age_scen = 0.9),
                     cv.j.08.df %>% mutate(climate='Longer duration', age_scen = 1.9),
                     cv.j.10.df %>% mutate(climate='Longer duration', age_scen = 2.9),
                     cv.j.11.df %>% mutate(climate='More frequent', age_scen = 1.1),
                     cv.j.13.df %>% mutate(climate='More frequent', age_scen = 2.1),
                     cv.j.15.df %>% mutate(climate='More frequent', age_scen = 3.1),
                     cv.j.16.df %>% mutate(climate='More intense', age_scen = 1.3),
                     cv.j.18.df %>% mutate(climate='More intense', age_scen = 2.3),
                     cv.j.20.df %>% mutate(climate='More intense', age_scen = 3.3))

cv.j.tau.cv.df <- data.frame(climate_scenario = rep(c('Contemporary','Duration','Frequency','Intensity'), each=3),
                             age_struct = c(seq(0.7,2.7,by=1),seq(0.9,2.9,by=1),seq(1.1,3.1,by=1),seq(1.3,3.3,by=1)),
                             spawn_cv = c(cv.j.01.df$spawn.cv, cv.j.03.df$spawn.cv, cv.j.05.df$spawn.cv, cv.j.06.df$spawn.cv, cv.j.08.df$spawn.cv, cv.j.10.df$spawn.cv, cv.j.11.df$spawn.cv, cv.j.13.df$spawn.cv, cv.j.15.df$spawn.cv, cv.j.16.df$spawn.cv, cv.j.18.df$spawn.cv, cv.j.20.df$spawn.cv),
                             harvest_cv = c(cv.j.01.df$harvest.cv, cv.j.03.df$harvest.cv, cv.j.05.df$harvest.cv, cv.j.06.df$harvest.cv, cv.j.08.df$harvest.cv, cv.j.10.df$harvest.cv, cv.j.11.df$harvest.cv, cv.j.13.df$harvest.cv, cv.j.15.df$harvest.cv, cv.j.16.df$harvest.cv, cv.j.18.df$harvest.cv, cv.j.20.df$harvest.cv))

cv.j.eta.df <- rbind(cv.j.02.df %>% mutate(climate='Contemporary', age_scen = 0.7),
                     cv.j.03.df %>% mutate(climate='Contemporary', age_scen = 1.7),
                     cv.j.04.df %>% mutate(climate='Contemporary', age_scen = 2.7),
                     cv.j.07.df %>% mutate(climate='Longer duration', age_scen=0.9),
                     cv.j.08.df %>% mutate(climate='Longer duration', age_scen=1.9),
                     cv.j.09.df %>% mutate(climate='Longer duration', age_scen=2.9),
                     cv.j.12.df %>% mutate(climate='More frequent', age_scen=1.1),
                     cv.j.13.df %>% mutate(climate='More frequent', age_scen=2.1),
                     cv.j.14.df %>% mutate(climate='More frequent', age_scen=3.1),
                     cv.j.17.df %>% mutate(climate='More intense', age_scen=1.3),
                     cv.j.18.df %>% mutate(climate='More intense', age_scen=2.3),
                     cv.j.19.df %>% mutate(climate='More intense', age_scen=3.3))

cv.j.eta.cv.df <- data.frame(climate_scenario = rep(c('Contemporary','Duration','Frequency','Intensity'), each=3),
                             age_struct = c(seq(0.7,2.7,by=1),seq(0.9,2.9,by=1),seq(1.1,3.1,by=1),seq(1.3,3.3,by=1)),
                             spawn_cv = c(cv.j.02.df$spawn.cv, cv.j.03.df$spawn.cv, cv.j.04.df$spawn.cv, cv.j.07.df$spawn.cv, cv.j.08.df$spawn.cv, cv.j.09.df$spawn.cv, cv.j.12.df$spawn.cv, cv.j.13.df$spawn.cv, cv.j.14.df$spawn.cv, cv.j.17.df$spawn.cv, cv.j.18.df$spawn.cv, cv.j.19.df$spawn.cv),
                             harvest_cv = c(cv.j.02.df$harvest.cv, cv.j.03.df$harvest.cv, cv.j.04.df$harvest.cv, cv.j.07.df$harvest.cv, cv.j.08.df$harvest.cv, cv.j.09.df$harvest.cv, cv.j.12.df$harvest.cv, cv.j.13.df$harvest.cv, cv.j.14.df$harvest.cv, cv.j.17.df$harvest.cv, cv.j.18.df$harvest.cv, cv.j.19.df$harvest.cv))

## SPAWN PLOTS
spawn.tau.plot <- ggplot(data = cv.j.tau.df) +
  geom_point(aes(x = age_scen, y = spawn.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = spawn.pi.lo/1000, ymax = spawn.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = 'Maturation') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 500)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        panel.background = element_rect(fill = 'gray90', color = 'gray90'), plot.background = element_rect(fill = 'gray90', color = 'gray90'),
        plot.title = element_text(hjust = 0.5))
spawnCV.tau.plot <- ggplot(data = cv.j.tau.cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(expression(tau[3]~"= 0.99"), 'Base case', expression(tau[3]~"= 0.25"))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.6, 0.8)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'),
        panel.background = element_rect(fill = 'gray90', color = 'gray90'), plot.background = element_rect(fill = 'gray90', color = 'gray90'))
spawn.tau <- ggarrange(spawn.tau.plot, spawnCV.tau.plot, nrow=2, labels = c('b', 'd'))

spawn.eta.plot <- ggplot(data = cv.j.eta.df) +
  geom_point(aes(x = age_scen, y = spawn.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = spawn.pi.lo/1000, ymax = spawn.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Spawner escapement (thousands)', title = 'Natural mortality') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 500)) +
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.25),
        text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))
spawnCV.eta.plot <- ggplot(data = cv.j.eta.cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'CV of spawner escapement') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(~paste(eta['4,5'], " = 0.01"), 'Base case', expression(~paste(eta['4,5'], ' = 0.99')))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.6, 0.8)) + 
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'))
spawn.eta <- ggarrange(spawn.eta.plot, spawnCV.eta.plot, nrow=2, labels = c('a', 'c'))

spawn.final <- ggarrange(spawn.eta, spawn.tau, ncol=2)

## HARVEST PLOTS
harvest.tau.plot <- ggplot(data = cv.j.tau.df) +
  geom_point(aes(x = age_scen, y = harvest.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = harvest.pi.lo/1000, ymax = harvest.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = '') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        plot.background = element_rect(fill = 'gray90', color = 'gray90'), panel.background = element_rect(fill = 'gray90', color = 'gray90'),
        plot.title = element_text(hjust = 0.5))
harvestCV.tau.plot <- ggplot(data = cv.j.tau.cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(expression(tau[3]~"= 0.99"), 'Base case', expression(tau[3]~"= 0.25"))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits = c(0.6, 0.9)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'),
        panel.background = element_rect(fill = 'gray90', color = 'gray90'), plot.background = element_rect(fill = 'gray90', color = 'gray90'))
harvest.tau <- ggarrange(harvest.tau.plot, harvestCV.tau.plot, nrow=2, labels = c('f', 'h'))

harvest.eta.plot <- ggplot(data = cv.j.eta.df) +
  geom_point(aes(x = age_scen, y = harvest.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = harvest.pi.lo/1000, ymax = harvest.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Harvest (thousands)', title = '') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))
harvestCV.eta.plot <- ggplot(data = cv.j.eta.cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of harvest') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(~paste(eta['4,5'], " = 0.01"), 'Base case', expression(~paste(eta['4,5'], ' = 0.99')))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits = c(0.6, 0.9)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'))
harvest.eta <- ggarrange(harvest.eta.plot, harvestCV.eta.plot, nrow=2, labels = c('e', 'g'))

harvest.final <- ggarrange(harvest.eta, harvest.tau, ncol=2)

cv.j.final <- ggarrange(spawn.final, harvest.final, nrow = 2)

# Sensitivity to mean NPGO effect ----------------------------------------------------------------------------
n.yr  <- 100
n.sim <- 500 # number of simulations
pars  <- c(0.068, 0.3, 0.1, 0.20)
# 1.  Base flow,       maturation = 0.99
npgo.01 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')
# 2.  Base flow,       survival = 0.01            
npgo.02 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'base')
# 3.  Base flow,       base maturity and survival
npgo.03 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')
# 4.  Base flow,       survival = 0.99            
npgo.04 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'base')
# 5.  Base flow,       maturation = 0.25           
npgo.05 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')

# 6.  Longer duration, maturation = 0.99            
npgo.06 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')
# 7.  Longer duration, survival = 0.01            
npgo.07 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'longer duration')
# 8.  Longer duration, base maturity and survival
npgo.08 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')
# 9.  Longer duration, survival = 0.99            
npgo.09 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'longer duration')
# 10. Longer duration, maturation = 0.25           
npgo.10 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')

# 11. More frequent,   maturation = 0.99       
npgo.11 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')
# 12. More frequent,   survival = 0.01            
npgo.12 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more frequent')
# 13. More frequent,   base maturity and survival
npgo.13 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')
# 14. More frequent,   survival = 0.99            
npgo.14 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more frequent')
# 15. More frequent,   maturation = 0.25           
npgo.15 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')

# 16. More intense,    maturation = 0.99      
npgo.16 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')
# 17. More intense,    survival = 0.01            
npgo.17 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more intense')
# 18. More intense,    base maturity and survival
npgo.18 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')
# 19. More intense,    survival = 0.99            
npgo.19 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more intense')
# 20. More intense,    maturation = 0.25           
npgo.20 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')

save(npgo.01, npgo.02, npgo.03, npgo.04, npgo.05,
     npgo.06, npgo.07, npgo.08, npgo.09, npgo.10,
     npgo.11, npgo.12, npgo.13, npgo.14, npgo.15,
     npgo.16, npgo.17, npgo.18, npgo.19, npgo.20,
     file = 'npgo_sa.RData')

npgo.01.df <- mse.summary(npgo.01)
npgo.02.df <- mse.summary(npgo.02)
npgo.03.df <- mse.summary(npgo.03)
npgo.04.df <- mse.summary(npgo.04)
npgo.05.df <- mse.summary(npgo.05)
npgo.06.df <- mse.summary(npgo.06)
npgo.07.df <- mse.summary(npgo.07)
npgo.08.df <- mse.summary(npgo.08)
npgo.09.df <- mse.summary(npgo.09)
npgo.10.df <- mse.summary(npgo.10)
npgo.11.df <- mse.summary(npgo.11)
npgo.12.df <- mse.summary(npgo.12)
npgo.13.df <- mse.summary(npgo.13)
npgo.14.df <- mse.summary(npgo.14)
npgo.15.df <- mse.summary(npgo.15)
npgo.16.df <- mse.summary(npgo.16)
npgo.17.df <- mse.summary(npgo.17)
npgo.18.df <- mse.summary(npgo.18)
npgo.19.df <- mse.summary(npgo.19)
npgo.20.df <- mse.summary(npgo.20)

npgo.tau.df <- rbind(npgo.01.df %>% mutate(climate='Contemporary', age_scen = 0.7),
                     npgo.03.df %>% mutate(climate='Contemporary', age_scen = 1.7),
                     npgo.05.df %>% mutate(climate='Contemporary', age_scen = 2.7),
                     npgo.06.df %>% mutate(climate='Longer duration', age_scen = 0.9),
                     npgo.08.df %>% mutate(climate='Longer duration', age_scen = 1.9),
                     npgo.10.df %>% mutate(climate='Longer duration', age_scen = 2.9),
                     npgo.11.df %>% mutate(climate='More frequent', age_scen = 1.1),
                     npgo.13.df %>% mutate(climate='More frequent', age_scen = 2.1),
                     npgo.15.df %>% mutate(climate='More frequent', age_scen = 3.1),
                     npgo.16.df %>% mutate(climate='More intense', age_scen = 1.3),
                     npgo.18.df %>% mutate(climate='More intense', age_scen = 2.3),
                     npgo.20.df %>% mutate(climate='More intense', age_scen = 3.3))

npgo.tau.cv.df <- data.frame(climate_scenario = rep(c('Contemporary','Duration','Frequency','Intensity'), each=3),
                             age_struct = c(seq(0.7,2.7,by=1),seq(0.9,2.9,by=1),seq(1.1,3.1,by=1),seq(1.3,3.3,by=1)),
                             spawn_cv = c(npgo.01.df$spawn.cv, npgo.03.df$spawn.cv, npgo.05.df$spawn.cv, npgo.06.df$spawn.cv, npgo.08.df$spawn.cv, npgo.10.df$spawn.cv, npgo.11.df$spawn.cv, npgo.13.df$spawn.cv, npgo.15.df$spawn.cv, npgo.16.df$spawn.cv, npgo.18.df$spawn.cv, npgo.20.df$spawn.cv),
                             harvest_cv = c(npgo.01.df$harvest.cv, npgo.03.df$harvest.cv, npgo.05.df$harvest.cv, npgo.06.df$harvest.cv, npgo.08.df$harvest.cv, npgo.10.df$harvest.cv, npgo.11.df$harvest.cv, npgo.13.df$harvest.cv, npgo.15.df$harvest.cv, npgo.16.df$harvest.cv, npgo.18.df$harvest.cv, npgo.20.df$harvest.cv))

npgo.eta.df <- rbind(npgo.02.df %>% mutate(climate='Contemporary', age_scen = 0.7),
                     npgo.03.df %>% mutate(climate='Contemporary', age_scen = 1.7),
                     npgo.04.df %>% mutate(climate='Contemporary', age_scen = 2.7),
                     npgo.07.df %>% mutate(climate='Longer duration', age_scen=0.9),
                     npgo.08.df %>% mutate(climate='Longer duration', age_scen=1.9),
                     npgo.09.df %>% mutate(climate='Longer duration', age_scen=2.9),
                     npgo.12.df %>% mutate(climate='More frequent', age_scen=1.1),
                     npgo.13.df %>% mutate(climate='More frequent', age_scen=2.1),
                     npgo.14.df %>% mutate(climate='More frequent', age_scen=3.1),
                     npgo.17.df %>% mutate(climate='More intense', age_scen=1.3),
                     npgo.18.df %>% mutate(climate='More intense', age_scen=2.3),
                     npgo.19.df %>% mutate(climate='More intense', age_scen=3.3))

npgo.eta.cv.df <- data.frame(climate_scenario = rep(c('Contemporary','Duration','Frequency','Intensity'), each=3),
                             age_struct = c(seq(0.7,2.7,by=1),seq(0.9,2.9,by=1),seq(1.1,3.1,by=1),seq(1.3,3.3,by=1)),
                             spawn_cv = c(npgo.02.df$spawn.cv, npgo.03.df$spawn.cv, npgo.04.df$spawn.cv, npgo.07.df$spawn.cv, npgo.08.df$spawn.cv, npgo.09.df$spawn.cv, npgo.12.df$spawn.cv, npgo.13.df$spawn.cv, npgo.14.df$spawn.cv, npgo.17.df$spawn.cv, npgo.18.df$spawn.cv, npgo.19.df$spawn.cv),
                             harvest_cv = c(npgo.02.df$harvest.cv, npgo.03.df$harvest.cv, npgo.04.df$harvest.cv, npgo.07.df$harvest.cv, npgo.08.df$harvest.cv, npgo.09.df$harvest.cv, npgo.12.df$harvest.cv, npgo.13.df$harvest.cv, npgo.14.df$harvest.cv, npgo.17.df$harvest.cv, npgo.18.df$harvest.cv, npgo.19.df$harvest.cv))

## SPAWN PLOTS
spawn.tau.plot <- ggplot(data = npgo.tau.df) +
  geom_point(aes(x = age_scen, y = spawn.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = spawn.pi.lo/1000, ymax = spawn.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = 'Maturation') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 400)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        panel.background = element_rect(fill = 'gray90', color = 'gray90'), plot.background = element_rect(fill = 'gray90', color = 'gray90'),
        plot.title = element_text(hjust = 0.5))
spawnCV.tau.plot <- ggplot(data = npgo.tau.cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(expression(tau[3]~"= 0.99"), 'Base case', expression(tau[3]~"= 0.25"))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.5, 0.7)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'),
        panel.background = element_rect(fill = 'gray90', color = 'gray90'), plot.background = element_rect(fill = 'gray90', color = 'gray90'))
spawn.tau <- ggarrange(spawn.tau.plot, spawnCV.tau.plot, nrow=2, labels = c('b', 'd'))

spawn.eta.plot <- ggplot(data = npgo.eta.df) +
  geom_point(aes(x = age_scen, y = spawn.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = spawn.pi.lo/1000, ymax = spawn.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Spawner escapement (thousands)', title = 'Natural mortality') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 400)) +
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.25),
        text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))
spawnCV.eta.plot <- ggplot(data = npgo.eta.cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'CV of spawner escapement') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(~paste(eta['4,5'], " = 0.01"), 'Base case', expression(~paste(eta['4,5'], ' = 0.99')))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.5, 0.7)) + 
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'))
spawn.eta <- ggarrange(spawn.eta.plot, spawnCV.eta.plot, nrow=2, labels = c('a', 'c'))

spawn.final <- ggarrange(spawn.eta, spawn.tau, ncol=2)

## HARVEST PLOTS
harvest.tau.plot <- ggplot(data = npgo.tau.df) +
  geom_point(aes(x = age_scen, y = harvest.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = harvest.pi.lo/1000, ymax = harvest.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = '') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        plot.background = element_rect(fill = 'gray90', color = 'gray90'), panel.background = element_rect(fill = 'gray90', color = 'gray90'),
        plot.title = element_text(hjust = 0.5))
harvestCV.tau.plot <- ggplot(data = npgo.tau.cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(expression(tau[3]~"= 0.99"), 'Base case', expression(tau[3]~"= 0.25"))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits = c(0.5, 0.7)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'),
        panel.background = element_rect(fill = 'gray90', color = 'gray90'), plot.background = element_rect(fill = 'gray90', color = 'gray90'))
harvest.tau <- ggarrange(harvest.tau.plot, harvestCV.tau.plot, nrow=2, labels = c('f', 'h'))

harvest.eta.plot <- ggplot(data = npgo.eta.df) +
  geom_point(aes(x = age_scen, y = harvest.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = harvest.pi.lo/1000, ymax = harvest.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Harvest (thousands)', title = '') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))
harvestCV.eta.plot <- ggplot(data = npgo.eta.cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of harvest') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(~paste(eta['4,5'], " = 0.01"), 'Base case', expression(~paste(eta['4,5'], ' = 0.99')))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits = c(0.5, 0.7)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'))
harvest.eta <- ggarrange(harvest.eta.plot, harvestCV.eta.plot, nrow=2, labels = c('e', 'g'))

harvest.final <- ggarrange(harvest.eta, harvest.tau, ncol=2)

npgo.final <- ggarrange(spawn.final, harvest.final, nrow = 2)

# Test 100-year model --------------------------------------------------------------------------------------------------
base.mod    <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')
base.mod.df <- mse.summary(base.mod)

base.mod %>% filter(year > 29 & year < 100) %>% summarise(NH.ratio = mean(NH.ratio))

# plots
sim.nums    <- paste0('s', sample(1:n.sim, 500, replace=FALSE))
base.mod1   <- base.mod %>% filter(sim %in% sim.nums)
base.mod2   <- base.mod1 %>% filter(sim %in% sample(sim.nums, 1))
hundo.spawn <- ggplot() +
  geom_line(data = base.mod1, aes(x = year, y = Spawn.est, group = sim), color = 'gray70', alpha = 0.3) +
  geom_line(data = base.mod2, aes(x = year, y = Spawn.est), color = 'black') +
  # geom_line(aes(x = 1:26, y = catch.esc$total.esc), color = 'red') +
  geom_hline(yintercept = base.mod.df$spawn.mean, color = 'black') +
  geom_hline(yintercept = base.mod.df$spawn.median, color = 'black', lty = 'dashed') +
  geom_hline(yintercept = mean(catch.esc$total.esc), color = 'blue') +
  geom_hline(yintercept = median(catch.esc$total.esc), color = 'blue', lty = 'dashed') +
  geom_hline(yintercept = 91500, color = 'red') +
  # geom_hline(yintercept = 122000, color = 'red', lty = 'dashed') +
  theme_classic() +
  scale_x_continuous(expand = c(0,0), limits = c(1,100)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, max(base.mod1$Spawn.est))) +
  labs(x = 'Year', y = 'Total escapement') +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), 'cm'))
hundo.harvest <- ggplot() +
  geom_line(data = base.mod1, aes(x = year, y = harvest, group = sim), color = 'gray70', alpha = 0.3) +
  geom_line(data = base.mod2, aes(x = year, y = harvest), color = 'black') +
  # geom_line(aes(x = 1:26, y = catch.esc$total.esc), color = 'red') +
  geom_hline(yintercept = base.mod.df$harvest.mean, color = 'black') +
  geom_hline(yintercept = base.mod.df$harvest.median, color = 'black', lty = 'dashed') +
  geom_hline(yintercept = mean(catch.esc$total.ocean.harvest + catch.esc$river.harvest), color = 'blue') +
  geom_hline(yintercept = median(catch.esc$total.ocean.harvest + catch.esc$river.harvest), color = 'blue', lty = 'dashed') +
  theme_classic() +
  scale_x_continuous(expand = c(0,0), limits = c(1,100)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, max(base.mod1$harvest))) +
  labs(x = 'Year', y = 'Harvest') +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), 'cm'))
ggarrange(hundo.spawn, hundo.harvest, nrow = 2, ncol = 1)

# Check age-composition of spawners
hundo.age.comp <- data.frame(age = c('2','3','4','5'),
                             mean = as.numeric(base.mod %>% filter(year >= 30) %>% dplyr::select(spawn.2, spawn.3, spawn.4, spawn.5) %>% summarise(across(1:4, mean))))
hundo.age.comp$prop <- hundo.age.comp$mean/sum(hundo.age.comp$mean)
hundo.age.comp$source <- 'Simulated'

wills.data <- data.frame(age = c('1-2', '3', '4', '5+'),
                         prop = c(0.13, 0.65, 0.22, 0.003))

sim.age.com <- ggplot() +
  geom_histogram(aes(x = hundo.age.comp$age, y = hundo.age.comp$prop), stat = 'identity') +
  labs(x = 'Age', y = 'Proportion of spawners', title = 'Simulated') +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.7)) +
  theme_classic()

will.age.com <- ggplot() +
  geom_histogram(aes(x = wills.data$age, y = wills.data$prop), stat = 'identity') +
  labs(x = 'Age', y = 'Proportion of spawners', title = 'Satterthwaite et al. 2017') +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.7)) +
  theme_classic()

# Check age-composition of harvest
harv.age.comp <- data.frame(age = c('2','3','4','5'),
                            mean = as.numeric(base.mod %>% filter(year >= 30) %>% dplyr::select(harvest.2, harvest.3, harvest.4, harvest.5) %>% summarise(across(1:4, mean))))
harv.age.comp$prop <- (harv.age.comp$mean/sum(harv.age.comp$mean))

melodies.data <- data.frame(age = c('2','3','4','5','2','3','4','5'),
                            year = c(rep('1998 BY', times = 4), rep('1999 BY', times = 4)),
                            prop = c(0.06, 0.82, 0.17, 0.0005, 0.008, 0.632, 0.352, 0.004))

sim.harv.plot <- ggplot() +
  geom_bar(aes(x = harv.age.comp$age, y = harv.age.comp$prop), stat = 'identity') +
  labs(x = 'Age', y = 'Proportio of harvest', title = 'Simulated') +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.9), breaks = seq(0, 0.8, by = 0.2)) +
  theme_classic()

melodi.harv.plot <- ggplot() +
  geom_bar(aes(x = melodies.data$age, y = melodies.data$prop, fill = melodies.data$year), stat = 'identity', position = 'dodge') +
  scale_fill_manual("legend", values = c("1998 BY" = "grey35", "1999 BY" = "grey")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.9), breaks = seq(0, 0.8, by = 0.2)) +
  labs(x = 'Age', y = 'Proportion of Feather River hatchery ocean impacts', title = 'Palmer-Zwahlen et al. 2006') +
  theme_classic() +
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.8))

ggarrange(sim.age.com, will.age.com, sim.harv.plot, melodi.harv.plot, nrow=2, ncol=2)

# Check juvenile survival
base.mod %>% filter(year > 29 & year < 100) %>% dplyr::select(j.surv) %>% summarise(across(1, mean))

# Check overfished status
base.mod.overfished <- calc_overfished(base.mod, n.sim = n.sim, n.yr = n.yr)
mean(base.mod.overfished$prop.overfished) * 100
mean(base.mod.overfished$prop.under.MSST) * 100
mean(base.mod.overfished$prop.70) * 100
mean(base.mod.overfished$prop.25) * 100
mean(base.mod.overfished$prop.10) * 100


# Check forecast error
fe.df <- base.mod %>% dplyr::select(year, SI.observed, SI.forecast, Spawn, harvest, sim) %>% filter(year > 29) %>% mutate(true.SI = Spawn+harvest)
fe1 <- ggplot() +
  geom_point(data = fe.df, aes(x = SI.observed, y = SI.forecast), color = '#0072B2', alpha = 0.1, size = 1.5) +
  # geom_point(data = fe.df, aes(x = true.SI, y = SI.forecast), color = 'red', alpha = 0.1, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, lty = 'dashed', size = 1) +
  geom_segment(aes(x = 406667, y = 0, xend = 406667, yend = 406667), size = 1, lty = 'dashed') +
  geom_segment(aes(x = 0, y = 406667, xend = 406667, yend = 406667), size = 1, lty = 'dashed') +
  scale_x_continuous(expand = c(0,0), limits = c(0,1e6)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1e6)) +
  labs(x = 'Postseason estimate', y = 'Preseason forecast') +
  theme_classic()
fe2 <- ggplot() +
  geom_point(data = fe.df, aes(x = true.SI, y = SI.forecast), color = '#0072B2', alpha = 0.1, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, lty = 'dashed', size = 1) +
  geom_segment(aes(x = 406667, y = 0, xend = 406667, yend = 406667), size = 1, lty = 'dashed') +
  geom_segment(aes(x = 0, y = 406667, xend = 406667, yend = 406667), size = 1, lty = 'dashed') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = 'Postseason "true" SI', y = 'Preseason SI forecast') +
  theme_classic()

# Check implementation error
ie.df <- base.mod %>% select(year, SI.observed, Spawn, harvest, mu.c, sim) %>% mutate(er.observed = harvest/SI.observed, er.true = harvest/(harvest+Spawn))
ggplot() +
  geom_point(data = ie.df, aes(x = mu.c, y = er.observed), color = '#0072B2', alpha = 0.1, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, lty = 'dashed', size = 1) +
  # scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = 'Allowable ER', y = 'Postseason observed ER') +
  theme_classic()
ggplot() +
  geom_point(data = ie.df, aes(x = mu.c, y = er.true), color = '#0072B2', alpha = 0.1, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, lty = 'dashed', size = 1) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()

# Check autocorrelation
tmp.acf <- acf(catch.esc$total.esc, 6)
tmp.acf <- data.frame(lag = 0:6, acf = tmp.acf$acf)
tmp.acf3 <- NULL
for(i in 1:n.sim){
  tmp.acf1 <- base.mod %>% filter(sim == paste0('s',i)) %>% filter(year < 100 & year > 29)
  tmp.acf2 <- acf(tmp.acf1$Spawn.est, 6, plot=FALSE)
  tmp.acf2 <- data.frame(lag = 0:6, acf = tmp.acf2$acf) 
  tmp.acf3 <- rbind(tmp.acf3, tmp.acf2)
}
tmp4 <- tmp.acf3 %>% group_by(lag) %>% summarise(acf = mean(acf))
plot(tmp4$lag, tmp4$acf)

# Check ocean abundance by age to see what is happening with overfishing percentage and fisheries restrictions ---------
test.yr  <- 100 # number of years to simulate
test.sim <- 1 # number of simulations to run 20000
pars  <- c(0.068, 0.30, 0.86, 0.20) # See 'fall_model_fit.r' for calibration process: (1) residual juvenile mortality, (2) CV in recruitment stochasticity, (3) NPGO-dependent mortality coefficient, (4) Variance of NPGO-dependent mortality 
test.mod1 <- mse.simulation(pars = pars, years = test.yr, sims = test.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8,  0.8), scenario = 'base')
test.mod2 <- mse.simulation(pars = pars, years = test.yr, sims = test.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'base')
test.mod3 <- mse.simulation(pars = pars, years = test.yr, sims = test.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8,  0.8), scenario = 'base')
test.mod4 <- mse.simulation(pars = pars, years = test.yr, sims = test.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'base')
test.mod5 <- mse.simulation(pars = pars, years = test.yr, sims = test.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8,  0.8), scenario = 'base')

test.plot <- function(test.df){
  ocean.plot <- ggplot(data = test.df) +
    geom_line(aes(x = year, y = ocean, group = sim), color = 'gray') +
    # geom_line(aes(x = year, y = ocean.3), color = "#56B4E9") +
    # geom_line(aes(x = year, y = ocean.4), color = "#0072B2") +
    geom_hline(yintercept = mean(test.df$ocean), lty = 'dashed', color = 'gray') +
    geom_hline(yintercept = 162667, lty = 'dashed', color = "#D55E00") +
    geom_hline(yintercept = 91500, lty = 'dashed', color = "#D55E00") +
    geom_line(aes(x = year, y = mu.c*14e5), color = 'red') +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 2.5e6),
                       name = 'Ocean abundance', sec.axis = sec_axis(~./14e5, name = 'Allowable exploitation rate')) +
    theme_classic()
  harvest.plot <- ggplot(data = test.df) +
    geom_line(aes(x = year, y = harvest), color = '#0072B2') +
    geom_line(aes(x = year, y = mu.c*10e5), color = 'red') +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(name = 'Harvest', sec.axis = sec_axis(~./10e5, name = 'Allowable exploitation rate')) +
    theme_classic()
  out <- ggarrange(ocean.plot, harvest.plot, nrow=2, ncol=1)
}
test1.plot <- test.plot(test.mod1 %>% filter(year > 29))
test2.plot <- test.plot(test.mod2 %>% filter(year > 29))
test3.plot <- test.plot(test.mod3 %>% filter(year > 29))
test4.plot <- test.plot(test.mod4 %>% filter(year > 29))
test5.plot <- test.plot(test.mod5 %>% filter(year > 29))
ggarrange(test1.plot, test2.plot, test3.plot, test4.plot, test5.plot, nrow=1)

test.cv <- function(test.df){
  out <- sd(test.df %>% filter(year > 29) %>% pull(ocean))/mean(test.df %>% filter(year > 29) %>% pull(ocean))
}
test.mod1.cv <- test.cv(test.mod1)
test.mod2.cv <- test.cv(test.mod2)
test.mod3.cv <- test.cv(test.mod3)
test.mod4.cv <- test.cv(test.mod4)
test.mod5.cv <- test.cv(test.mod5)

# Model scenarios ------------------------------------------------------------------------------------------------------
# 1.  Base flow,       maturation = 0.99
mod.01 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')
# 2.  Base flow,       survival = 0.01            
mod.02 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.01, 0.01, 0.01), scenario = 'base')
# 3.  Base flow,       base maturity and survival
mod.03 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')
# 4.  Base flow,       survival = 0.99            
mod.04 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.99, 0.99, 0.99), scenario = 'base')
# 5.  Base flow,       maturation = 0.25           
mod.05 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')
save(mod.01, mod.02, mod.03, mod.04, mod.05, file = 'age_flow_mod1.RData')

# 6.  Longer duration, maturation = 0.99            
mod.06 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')
# 7.  Longer duration, survival = 0.01            
mod.07 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'longer duration')
# 8.  Longer duration, base maturity and survival
mod.08 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')
# 9.  Longer duration, survival = 0.99            
mod.09 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'longer duration')
# 10. Longer duration, maturation = 0.25           
mod.10 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')
save(mod.06, mod.07, mod.08, mod.09, mod.10, file = 'age_flow_mod2.RData')

# 11. More frequent,   maturation = 0.99       
mod.11 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')
# 12. More frequent,   survival = 0.01            
mod.12 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more frequent')
# 13. More frequent,   base maturity and survival
mod.13 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')
# 14. More frequent,   survival = 0.99            
mod.14 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more frequent')
# 15. More frequent,   maturation = 0.25           
mod.15 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')
save(mod.11, mod.12, mod.13, mod.14, mod.15, file = 'age_flow_mod3.RData')

# 16. More intense,    maturation = 0.99      
mod.16 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')
# 17. More intense,    survival = 0.01            
mod.17 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more intense')
# 18. More intense,    base maturity and survival
mod.18 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')
# 19. More intense,    survival = 0.99            
mod.19 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more intense')
# 20. More intense,    maturation = 0.25           
mod.20 <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')
save(mod.16, mod.17, mod.18, mod.19, mod.20, file = 'age_flow_mod4.RData')


## Calculate summary values for all scenarios --------------------------------------------------------------------------
load('age_flow_mod1.RData')
mod01.df <- mse.summary(mod.01)
mod02.df <- mse.summary(mod.02)
mod03.df <- mse.summary(mod.03)
mod04.df <- mse.summary(mod.04)
mod05.df <- mse.summary(mod.05)
mod01.overfished <- calc_overfished(mod.01, n.sim = n.sim, n.yr = n.yr)
mod02.overfished <- calc_overfished(mod.02, n.sim = n.sim, n.yr = n.yr)
mod03.overfished <- calc_overfished(mod.03, n.sim = n.sim, n.yr = n.yr)
mod04.overfished <- calc_overfished(mod.04, n.sim = n.sim, n.yr = n.yr)
mod05.overfished <- calc_overfished(mod.05, n.sim = n.sim, n.yr = n.yr)
rm(mod.01, mod.02, mod.03, mod.04, mod.05)

load('age_flow_mod2.RData')
mod06.df <- mse.summary(mod.06)
mod07.df <- mse.summary(mod.07)
mod08.df <- mse.summary(mod.08)
mod09.df <- mse.summary(mod.09)
mod10.df <- mse.summary(mod.10)
mod06.overfished <- calc_overfished(mod.06, n.sim = n.sim, n.yr = n.yr)
mod07.overfished <- calc_overfished(mod.07, n.sim = n.sim, n.yr = n.yr)
mod08.overfished <- calc_overfished(mod.08, n.sim = n.sim, n.yr = n.yr)
mod09.overfished <- calc_overfished(mod.09, n.sim = n.sim, n.yr = n.yr)
mod10.overfished <- calc_overfished(mod.10, n.sim = n.sim, n.yr = n.yr)
rm(mod.06, mod.07, mod.08, mod.09, mod.10)

load('age_flow_mod3.RData')
mod11.df <- mse.summary(mod.11)
mod12.df <- mse.summary(mod.12)
mod13.df <- mse.summary(mod.13)
mod14.df <- mse.summary(mod.14)
mod15.df <- mse.summary(mod.15)
mod11.overfished <- calc_overfished(mod.11, n.sim = n.sim, n.yr = n.yr)
mod12.overfished <- calc_overfished(mod.12, n.sim = n.sim, n.yr = n.yr)
mod13.overfished <- calc_overfished(mod.13, n.sim = n.sim, n.yr = n.yr)
mod14.overfished <- calc_overfished(mod.14, n.sim = n.sim, n.yr = n.yr)
mod15.overfished <- calc_overfished(mod.15, n.sim = n.sim, n.yr = n.yr)
rm(mod.11, mod.12, mod.13, mod.14, mod.15)

load('age_flow_mod4.RData')
mod16.df <- mse.summary(mod.16)
mod17.df <- mse.summary(mod.17)
mod18.df <- mse.summary(mod.18)
mod19.df <- mse.summary(mod.19)
mod20.df <- mse.summary(mod.20)
mod16.overfished <- calc_overfished(mod.16, n.sim = n.sim, n.yr = n.yr)
mod17.overfished <- calc_overfished(mod.17, n.sim = n.sim, n.yr = n.yr)
mod18.overfished <- calc_overfished(mod.18, n.sim = n.sim, n.yr = n.yr)
mod19.overfished <- calc_overfished(mod.19, n.sim = n.sim, n.yr = n.yr)
mod20.overfished <- calc_overfished(mod.20, n.sim = n.sim, n.yr = n.yr)
rm(mod.16, mod.17, mod.18, mod.19, mod.20)

# save(mod01.df, mod02.df, mod03.df, mod04.df, mod05.df, mod06.df, mod07.df, mod08.df, mod09.df, mod10.df,
#      mod11.df, mod12.df, mod13.df, mod14.df, mod15.df, mod16.df, mod17.df, mod18.df, mod19.df, mod20.df,
#      mod01.overfished, mod02.overfished, mod03.overfished, mod04.overfished, mod05.overfished, mod06.overfished, mod07.overfished, mod08.overfished, mod09.overfished, mod10.overfished,
#      mod11.overfished, mod12.overfished, mod13.overfished, mod14.overfished, mod15.overfished, mod16.overfished, mod17.overfished, mod18.overfished, mod19.overfished, mod20.overfished,
#      file = 'age_flow_summary.RData')

load('age_flow_summary.RData')

all.scen.df <- rbind(mod01.df%>%mutate(climate='Contemporary', age_scen=0.7),
                     mod02.df%>%mutate(climate='Contemporary', age_scen=1.7),
                     mod03.df%>%mutate(climate='Contemporary', age_scen=2.7),
                     mod04.df%>%mutate(climate='Contemporary', age_scen=3.7),
                     mod05.df%>%mutate(climate='Contemporary', age_scen=4.7),
                     
                     mod06.df%>%mutate(climate='Longer duration', age_scen=0.9),
                     mod07.df%>%mutate(climate='Longer duration', age_scen=1.9),
                     mod08.df%>%mutate(climate='Longer duration', age_scen=2.9),
                     mod09.df%>%mutate(climate='Longer duration', age_scen=3.9),
                     mod10.df%>%mutate(climate='Longer duration', age_scen=4.9),
                     
                     mod11.df%>%mutate(climate='More frequent', age_scen=1.1),
                     mod12.df%>%mutate(climate='More frequent', age_scen=2.1),
                     mod13.df%>%mutate(climate='More frequent', age_scen=3.1),
                     mod14.df%>%mutate(climate='More frequent', age_scen=4.1),
                     mod15.df%>%mutate(climate='More frequent', age_scen=5.1),
                     
                     mod16.df%>%mutate(climate='More intense', age_scen=1.3),
                     mod17.df%>%mutate(climate='More intense', age_scen=2.3),
                     mod18.df%>%mutate(climate='More intense', age_scen=3.3),
                     mod19.df%>%mutate(climate='More intense', age_scen=4.3),
                     mod20.df%>%mutate(climate='More intense', age_scen=5.3))

tau.df <- rbind(mod01.df %>% mutate(climate='Contemporary', age_scen = 0.7),
                mod03.df %>% mutate(climate='Contemporary', age_scen = 1.7),
                mod05.df %>% mutate(climate='Contemporary', age_scen = 2.7),
                mod06.df %>% mutate(climate='Longer duration', age_scen = 0.9),
                mod08.df %>% mutate(climate='Longer duration', age_scen = 1.9),
                mod10.df %>% mutate(climate='Longer duration', age_scen = 2.9),
                mod11.df %>% mutate(climate='More frequent', age_scen = 1.1),
                mod13.df %>% mutate(climate='More frequent', age_scen = 2.1),
                mod15.df %>% mutate(climate='More frequent', age_scen = 3.1),
                mod16.df %>% mutate(climate='More intense', age_scen = 1.3),
                mod18.df %>% mutate(climate='More intense', age_scen = 2.3),
                mod20.df %>% mutate(climate='More intense', age_scen = 3.3))

tau.cv.df <- data.frame(climate_scenario = rep(c('Contemporary','Duration','Frequency','Intensity'), each=3),
                        age_struct = c(seq(0.7,2.7,by=1),seq(0.9,2.9,by=1),seq(1.1,3.1,by=1),seq(1.3,3.3,by=1)),
                        spawn_cv = c(mod01.df$spawn.cv, mod03.df$spawn.cv, mod05.df$spawn.cv, mod06.df$spawn.cv, mod08.df$spawn.cv, mod10.df$spawn.cv, mod11.df$spawn.cv, mod13.df$spawn.cv, mod15.df$spawn.cv, mod16.df$spawn.cv, mod18.df$spawn.cv, mod20.df$spawn.cv),
                        harvest_cv = c(mod01.df$harvest.cv, mod03.df$harvest.cv, mod05.df$harvest.cv, mod06.df$harvest.cv, mod08.df$harvest.cv, mod10.df$harvest.cv, mod11.df$harvest.cv, mod13.df$harvest.cv, mod15.df$harvest.cv, mod16.df$harvest.cv, mod18.df$harvest.cv, mod20.df$harvest.cv))

eta.df <- rbind(mod02.df %>% mutate(climate='Contemporary', age_scen = 0.7),
                mod03.df %>% mutate(climate='Contemporary', age_scen = 1.7),
                mod04.df %>% mutate(climate='Contemporary', age_scen = 2.7),
                mod07.df %>% mutate(climate='Longer duration', age_scen=0.9),
                mod08.df %>% mutate(climate='Longer duration', age_scen=1.9),
                mod09.df %>% mutate(climate='Longer duration', age_scen=2.9),
                mod12.df %>% mutate(climate='More frequent', age_scen=1.1),
                mod13.df %>% mutate(climate='More frequent', age_scen=2.1),
                mod14.df %>% mutate(climate='More frequent', age_scen=3.1),
                mod17.df %>% mutate(climate='More intense', age_scen=1.3),
                mod18.df %>% mutate(climate='More intense', age_scen=2.3),
                mod19.df %>% mutate(climate='More intense', age_scen=3.3))

eta.cv.df <- data.frame(climate_scenario = rep(c('Contemporary','Duration','Frequency','Intensity'), each=3),
                        age_struct = c(seq(0.7,2.7,by=1),seq(0.9,2.9,by=1),seq(1.1,3.1,by=1),seq(1.3,3.3,by=1)),
                        spawn_cv = c(mod02.df$spawn.cv, mod03.df$spawn.cv, mod04.df$spawn.cv, mod07.df$spawn.cv, mod08.df$spawn.cv, mod09.df$spawn.cv, mod12.df$spawn.cv, mod13.df$spawn.cv, mod14.df$spawn.cv, mod17.df$spawn.cv, mod18.df$spawn.cv, mod19.df$spawn.cv),
                        harvest_cv = c(mod02.df$harvest.cv, mod03.df$harvest.cv, mod04.df$harvest.cv, mod07.df$harvest.cv, mod08.df$harvest.cv, mod09.df$harvest.cv, mod12.df$harvest.cv, mod13.df$harvest.cv, mod14.df$harvest.cv, mod17.df$harvest.cv, mod18.df$harvest.cv, mod19.df$harvest.cv))

## Plot scenarios ------------------------------------------------------------------------------------------------------
# base case flow
spawn.plot1   <- age_struct_plot(df.list = list(mod01.df, mod02.df, mod03.df, mod04.df, mod05.df), spawn1_harvest0 = 1, labels = c('', 'Spawner escapement (1000s)', 'Contemporary flow'))
harvest.plot1 <- age_struct_plot(df.list = list(mod01.df, mod02.df, mod03.df, mod04.df, mod05.df), spawn1_harvest0 = 0, labels = c('', 'Harvest (1000s)', 'Contemporary flow'))
# longer duration
spawn.plot2   <- age_struct_plot(df.list = list(mod06.df, mod07.df, mod08.df, mod09.df, mod10.df), spawn1_harvest0 = 1, labels = c('', '', 'Longer duration'))
harvest.plot2 <- age_struct_plot(df.list = list(mod06.df, mod07.df, mod08.df, mod09.df, mod10.df), spawn1_harvest0 = 0, labels = c('', '', 'Longer duration'))
# more frequent
spawn.plot3   <- age_struct_plot(df.list = list(mod11.df, mod12.df, mod13.df, mod14.df, mod15.df), spawn1_harvest0 = 1, labels = c('Age structure scenario', 'Spawner escapement (1000s)', 'More frequent'))
harvest.plot3 <- age_struct_plot(df.list = list(mod11.df, mod12.df, mod13.df, mod14.df, mod15.df), spawn1_harvest0 = 0, labels = c('Age structure scenario', 'Harvest (1000s)', 'More frequent'))
# more intense
spawn.plot4   <- age_struct_plot(df.list = list(mod16.df, mod17.df, mod18.df, mod19.df, mod20.df), spawn1_harvest0 = 1, labels = c('Age structure scenario', '', 'More intense'))
harvest.plot4 <- age_struct_plot(df.list = list(mod16.df, mod17.df, mod18.df, mod19.df, mod20.df), spawn1_harvest0 = 0, labels = c('Age structure scenario', '', 'More intense'))

spawn.plot.all <- ggplot(data = all.scen.df) +
  geom_point(aes(x = age_scen, y = spawn.mean/1000, color = climate), size = 3) +
  # geom_point(aes(x = age_scen, y = spawn.median/1000, color = climate), shape = 3, size = 3) +
  geom_errorbar(aes(x = age_scen, ymin = spawn.pi.lo/1000, ymax = spawn.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Spawner escapement (thousands)') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1300)) +
  theme(legend.title = element_blank(), legend.position = c(0.87, 0.9), text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank())
harvest.plot.all <- ggplot(data = all.scen.df) +
  geom_point(aes(x = age_scen, y = harvest.median/1000, color = climate), size = 3) +
  # geom_point(aes(x = age_scen, y = harvest.median/1000, color = climate), shape = 3, size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = harvest.pi.lo/1000, ymax = harvest.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Harvest (thousands)') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1900)) +
  # scale_x_continuous(breaks = seq(1,5), labels = c(expression(tau[3]~"= 0.99"), ~paste(eta[4], ''[',5'], '=0.01'), 'Base case', ~paste(eta[4], ''[',5'], '=0.99'), expression(tau[3]~"= 0.25"))) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0,0,0.5,0.7),'cm'), axis.text.x = element_blank())

# Arrange plots
# ggarrange(spawn.plot.all, harvest.plot.all, nrow=2, ncol=1, labels=c('a','b'))

# CV for each scenario
cv.df <- data.frame(climate_scenario = rep(c('Contemporary','Duration','Frequency','Intensity'), each=5),
                    age_struct = c(seq(0.7,4.7,by=1),seq(0.9,4.9,by=1),seq(1.1,5.1,by=1),seq(1.3,5.3,by=1)),
                    spawn_cv = c(mod01.df$spawn.cv, mod02.df$spawn.cv, mod03.df$spawn.cv, mod04.df$spawn.cv, mod05.df$spawn.cv, mod06.df$spawn.cv, mod07.df$spawn.cv, mod08.df$spawn.cv, mod09.df$spawn.cv, mod10.df$spawn.cv, mod11.df$spawn.cv, mod12.df$spawn.cv, mod13.df$spawn.cv, mod14.df$spawn.cv, mod15.df$spawn.cv, mod16.df$spawn.cv, mod17.df$spawn.cv, mod18.df$spawn.cv, mod19.df$spawn.cv, mod20.df$spawn.cv),
                    harvest_cv = c(mod01.df$harvest.cv, mod02.df$harvest.cv, mod03.df$harvest.cv, mod04.df$harvest.cv, mod05.df$harvest.cv, mod06.df$harvest.cv, mod07.df$harvest.cv, mod08.df$harvest.cv, mod09.df$harvest.cv, mod10.df$harvest.cv, mod11.df$harvest.cv, mod12.df$harvest.cv, mod13.df$harvest.cv, mod14.df$harvest.cv, mod15.df$harvest.cv, mod16.df$harvest.cv, mod17.df$harvest.cv, mod18.df$harvest.cv, mod19.df$harvest.cv, mod20.df$harvest.cv))
spawn.cv.plot1 <- cv_plot(cv.df, scen = 'Contemporary', spawn1_harvest0 = 1, lims = c(0.50, 0.75), labels = c('Age structure scenario', 'CV of spawner escapement', 'Contemporary flow'))
spawn.cv.plot2 <- cv_plot(cv.df, scen = 'Duration', spawn1_harvest0 = 1, lims = c(0.50, 0.75), labels = c('Age structure scenario', 'CV of spawner escapement', 'Longer duration'))
spawn.cv.plot3 <- cv_plot(cv.df, scen = 'Frequency', spawn1_harvest0 = 1, lims = c(0.50, 0.75), labels = c('Age structure scenario', 'CV of spawner escapement', 'More frequent'))
spawn.cv.plot4 <- cv_plot(cv.df, scen = 'Intensity', spawn1_harvest0 = 1, lims = c(0.50, 0.75), labels = c('Age structure scenario', 'CV of spawner escapement', 'More intense'))

harvest.cv.plot1 <- cv_plot(cv.df, scen = 'Contemporary', spawn1_harvest0 = 0, lims = c(0.60, 0.80), labels = c('Age structure scenario', 'CV of harvest', 'Contemporary flow'))
harvest.cv.plot2 <- cv_plot(cv.df, scen = 'Duration', spawn1_harvest0 = 0, lims = c(0.60, 0.80), labels = c('Age structure scenario', 'CV of harvest', 'Longer duration'))
harvest.cv.plot3 <- cv_plot(cv.df, scen = 'Frequency', spawn1_harvest0 = 0, lims = c(0.60, 0.80), labels = c('Age structure scenario', 'CV of harvest', 'More frequent'))
harvest.cv.plot4 <- cv_plot(cv.df, scen = 'Intensity', spawn1_harvest0 = 0, lims = c(0.60, 0.80), labels = c('Age structure scenario', 'CV of harvest', 'More intense'))

spawn.cv.all <- ggplot(data = cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of spawner escapement') +
  # scale_y_continuous(expand = c(0, 0), limits = c(0.52, 0.72)) +
  scale_x_continuous(breaks = seq(1,5), labels = c(expression(tau[3]~"= 0.99"), ~paste(eta[4], ''[',5'], '=0.01'), 'Base case', ~paste(eta[4], ''[',5'], '=0.99'), expression(tau[3]~"= 0.25"))) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'))
harvest.cv.all <- ggplot(data = cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of harvest') +
  # scale_y_continuous(expand = c(0, 0), limits = c(0.65, 0.80)) +
  scale_x_continuous(breaks = seq(1,5), labels = c(expression(tau[3]~"= 0.99"), ~paste(eta[4], ''[',5'], '=0.01'), 'Base case', ~paste(eta[4], ''[',5'], '=0.99'), expression(tau[3]~"= 0.25"))) +
  theme(legend.title = element_blank(), legend.position = c(0.85, 0.85), text = element_text(size = 13), plot.margin = unit(c(0,0,0.5,0.7),'cm'))
# ggarrange(spawn.cv.all, harvest.cv.all, nrow=2, ncol=1, labels=c('a','b'))
ggarrange(spawn.plot.all, spawn.cv.all, nrow=2, ncol=1, labels=c('a','b'))
ggarrange(harvest.plot.all, harvest.cv.all, nrow=2, ncol=1, labels=c('a','b'))


## SPAWN PLOTS
spawn.tau.plot <- ggplot(data = tau.df) +
  geom_point(aes(x = age_scen, y = spawn.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = spawn.pi.lo/1000, ymax = spawn.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = 'Maturation') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 400)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        panel.background = element_rect(fill = 'gray90', color = 'gray90'), plot.background = element_rect(fill = 'gray90', color = 'gray90'),
        plot.title = element_text(hjust = 0.5))
spawnCV.tau.plot <- ggplot(data = tau.cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(expression(tau[3]~"= 0.99"), 'Base case', expression(tau[3]~"= 0.25"))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.58, 0.72)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'),
        panel.background = element_rect(fill = 'gray90', color = 'gray90'), plot.background = element_rect(fill = 'gray90', color = 'gray90'))
spawn.tau <- ggarrange(spawn.tau.plot, spawnCV.tau.plot, nrow=2, labels = c('b', 'd'))

spawn.eta.plot <- ggplot(data = eta.df) +
  geom_point(aes(x = age_scen, y = spawn.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = spawn.pi.lo/1000, ymax = spawn.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Spawner escapement (thousands)', title = 'Natural mortality') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 400)) +
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.2),
        text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))
spawnCV.eta.plot <- ggplot(data = eta.cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of spawner escapement') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(~paste(eta['4,5'], " = 0.01"), 'Base case', expression(~paste(eta['4,5'], ' = 0.99')))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.58, 0.72)) + 
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'))
spawn.eta <- ggarrange(spawn.eta.plot, spawnCV.eta.plot, nrow=2, labels = c('a', 'c'))

spawn.final <- ggarrange(spawn.eta, spawn.tau, ncol=2)

## HARVEST PLOTS
harvest.tau.plot <- ggplot(data = tau.df) +
  geom_point(aes(x = age_scen, y = harvest.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = harvest.pi.lo/1000, ymax = harvest.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = 'Maturation') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        plot.background = element_rect(fill = 'gray90', color = 'gray90'), panel.background = element_rect(fill = 'gray90', color = 'gray90'),
        plot.title = element_text(hjust = 0.5))
harvestCV.tau.plot <- ggplot(data = tau.cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(expression(tau[3]~"= 0.99"), 'Base case', expression(tau[3]~"= 0.25"))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits = c(0.6, 0.73)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'),
        panel.background = element_rect(fill = 'gray90', color = 'gray90'), plot.background = element_rect(fill = 'gray90', color = 'gray90'))
harvest.tau <- ggarrange(harvest.tau.plot, harvestCV.tau.plot, nrow=2, labels = c('b', 'd'))

harvest.eta.plot <- ggplot(data = eta.df) +
  geom_point(aes(x = age_scen, y = harvest.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = harvest.pi.lo/1000, ymax = harvest.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Harvest (thousands)', title = 'Natural mortality') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.2), text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))
harvestCV.eta.plot <- ggplot(data = eta.cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of harvest') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(~paste(eta['4,5'], " = 0.01"), 'Base case', expression(~paste(eta['4,5'], ' = 0.99')))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits = c(0.6, 0.73)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'))
harvest.eta <- ggarrange(harvest.eta.plot, harvestCV.eta.plot, nrow=2, labels = c('a', 'c'))

harvest.final <- ggarrange(harvest.eta, harvest.tau, ncol=2)

# Check 100-year simulations for contemporary flow conditions ------
sim.nums <- paste0('s', sample(1:n.sim, 500, replace=FALSE))
mod.01.1 <- mod.01 %>% filter(sim %in% sim.nums); mod.01.2 <- mod.01 %>% filter(sim %in% sample(sim.nums, 1))
mod.02.1 <- mod.02 %>% filter(sim %in% sim.nums); mod.02.2 <- mod.02 %>% filter(sim %in% sample(sim.nums, 1))
mod.03.1 <- mod.03 %>% filter(sim %in% sim.nums); mod.03.2 <- mod.03 %>% filter(sim %in% sample(sim.nums, 1))
mod.04.1 <- mod.04 %>% filter(sim %in% sim.nums); mod.04.2 <- mod.04 %>% filter(sim %in% sample(sim.nums, 1))
mod.05.1 <- mod.05 %>% filter(sim %in% sim.nums); mod.05.2 <- mod.05 %>% filter(sim %in% sample(sim.nums, 1))

plot.hundo.spawn <- function(data1, data2, df){
  p <- ggplot() +
    geom_line(data = data1, aes(x = year, y = Spawn.est, group = sim), color = 'gray70', alpha = 0.3) +
    geom_line(data = data2, aes(x = year, y = Spawn.est), color = 'black') +
    geom_hline(yintercept = df$spawn.mean, color = 'black', lty = 'dashed') +
    geom_hline(yintercept = 91500, color = 'red') +
    theme_classic() +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(x = 'Year', y = 'Estimated total escapement')
}
mod.01.hundo <- plot.hundo.spawn(data1 = mod.01.1, data2 = mod.01.2, df = mod01.df)
mod.02.hundo <- plot.hundo.spawn(data1 = mod.02.1, data2 = mod.02.2, df = mod02.df)
mod.03.hundo <- plot.hundo.spawn(data1 = mod.03.1, data2 = mod.03.2, df = mod03.df)
mod.04.hundo <- plot.hundo.spawn(data1 = mod.04.1, data2 = mod.04.2, df = mod04.df)
mod.05.hundo <- plot.hundo.spawn(data1 = mod.05.1, data2 = mod.05.2, df = mod05.df)
ggarrange(mod.01.hundo, mod.02.hundo, mod.03.hundo, mod.04.hundo, mod.05.hundo, nrow=5, ncol=1)

# Check forecast error
mod01.fe <- mod.01 %>% filter(sim %in% sim.nums) %>% dplyr::select(year, SI.observed, SI.forecast, Spawn, harvest, sim) %>% filter(year > 29) %>% mutate(true.SI = Spawn+harvest)
mod02.fe <- mod.02 %>% filter(sim %in% sim.nums) %>% dplyr::select(year, SI.observed, SI.forecast, Spawn, harvest, sim) %>% filter(year > 29) %>% mutate(true.SI = Spawn+harvest)
mod03.fe <- mod.03 %>% filter(sim %in% sim.nums) %>% dplyr::select(year, SI.observed, SI.forecast, Spawn, harvest, sim) %>% filter(year > 29) %>% mutate(true.SI = Spawn+harvest)
mod04.fe <- mod.04 %>% filter(sim %in% sim.nums) %>% dplyr::select(year, SI.observed, SI.forecast, Spawn, harvest, sim) %>% filter(year > 29) %>% mutate(true.SI = Spawn+harvest)
mod05.fe <- mod.05 %>% filter(sim %in% sim.nums) %>% dplyr::select(year, SI.observed, SI.forecast, Spawn, harvest, sim) %>% filter(year > 29) %>% mutate(true.SI = Spawn+harvest)

plot.fe <- function(fe.df){
  p <- ggplot() +
    geom_point(data = fe.df, aes(x = SI.observed, y = SI.forecast), color = '#0072B2', alpha = 0.1, size = 1.5) +
    geom_abline(intercept = 0, slope = 1, lty = 'dashed', size = 1) +
    geom_segment(aes(x = 0, y = 406667, xend = 406667, yend = 406667), size = 1, lty = 'dashed') +
    geom_segment(aes(x = 406667, y = 0, xend = 406667, yend = 406667), size = 1, lty = 'dashed') +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(x = 'Postseason estimate', y = 'Preseason forecast') +
    theme_classic()
}
mod01.fe.plot <- plot.fe(mod01.fe)
mod02.fe.plot <- plot.fe(mod02.fe)
mod03.fe.plot <- plot.fe(mod03.fe)
mod04.fe.plot <- plot.fe(mod04.fe)
mod05.fe.plot <- plot.fe(mod05.fe)

mod01.ie <- mod.01 %>% filter(sim %in% sim.nums) %>% select(year, SI.observed, Spawn, harvest, mu.c, sim) %>% mutate(er.observed = harvest/SI.observed, er.true = harvest/(harvest+Spawn))
mod02.ie <- mod.02 %>% filter(sim %in% sim.nums) %>% select(year, SI.observed, Spawn, harvest, mu.c, sim) %>% mutate(er.observed = harvest/SI.observed, er.true = harvest/(harvest+Spawn))
mod03.ie <- mod.03 %>% filter(sim %in% sim.nums) %>% select(year, SI.observed, Spawn, harvest, mu.c, sim) %>% mutate(er.observed = harvest/SI.observed, er.true = harvest/(harvest+Spawn))
mod04.ie <- mod.04 %>% filter(sim %in% sim.nums) %>% select(year, SI.observed, Spawn, harvest, mu.c, sim) %>% mutate(er.observed = harvest/SI.observed, er.true = harvest/(harvest+Spawn))
mod05.ie <- mod.05 %>% filter(sim %in% sim.nums) %>% select(year, SI.observed, Spawn, harvest, mu.c, sim) %>% mutate(er.observed = harvest/SI.observed, er.true = harvest/(harvest+Spawn))

plot.ie <- function(ie.df){
  p <- ggplot() +
    geom_point(data = ie.df, aes(x = mu.c, y = er.observed), color = '#0072B2', alpha = 0.1, size = 1.5) +
    geom_abline(intercept = 0, slope = 1, lty = 'dashed', size = 1) +
    # scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(x = 'Allowable ER', y = 'Postseason observed ER') +
    theme_classic()
}
mod01.ie.plot <- plot.ie(mod01.ie)
mod02.ie.plot <- plot.ie(mod02.ie)
mod03.ie.plot <- plot.ie(mod03.ie)
mod04.ie.plot <- plot.ie(mod04.ie)
mod05.ie.plot <- plot.ie(mod05.ie)


# Percent over fishing with 95% prob intervals
overfished.plot1 <- overfished_plot(dflist = list(mod01.overfished, mod02.overfished, mod03.overfished, mod04.overfished, mod05.overfished), data = 'prop.overfished', labels = c('Age structure scenario', '% overfished status', 'Contemporary flow'), lims = c(0, 30))  
overfished.plot2 <- overfished_plot(dflist = list(mod06.overfished, mod07.overfished, mod08.overfished, mod09.overfished, mod10.overfished), data = 'prop.overfished', labels = c('Age structure scenario', '% overfished status', 'Longer duration'), lims = c(0, 30))
overfished.plot3 <- overfished_plot(dflist = list(mod11.overfished, mod12.overfished, mod13.overfished, mod14.overfished, mod15.overfished), data = 'prop.overfished', labels = c('Age structure scenario', '% overfished status', 'More frequent'), lims = c(0, 30))
overfished.plot4 <- overfished_plot(dflist = list(mod16.overfished, mod17.overfished, mod18.overfished, mod19.overfished, mod20.overfished), data = 'prop.overfished', labels = c('Age structure scenario', '% overfished status', 'More intense'), lims = c(0, 30))

prop70.plot1 <- overfished_plot(dflist = list(mod01.overfished, mod02.overfished, mod03.overfished, mod04.overfished, mod05.overfished), data = 'prop.70', labels = c('Age structure scenario', '', 'Contemporary flow'), lims = c(48, 85))
prop70.plot2 <- overfished_plot(dflist = list(mod06.overfished, mod07.overfished, mod08.overfished, mod09.overfished, mod10.overfished), data = 'prop.70', labels = c('Age structure scenario', '', 'Longer duration'), lims = c(48, 85))
prop70.plot3 <- overfished_plot(dflist = list(mod11.overfished, mod12.overfished, mod13.overfished, mod14.overfished, mod15.overfished), data = 'prop.70', labels = c('Age structure scenario', '', 'More frequent'), lims = c(48, 85))
prop70.plot4 <- overfished_plot(dflist = list(mod16.overfished, mod17.overfished, mod18.overfished, mod19.overfished, mod20.overfished), data = 'prop.70', labels = c('Age structure scenario', '', 'More intense'), lims = c(48, 85))

prop25.plot1 <- overfished_plot(dflist = list(mod01.overfished, mod02.overfished, mod03.overfished, mod04.overfished, mod05.overfished), data = 'prop.25', labels = c('Age structure scenario', '', 'Contemporary flow'), lims = c(0, 10))
prop25.plot2 <- overfished_plot(dflist = list(mod06.overfished, mod07.overfished, mod08.overfished, mod09.overfished, mod10.overfished), data = 'prop.25', labels = c('Age structure scenario', '', 'Longer duration'), lims = c(0, 10))
prop25.plot3 <- overfished_plot(dflist = list(mod11.overfished, mod12.overfished, mod13.overfished, mod14.overfished, mod15.overfished), data = 'prop.25', labels = c('Age structure scenario', '', 'More frequent'), lims = c(0, 10))
prop25.plot4 <- overfished_plot(dflist = list(mod16.overfished, mod17.overfished, mod18.overfished, mod19.overfished, mod20.overfished), data = 'prop.25', labels = c('Age structure scenario', '', 'More intense'), lims = c(0, 10))

prop10.plot1 <- overfished_plot(dflist = list(mod01.overfished, mod02.overfished, mod03.overfished, mod04.overfished, mod05.overfished), data = 'prop.10', labels = c('Age structure scenario', '', 'Contemporary flow'), lims = c(0, 2.5))
prop10.plot2 <- overfished_plot(dflist = list(mod06.overfished, mod07.overfished, mod08.overfished, mod09.overfished, mod10.overfished), data = 'prop.10', labels = c('Age structure scenario', '', 'Longer duration'), lims = c(0, 2.5))
prop10.plot3 <- overfished_plot(dflist = list(mod11.overfished, mod12.overfished, mod13.overfished, mod14.overfished, mod15.overfished), data = 'prop.10', labels = c('Age structure scenario', '', 'More frequent'), lims = c(0, 2.5))
prop10.plot4 <- overfished_plot(dflist = list(mod16.overfished, mod17.overfished, mod18.overfished, mod19.overfished, mod20.overfished), data = 'prop.10', labels = c('Age structure scenario', '', 'More intense'), lims = c(0, 2.5))

overfished.df <- data.frame(climate_scenario = rep(c('Contemporary','Duration','Frequency','Intensity'), each=5),
                            age_struct = c(seq(0.7,4.7,by=1),seq(0.9,4.9,by=1),seq(1.1,5.1,by=1),seq(1.3,5.3,by=1)), 
                            prop.of = NA,pilo.of = NA,piup.of = NA,prop.70 = NA,pilo.70 = NA,piup.70 = NA,prop.25 = NA,pilo.25 = NA,piup.25 = NA,prop.10 = NA,pilo.10 = NA,piup.10 = NA)
for(i in 1:20){
  tmp.name <- paste0('mod',stringr::str_pad(i, 2, pad = '0'),'.overfished')
  tmp.of <- get(tmp.name)
  overfished.df$prop.of[i] <- mean(tmp.of$prop.overfished) * 100
  overfished.df$pilo.of[i] <- quantile(tmp.of$prop.overfished, probs = 0.025) * 100
  overfished.df$piup.of[i] <- quantile(tmp.of$prop.overfished, probs = 0.975) * 100
  overfished.df$prop.70[i] <- mean(tmp.of$prop.70) * 100
  overfished.df$pilo.70[i] <- quantile(tmp.of$prop.70, probs = 0.025) * 100
  overfished.df$piup.70[i] <- quantile(tmp.of$prop.70, probs = 0.975) * 100
  overfished.df$prop.25[i] <- mean(tmp.of$prop.25) * 100
  overfished.df$pilo.25[i] <- quantile(tmp.of$prop.25, probs = 0.025) * 100
  overfished.df$piup.25[i] <- quantile(tmp.of$prop.25, probs = 0.975) * 100
  overfished.df$prop.10[i] <- mean(tmp.of$prop.10) * 100
  overfished.df$pilo.10[i] <- quantile(tmp.of$prop.10, probs = 0.025) * 100
  overfished.df$piup.10[i] <- quantile(tmp.of$prop.10, probs = 0.975) * 100
}          

overfished.plot <- ggplot(data = overfished.df) +
  geom_point(aes(x = age_struct, y = prop.of, color = climate_scenario), size = 3) +
  geom_errorbar(aes(x = age_struct, ymin = pilo.of, ymax = piup.of, color = climate_scenario), width = 0) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '% overfished status') +
  scale_y_continuous(limits = c(0,35), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1,5), labels = c(expression(tau[3]~"= 0.99"), ~paste(eta[4], ''[',5'], '=0.01'), 'Base case', ~paste(eta[4], ''[',5'], '=0.99'), expression(tau[3]~"= 0.25"))) +
  theme(legend.title = element_blank(), legend.position = c(0.9, 0.9), text = element_text(size = 13)) 

prop.70.plot <- ggplot(data = overfished.df) +
  geom_point(aes(x = age_struct, y = prop.70, color = climate_scenario), size = 3) +
  geom_errorbar(aes(x = age_struct, ymin = pilo.70, ymax = piup.70, color = climate_scenario), width = 0) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = expression(paste('% ', italic('c'), ' = 0.7'))) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1,5), labels = c(expression(tau[3]~"= 0.99"), ~paste(eta[4], ''[',5'], '=0.01'), 'Base case', ~paste(eta[4], ''[',5'], '=0.99'), expression(tau[3]~"= 0.25"))) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), axis.text.x = element_blank()) 

prop.25.plot <- ggplot(data = overfished.df) +
  geom_point(aes(x = age_struct, y = prop.25, color = climate_scenario), size = 3) +
  geom_errorbar(aes(x = age_struct, ymin = pilo.25, ymax = piup.25, color = climate_scenario), width = 0) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = expression(paste('% ', italic('c'), ' = 0.25'))) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1,5), labels = c(expression(tau[3]~"= 0.99"), ~paste(eta[4], ''[',5'], '=0.01'), 'Base case', ~paste(eta[4], ''[',5'], '=0.99'), expression(tau[3]~"= 0.25"))) +
  theme(legend.title = element_blank(), legend.position = c(0.9,0.9), text = element_text(size = 13), axis.text.x = element_blank())

prop.10.plot <- ggplot(data = overfished.df) +
  geom_point(aes(x = age_struct, y = prop.10, color = climate_scenario), size = 3) +
  geom_errorbar(aes(x = age_struct, ymin = pilo.10, ymax = piup.10, color = climate_scenario), width = 0) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = expression(paste('% ', italic('c'), ' = 0.1'))) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 5)) +
  scale_x_continuous(breaks = seq(1,5), labels = c(expression(tau[3]~"= 0.99"), ~paste(eta[4], ''[',5'], '=0.01'), 'Base case', ~paste(eta[4], ''[',5'], '=0.99'), expression(tau[3]~"= 0.25"))) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13))

ggarrange(prop.70.plot, prop.25.plot, prop.10.plot, nrow=3, ncol=1, labels=c('a','b','c'))


### OVERFISHING TAU AND ETA
tau.overfished.df <- data.frame(climate_scenario = rep(c('Contemporary','Longer duration','More frequent','More intense'), each=3),
                                age_struct = c(seq(0.7,2.7,by=1),seq(0.9,2.9,by=1),seq(1.1,3.1,by=1),seq(1.3,3.3,by=1)), 
                                prop.of = NA,pilo.of = NA,piup.of = NA,prop.70 = NA,pilo.70 = NA,piup.70 = NA,prop.25 = NA,pilo.25 = NA,piup.25 = NA,prop.10 = NA,pilo.10 = NA,piup.10 = NA)
for(index in 1:12){
  mods <- c(1,3,5,6,8,10,11,13,15,16,18,20)
  i <- mods[index]
  tmp.name <- paste0('mod',stringr::str_pad(i, 2, pad = '0'),'.overfished')
  tmp.of <- get(tmp.name)
  tau.overfished.df$prop.of[index] <- mean(tmp.of$prop.overfished) * 100
  tau.overfished.df$pilo.of[index] <- quantile(tmp.of$prop.overfished, probs = 0.025) * 100
  tau.overfished.df$piup.of[index] <- quantile(tmp.of$prop.overfished, probs = 0.975) * 100
  tau.overfished.df$prop.70[index] <- mean(tmp.of$prop.70) * 100
  tau.overfished.df$pilo.70[index] <- quantile(tmp.of$prop.70, probs = 0.025) * 100
  tau.overfished.df$piup.70[index] <- quantile(tmp.of$prop.70, probs = 0.975) * 100
  tau.overfished.df$prop.25[index] <- mean(tmp.of$prop.25) * 100
  tau.overfished.df$pilo.25[index] <- quantile(tmp.of$prop.25, probs = 0.025) * 100
  tau.overfished.df$piup.25[index] <- quantile(tmp.of$prop.25, probs = 0.975) * 100
  tau.overfished.df$prop.10[index] <- mean(tmp.of$prop.10) * 100
  tau.overfished.df$pilo.10[index] <- quantile(tmp.of$prop.10, probs = 0.025) * 100
  tau.overfished.df$piup.10[index] <- quantile(tmp.of$prop.10, probs = 0.975) * 100
}       
eta.overfished.df <- data.frame(climate_scenario = rep(c('Contemporary','Longer duration','More frequent','More intense'), each=3),
                                age_struct = c(seq(0.7,2.7,by=1),seq(0.9,2.9,by=1),seq(1.1,3.1,by=1),seq(1.3,3.3,by=1)), 
                                prop.of = NA,pilo.of = NA,piup.of = NA,prop.70 = NA,pilo.70 = NA,piup.70 = NA,prop.25 = NA,pilo.25 = NA,piup.25 = NA,prop.10 = NA,pilo.10 = NA,piup.10 = NA)
for(index in 1:12){
  mods <- c(2,3,4,7,8,9,12,13,14,17,18,19)
  i <- mods[index]
  tmp.name <- paste0('mod',stringr::str_pad(i, 2, pad = '0'),'.overfished')
  tmp.of <- get(tmp.name)
  eta.overfished.df$prop.of[index] <- mean(tmp.of$prop.overfished) * 100
  eta.overfished.df$pilo.of[index] <- quantile(tmp.of$prop.overfished, probs = 0.025) * 100
  eta.overfished.df$piup.of[index] <- quantile(tmp.of$prop.overfished, probs = 0.975) * 100
  eta.overfished.df$prop.70[index] <- mean(tmp.of$prop.70) * 100
  eta.overfished.df$pilo.70[index] <- quantile(tmp.of$prop.70, probs = 0.025) * 100
  eta.overfished.df$piup.70[index] <- quantile(tmp.of$prop.70, probs = 0.975) * 100
  eta.overfished.df$prop.25[index] <- mean(tmp.of$prop.25) * 100
  eta.overfished.df$pilo.25[index] <- quantile(tmp.of$prop.25, probs = 0.025) * 100
  eta.overfished.df$piup.25[index] <- quantile(tmp.of$prop.25, probs = 0.975) * 100
  eta.overfished.df$prop.10[index] <- mean(tmp.of$prop.10) * 100
  eta.overfished.df$pilo.10[index] <- quantile(tmp.of$prop.10, probs = 0.025) * 100
  eta.overfished.df$piup.10[index] <- quantile(tmp.of$prop.10, probs = 0.975) * 100
}       

tau.overfished.plot <- ggplot(data = tau.overfished.df) +
  geom_point(aes(x = age_struct, y = prop.of, color = climate_scenario), size = 3) +
  # geom_errorbar(aes(x = age_struct, ymin = pilo.of, ymax = piup.of, color = climate_scenario), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '', title = 'Maturation') +
  scale_y_continuous(limits = c(0,25), expand = c(0,0)) +
  # scale_x_continuous(breaks = seq(1,3), labels = c(expression(tau[3]~"= 0.99"), 'Base case', expression(tau[3]~"= 0.25"))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13),
        plot.background = element_rect(fill = 'gray90', color = 'gray90'), panel.background = element_rect(fill = 'gray90', color = 'gray90'),
        plot.title = element_text(hjust = 0.5)) 

eta.overfished.plot <- ggplot(data = eta.overfished.df) +
  geom_point(aes(x = age_struct, y = prop.of, color = climate_scenario), size = 3) +
  # geom_errorbar(aes(x = age_struct, ymin = pilo.of, ymax = piup.of, color = climate_scenario), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '% overfished status', title = 'Natural mortality') +
  scale_y_continuous(limits = c(0,25), expand = c(0,0)) +
  # scale_x_continuous(breaks = seq(1,3), labels = c(expression(eta['4,5']~"= 0.01"), 'Base case', expression(eta['4,5']~"= 0.99"))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.85), text = element_text(size = 13),
        plot.title = element_text(hjust = 0.5)) 

ggarrange(eta.overfished.plot, tau.overfished.plot, ncol=2, labels = c('a', 'b'))


tau.prop.70.plot <- ggplot(data = tau.overfished.df) +
  geom_point(aes(x = age_struct, y = prop.70, color = climate_scenario), size = 3) +
  # geom_errorbar(aes(x = age_struct, ymin = pilo.70, ymax = piup.70, color = climate_scenario), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = 'Maturation') +
  scale_y_continuous(expand = c(0,0), limits=c(60,90)) +
  scale_x_continuous(breaks = seq(1,3)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), axis.text.x = element_blank(),
        panel.background = element_rect(fill = 'gray90', color = 'gray90'), plot.background = element_rect(fill = 'gray90', color = 'gray90'),
        plot.title = element_text(hjust = 0.5)) 

tau.prop.25.plot <- ggplot(data = tau.overfished.df) +
  geom_point(aes(x = age_struct, y = prop.25, color = climate_scenario), size = 3) +
  # geom_errorbar(aes(x = age_struct, ymin = pilo.25, ymax = piup.25, color = climate_scenario), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '') +
  scale_y_continuous(expand = c(0,0), limits=c(0,5)) +
  scale_x_continuous(breaks = seq(1,3)) +
  theme(legend.title = element_blank(), legend.position = c(0.8,0.9), text = element_text(size = 13), axis.text.x = element_blank(), legend.background = element_rect(fill = 'gray90', color = 'gray90'),
        panel.background = element_rect(fill = 'gray90', color = 'gray90'), plot.background = element_rect(fill = 'gray90', color = 'gray90'))

tau.prop.10.plot <- ggplot(data = tau.overfished.df) +
  geom_point(aes(x = age_struct, y = prop.10, color = climate_scenario), size = 3) +
  # geom_errorbar(aes(x = age_struct, ymin = pilo.10, ymax = piup.10, color = climate_scenario), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '') +
  scale_y_continuous(expand = c(0,0), limits = c(0, 2)) +
  # scale_x_continuous(breaks = seq(1,3), labels = c(expression(tau[3]~"= 0.99"), 'Base case', expression(tau[3]~"= 0.25"))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), 
        panel.background = element_rect(fill = 'gray90', color = 'gray90'), plot.background = element_rect(fill = 'gray90', color = 'gray90'))

tau.prop.plot <- ggarrange(tau.prop.70.plot, tau.prop.25.plot, tau.prop.10.plot, nrow=3, labels = c('b','d','f'))

eta.prop.70.plot <- ggplot(data = eta.overfished.df) +
  geom_point(aes(x = age_struct, y = prop.70, color = climate_scenario), size = 3) +
  # geom_errorbar(aes(x = age_struct, ymin = pilo.70, ymax = piup.70, color = climate_scenario), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = expression(paste('% ', italic('c'), ' = 0.7')), title = 'Natural mortality') +
  scale_y_continuous(expand = c(0,0), limits=c(60,90)) +
  scale_x_continuous(breaks = seq(1,3)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

eta.prop.25.plot <- ggplot(data = eta.overfished.df) +
  geom_point(aes(x = age_struct, y = prop.25, color = climate_scenario), size = 3) +
  # geom_errorbar(aes(x = age_struct, ymin = pilo.25, ymax = piup.25, color = climate_scenario), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = expression(paste('% ', italic('c'), ' = 0.25'))) +
  scale_y_continuous(expand = c(0,0), limits=c(0,5)) +
  scale_x_continuous(breaks = seq(1,3)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), axis.text.x = element_blank())

eta.prop.10.plot <- ggplot(data = eta.overfished.df) +
  geom_point(aes(x = age_struct, y = prop.10, color = climate_scenario), size = 3) +
  # geom_errorbar(aes(x = age_struct, ymin = pilo.10, ymax = piup.10, color = climate_scenario), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = expression(paste('% ', italic('c'), ' = 0.1'))) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 2)) +
  # scale_x_continuous(breaks = seq(1,3), labels = c(expression(eta['4,5']~"= 0.01"), 'Base case', expression(eta['4,5']~"= 0.99"))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13))

eta.prop.plot <- ggarrange(eta.prop.70.plot, eta.prop.25.plot, eta.prop.10.plot, nrow=3, labels = c('a', 'c', 'e'))

all.prop.plot <- ggarrange(eta.prop.plot, tau.prop.plot, ncol=2)

## Calculate proportional difference -----------------------------------------------------------------------------------
# Function for calculating proportional difference
# Base flow scenarios
la.bf.spd <- cal_prop_diff(la.bf.df$spawn.mean, ba.bf.df$spawn.mean)
ha.bf.spd <- cal_prop_diff(ha.bf.df$spawn.mean, ba.bf.df$spawn.mean)
la.bf.hpd <- cal_prop_diff(la.bf.df$harvest.mean, ba.bf.df$harvest.mean)
ha.bf.hpd <- cal_prop_diff(ha.bf.df$harvest.mean, ba.bf.df$harvest.mean)

# Low flow scenarios
la.lmf.spd <- cal_prop_diff(la.lmf.df$spawn.mean, ba.lmf.df$spawn.mean)
ha.lmf.spd <- cal_prop_diff(ha.lmf.df$spawn.mean, ba.lmf.df$spawn.mean)
la.lmf.hpd <- cal_prop_diff(la.lmf.df$harvest.mean, ba.lmf.df$harvest.mean)
ha.lmf.hpd <- cal_prop_diff(ha.lmf.df$harvest.mean, ba.lmf.df$harvest.mean)


# Sensitivity analyses ---------------------------------------------------------------------------------------------------------------
## Test effects of juvenile survival ----
# A. Base age-structure, base flow, low juvenile survival
pars.lj  <- c(0.018, 0.30, 0.86, 0.20)
ba.bf.lj <- mse.simulation(pars=pars.lj, years=n.yr, sims=n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), vars=base.flow)
# B. Low age-structure, base flow, low juvenile survival
la.bf.lj <- mse.simulation(pars=pars.lj, years=n.yr, sims=n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), vars=base.flow)
# C. High age-structure, base flow, low juvenile survival
ha.bf.lj <- mse.simulation(pars=pars.lj, years=n.yr, sims=n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), vars=base.flow)
# Summarize results
ba.bf.lj.df  <- mse.summary(ba.bf.lj)  
la.bf.lj.df  <- mse.summary(la.bf.lj)  
ha.bf.lj.df  <- mse.summary(ha.bf.lj) 

ggplot() +
  geom_point(aes(x = c("1", "2", "3"), y = c(la.bf.df$spawn.mean/1000, ba.bf.df$spawn.mean/1000, ha.bf.df$spawn.mean/1000)), size = 3) +
  geom_point(aes(x = c("1", "2", "3"), y = c(la.bf.lj.df$spawn.mean/1000, ba.bf.lj.df$spawn.mean/1000, ha.bf.lj.df$spawn.mean/1000)), size = 3, color = 'red') 
ggplot() +
  geom_point(aes(x = c("1", "2", "3"), y = c(la.bf.df$harvest.mean/1000, ba.bf.df$harvest.mean/1000, ha.bf.df$harvest.mean/1000)), size = 3) +
  geom_point(aes(x = c("1", "2", "3"), y = c(la.bf.lj.df$harvest.mean/1000, ba.bf.lj.df$harvest.mean/1000, ha.bf.lj.df$harvest.mean/1000)), size = 3, color = 'red') 

## Plot spawner age composition for age structure scenarios ----
age.comp.df <- data.frame(age = rep(c('2','3','4','5'), times = 3),
                          mean = c(as.numeric(ba.bf %>% filter(year >= 30) %>% dplyr::select(spawn.2, spawn.3, spawn.4, spawn.5) %>% summarise(across(1:4, mean))),
                                   as.numeric(la.bf %>% filter(year >= 30) %>% dplyr::select(spawn.2, spawn.3, spawn.4, spawn.5) %>% summarise(across(1:4, mean))),
                                   as.numeric(ha.bf %>% filter(year >= 30) %>% dplyr::select(spawn.2, spawn.3, spawn.4, spawn.5) %>% summarise(across(1:4, mean)))),
                          sterr = c(as.numeric(ba.bf %>% filter(year >= 30) %>% dplyr::select(spawn.2, spawn.3, spawn.4, spawn.5) %>% summarise(across(1:4, se))),
                                    as.numeric(la.bf %>% filter(year >= 30) %>% dplyr::select(spawn.2, spawn.3, spawn.4, spawn.5) %>% summarise(across(1:4, se))),
                                    as.numeric(ha.bf %>% filter(year >= 30) %>% dplyr::select(spawn.2, spawn.3, spawn.4, spawn.5) %>% summarise(across(1:4, se)))),
                          scen = as.factor(rep(c('base', 'low', 'high'), each = 4)))

low.age.comp <- ggplot() +
  geom_bar(data = age.comp.df %>% filter(scen == 'low'), aes(x = age, y = mean/sum(mean)), stat = 'identity') +
  labs(x = 'Age', y = 'Proportion', title = 'Low age structure') +
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) 
base.age.comp <- ggplot() +
  geom_bar(data = age.comp.df %>% filter(scen == 'base'), aes(x = age, y = mean/sum(mean)), stat = 'identity') +
  labs(x = 'Age', y = 'Proportion', title = 'Base age structure') +
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0,1))
high.age.comp <- ggplot() +
  geom_bar(data = age.comp.df %>% filter(scen == 'high'), aes(x = age, y = mean/sum(mean)), stat = 'identity') +
  labs(x = 'Age', y = 'Proportion', title = 'High age structure') +
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0,1))
ggarrange(low.age.comp, base.age.comp, high.age.comp, nrow = 1, ncol = 3)


## Test method for calculating CV ----
# 1. Base scenario with all ages
tmp.df1 <- ba.bf %>% dplyr::filter(year >= 30) %>% dplyr::select(year, Spawn.est, harvest, sim) %>% mutate(esc.har = Spawn.est + harvest)
tmp.cv1 <- tmp.df1 %>% dplyr::group_by(sim) %>% dplyr::summarise(spawn.cv = sd(Spawn.est, na.rm=TRUE)/mean(Spawn.est, na.rm=TRUE), harvest.cv = sd(harvest)/mean(harvest), esc.har.cv = sd(esc.har, na.rm=TRUE)/mean(esc.har, na.rm=TRUE))
tmp.df1 <- data.frame(spawn.mean = mean(tmp.df1$Spawn.est, na.rm = TRUE),
                      spawn.median = median(tmp.df1$Spawn.est, na.rm = TRUE),
                      spawn.pi.lo = quantile(tmp.df1$Spawn.est, probs = 0.025, na.rm = TRUE),
                      spawn.pi.up = quantile(tmp.df1$Spawn.est, probs = 0.975, na.rm = TRUE),
                      spawn.cv = mean(tmp.cv1$spawn.cv),
                      spawn.cv.lo = quantile(tmp.cv1$spawn.cv, probs = 0.025),
                      spawn.cv.up = quantile(tmp.cv1$spawn.cv, probs = 0.975),
                      harvest.mean = mean(tmp.df1$harvest),
                      harvest.median = median(tmp.df1$harvest),
                      harvest.pi.lo = quantile(tmp.df1$harvest, probs = 0.025),
                      harvest.pi.up = quantile(tmp.df1$harvest, probs = 0.975),
                      harvest.cv = mean(tmp.cv1$harvest.cv),
                      harvest.cv.lo = quantile(tmp.cv1$harvest.cv, probs = 0.025),
                      harvest.cv.up = quantile(tmp.cv1$harvest.cv, probs = 0.975),
                      esc.har.mean = mean(tmp.df1$esc.har, na.rm=TRUE),
                      esc.har.cv = mean(tmp.cv1$esc.har.cv))

# 2. Base scenario with two dominant age classes
tmp.df2 <- ba.bf %>% dplyr::filter(year >= 30) %>% dplyr::select(year, Spawn.est, spawn.2, spawn.3, spawn.4, spawn.5, harvest, sim, harvest.2, harvest.3, harvest.4, harvest.5) %>% mutate(esc.har.2 = spawn.2+harvest.2, esc.har.3 = spawn.3+harvest.3, esc.har.4 = spawn.4+harvest.4, esc.har.5 = spawn.5+harvest.5)
tmp.df2$no.age.struct <- tmp.df2$esc.har.3 + tmp.df2$esc.har.4
tmp.cv2 <- tmp.df2 %>% dplyr::group_by(sim) %>% dplyr::summarise(spawn.cv = sd(no.age.struct, na.rm = TRUE)/mean(no.age.struct, na.rm = TRUE))
tmp.df2 <- data.frame(spawn.mean = mean(tmp.df2$no.age.struct, na.rm = TRUE),
                      spawn.pi.lo = quantile(tmp.df2$no.age.struct, probs = 0.025, na.rm = TRUE),
                      spawn.pi.up = quantile(tmp.df2$no.age.struct, probs = 0.975, na.rm = TRUE),
                      spawn.cv = mean(tmp.cv2$spawn.cv),
                      spawn.cv.lo = quantile(tmp.cv2$spawn.cv, probs = 0.025),
                      spawn.cv.up = quantile(tmp.cv2$spawn.cv, probs = 0.975))

# 3. Base scenario with dominant age class
tmp.df3 <- ba.bf %>% dplyr::filter(year >= 30) %>% dplyr::select(year, Spawn.est, spawn.2, spawn.3, spawn.4, spawn.5, harvest, sim)
tmp.df3$no.age.struct <- tmp.df3$spawn.3
tmp.cv3 <- tmp.df3 %>% dplyr::group_by(sim) %>% dplyr::summarise(spawn.cv = sd(no.age.struct, na.rm = TRUE)/mean(no.age.struct, na.rm = TRUE))
tmp.df3 <- data.frame(spawn.mean = mean(tmp.df3$no.age.struct, na.rm = TRUE),
                      spawn.pi.lo = quantile(tmp.df3$no.age.struct, probs = 0.025, na.rm = TRUE),
                      spawn.pi.up = quantile(tmp.df3$no.age.struct, probs = 0.975, na.rm = TRUE),
                      spawn.cv = mean(tmp.cv3$spawn.cv),
                      spawn.cv.lo = quantile(tmp.cv3$spawn.cv, probs = 0.025),
                      spawn.cv.up = quantile(tmp.cv3$spawn.cv, probs = 0.975))

# 4. Low age structure
tmp.df4 <- la.bf %>% dplyr::filter(year >= 30) %>% dplyr::select(year, Spawn.est, harvest, sim) %>% mutate(esc.har = Spawn.est + harvest)
tmp.cv4 <- tmp.df4 %>% dplyr::group_by(sim) %>% dplyr::summarise(spawn.cv = sd(Spawn.est, na.rm=TRUE)/mean(Spawn.est, na.rm=TRUE), harvest.cv = sd(harvest)/mean(harvest), esc.har.cv = sd(esc.har, na.rm=TRUE)/mean(esc.har, na.rm=TRUE))
tmp.df4 <- data.frame(spawn.mean = mean(tmp.df4$Spawn.est, na.rm = TRUE),
                      spawn.median = median(tmp.df4$Spawn.est, na.rm = TRUE),
                      spawn.pi.lo = quantile(tmp.df4$Spawn.est, probs = 0.025, na.rm = TRUE),
                      spawn.pi.up = quantile(tmp.df4$Spawn.est, probs = 0.975, na.rm = TRUE),
                      spawn.cv = mean(tmp.cv4$spawn.cv),
                      spawn.cv.lo = quantile(tmp.cv4$spawn.cv, probs = 0.025),
                      spawn.cv.up = quantile(tmp.cv4$spawn.cv, probs = 0.975),
                      harvest.mean = mean(tmp.df4$harvest),
                      harvest.median = median(tmp.df4$harvest),
                      harvest.pi.lo = quantile(tmp.df4$harvest, probs = 0.025),
                      harvest.pi.up = quantile(tmp.df4$harvest, probs = 0.975),
                      harvest.cv = mean(tmp.cv4$harvest.cv),
                      harvest.cv.lo = quantile(tmp.cv4$harvest.cv, probs = 0.025),
                      harvest.cv.up = quantile(tmp.cv4$harvest.cv, probs = 0.975),
                      esc.har.cv = mean(tmp.cv4$esc.har.cv))

ggplot() +
  geom_point(aes(x = c(1, 2, 3, 4), y = c(tmp.df1$spawn.cv, tmp.df2$spawn.cv, tmp.df3$spawn.cv, tmp.df4$spawn.cv)), size = 3) +
  scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c('Base', 'Dominant ages (2)', 'Dominant age (1)', 'Low age structure')) +
  labs(x = '', y = 'CV escapement') +
  theme_classic()



## Hatchery smolts released ----------------------------------------------------------------------------------------------------------
# base flow and age, and constant number of smolts released set to mean of the data
mean.hat.release     <- set.vars(n.yr, flow, flow.m=NA, flow.sd=NA, hat.release, hat.distance, catch.esc, npgo) # base flow 
ba.bf.meanRelease    <- mse.simulation(pars=pars, years=n.yr, sims=n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), vars=mean.hat.release)
ba.bf.meanRelease.df <- mse.summary(ba.bf.meanRelease)
mean.hat.spawn <- ggplot() + geom_point(aes(x = c(1, 2), y = c(ba.bf.df$spawn.mean, ba.bf.meanRelease.df$spawn.mean))) + geom_errorbar(aes(x = c(1, 2), ymin = c(ba.bf.df$spawn.pi.lo, ba.bf.meanRelease.df$spawn.pi.lo), ymax = c(ba.bf.df$spawn.pi.up, ba.bf.meanRelease.df$spawn.pi.up)))
mean.hat.harvest <- ggplot() + geom_point(aes(x = c(1, 2), y = c(ba.bf.df$harvest.mean, ba.bf.meanRelease.df$harvest.mean))) + geom_errorbar(aes(x = c(1, 2), ymin = c(ba.bf.df$harvest.pi.lo, ba.bf.meanRelease.df$harvest.pi.lo), ymax = c(ba.bf.df$harvets.pi.up, ba.bf.meanRelease.df$harvest.pi.up)))

((ba.bf.df$spawn.mean - ba.bf.meanRelease.df$spawn.mean)/ba.bf.df$spawn.mean)*100
((ba.bf.df$harvest.mean - ba.bf.meanRelease.df$harvest.mean)/ba.bf.df$harvest.mean)*100










# EXTRA CODE ------------------------------------------------------------------------------------------------------------------------
# Test age structure
bFlow.bMat.bMort <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, 
                                   m.maturity = c(0.038, 0.500, 0.999, 1), 
                                   n.surv = c(0.5, 0.8, 0.8, 0.8),
                                   scenario = 'base')
bFlow.lMat <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, 
                             m.maturity = c(0.038, 0.999, 0.999, 1),
                             n.surv = c(0.5, 0.8, 0.8, 0.8),
                             scenario = 'base')
bFlow.hMat <- mse.simulation(pars = pars, years = n.yr, sims = n.sim, 
                             m.maturity = c(0.038, 0.250, 0.999, 1),
                             n.surv = c(0.5, 0.8, 0.8, 0.8),
                             scenario = 'base')
bFlow.lMort <- mse.simulation(pars = pars, years = n.yr, sims = n.sim,
                              m.maturity = c(0.038, 0.500, 0.999, 1), 
                              n.surv = c(0.5, 0.8, 0.99, 0.99),
                              scenario = 'base')
bFlow.hMort <- mse.simulation(pars = pars, years = n.yr, sims = n.sim,
                              m.maturity = c(0.038, 0.500, 0.999, 1), 
                              n.surv = c(0.5, 0.8, 0.01, 0.01),
                              scenario = 'base')

age.comp.df <- data.frame(age = rep(c('2','3','4','5'), times = 5),
                          mean = c(as.numeric(bFlow.bMat.bMort %>% filter(year >= 30) %>% dplyr::select(spawn.2, spawn.3, spawn.4, spawn.5) %>% summarise(across(1:4, mean))),
                                   as.numeric(bFlow.lMat %>% filter(year >= 30) %>% dplyr::select(spawn.2, spawn.3, spawn.4, spawn.5) %>% summarise(across(1:4, mean))),
                                   as.numeric(bFlow.hMat %>% filter(year >= 30) %>% dplyr::select(spawn.2, spawn.3, spawn.4, spawn.5) %>% summarise(across(1:4, mean))),
                                   as.numeric(bFlow.lMort %>% filter(year >= 30) %>% dplyr::select(spawn.2, spawn.3, spawn.4, spawn.5) %>% summarise(across(1:4, mean))),
                                   as.numeric(bFlow.hMort %>% filter(year >= 30) %>% dplyr::select(spawn.2, spawn.3, spawn.4, spawn.5) %>% summarise(across(1:4, mean)))),
                          # sterr = c(as.numeric(ba.bf %>% filter(year >= 30) %>% dplyr::select(spawn.2, spawn.3, spawn.4, spawn.5) %>% summarise(across(1:4, se))),
                          #           as.numeric(la.bf %>% filter(year >= 30) %>% dplyr::select(spawn.2, spawn.3, spawn.4, spawn.5) %>% summarise(across(1:4, se))),
                          #           as.numeric(ha.bf %>% filter(year >= 30) %>% dplyr::select(spawn.2, spawn.3, spawn.4, spawn.5) %>% summarise(across(1:4, se)))),
                          scen = as.factor(rep(c('base', 'low.mat', 'high.mat', 'low.mort', 'high.mort'), each = 4)))

tmp.plot1 <- ggplot() +
  geom_bar(data = age.comp.df %>% filter(scen == 'base'), aes(x = age, y = mean/sum(mean)), stat = 'identity') +
  labs(x = 'Age', y = '', title = 'Base case') +
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  theme(text = element_text(size = 14), plot.margin = unit(c(0.1,0.1,0.1,0.1), 'cm'))
tmp.plot2 <- ggplot() +
  geom_bar(data = age.comp.df %>% filter(scen == 'low.mat'), aes(x = age, y = mean/sum(mean)), stat = 'identity') +
  labs(x = '', y = 'Proportion', title = ~paste(tau[3], '=', 0.99)) + # low.mat
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  theme(text = element_text(size = 14), plot.margin = unit(c(0.1,0.1,0.1,0.1), 'cm'))
tmp.plot3 <- ggplot() +
  geom_bar(data = age.comp.df %>% filter(scen == 'high.mat'), aes(x = age, y = mean/sum(mean)), stat = 'identity') +
  labs(x = '', y = '', title = ~paste(tau[3], '=', 0.25)) + # high.mat
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  theme(text = element_text(size = 14), plot.margin = unit(c(0.1,0.1,0.1,0.1), 'cm'))
tmp.plot4 <- ggplot() +
  geom_bar(data = age.comp.df %>% filter(scen == 'low.mort'), aes(x = age, y = mean/sum(mean)), stat = 'identity') +
  labs(x = '', y = '', title = ~paste(eta[paste('4,5')], '=', 0.99)) + # low.mort
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  theme(text = element_text(size = 14), plot.margin = unit(c(0.1,0.1,0.1,0.1), 'cm'))
tmp.plot5 <- ggplot() +
  geom_bar(data = age.comp.df %>% filter(scen == 'high.mort'), aes(x = age, y = mean/sum(mean)), stat = 'identity') +
  labs(x = '', y = '', title = ~paste(eta[paste('4,5')], '=', 0.01)) + # high.mort
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  theme(text = element_text(size = 14), plot.margin = unit(c(0.1,0.1,0.1,0.1), 'cm'))

ggarrange(tmp.plot2, tmp.plot5, tmp.plot1, tmp.plot4, tmp.plot3, nrow = 1)




tmp.plot1 <- ggplot() +
  geom_errorbar(aes(x = c(1, 1.5, 2.5, 3, 4, 4.5), ymin = c(la.bf.df$spawn.pi.lo/1000, la.lmf.df$spawn.pi.lo/1000, ba.bf.df$spawn.pi.lo/1000, ba.lmf.df$spawn.pi.lo/1000, ha.bf.df$spawn.pi.lo/1000, ha.lmf.df$spawn.pi.lo/1000), 
                    ymax = c(la.bf.df$spawn.pi.up/1000, la.lmf.df$spawn.pi.up/1000, ba.bf.df$spawn.pi.up/1000, ba.lmf.df$spawn.pi.up/1000, ha.bf.df$spawn.pi.up/1000, ha.lmf.df$spawn.pi.up/1000)), width = 0) +
  geom_point(aes(x = c(1, 2.5, 4), y = c(la.bf.df$spawn.mean/1000, ba.bf.df$spawn.mean/1000, ha.bf.df$spawn.mean/1000), color = 'darkblue'),size = 4) +
  geom_point(aes(x = c(1.5, 3, 4.5), y = c(la.lmf.df$spawn.mean/1000, ba.lmf.df$spawn.mean/1000, ha.lmf.df$spawn.mean/1000), color = '#BF8943'), size = 4) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1200)) +
  scale_x_continuous(breaks = c(1.25, 2.75, 4.25), labels = c('Low', 'Base', 'High')) + 
  scale_color_manual(values = c('darkblue' = 'darkblue', '#BF8943' = '#BF8943'), labels = c('Base flow', 'Mean flow reduced 5%')) +
  labs(x = 'Age structure', y = 'Spawner escapement (thousands)') +
  # annotate('text', x = c(1.1, 1.5, 2.5, 3, 4, 4.4), 
  # y = c(s1.summary$spawn.pi.up/1000+40, s7.summary$spawn.pi.up/1000+40, base.summary$spawn.pi.up/1000+40, s5.summary$spawn.pi.up/1000+40, s2.summary$spawn.pi.up/1000+40, s8.summary$spawn.pi.up/1000+40), 
  # label = c(paste0('CV = ', round(s1.summary$spawn.cv, 2)), paste0('CV = ', round(s7.summary$spawn.cv, 2)), paste0('CV = ', round(base.summary$spawn.cv, 2)), paste0('CV = ', round(s5.summary$spawn.cv, 2)), paste0('CV = ', round(s2.summary$spawn.cv, 2)), paste0('CV = ', round(s8.summary$spawn.cv, 2)))) +
  theme_classic() +
  theme(legend.position = c(0.6, 0.95), legend.title = element_blank(),
        rect = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'transparent', colour = NA),
        plot.background = element_rect(fill = 'transparent', colour = NA),
        text = element_text(size = 12))

tmp.plot2 <- ggplot() +
  geom_errorbar(aes(x = c(1, 1.5, 2.5, 3, 4, 4.5), ymin = c(la.bf.df$harvest.pi.lo/1000, la.lmf.df$harvest.pi.lo/1000, ba.bf.df$harvest.pi.lo/1000, ba.lmf.df$harvest.pi.lo/1000, ha.bf.df$harvest.pi.lo/1000, ha.lmf.df$harvest.pi.lo/1000), 
                    ymax = c(la.bf.df$harvest.pi.up/1000, la.lmf.df$harvest.pi.up/1000, ba.bf.df$harvest.pi.up/1000, ba.lmf.df$harvest.pi.up/1000, ha.bf.df$harvest.pi.up/1000, ha.lmf.df$harvest.pi.up/1000)), width = 0) +
  geom_point(aes(x = c(1, 2.5, 4), y = c(la.bf.df$harvest.mean/1000, ba.bf.df$harvest.mean/1000, ha.bf.df$harvest.mean/1000), color = 'darkblue'),size = 4) +
  geom_point(aes(x = c(1.5, 3, 4.5), y = c(la.lmf.df$harvest.mean/1000, ba.lmf.df$harvest.mean/1000, ha.lmf.df$harvest.mean/1000), color = '#BF8943'), size = 4) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1800)) +
  scale_x_continuous(breaks = c(1.25, 2.75, 4.25), labels = c('Low', 'Base', 'High')) + 
  scale_color_manual(values = c('darkblue' = 'darkblue', '#BF8943' = '#BF8943'), labels = c('Base flow', 'Mean flow reduced 5%')) +
  labs(x = 'Age structure', y = 'Harvest (thousands)') +
  # annotate('text', x = c(1.1, 1.5, 2.5, 3, 4, 4.4), 
  # y = c(s1.summary$spawn.pi.up/1000+40, s7.summary$spawn.pi.up/1000+40, base.summary$spawn.pi.up/1000+40, s5.summary$spawn.pi.up/1000+40, s2.summary$spawn.pi.up/1000+40, s8.summary$spawn.pi.up/1000+40), 
  # label = c(paste0('CV = ', round(s1.summary$spawn.cv, 2)), paste0('CV = ', round(s7.summary$spawn.cv, 2)), paste0('CV = ', round(base.summary$spawn.cv, 2)), paste0('CV = ', round(s5.summary$spawn.cv, 2)), paste0('CV = ', round(s2.summary$spawn.cv, 2)), paste0('CV = ', round(s8.summary$spawn.cv, 2)))) +
  theme_classic() +
  theme(legend.position = 'none', legend.title = element_blank(),
        rect = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'transparent', colour = NA),
        plot.background = element_rect(fill = 'transparent', colour = NA),
        text = element_text(size = 12))

ggarrange(tmp.plot1, tmp.plot2, nrow=1, ncol=2)

ggplot() +
  geom_errorbar(aes(x = c(1, 1.5, 2.5, 3, 4, 4.5), ymin = c(s1.summary$spawn.cv.lo, s7.summary$spawn.cv.lo, base.summary$spawn.cv.lo, s5.summary$spawn.cv.lo, s2.summary$spawn.cv.lo, s8.summary$spawn.cv.lo), ymax = c(s1.summary$spawn.cv.up, s7.summary$spawn.cv.up, base.summary$spawn.cv.up, s5.summary$spawn.cv.up, s2.summary$spawn.cv.up, s8.summary$spawn.cv.up)), width = 0) +
  geom_point(aes(x = c(1, 2.5, 4), y = c(s1.summary$spawn.cv, base.summary$spawn.cv, s2.summary$spawn.cv)), color = 'darkblue', size = 4) +
  geom_point(aes(x = c(1.5, 3, 4.5), y = c(s7.summary$spawn.cv, s5.summary$spawn.cv, s8.summary$spawn.cv)), color = '#BF8943', size = 4) +
  scale_y_continuous(expand = c(0.05, 0.05)) +
  scale_x_continuous(breaks = c(1.25, 2.75, 4.25), labels = c('Low', 'Base', 'High')) + 
  labs(x = 'Age structure', y = 'Spawner variability (CV)') +
  theme_classic() +
  theme(rect = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'transparent', colour = NA),
        plot.background = element_rect(fill = 'transparent', colour = NA))

# harvest
ggplot() +
  geom_errorbar(aes(x = c(1, 1.5, 2.5, 3, 4, 4.5), ymin = c(s1.summary$harvest.pi.lo/1000, s7.summary$harvest.pi.lo/1000, base.summary$harvest.pi.lo/1000, s5.summary$harvest.pi.lo/1000, s2.summary$harvest.pi.lo/1000, s8.summary$harvest.pi.lo/1000), ymax = c(s1.summary$harvest.pi.up/1000, s7.summary$harvest.pi.up/1000, base.summary$harvest.pi.up/1000, s5.summary$harvest.pi.up/1000, s2.summary$harvest.pi.up/1000, s8.summary$harvest.pi.up/1000)), width = 0) +
  geom_point(aes(x = c(1, 2.5, 4), y = c(s1.summary$harvest.mean/1000, base.summary$harvest.mean/1000, s2.summary$harvest.mean/1000)), color = 'darkblue', size = 4) +
  geom_point(aes(x = c(1.5, 3, 4.5), y = c(s7.summary$harvest.mean/1000, s5.summary$harvest.mean/1000, s8.summary$harvest.mean/1000)), color = '#BF8943', size = 4) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1700)) +
  scale_x_continuous(breaks = c(1.25, 2.75, 4.25), labels = c('Low', 'Base', 'High')) + 
  labs(x = 'Age structure', y = 'Harvest (thousands)') +
  theme_classic() +
  theme(rect = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'transparent', colour = NA),
        plot.background = element_rect(fill = 'transparent', colour = NA))

ggplot() +
  geom_errorbar(aes(x = c(1, 1.5, 2.5, 3, 4, 4.5), ymin = c(s1.summary$harvest.cv.lo, s7.summary$harvest.cv.lo, base.summary$harvest.cv.lo, s5.summary$harvest.cv.lo, s2.summary$harvest.cv.lo, s8.summary$harvest.cv.lo), ymax = c(s1.summary$harvest.cv.up, s7.summary$harvest.cv.up, base.summary$harvest.cv.up, s5.summary$harvest.cv.up, s2.summary$harvest.cv.up, s8.summary$harvest.cv.up)), width = 0) +
  geom_point(aes(x = c(1, 2.5, 4), y = c(s1.summary$harvest.cv, base.summary$harvest.cv, s2.summary$harvest.cv)), color = 'darkblue', size = 4) +
  geom_point(aes(x = c(1.5, 3, 4.5), y = c(s7.summary$harvest.cv, s5.summary$harvest.cv, s8.summary$harvest.cv)), color = '#BF8943', size = 4) +
  scale_y_continuous(expand = c(0.05, 0.05)) +
  scale_x_continuous(breaks = c(1.25, 2.75, 4.25), labels = c('Low', 'Base', 'High')) + 
  labs(x = 'Age structure', y = 'Harvest variability (CV)') +
  theme_classic() +
  theme(rect = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'transparent', colour = NA),
        plot.background = element_rect(fill = 'transparent', colour = NA))

fig3 <- ggplot() +
  geom_errorbar(aes(x = c(1, 1.5, 2.5, 3, 4, 4.5), ymin = c(s1.summary$spawn.cv.lo, s7.summary$spawn.cv.lo, base.summary$spawn.cv.lo, s5.summary$spawn.cv.lo, s2.summary$spawn.cv.lo, s8.summary$spawn.cv.lo), ymax = c(s1.summary$spawn.cv.up, s7.summary$spawn.cv.up, base.summary$spawn.cv.up, s5.summary$spawn.cv.up, s2.summary$spawn.cv.up, s8.summary$spawn.cv.up)), width = 0) +
  geom_point(aes(x = c(1, 2.5, 4), y = c(s1.summary$spawn.cv, base.summary$spawn.cv, s2.summary$spawn.cv)), color = 'darkblue', size = 4) +
  geom_point(aes(x = c(1.5, 3, 4.5), y = c(s7.summary$spawn.cv, s5.summary$spawn.cv, s8.summary$spawn.cv)), color = '#BF8943', size = 4) +
  scale_y_continuous(expand = c(0.05, 0.05)) +
  scale_x_continuous(breaks = c(1.25, 2.75, 4.25), labels = c('Low', 'Base', 'High')) + 
  labs(x = 'Age structure', y = 'Spawner variability (CV)') +
  theme_classic() +
  theme(rect = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'transparent', colour = NA),
        plot.background = element_rect(fill = 'transparent', colour = NA))

ggsave('fig3.png', fig3, bg = 'transparent')


# # plot step function for survival
test.flow <- seq(0, 30000)
test.surv <- NULL
for(test.i in 1:length(test.flow)){ 
  test.surv <- c(test.surv, n.2.survival(deterministic = TRUE, flow.survival = 'stepfunction', river.flow = test.flow[test.i])) 
}
test.df <- data.frame(flow = test.flow, surv = test.surv)
`test.step1 <- data.frame(flow = seq(0, 4045), up = inv.logit(logit(0.03) + (1.96 * 0.276)), lo = inv.logit(logit(0.03) - (1.96 * 0.276))) # step 1
test.step2 <- data.frame(flow = seq(4795, 10175), up = inv.logit(logit(0.189) + (1.96 * 0.094)), lo = inv.logit(logit(0.189) - (1.96 * 0.094))) # step 2
test.step3 <- data.frame(flow = seq(11248, 18000), up = inv.logit(logit(0.508) + (1.96 * 0.082)), lo = inv.logit(logit(0.508) - (1.96 * 0.082))) # step 3
test.line1 <- data.frame(flow = seq(4045, 4795), y = test.df$surv[4046:4796], up = inv.logit(logit(test.df$surv[4050]) + (1.96 * 0.276)), lo = inv.logit(logit(test.df$surv[4050]) - (1.96 * 0.276)))
test.line2 <- data.frame(flow = seq(10175, 11248), y = test.df$surv[10176:11249], up = inv.logit(logit(test.df$surv[10200]) + (1.96 * 0.094)), lo = inv.logit(logit(test.df$surv[10200]) - (1.96 * 0.094)))
ggplot() +
  geom_ribbon(data = test.step1, aes(x = flow, ymin = 0.03 - (up-lo)/2, ymax = 0.03 + (up-lo)/2), fill = 'grey70') +
  geom_ribbon(data = test.step2, aes(x = flow, ymin = 0.189 - (up-lo)/2, ymax = 0.189 + (up-lo)/2), fill = 'grey70') +
  geom_ribbon(data = test.step3, aes(x = flow, ymin = 0.508 - (up-lo)/2, ymax = 0.508 + (up-lo)/2), fill = 'grey70') +
  geom_ribbon(data = test.line1, aes(x = flow, ymin = y - (up-lo)/2, ymax = y + (up-lo)/2), fill = 'grey70') +
  geom_ribbon(data = test.line2, aes(x = flow, ymin = y - (up-lo)/2, ymax = y + (up-lo)/2), fill = 'grey70') +
  geom_line(data = test.df, aes(x = flow, y = surv)) +
  theme_classic() +
  labs(y = 'River survival', x = 'Flow (cfs)')


# Plot example climate scenarios
library(dplyr)
library(ggplot2)
library(ggpubr)
contemporary <- flow.sim(n.yr = 100, scenario = 'base', flow.full = flow.full)
duration <- flow.sim(n.yr = 100, scenario = 'longer duration', flow.full = flow.full)
frequent <- flow.sim(n.yr = 100, scenario = 'more frequent', flow.full = flow.full)
intense <- flow.sim(n.yr = 100, scenario = 'more intense', flow.full = flow.full)

cplot <- ggplot() +
  geom_line(aes(x = c(1:100), y = contemporary), size = 0.55) +
  geom_hline(yintercept = 10712, lty = 'dashed') +
  geom_hline(yintercept = 4295, lty = 'dashed') +
  labs(x = "", y = 'Flow (cfs)', title = 'Contemporary') +
  theme_classic() +
  theme(text = element_text(size = 15))

dplot <- ggplot() +
  geom_line(aes(x = c(1:100), y = duration), size = 0.55) +
  geom_hline(yintercept = 10712, lty = 'dashed') +
  geom_hline(yintercept = 4295, lty = 'dashed') +
  labs(x = "", y = '', title = 'Longer duration') +
  theme_classic() +
  theme(text = element_text(size = 15))

fplot <- ggplot() +
  geom_line(aes(x = c(1:100), y = frequent), size = 0.55) +
  geom_hline(yintercept = 10712, lty = 'dashed') +
  geom_hline(yintercept = 4295, lty = 'dashed') +
  labs(x = "Simulation year", y = 'Flow (cfs)', title = 'More frequent') +
  theme_classic() +
  theme(text = element_text(size = 15))

iplot <- ggplot() +
  geom_line(aes(x = c(1:100), y = intense), size = 0.55) +
  geom_hline(yintercept = 10712, lty = 'dashed') +
  geom_hline(yintercept = 4295, lty = 'dashed') +
  labs(x = "Simulation year", y = '', title = 'More intense') +
  theme_classic() +
  theme(text = element_text(size = 15))

ggarrange(cplot, dplot, fplot, iplot, nrow=2, ncol=2)









