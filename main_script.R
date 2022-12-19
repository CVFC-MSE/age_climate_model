## -------------------------------------------------------------------------------------------------------------------------
## model_fitting.R
##
## Author: Paul Carvalho (paul.carvalho@noaa.gov, pcarvalh@ucsc.edu)
##
## Description: Run simulation models to test the effects of drought on Sacramento River fall run Chinook salmon under
##              different age structure scenarios. Two mechanisms that affect age structure were tested separately - 
##              mortality and maturation.
## -------------------------------------------------------------------------------------------------------------------------


# 1. GENERAL NOTES ---------------------------------------------------------------------------------------------------------
# a. Use docstring('insert function name') to view function documentation and information.


# 2. SETUP WORKSPACE -------------------------------------------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Load libraries
library(docstring) # NOTE: Use docstring("function name") to display R documentation for functions defined in this script
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(foreach)
library(doParallel)

# Load empirical data
load('srfc_data.RData')

# Load functions stored in other scripts
source('operating_model_functions.r')
source('operating_model.r')
source('main_script_functions.r')


# 3. SET MODEL PARAMETERS --------------------------------------------------------------------------------------------------
n.yr  <- 100   # number of years to simulate
n.sim <- 20000 # number of simulations to run 20000
pars  <- c(0.068, 0.215, 0.828, 0.132) # See 'fall_model_fit.r' for calibration process: (1) residual juvenile mortality, (2) CV in recruitment stochasticity, (3) NPGO-dependent mortality coefficient, (4) Variance of NPGO-dependent mortality 


# 4. RUN MAIN MODEL SCENARIOS ----------------------------------------------------------------------------------------------
## 4.1 Base flow models ----------------------------------------------------------------------------------------------------
# 1. maturation = 0.99 (base natural mortality)
# 2. mortality  = 0.01 (base maturation)
# 3. base maturity and mortality
# 4. mortality  = 0.99 (base maturation)
# 5. maturation = 0.25 (base natural mortality)        
mod.01 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.999, 0.95, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'base')
mod.02 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'base')
mod.03 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'base')
mod.04 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'base')
mod.05 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.250, 0.95, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'base')
save(mod.01, mod.02, mod.03, mod.04, mod.05, file = 'age_flow_mod1.RData')

## 4.2 Longer duration drought models --------------------------------------------------------------------------------------
# 6.  maturation = 0.99 (base natural mortality)
# 7.  mortality  = 0.01 (base maturation)
# 8.  base maturity and mortality
# 9.  mortality  = 0.99 (base maturation)
# 10. maturation = 0.25 (base natural mortality)
mod.06 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.999, 0.95, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'longer duration')
mod.07 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'longer duration')
mod.08 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'longer duration')
mod.09 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'longer duration')
mod.10 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.250, 0.95, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'longer duration')
save(mod.06, mod.07, mod.08, mod.09, mod.10, file = 'age_flow_mod2.RData')

## 4.3 More frequent drought models ----------------------------------------------------------------------------------------
# 11. maturation = 0.99 (base natural mortality)
# 12. mortality  = 0.01 (base maturation)
# 13. base maturity and mortality
# 14. mortality  = 0.99 (base maturation)
# 15. maturation = 0.25 (base natural mortality)
mod.11 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.999, 0.95, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more frequent')
mod.12 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more frequent')
mod.13 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more frequent')
mod.14 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more frequent')
mod.15 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.250, 0.95, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more frequent')
save(mod.11, mod.12, mod.13, mod.14, mod.15, file = 'age_flow_mod3.RData')

## 4.4 More intense drought models -----------------------------------------------------------------------------------------
# 16. maturation = 0.99 (base natural mortality)
# 17. mortality  = 0.01 (base maturation)
# 18. base maturity and mortality
# 19. mortality  = 0.99 (base maturation)
# 20. maturation = 0.25 (base natural mortality)
mod.16 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.999, 0.95, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more intense')
mod.17 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more intense')
mod.18 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more intense')
mod.19 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more intense')
mod.20 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.250, 0.95, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more intense')
save(mod.16, mod.17, mod.18, mod.19, mod.20, file = 'age_flow_mod4.RData')


# 5. SUMMARIZE MAIN MODEL DATA ---------------------------------------------------------------------------------------------
load('age_flow_mod1.RData')
mod01.df <- model.summary(mod.01)
mod02.df <- model.summary(mod.02)
mod03.df <- model.summary(mod.03)
mod04.df <- model.summary(mod.04)
mod05.df <- model.summary(mod.05)
mod01.overfished <- calc_overfished(mod.01, n.sim = n.sim, n.yr = n.yr)
mod02.overfished <- calc_overfished(mod.02, n.sim = n.sim, n.yr = n.yr)
mod03.overfished <- calc_overfished(mod.03, n.sim = n.sim, n.yr = n.yr)
mod04.overfished <- calc_overfished(mod.04, n.sim = n.sim, n.yr = n.yr)
mod05.overfished <- calc_overfished(mod.05, n.sim = n.sim, n.yr = n.yr)
vio1.df <- violin_df(mod.01, "1") 
vio2.df <- violin_df(mod.02, "2") 
vio3.df <- violin_df(mod.03, "3")
vio4.df <- violin_df(mod.04, "4") 
vio5.df <- violin_df(mod.05, "5")
vio.df <- rbind(vio1.df,vio2.df,vio3.df,vio4.df,vio5.df)
rm(mod.01, mod.02, mod.03, mod.04, mod.05, vio1.df, vio2.df, vio3.df, vio4.df, vio5.df)

load('age_flow_mod2.RData')
mod06.df <- model.summary(mod.06)
mod07.df <- model.summary(mod.07)
mod08.df <- model.summary(mod.08)
mod09.df <- model.summary(mod.09)
mod10.df <- model.summary(mod.10)
mod06.overfished <- calc_overfished(mod.06, n.sim = n.sim, n.yr = n.yr)
mod07.overfished <- calc_overfished(mod.07, n.sim = n.sim, n.yr = n.yr)
mod08.overfished <- calc_overfished(mod.08, n.sim = n.sim, n.yr = n.yr)
mod09.overfished <- calc_overfished(mod.09, n.sim = n.sim, n.yr = n.yr)
mod10.overfished <- calc_overfished(mod.10, n.sim = n.sim, n.yr = n.yr)
vio6.df <- violin_df(mod.06, "6") 
vio7.df <- violin_df(mod.07, "7") 
vio8.df <- violin_df(mod.08, "8")
vio9.df <- violin_df(mod.09, "9") 
vio10.df <- violin_df(mod.10, "10")
vio.df <- rbind(vio.df,vio6.df,vio7.df,vio8.df,vio9.df,vio10.df)
rm(mod.06, mod.07, mod.08, mod.09, mod.10, vio6.df, vio7.df, vio8.df, vio9.df, vio10.df)

load('age_flow_mod3.RData')
mod11.df <- model.summary(mod.11)
mod12.df <- model.summary(mod.12)
mod13.df <- model.summary(mod.13)
mod14.df <- model.summary(mod.14)
mod15.df <- model.summary(mod.15)
mod11.overfished <- calc_overfished(mod.11, n.sim = n.sim, n.yr = n.yr)
mod12.overfished <- calc_overfished(mod.12, n.sim = n.sim, n.yr = n.yr)
mod13.overfished <- calc_overfished(mod.13, n.sim = n.sim, n.yr = n.yr)
mod14.overfished <- calc_overfished(mod.14, n.sim = n.sim, n.yr = n.yr)
mod15.overfished <- calc_overfished(mod.15, n.sim = n.sim, n.yr = n.yr)
vio11.df <- violin_df(mod.11, "11") 
vio12.df <- violin_df(mod.12, "12") 
vio13.df <- violin_df(mod.13, "13")
vio14.df <- violin_df(mod.14, "14") 
vio15.df <- violin_df(mod.15, "15")
vio.df <- rbind(vio.df,vio11.df,vio12.df,vio13.df,vio14.df,vio15.df)
rm(mod.11, mod.12, mod.13, mod.14, mod.15, vio11.df, vio12.df, vio13.df, vio14.df, vio15.df)

load('age_flow_mod4.RData')
mod16.df <- model.summary(mod.16)
mod17.df <- model.summary(mod.17)
mod18.df <- model.summary(mod.18)
mod19.df <- model.summary(mod.19)
mod20.df <- model.summary(mod.20)
mod16.overfished <- calc_overfished(mod.16, n.sim = n.sim, n.yr = n.yr)
mod17.overfished <- calc_overfished(mod.17, n.sim = n.sim, n.yr = n.yr)
mod18.overfished <- calc_overfished(mod.18, n.sim = n.sim, n.yr = n.yr)
mod19.overfished <- calc_overfished(mod.19, n.sim = n.sim, n.yr = n.yr)
mod20.overfished <- calc_overfished(mod.20, n.sim = n.sim, n.yr = n.yr)
vio16.df <- violin_df(mod.16, "16") 
vio17.df <- violin_df(mod.17, "17") 
vio18.df <- violin_df(mod.18, "18")
vio19.df <- violin_df(mod.19, "19") 
vio20.df <- violin_df(mod.20, "20")
vio.df <- rbind(vio.df,vio16.df,vio17.df,vio18.df,vio19.df,vio20.df)
rm(mod.16, mod.17, mod.18, mod.19, mod.20, vio16.df, vio17.df, vio18.df, vio19.df, vio20.df)

# Save summary data for plotting
save(mod01.df, mod02.df, mod03.df, mod04.df, mod05.df, mod06.df, mod07.df, mod08.df, mod09.df, mod10.df,
     mod11.df, mod12.df, mod13.df, mod14.df, mod15.df, mod16.df, mod17.df, mod18.df, mod19.df, mod20.df,
     mod01.overfished, mod02.overfished, mod03.overfished, mod04.overfished, mod05.overfished, 
     mod06.overfished, mod07.overfished, mod08.overfished, mod09.overfished, mod10.overfished,
     mod11.overfished, mod12.overfished, mod13.overfished, mod14.overfished, mod15.overfished, 
     mod16.overfished, mod17.overfished, mod18.overfished, mod19.overfished, mod20.overfished,
     vio.df,
     file = 'age_flow_summary.RData')


# 6. SENSITIVITY ANALYSES --------------------------------------------------------------------------------------------------
# Note: must set model parameters above.
## 6.1 CV of realized harvest rate -----------------------------------------------------------------------------------------
### 6.1.1 Base flow models -------------------------------------------------------------------------------------------------
# 1. maturation = 0.99 (base natural mortality)
# 2. mortality  = 0.01 (base maturation)
# 3. base maturity and mortality
# 4. mortality  = 0.99 (base maturation)
# 5. maturation = 0.25 (base natural mortality)
cver.sa.01 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')
cver.sa.02 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'base')
cver.sa.03 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')
cver.sa.04 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'base')
cver.sa.05 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')

### 6.1.2 Longer duration drought models -----------------------------------------------------------------------------------
# 6.  maturation = 0.99 (base natural mortality)
# 7.  mortality  = 0.01 (base maturation)
# 8.  base maturity and mortality
# 9.  mortality  = 0.99 (base maturation)
# 10. maturation = 0.25 (base natural mortality)
cver.sa.06 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')
cver.sa.07 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'longer duration')
cver.sa.08 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')
cver.sa.09 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'longer duration')
cver.sa.10 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')

### 6.1.3 More frequent drought models -------------------------------------------------------------------------------------
# 11. maturation = 0.99 (base natural mortality)
# 12. mortality  = 0.01 (base maturation)
# 13. base maturity and mortality
# 14. mortality  = 0.99 (base maturation)
# 15. maturation = 0.25 (base natural mortality)
cver.sa.11 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')
cver.sa.12 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more frequent')
cver.sa.13 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')
cver.sa.14 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more frequent')
cver.sa.15 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')

### 6.1.4 More intense drought models --------------------------------------------------------------------------------------
# 16. maturation = 0.99 (base natural mortality)
# 17. mortality  = 0.01 (base maturation)
# 18. base maturity and mortality
# 19. mortality  = 0.99 (base maturation)
# 20. maturation = 0.25 (base natural mortality)
cver.sa.16 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')
cver.sa.17 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more intense')
cver.sa.18 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')
cver.sa.19 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more intense')
cver.sa.20 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')

### 6.1.5 Save data for sensitivity to the CV of realized harvest rate -----------------------------------------------------
save(cver.sa.01, cver.sa.02, cver.sa.03, cver.sa.04, cver.sa.05,
     cver.sa.06, cver.sa.07, cver.sa.08, cver.sa.09, cver.sa.10,
     cver.sa.11, cver.sa.12, cver.sa.13, cver.sa.14, cver.sa.15,
     cver.sa.16, cver.sa.17, cver.sa.18, cver.sa.19, cver.sa.20,
     file = 'cv_er_sa.RData')


## 6.2 CV of recruitment stochasticity -------------------------------------------------------------------------------------
### 6.2.1 Base flow models -------------------------------------------------------------------------------------------------
# 1. maturation = 0.99 (base natural mortality)
# 2. mortality  = 0.01 (base maturation)
# 3. base maturity and mortality
# 4. mortality  = 0.99 (base maturation)
# 5. maturation = 0.25 (base natural mortality)
cv.j.01 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')
cv.j.02 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'base')
cv.j.03 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')
cv.j.04 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'base')
cv.j.05 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')

### 6.2.2 Longer duration drought models -----------------------------------------------------------------------------------
# 6.  maturation = 0.99 (base natural mortality)
# 7.  mortality  = 0.01 (base maturation)
# 8.  base maturity and mortality
# 9.  mortality  = 0.99 (base maturation)
# 10. maturation = 0.25 (base natural mortality)
cv.j.06 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')
cv.j.07 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'longer duration')
cv.j.08 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')
cv.j.09 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'longer duration')
cv.j.10 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'longer duration')

### 6.2.3 Longer duration drought models -----------------------------------------------------------------------------------
# 11. maturation = 0.99 (base natural mortality)
# 12. mortality  = 0.01 (base maturation)
# 13. base maturity and mortality
# 14. mortality  = 0.99 (base maturation)
# 15. maturation = 0.25 (base natural mortality)
cv.j.11 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')
cv.j.12 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more frequent')
cv.j.13 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')
cv.j.14 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more frequent')
cv.j.15 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more frequent')

### 6.2.4 Longer duration drought models -----------------------------------------------------------------------------------
# 16. maturation = 0.99 (base natural mortality)
# 17. mortality  = 0.01 (base maturation)
# 18. base maturity and mortality
# 19. mortality  = 0.99 (base maturation)
# 20. maturation = 0.25 (base natural mortality)
cv.j.16 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')
cv.j.17 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more intense')
cv.j.18 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')
cv.j.19 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.500, 0.999, 1), n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more intense')
cv.j.20 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.038, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'more intense')

### 6.2.5 Save data for sensitivity to the CV of recruitment stochasticity -------------------------------------------------
save(cv.j.01, cv.j.02, cv.j.03, cv.j.04, cv.j.05,
     cv.j.06, cv.j.07, cv.j.08, cv.j.09, cv.j.10,
     cv.j.11, cv.j.12, cv.j.13, cv.j.14, cv.j.15,
     cv.j.16, cv.j.17, cv.j.18, cv.j.19, cv.j.20,
     file = 'cv_j_sa.RData')


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
base.mod    <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1), n.surv = c(0.5, 0.8, 0.8, 0.8), scenario = 'base')
base.mod.df <- model.summary(base.mod)

base.mod %>% filter(year > 29 & year < 100) %>% summarise(NH.ratio = mean(NH.ratio))

# plots
# sim.nums    <- paste0('s', sample(1:n.sim, 100, replace=FALSE))
sim.nums <- n.sim
base.mod1   <- base.mod #%>% filter(sim %in% sim.nums)
# base.mod2   <- base.mod1 %>% filter(sim %in% sample(sim.nums, 1))
hundo.spawn <- ggplot() +
  geom_line(data = base.mod1, aes(x = year, y = Spawn.est, group = sim), color = 'gray70', alpha = 0.3) +
  # geom_line(data = base.mod2, aes(x = year, y = Spawn.est), color = 'black') +
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
  # geom_line(data = base.mod2, aes(x = year, y = harvest), color = 'black') +
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


## HARVEST PLOTS

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




tau.overfished.plot <- ggplot(data = tau.overfished.df) +
  geom_point(aes(x = age_struct, y = prop.of, color = climate_scenario), size = 3) +
  # geom_errorbar(aes(x = age_struct, ymin = pilo.of, ymax = piup.of, color = climate_scenario), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '', title = 'Maturation') +
  scale_y_continuous(limits = c(0,25), expand = c(0,0)) +
  annotate('text', x = 1.1, y = 23, label = '[Early maturation]', size = 4) +
  annotate('text', x = 2.9, y = 23, label = '[Delayed maturation]', size = 4) +
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
  annotate('text', x = 1.1, y = 23, label = '[High mortality]', size = 4) +
  annotate('text', x = 2.9, y = 23, label = '[Low mortality]', size = 4) +
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.7), text = element_text(size = 13),
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
  annotate('text', x = 1.1, y = 88, label = '[Early maturation]', size = 4) +
  annotate('text', x = 2.9, y = 88, label = '[Delayed maturation]', size = 4) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), axis.text.x = element_blank(),
        panel.background = element_rect(fill = 'gray85', color = 'gray85'), plot.background = element_rect(fill = 'gray85', color = 'gray85'),
        plot.title = element_text(hjust = 0.5)) 

tau.prop.25.plot <- ggplot(data = tau.overfished.df) +
  geom_point(aes(x = age_struct, y = prop.25, color = climate_scenario), size = 3) +
  # geom_errorbar(aes(x = age_struct, ymin = pilo.25, ymax = piup.25, color = climate_scenario), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '') +
  scale_y_continuous(expand = c(0,0), limits=c(0,5)) +
  scale_x_continuous(breaks = seq(1,3)) +
  theme(legend.title = element_blank(), legend.position = c(0.8,0.9), text = element_text(size = 13), axis.text.x = element_blank(), legend.background = element_rect(fill = 'gray85', color = 'gray85'),
        panel.background = element_rect(fill = 'gray85', color = 'gray85'), plot.background = element_rect(fill = 'gray85', color = 'gray85'))

tau.prop.10.plot <- ggplot(data = tau.overfished.df) +
  geom_point(aes(x = age_struct, y = prop.10, color = climate_scenario), size = 3) +
  # geom_errorbar(aes(x = age_struct, ymin = pilo.10, ymax = piup.10, color = climate_scenario), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '') +
  scale_y_continuous(expand = c(0,0), limits = c(0, 2.5)) +
  # scale_x_continuous(breaks = seq(1,3), labels = c(expression(tau[3]~"= 0.99"), 'Base case', expression(tau[3]~"= 0.25"))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), 
        panel.background = element_rect(fill = 'gray85', color = 'gray85'), plot.background = element_rect(fill = 'gray85', color = 'gray85'))

tau.prop.plot <- ggarrange(tau.prop.70.plot, tau.prop.25.plot, tau.prop.10.plot, nrow=3, labels = c('b','d','f'))

eta.prop.70.plot <- ggplot(data = eta.overfished.df) +
  geom_point(aes(x = age_struct, y = prop.70, color = climate_scenario), size = 3) +
  # geom_errorbar(aes(x = age_struct, ymin = pilo.70, ymax = piup.70, color = climate_scenario), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = expression(paste('% ', italic('c'), ' = 0.7')), title = 'Natural mortality') +
  scale_y_continuous(expand = c(0,0), limits=c(60,90)) +
  scale_x_continuous(breaks = seq(1,3)) +
  annotate('text', x = 1.1, y = 88, label = '[High mortality]', size = 4) +
  annotate('text', x = 2.9, y = 88, label = '[Low maturation]', size = 4) +
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
  scale_y_continuous(expand = c(0,0), limits = c(0, 2.5)) +
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









