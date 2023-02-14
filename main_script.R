## -------------------------------------------------------------------------------------------------------------------------
## Paper: Carvalho, PG, Satterthwaite, WH, O'Farrell, MR, Speir, C, and Palkovacs, EP. Role of maturation and mortality in portfolio
##        effects and climate resilience.
##
## Contact: Paul Carvalho (paul.carvalho@noaa.gov, pcarvalh@ucsc.edu)
##
## Description: Run simulation models to test the effects of drought on Sacramento River fall run Chinook salmon under
##              different age structure scenarios. Two mechanisms that affect age structure were tested separately - 
##              natural mortality and maturation.
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
n.sim <- 500 # number of simulations to run 20000
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
mod01.df <- model_summary(mod.01)
mod02.df <- model_summary(mod.02)
mod03.df <- model_summary(mod.03)
mod04.df <- model_summary(mod.04)
mod05.df <- model_summary(mod.05)
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
mod06.df <- model_summary(mod.06)
mod07.df <- model_summary(mod.07)
mod08.df <- model_summary(mod.08)
mod09.df <- model_summary(mod.09)
mod10.df <- model_summary(mod.10)
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
mod11.df <- model_summary(mod.11)
mod12.df <- model_summary(mod.12)
mod13.df <- model_summary(mod.13)
mod14.df <- model_summary(mod.14)
mod15.df <- model_summary(mod.15)
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
mod16.df <- model_summary(mod.16)
mod17.df <- model_summary(mod.17)
mod18.df <- model_summary(mod.18)
mod19.df <- model_summary(mod.19)
mod20.df <- model_summary(mod.20)
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
cver.sa.01 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.999, 0.95, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'base')
cver.sa.02 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'base')
cver.sa.03 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'base')
cver.sa.04 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'base')
cver.sa.05 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.250, 0.95, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'base')

### 6.1.2 Longer duration drought models -----------------------------------------------------------------------------------
# 6.  maturation = 0.99 (base natural mortality)
# 7.  mortality  = 0.01 (base maturation)
# 8.  base maturity and mortality
# 9.  mortality  = 0.99 (base maturation)
# 10. maturation = 0.25 (base natural mortality)
cver.sa.06 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.999, 0.95, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'longer duration')
cver.sa.07 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'longer duration')
cver.sa.08 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'longer duration')
cver.sa.09 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.95, 1),  n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'longer duration')
cver.sa.10 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.250, 0.95, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'longer duration')

### 6.1.3 More frequent drought models -------------------------------------------------------------------------------------
# 11. maturation = 0.99 (base natural mortality)
# 12. mortality  = 0.01 (base maturation)
# 13. base maturity and mortality
# 14. mortality  = 0.99 (base maturation)
# 15. maturation = 0.25 (base natural mortality)
cver.sa.11 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more frequent')
cver.sa.12 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more frequent')
cver.sa.13 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more frequent')
cver.sa.14 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more frequent')
cver.sa.15 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more frequent')

### 6.1.4 More intense drought models --------------------------------------------------------------------------------------
# 16. maturation = 0.99 (base natural mortality)
# 17. mortality  = 0.01 (base maturation)
# 18. base maturity and mortality
# 19. mortality  = 0.99 (base maturation)
# 20. maturation = 0.25 (base natural mortality)
cver.sa.16 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more intense')
cver.sa.17 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more intense')
cver.sa.18 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more intense')
cver.sa.19 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more intense')
cver.sa.20 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more intense')

### 6.1.5 Save data for sensitivity to the CV of realized harvest rate -----------------------------------------------------
save(cver.sa.01, cver.sa.02, cver.sa.03, cver.sa.04, cver.sa.05,
     cver.sa.06, cver.sa.07, cver.sa.08, cver.sa.09, cver.sa.10,
     cver.sa.11, cver.sa.12, cver.sa.13, cver.sa.14, cver.sa.15,
     cver.sa.16, cver.sa.17, cver.sa.18, cver.sa.19, cver.sa.20,
     file = 'cv_er_sa.RData')
rm(cver.sa.01, cver.sa.02, cver.sa.03, cver.sa.04, cver.sa.05,
   cver.sa.06, cver.sa.07, cver.sa.08, cver.sa.09, cver.sa.10,
   cver.sa.11, cver.sa.12, cver.sa.13, cver.sa.14, cver.sa.15,
   cver.sa.16, cver.sa.17, cver.sa.18, cver.sa.19, cver.sa.20)

## 6.2 CV of recruitment stochasticity -------------------------------------------------------------------------------------
### 6.2.1 Base flow models -------------------------------------------------------------------------------------------------
# 1. maturation = 0.99 (base natural mortality)
# 2. mortality  = 0.01 (base maturation)
# 3. base maturity and mortality
# 4. mortality  = 0.99 (base maturation)
# 5. maturation = 0.25 (base natural mortality)
cv.j.01 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'base')
cv.j.02 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'base')
cv.j.03 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'base')
cv.j.04 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'base')
cv.j.05 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'base')

### 6.2.2 Longer duration drought models -----------------------------------------------------------------------------------
# 6.  maturation = 0.99 (base natural mortality)
# 7.  mortality  = 0.01 (base maturation)
# 8.  base maturity and mortality
# 9.  mortality  = 0.99 (base maturation)
# 10. maturation = 0.25 (base natural mortality)
cv.j.06 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'longer duration')
cv.j.07 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'longer duration')
cv.j.08 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'longer duration')
cv.j.09 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'longer duration')
cv.j.10 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'longer duration')

### 6.2.3 Longer duration drought models -----------------------------------------------------------------------------------
# 11. maturation = 0.99 (base natural mortality)
# 12. mortality  = 0.01 (base maturation)
# 13. base maturity and mortality
# 14. mortality  = 0.99 (base maturation)
# 15. maturation = 0.25 (base natural mortality)
cv.j.11 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more frequent')
cv.j.12 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more frequent')
cv.j.13 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more frequent')
cv.j.14 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more frequent')
cv.j.15 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more frequent')

### 6.2.4 Longer duration drought models -----------------------------------------------------------------------------------
# 16. maturation = 0.99 (base natural mortality)
# 17. mortality  = 0.01 (base maturation)
# 18. base maturity and mortality
# 19. mortality  = 0.99 (base maturation)
# 20. maturation = 0.25 (base natural mortality)
cv.j.16 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more intense')
cv.j.17 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more intense')
cv.j.18 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more intense')
cv.j.19 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more intense')
cv.j.20 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more intense')

### 6.2.5 Save data for sensitivity to the CV of recruitment stochasticity -------------------------------------------------
save(cv.j.01, cv.j.02, cv.j.03, cv.j.04, cv.j.05,
     cv.j.06, cv.j.07, cv.j.08, cv.j.09, cv.j.10,
     cv.j.11, cv.j.12, cv.j.13, cv.j.14, cv.j.15,
     cv.j.16, cv.j.17, cv.j.18, cv.j.19, cv.j.20,
     file = 'cv_j_sa.RData')
rm(cv.j.01, cv.j.02, cv.j.03, cv.j.04, cv.j.05,
   cv.j.06, cv.j.07, cv.j.08, cv.j.09, cv.j.10,
   cv.j.11, cv.j.12, cv.j.13, cv.j.14, cv.j.15,
   cv.j.16, cv.j.17, cv.j.18, cv.j.19, cv.j.20)

## 6.3 mean NPGO effect ----------------------------------------------------------------------------------------------------
### 6.3.1 Base flow models -------------------------------------------------------------------------------------------------
# 1. maturation = 0.99 (base natural mortality)
# 2. mortality  = 0.01 (base maturation)
# 3. base maturity and mortality
# 4. mortality  = 0.99 (base maturation)
# 5. maturation = 0.25 (base natural mortality)
npgo.01 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'base')
npgo.02 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'base')
npgo.03 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'base')
npgo.04 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'base')
npgo.05 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'base')

### 6.3.2 Longer duration drought models -----------------------------------------------------------------------------------
# 6.  maturation = 0.99 (base natural mortality)
# 7.  mortality  = 0.01 (base maturation)
# 8.  base maturity and mortality
# 9.  mortality  = 0.99 (base maturation)
# 10. maturation = 0.25 (base natural mortality)
npgo.06 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'longer duration')
npgo.07 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'longer duration')
npgo.08 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'longer duration')
npgo.09 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'longer duration')
npgo.10 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'longer duration')

### 6.3.3 More frequent drought models -------------------------------------------------------------------------------------
# 11. maturation = 0.99 (base natural mortality)
# 12. mortality  = 0.01 (base maturation)
# 13. base maturity and mortality
# 14. mortality  = 0.99 (base maturation)
# 15. maturation = 0.25 (base natural mortality)
npgo.11 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more frequent')
npgo.12 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more frequent')
npgo.13 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more frequent')
npgo.14 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more frequent')
npgo.15 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more frequent')

### 6.3.4 More intense drought models --------------------------------------------------------------------------------------
# 16. maturation = 0.99 (base natural mortality)
# 17. mortality  = 0.01 (base maturation)
# 18. base maturity and mortality
# 19. mortality  = 0.99 (base maturation)
# 20. maturation = 0.25 (base natural mortality)
npgo.16 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.999, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more intense')
npgo.17 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.01, 0.01), scenario = 'more intense')
npgo.18 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more intense')
npgo.19 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.55, 0.999, 1),  n.surv = c(0.5, 0.8, 0.99, 0.99), scenario = 'more intense')
npgo.20 <- operating.model(pars = pars, years = n.yr, sims = n.sim, m.maturity = c(0.035, 0.250, 0.999, 1), n.surv = c(0.5, 0.8, 0.8, 0.8),   scenario = 'more intense')

### 6.3.5 Save data for sensitivity to mean NPGO effect --------------------------------------------------------------------
save(npgo.01, npgo.02, npgo.03, npgo.04, npgo.05,
     npgo.06, npgo.07, npgo.08, npgo.09, npgo.10,
     npgo.11, npgo.12, npgo.13, npgo.14, npgo.15,
     npgo.16, npgo.17, npgo.18, npgo.19, npgo.20,
     file = 'npgo_sa.RData')
rm(npgo.01, npgo.02, npgo.03, npgo.04, npgo.05,
   npgo.06, npgo.07, npgo.08, npgo.09, npgo.10,
   npgo.11, npgo.12, npgo.13, npgo.14, npgo.15,
   npgo.16, npgo.17, npgo.18, npgo.19, npgo.20)
