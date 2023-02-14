# Role of maturation and mortality in portfolio effects and climate resilience

This repository accompanies the paper: Carvalho, PG, Satterthwaite, WH, O'Farrell, MR, and Palkovacs, EP. 2023. Role of maturation and mortality in portfolio effects and climate resilience. _Canadian Journal of Fisheries and Aquatic Sciences_. [10.1139/cjfas-2022-0171](https://cdnsciencepub.com/doi/abs/10.1139/cjfas-2022-0171).

## Abstract
The portfolio effect plays a critical role in population productivity and stability. Age structure of spawning salmon represents an example of portfolio effects such that the risks of experiencing unfavorable conditions are spread across time. However, the distribution of maturation ages for Pacific salmon (_Oncorhynchus_ spp.) is increasingly concentrated into fewer and younger ages, which may impact population resilience to climate change. We explored the population dynamics of Sacramento River fall-run Chinook salmon (_O. tshawytscha_) under different age structure scenarios using a life-cycle model and compared two mechanisms that can underlie these changes – mortality and maturation. In addition, we tested whether age structure promotes resilience to drought. We found that high age structure diversity increased the stability of population size and harvest compared with low diversity. However, mean population size responded differently depending on the underlying mechanism. Reduced mortality of adult fish ages 4-5 increased escapement whereas delayed maturation decreased escapement. Overall, high age structure diversity was able to buffer against the adverse effects of droughts by reducing the variability of population size and harvest compared with low diversity. Results suggest that age structure promotes stability of salmon in an increasingly variable climate.

## Usage Notes
###  A. model_fitting.R
This file contains code to fit a population dynamics model to Sacramento River fall-run Chinook salmon (_Oncorhynchus tshawytscha_) escapement and harvest. The parameters estimated through model fitting are juvenile survival (not flow related), the coefficient of variation of recruitment stochasticity, mean NPGO (North Pacific Gyre Oscillation Index) effect on survival, and variance of NPGO effect on survival.

Supporting files:  
_model_fitting_functions.R_ contains code for the stock-recruitment relationship, fishery impact, flow-dependent juvenile survival, and smart rounding numbers while preserving overall sum.

### B. main_script.R
Run simulation models to test the effects of drought on Sacramento River fall-run Chinook salmon under different age structure diversity scenarios. The two mechanisms that affect age sctructure (mortality and maturation) were tested separately. For each scenario (full factorial design of drought and age structure diversity), 20,000 stochastic simulations were conducted. Simulated data were saved in .Rdata files.

Supporting files:
_operating_model.R_ contains a function for an age-structure, life-cycle model for Sacramento River fall-run Chinook salmon. The model inputs include age-specific maturation and natural mortality rates, which were used to modify age structure diversity. In addition, the 'scenario' input specifies drought scenarios to model (contemporary, longer duration, more frequent and more intense).
