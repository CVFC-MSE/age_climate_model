#### 
## main_script_functions.R
##
## Author: Paul Carvalho (paul.carvalho@noaa.gov, pcarvalh@ucsc.edu)
## Last update: 10/11/2022
##
## Description: Functions used in the in the main script of the simulation models
####

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
    # prop.under.MSST <- sum(tmp.mod$Spawn.est[70:n.yr-1]<91500) / length(c(70:n.yr-1))
    
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

violin_df <- function(x, scenario){
  out <- x %>% 
    dplyr::filter(year >= 30) %>% 
    dplyr::select(year, Spawn.est, harvest, sim) %>%
    group_by(sim) %>%
    summarise(mean.spawn = mean(Spawn.est, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(scenario = scenario) %>%
    select(mean.spawn, scenario)
}
