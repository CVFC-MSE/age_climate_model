## -------------------------------------------------------------------------------------------------------------------------
## Author: Paul Carvalho (paul.carvalho@noaa.gov, pcarvalh@ucsc.edu)
##
## Description: Supporting functions used in 'main_script.R'
## -------------------------------------------------------------------------------------------------------------------------

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

violin_df <- function(x, scenario){
  out <- x %>% 
    dplyr::filter(year >= 30) %>% 
    dplyr::select(year, Spawn.est, harvest, sim) %>%
    group_by(sim) %>%
    summarise(spawn = mean(Spawn.est, na.rm=TRUE),
              harvest = mean(harvest, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(scenario = scenario) %>%
    select(spawn, harvest, scenario)
}
