## -------------------------------------------------------------------------------------------------------------------------
## Author: Paul Carvalho (paul.carvalho@noaa.gov, pcarvalh@ucsc.edu)
##
## Description: Supporting functions used in 'main_script.R'
## -------------------------------------------------------------------------------------------------------------------------

model_summary <- function(x){
  tmp    <- x %>% dplyr::filter(year >= 30) %>% dplyr::select(year, Spawn.est, harvest, sim)
  # tmp    <- x %>% dplyr::filter(year >= 70) %>% dplyr::select(year, Spawn.est, harvest, sim)
  tmp.cv <- tmp %>%
    dplyr::group_by(sim) %>% 
    dplyr::summarise(spawn.cv = sd(Spawn.est, na.rm=TRUE)/mean(Spawn.est, na.rm=TRUE), 
                     harvest.cv = sd(harvest)/mean(harvest),
                     total.run.cv = sd(Spawn.est+harvest,na.rm=TRUE)/mean(Spawn.est+harvest,na.rm=TRUE))
  tmp.CVsim <- tmp %>% 
    dplyr::group_by(sim) %>% 
    summarise(mean = mean(Spawn.est, na.rm=TRUE), mean.h = mean(harvest)) 
  tmp.CVsim.esc <- sd(tmp.CVsim$mean)/mean(tmp.CVsim$mean)
  tmp.CVsim.har <- sd(tmp.CVsim$mean.h)/mean(tmp.CVsim$mean.h)
  tmp.df <- data.frame(spawn.mean = mean(tmp$Spawn.est, na.rm = TRUE),
                       spawn.sd = sd(tmp$Spawn.est, na.rm = TRUE),
                       spawn.median = median(tmp$Spawn.est, na.rm = TRUE),
                       spawn.pi.lo = quantile(tmp$Spawn.est, probs = 0.025, na.rm = TRUE),
                       spawn.pi.up = quantile(tmp$Spawn.est, probs = 0.975, na.rm = TRUE),
                       spawn.cv = mean(tmp.cv$spawn.cv),
                       spawn.cv.lo = quantile(tmp.cv$spawn.cv, probs = 0.025),
                       spawn.cv.up = quantile(tmp.cv$spawn.cv, probs = 0.975),
                       harvest.mean = mean(tmp$harvest),
                       harvest.sd = sd(tmp$harvest),
                       harvest.median = median(tmp$harvest),
                       harvest.pi.lo = quantile(tmp$harvest, probs = 0.025),
                       harvest.pi.up = quantile(tmp$harvest, probs = 0.975),
                       harvest.cv = mean(tmp.cv$harvest.cv),
                       harvest.cv.lo = quantile(tmp.cv$harvest.cv, probs = 0.025),
                       harvest.cv.up = quantile(tmp.cv$harvest.cv, probs = 0.975),
                       total.run.mean = mean(tmp$Spawn.est + tmp$harvest, na.rm = TRUE),
                       total.run.cv = mean(tmp.cv$total.run.cv),
                       cv.esc.sim = tmp.CVsim.esc,
                       cv.har.sim = tmp.CVsim.har)
  row.names(tmp.df) <- NULL
  return(tmp.df)
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
