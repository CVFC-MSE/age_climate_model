# Main figures

rm(list = ls())

# Load libraries and data -------------------------------------------------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(ggpubr)

load('age_flow_summary.RData')

# Organize data -----------------------------------------------------------------------------------------------------------------
# Organize data for alternative maturation rate scenarios
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
age.scen.df1 <- data.frame(scenario = as.character(c(1,3,5,6,8,10,11,13,15,16,18,20)),
                           age_scen = as.character(c(0.7,1.7,2.7,0.9,1.9,2.9,1.1,2.1,3.1,1.3,2.3,3.3)))
tau.vio.df <- vio.df %>%
              filter(scenario %in% as.character(c(1,3,5,6,8,10,11,13,15,16,18,20))) %>%
              mutate(climate = ifelse(scenario %in% as.character(c(1,3,5)), 'Contemporary',
                               ifelse(scenario %in% as.character(c(6,8,10)), 'Longer duration',
                               ifelse(scenario %in% as.character(c(11,13,15)), 'More frequent',
                               'More intense')))) %>%
              left_join(., age.scen.df1, by = "scenario")
tau.cv.df <- data.frame(climate_scenario = rep(c('Contemporary','Duration','Frequency','Intensity'), each=3),
                        age_struct = c(seq(0.7,2.7,by=1),seq(0.9,2.9,by=1),seq(1.1,3.1,by=1),seq(1.3,3.3,by=1)),
                        spawn_cv = c(mod01.df$spawn.cv, mod03.df$spawn.cv, mod05.df$spawn.cv, mod06.df$spawn.cv, mod08.df$spawn.cv, mod10.df$spawn.cv, mod11.df$spawn.cv, mod13.df$spawn.cv, mod15.df$spawn.cv, mod16.df$spawn.cv, mod18.df$spawn.cv, mod20.df$spawn.cv),
                        harvest_cv = c(mod01.df$harvest.cv, mod03.df$harvest.cv, mod05.df$harvest.cv, mod06.df$harvest.cv, mod08.df$harvest.cv, mod10.df$harvest.cv, mod11.df$harvest.cv, mod13.df$harvest.cv, mod15.df$harvest.cv, mod16.df$harvest.cv, mod18.df$harvest.cv, mod20.df$harvest.cv))


# Organize data for alternative natural mortality rate scenarios
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
age.scen.df2 <- data.frame(scenario = as.character(c(2,3,4,7,8,9,12,13,14,17,18,19)),
                           age_scen = as.character(c(0.7,1.7,2.7,0.9,1.9,2.9,1.1,2.1,3.1,1.3,2.3,3.3)))
eta.vio.df <- vio.df %>%
              filter(scenario %in% as.character(c(2,3,4,7,8,9,12,13,14,17,18,19))) %>%
              mutate(climate = ifelse(scenario %in% as.character(c(2,3,4)), 'Contemporary',
                               ifelse(scenario %in% as.character(c(7,8,9)), 'Longer duration',
                               ifelse(scenario %in% as.character(c(12,13,14)), 'More frequent',
                               'More intense')))) %>%
              left_join(., age.scen.df2, by = "scenario")
eta.cv.df <- data.frame(climate_scenario = rep(c('Contemporary','Duration','Frequency','Intensity'), each=3),
                        age_struct = c(seq(0.7,2.7,by=1),seq(0.9,2.9,by=1),seq(1.1,3.1,by=1),seq(1.3,3.3,by=1)),
                        spawn_cv = c(mod02.df$spawn.cv, mod03.df$spawn.cv, mod04.df$spawn.cv, mod07.df$spawn.cv, mod08.df$spawn.cv, mod09.df$spawn.cv, mod12.df$spawn.cv, mod13.df$spawn.cv, mod14.df$spawn.cv, mod17.df$spawn.cv, mod18.df$spawn.cv, mod19.df$spawn.cv),
                        harvest_cv = c(mod02.df$harvest.cv, mod03.df$harvest.cv, mod04.df$harvest.cv, mod07.df$harvest.cv, mod08.df$harvest.cv, mod09.df$harvest.cv, mod12.df$harvest.cv, mod13.df$harvest.cv, mod14.df$harvest.cv, mod17.df$harvest.cv, mod18.df$harvest.cv, mod19.df$harvest.cv))


# Figure. Spawner escapement violin plots and CV --------------------------------------------------------------------------------
vio.plot.settings <- theme(legend.title = element_blank(), 
                           legend.position = 'none', 
                           axis.text.x = element_blank(), 
                           axis.ticks.x = element_blank(),
                           plot.title = element_text(hjust = 0.5),
                           text = element_text(size = 16),
                           plot.margin = unit(c(0.5,0,0,0.7),'cm'))
cv.plot.settings <- theme(legend.title = element_blank(), 
                          legend.position = 'none', 
                          text = element_text(size = 16), 
                          plot.margin = unit(c(0.5,0,0,0.7),'cm'))
spawn.tau.vio.plot <- ggplot() +
  geom_violin(data = tau.vio.df, aes(x = age_scen, y = spawn/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey75", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = 'Maturation') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  scale_x_discrete(expand = c(0,0)) +
  annotate('text', x = 2.5, y = 570, label = '[Early maturation]', size = 5) +
  annotate('text', x = 10.5, y = 570, label = '[Delayed maturation]', size = 5) +
  vio.plot.settings

spawn.eta.vio.plot <- ggplot(data = eta.vio.df) +
  geom_violin(aes(x = age_scen, y = spawn/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Spawner escapement (thousands)', title = 'Natural mortality') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  annotate('text', x = 2, y = 570, label = '[High mortality]', size = 5) +
  annotate('text', x = 11, y = 570, label = '[Low mortality]', size = 5) +
  vio.plot.settings

spawnCV.tau.plot <- ggplot(data = tau.cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(expression(tau[3]~"= 0.99"), 'Base case', expression(tau[3]~"= 0.25"))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.65, 0.8)) +
  cv.plot.settings

spawnCV.eta.plot <- ggplot(data = eta.cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of spawner escapement') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.65, 0.8)) +
  cv.plot.settings +
  theme(legend.position = c(0.8, 0.9))

spawn.tau <- ggarrange(spawn.tau.vio.plot, spawnCV.tau.plot, nrow=2, labels = c('b', 'd'))
spawn.eta <- ggarrange(spawn.eta.vio.plot, spawnCV.eta.plot, nrow=2, labels = c('a', 'c'))
spawn.final <- ggarrange(spawn.eta, spawn.tau, ncol=2)


# Figure. Harvest violin plots and CV -------------------------------------------------------------------------------------------
harvest.tau.vio.plot <- ggplot() +
  geom_violin(data = tau.vio.df, aes(x = age_scen, y = harvest/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey75", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = 'Maturation') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 700), breaks = seq(0,700,100)) +
  scale_x_discrete(expand = c(0,0)) +
  annotate('text', x = 2.5, y = 650, label = '[Early maturation]', size = 4) +
  annotate('text', x = 10.5, y = 650, label = '[Delayed maturation]', size = 4) +
  vio.plot.settings

harvest.eta.vio.plot <- ggplot(data = eta.vio.df) +
  geom_violin(aes(x = age_scen, y = harvest/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Harvest (thousands)', title = 'Natural mortality') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 700), breaks = seq(0,700,100)) +
  annotate('text', x = 2, y = 650, label = '[High mortality]', size = 4) +
  annotate('text', x = 11, y = 650, label = '[Low mortality]', size = 4) +
  vio.plot.settings

harvestCV.tau.plot <- ggplot(data = tau.cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.7, 0.825), breaks = seq(0.7,0.825,0.025)) +
  cv.plot.settings

harvestCV.eta.plot <- ggplot(data = eta.cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of harvest') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.7, 0.825), breaks = seq(0.7,0.825,0.025)) +
  cv.plot.settings +
  theme(legend.position = c(0.8, 0.9))

harvest.tau <- ggarrange(harvest.tau.vio.plot, harvestCV.tau.plot, nrow=2, labels = c('b', 'd'))
harvest.eta <- ggarrange(harvest.eta.vio.plot, harvestCV.eta.plot, nrow=2, labels = c('a', 'c'))
harvest.final <- ggarrange(harvest.eta, harvest.tau, ncol=2)


# Figure. Percent overfished status violin --------------------------------------------------------------------------------------

