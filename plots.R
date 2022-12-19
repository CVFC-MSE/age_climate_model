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

### OVERFISHING TAU AND ETA
tau.mods <- c(1,3,5,6,8,10,11,13,15,16,18,20)
tau.overfished.df <- NULL
for(index in 1:12){
  i <- tau.mods[index]
  tmp.name <- paste0('mod',stringr::str_pad(i, 2, pad = '0'),'.overfished')
  tmp.of <- get(tmp.name)
  tmp.of <- tmp.of %>% 
            mutate(scenario = as.character(i)) %>%
            mutate(climate = ifelse(scenario %in% c(1,3,5), 'Contemporary',
                             ifelse(scenario %in% c(6,8,10), 'Longer duration', 
                             ifelse(scenario %in% c(11,13,15), 'More frequent', 
                             'More intense')))) %>%
            left_join(., age.scen.df1, by = 'scenario')
  tau.overfished.df <- rbind(tau.overfished.df, tmp.of)
}       

eta.mods <- c(2,3,4,7,8,9,12,13,14,17,18,19)
eta.overfished.df <- NULL
for(index in 1:12){
  i <- eta.mods[index]
  tmp.name <- paste0('mod',stringr::str_pad(i, 2, pad = '0'),'.overfished')
  tmp.of <- get(tmp.name)
  tmp.of <- tmp.of %>% 
            mutate(scenario = as.character(i)) %>%
            mutate(climate = ifelse(scenario %in% c(2,3,4), 'Contemporary',
                             ifelse(scenario %in% c(7,8,9), 'Longer duration', 
                             ifelse(scenario %in% c(12,13,14), 'More frequent', 
                             'More intense')))) %>%
                   
            left_join(., age.scen.df2, by = 'scenario')
  eta.overfished.df <- rbind(eta.overfished.df, tmp.of)
}     

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


# Figure. Percent overfished status boxplot -------------------------------------------------------------------------------------
of.theme.settings <- theme(legend.position = 'none',
                           legend.title = element_blank(),
                           legend.text = element_text(size = 14),
                           plot.title = element_text(hjust = 0.5, size = 16),
                           axis.ticks.x = element_blank(),
                           axis.text.x = element_text(hjust = 0, size = 16),
                           axis.text.y = element_text(size = 16),
                           axis.title.y = element_text(size = 16),
                           axis.title.x = element_text(size = 16),
                           plot.margin = unit(c(0.7,0.5,0,0.5), "cm"))

of.tau.plot <- ggplot() +
  geom_boxplot(data = tau.overfished.df, aes(x = age_scen, y = prop.overfished*100, fill = climate), outlier.shape = NA) +
  scale_fill_manual(values = c("grey75", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '', title = 'Maturation') +
  scale_x_discrete(breaks = c('0.9','1.9','2.9'), labels = c("Low","Base","High")) +
  annotate('segment', x = 2.5, xend = 2.5, y = -0.25, yend = 0) +
  annotate('segment', x = 6.5, xend = 6.5, y = -0.25, yend = 0) +
  annotate('segment', x = 10.5, xend = 10.5, y = -0.25, yend = 0) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0,40), clip = "off") +
  annotate('text', x = 2.5, y = 39, label = '[Early maturation]', size = 5) +
  annotate('text', x = 10.5, y = 39, label = '[Delayed maturation]', size = 5) +
  of.theme.settings

of.eta.plot <- ggplot() +
  geom_boxplot (data = eta.overfished.df, aes(x = age_scen, y = prop.overfished*100, fill = climate), outlier.shape = NA) +
  scale_fill_manual(values = c("grey75", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '% overfished status', title = 'Natural mortality') +
  scale_x_discrete(breaks = c('0.9','1.9','2.9'), labels = c("Low","Base","High")) +
  annotate('segment', x = 2.5, xend = 2.5, y = -0.25, yend = 0) +
  annotate('segment', x = 6.5, xend = 6.5, y = -0.25, yend = 0) +
  annotate('segment', x = 10.5, xend = 10.5, y = -0.25, yend = 0) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0,40), clip = "off") +
  annotate('text', x = 2, y = 39, label = '[High mortality]', size = 5) +
  annotate('text', x = 11, y = 39, label = '[Low mortality]', size = 5) +
  of.theme.settings +
  theme(legend.position = c(0.85,0.75))

ggarrange(of.eta.plot, of.tau.plot, labels = c('a','b'))


# Figure. Percent fishery restrictions ------------------------------------------------------------------------------------------
c.plot.settings <- theme(legend.position = 'none',
                         plot.title = element_text(hjust = 0.5, size = 15),
                         axis.ticks.x = element_blank(),
                         axis.text.x = element_blank(),
                         axis.title = element(size = 15),
                         text = element_text(size = 15),
                         plot.margin = unit(c(0.5,0.2,0,0.5), "cm"))

prop70.tau.plot <- ggplot() +
  geom_boxplot(data = tau.overfished.df, aes(x = age_scen, y = prop.70*100, fill = climate), outlier.shape = NA) +
  scale_fill_manual(values = c("grey75", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = 'Maturation') +
  scale_x_discrete(breaks = c('0.9','1.9','2.9'), labels = c("Low","Base","High")) +
  annotate('segment', x = 2.5, xend = 2.5, y = 49, yend = 50) +
  annotate('segment', x = 6.5, xend = 6.5, y = 49, yend = 50) +
  annotate('segment', x = 10.5, xend = 10.5, y = 49, yend = 50) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(50,105), clip = "off") +
  annotate('text', x = 2.5, y = 100, label = '[Early maturation]', size = 4) +
  annotate('text', x = 10.5, y = 100, label = '[Delayed maturation]', size = 4) +
  c.plot.settings

prop70.eta.plot <- ggplot() +
  geom_boxplot(data = eta.overfished.df, aes(x = age_scen, y = prop.70*100, fill = climate), outlier.shape = NA) +
  scale_fill_manual(values = c("grey75", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = expression(paste("% ",italic(c)," = 0.7")), title = 'Natural mortality') +
  scale_x_discrete(breaks = c('0.9','1.9','2.9'), labels = c("Low","Base","High")) +
  annotate('segment', x = 2.5, xend = 2.5, y = 49, yend = 50) +
  annotate('segment', x = 6.5, xend = 6.5, y = 49, yend = 50) +
  annotate('segment', x = 10.5, xend = 10.5, y = 49, yend = 50) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(50,105), clip = "off") +
  annotate('text', x = 2.5, y = 100, label = '[High mortality]', size = 4) +
  annotate('text', x = 10.5, y = 100, label = '[Low mortality]', size = 4) +
  c.plot.settings

prop25.tau.plot <- ggplot() +
  geom_boxplot(data = tau.overfished.df, aes(x = age_scen, y = prop.25*100, fill = climate), outlier.shape = NA) +
  scale_fill_manual(values = c("grey75", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = '') +
  scale_x_discrete(breaks = c('0.9','1.9','2.9'), labels = c("Low","Base","High")) +
  annotate('segment', x = 2.5, xend = 2.5, y = -0.25, yend = 0) +
  annotate('segment', x = 6.5, xend = 6.5, y = -0.25, yend = 0) +
  annotate('segment', x = 10.5, xend = 10.5, y = -0.25, yend = 0) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,15), clip = "off") +
  c.plot.settings

prop25.eta.plot <- ggplot() +
  geom_boxplot(data = eta.overfished.df, aes(x = age_scen, y = prop.25*100, fill = climate), outlier.shape = NA) +
  scale_fill_manual(values = c("grey75", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = expression(paste("% ",italic(c)," = 0.25")), title = '') +
  scale_x_discrete(breaks = c('0.9','1.9','2.9'), labels = c("Low","Base","High")) +
  annotate('segment', x = 2.5, xend = 2.5, y = -0.25, yend = 0) +
  annotate('segment', x = 6.5, xend = 6.5, y = -0.25, yend = 0) +
  annotate('segment', x = 10.5, xend = 10.5, y = -0.25, yend = 0) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,15), clip = "off") +
  c.plot.settings +
  theme(legend.position = c(0.8,0.9),
        legend.title = element_blank())

prop10.tau.plot <- ggplot() +
  geom_boxplot(data = tau.overfished.df, aes(x = age_scen, y = prop.10*100, fill = climate), outlier.shape = NA) +
  scale_fill_manual(values = c("grey75", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '', title = '') +
  scale_x_discrete(breaks = c('0.9','1.9','2.9'), labels = c("Low","Base","High")) +
  annotate('segment', x = 2.5, xend = 2.5, y = -0.12, yend = 0) +
  annotate('segment', x = 6.5, xend = 6.5, y = -0.12, yend = 0) +
  annotate('segment', x = 10.5, xend = 10.5, y = -0.12, yend = 0) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,10), clip = "off") +
  c.plot.settings +
  theme(axis.text.x = element_text(size = 15, hjust = 0))

prop10.eta.plot <- ggplot() +
  geom_boxplot(data = eta.overfished.df, aes(x = age_scen, y = prop.10*100, fill = climate), outlier.shape = NA) +
  scale_fill_manual(values = c("grey75", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = expression(paste("% ",italic(c)," = 0.10")), title = '') +
  scale_x_discrete(breaks = c('0.9','1.9','2.9'), labels = c("Low","Base","High")) +
  annotate('segment', x = 2.5, xend = 2.5, y = -0.12, yend = 0) +
  annotate('segment', x = 6.5, xend = 6.5, y = -0.12, yend = 0) +
  annotate('segment', x = 10.5, xend = 10.5, y = -0.12, yend = 0) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,10), clip = "off") +
  c.plot.settings +
  theme(axis.text.x = element_text(size = 15, hjust = 0))
  

ggarrange(prop70.eta.plot,prop70.tau.plot,
          prop25.eta.plot,prop25.tau.plot,
          prop10.eta.plot,prop10.tau.plot,
          nrow = 3, ncol = 2, labels = c('a','b','c','d','e','f'))


# total escapement plots ----------
## SPAWN PLOTS
total.run.tau.plot <- ggplot(data = tau.df) +
  geom_point(aes(x = age_scen, y = total.run.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = spawn.pi.lo/1000, ymax = spawn.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = 'Maturation') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 875)) +
  annotate('text', x = 1.1, y = 840, label = '[Early maturation]', size = 4) +
  annotate('text', x = 2.9, y = 840, label = '[Delayed maturation]', size = 4) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), 
        plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))

total.run.cv.tau.plot <- ggplot(data = tau.cv.df) +
  geom_point(aes(x = age_struct, y = totalrun_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(expression(tau[3]~"= 0.99"), 'Base case', expression(tau[3]~"= 0.25"))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.5, 0.7)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'))

total.run.tau <- ggarrange(total.run.tau.plot, total.run.cv.tau.plot, nrow=2, labels = c('b', 'd'))

total.run.eta.plot <- ggplot(data = eta.df) +
  geom_point(aes(x = age_scen, y = total.run.mean/1000, color = climate), size = 3) +
  # geom_errorbar(aes(x = age_scen, ymin = spawn.pi.lo/1000, ymax = spawn.pi.up/1000, color = climate), width = 0) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Total run size (thousands)', title = 'Natural mortality') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 900)) +
  annotate('text', x = 1.1, y = 875, label = '[High mortality]', size = 4) +
  annotate('text', x = 2.9, y = 875, label = '[Low mortality]', size = 4) +
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.2),
        text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'), axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))


total.run.cv.eta.plot <- ggplot(data = eta.cv.df) +
  geom_point(aes(x = age_struct, y = totalrun_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of total run size') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(expression(tau[3]~"= 0.99"), 'Base case', expression(tau[3]~"= 0.25"))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.5, 0.7)) +
  theme(legend.title = element_blank(), legend.position = 'none', text = element_text(size = 13), plot.margin = unit(c(0.5,0,0,0.7),'cm'))


total.run.eta <- ggarrange(total.run.eta.plot, total.run.cv.eta.plot, nrow=2, labels = c('a', 'c'))
totalrun.final <- ggarrange(total.run.eta, total.run.tau, ncol=2)


# Sensitivity to the CV of realized harvest rate ---------------------------------

cver.sa.01.df <- model.summary(cver.sa.01)
cver.sa.02.df <- model.summary(cver.sa.02)
cver.sa.03.df <- model.summary(cver.sa.03)
cver.sa.04.df <- model.summary(cver.sa.04)
cver.sa.05.df <- model.summary(cver.sa.05)
cver.sa.06.df <- model.summary(cver.sa.06)
cver.sa.07.df <- model.summary(cver.sa.07)
cver.sa.08.df <- model.summary(cver.sa.08)
cver.sa.09.df <- model.summary(cver.sa.09)
cver.sa.10.df <- model.summary(cver.sa.10)
cver.sa.11.df <- model.summary(cver.sa.11)
cver.sa.12.df <- model.summary(cver.sa.12)
cver.sa.13.df <- model.summary(cver.sa.13)
cver.sa.14.df <- model.summary(cver.sa.14)
cver.sa.15.df <- model.summary(cver.sa.15)
cver.sa.16.df <- model.summary(cver.sa.16)
cver.sa.17.df <- model.summary(cver.sa.17)
cver.sa.18.df <- model.summary(cver.sa.18)
cver.sa.19.df <- model.summary(cver.sa.19)
cver.sa.20.df <- model.summary(cver.sa.20)

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

# Sensitivity to CV of recruitment stochasticity ------------------
cv.j.01.df <- model.summary(cv.j.01)
cv.j.02.df <- model.summary(cv.j.02)
cv.j.03.df <- model.summary(cv.j.03)
cv.j.04.df <- model.summary(cv.j.04)
cv.j.05.df <- model.summary(cv.j.05)
cv.j.06.df <- model.summary(cv.j.06)
cv.j.07.df <- model.summary(cv.j.07)
cv.j.08.df <- model.summary(cv.j.08)
cv.j.09.df <- model.summary(cv.j.09)
cv.j.10.df <- model.summary(cv.j.10)
cv.j.11.df <- model.summary(cv.j.11)
cv.j.12.df <- model.summary(cv.j.12)
cv.j.13.df <- model.summary(cv.j.13)
cv.j.14.df <- model.summary(cv.j.14)
cv.j.15.df <- model.summary(cv.j.15)
cv.j.16.df <- model.summary(cv.j.16)
cv.j.17.df <- model.summary(cv.j.17)
cv.j.18.df <- model.summary(cv.j.18)
cv.j.19.df <- model.summary(cv.j.19)
cv.j.20.df <- model.summary(cv.j.20)

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