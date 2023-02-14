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
                        harvest_cv = c(mod01.df$harvest.cv, mod03.df$harvest.cv, mod05.df$harvest.cv, mod06.df$harvest.cv, mod08.df$harvest.cv, mod10.df$harvest.cv, mod11.df$harvest.cv, mod13.df$harvest.cv, mod15.df$harvest.cv, mod16.df$harvest.cv, mod18.df$harvest.cv, mod20.df$harvest.cv),
                        totalrun_cv = c(mod01.df$total.run.cv, mod03.df$total.run.cv, mod05.df$total.run.cv, mod06.df$total.run.cv, mod08.df$total.run.cv, mod10.df$total.run.cv, mod11.df$total.run.cv, mod13.df$total.run.cv, mod15.df$total.run.cv, mod16.df$total.run.cv, mod18.df$total.run.cv, mod20.df$total.run.cv))


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
                        harvest_cv = c(mod02.df$harvest.cv, mod03.df$harvest.cv, mod04.df$harvest.cv, mod07.df$harvest.cv, mod08.df$harvest.cv, mod09.df$harvest.cv, mod12.df$harvest.cv, mod13.df$harvest.cv, mod14.df$harvest.cv, mod17.df$harvest.cv, mod18.df$harvest.cv, mod19.df$harvest.cv),
                        totalrun_cv = c(mod01.df$total.run.cv, mod03.df$total.run.cv, mod05.df$total.run.cv, mod06.df$total.run.cv, mod08.df$total.run.cv, mod10.df$total.run.cv, mod11.df$total.run.cv, mod13.df$total.run.cv, mod15.df$total.run.cv, mod16.df$total.run.cv, mod18.df$total.run.cv, mod20.df$total.run.cv))

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

# MAIN FIGURES ----------------------------

## FIGURE 1. Harvest control rule -----------------------------------------------------------------------------
library(ggplot2)
library(ggpubr)
tmp.si <- seq(0, 500000, length.out = 1000)
tmp.er <- sapply(tmp.si, control.rule)
plot1 <- ggplot() +
  geom_line(aes(x = tmp.si/1000, y = tmp.er), size = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.8)) +
  labs(x = 'Sacramento Index (thousands)', y = 'Allowable exploitation rate') +
  theme_classic() +
  theme(text = element_text(size = 16), plot.margin = unit(c(0.5,1,0.5,0.5), 'cm'))


## FIGURE 3. Spawner escapement violin plots and CV --------------------------------------------------------------------------------
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
  # scale_y_continuous(limits=c(0.65, 0.8)) +
  cv.plot.settings

spawnCV.eta.plot <- ggplot(data = eta.cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of spawner escapement') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  # scale_y_continuous(limits=c(0.60, 0.7)) +
  cv.plot.settings +
  theme(legend.position = c(0.8, 0.9))

spawn.tau <- ggarrange(spawn.tau.vio.plot, spawnCV.tau.plot, nrow=2, labels = c('b', 'd'))
spawn.eta <- ggarrange(spawn.eta.vio.plot, spawnCV.eta.plot, nrow=2, labels = c('a', 'c'))
spawn.final <- ggarrange(spawn.eta, spawn.tau, ncol=2)


## FIGURE 4. Harvest violin plots and CV -------------------------------------------------------------------------------------------
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
  # scale_y_continuous(limits=c(0.7, 0.825), breaks = seq(0.7,0.825,0.025)) +
  cv.plot.settings

harvestCV.eta.plot <- ggplot(data = eta.cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of harvest') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  # scale_y_continuous(limits=c(0.7, 0.825), breaks = seq(0.7,0.825,0.025)) +
  cv.plot.settings +
  theme(legend.position = c(0.8, 0.9))

harvest.tau <- ggarrange(harvest.tau.vio.plot, harvestCV.tau.plot, nrow=2, labels = c('b', 'd'))
harvest.eta <- ggarrange(harvest.eta.vio.plot, harvestCV.eta.plot, nrow=2, labels = c('a', 'c'))
harvest.final <- ggarrange(harvest.eta, harvest.tau, ncol=2)


## FIGURE 5. Percent overfished status boxplot -------------------------------------------------------------------------------------
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


## FIGURE 6. Percent fishery restrictions ------------------------------------------------------------------------------------------
c.plot.settings <- theme(legend.position = 'none',
                         plot.title = element_text(hjust = 0.5, size = 15),
                         axis.ticks.x = element_blank(),
                         axis.text.x = element_blank(),
                         axis.title = element_text(size = 15),
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

# SUPPLEMENTAL FIGURES -----------------------------

## FIGURE S10. Simulated hydrographs -------------------------------------------
hydro.df <- data.frame(year = seq(1,n.yr),
                       base = flow.sim(100, 'base', flow.full),
                       duration = flow.sim(100, 'longer duration', flow.full),
                       frequency = flow.sim(100, 'more frequent', flow.full),
                       intensity = flow.sim(100, 'more intense', flow.full))

hydro.plot.settings <- theme(axis.text = element_text(size = 14),
                             axis.title = element_text(size = 14),
                             title = element_text(size = 14),
                             plot.margin = unit(c(0.5,0,0,0.1),'cm'))

hydro.contemporary <- ggplot() +
  geom_line(data = hydro.df, aes(x = year, y = base), lwd = 0.5) +
  geom_segment(aes(x = 0, xend = 100, y = 10712, yend = 10712), lwd = 1, lty = 'dashed') +
  geom_segment(aes(x = 0, xend = 100, y = 4295, yend = 4295), lwd = 1, lty = 'dashed') +
  labs(x = '', y = 'Flow (csf)', title = 'Contemporary') +
  theme_classic() +
  scale_x_continuous(expand = c(0,0), limits = c(0, 120), breaks = seq(0,100,20)) +
  annotate('text', x = n.yr+8, y = 10712, label = paste0(as.character((sum(hydro.df$base<10712)/n.yr)*100),'%'), size = 6) + 
  annotate('text', x = n.yr+8, y = 4295, label = paste0(as.character((sum(hydro.df$base<4295)/n.yr)*100),'%'), size = 6) + 
  hydro.plot.settings

hydro.duration <- ggplot() +
  geom_line(data = hydro.df, aes(x = year, y = duration), lwd = 0.5) +
  geom_segment(aes(x = 0, xend = 100, y = 10712, yend = 10712), lwd = 1, lty = 'dashed') +
  geom_segment(aes(x = 0, xend = 100, y = 4295, yend = 4295), lwd = 1, lty = 'dashed') +
  labs(x = '', y = '', title = 'Longer duration') +
  theme_classic() +
  scale_x_continuous(expand = c(0,0), limits = c(0, 120), breaks = seq(0,100,20)) +
  annotate('text', x = n.yr+8, y = 10712, label = paste0(as.character((sum(hydro.df$duration<10712)/n.yr)*100),'%'), size = 6) + 
  annotate('text', x = n.yr+8, y = 4295, label = paste0(as.character((sum(hydro.df$duration<4295)/n.yr)*100),'%'), size = 6) + 
  hydro.plot.settings

hydro.frequency <- ggplot() +
  geom_line(data = hydro.df, aes(x = year, y = frequency), lwd = 0.5) +
  geom_segment(aes(x = 0, xend = 100, y = 10712, yend = 10712), lwd = 1, lty = 'dashed') +
  geom_segment(aes(x = 0, xend = 100, y = 4295, yend = 4295), lwd = 1, lty = 'dashed') +
  labs(x = 'Simulation year', y = 'Flow (csf)', title = 'More frequent') +
  theme_classic() +
  scale_x_continuous(expand = c(0,0), limits = c(0, 120), breaks = seq(0,100,20)) +
  annotate('text', x = n.yr+8, y = 10712, label = paste0(as.character((sum(hydro.df$frequency<10712)/n.yr)*100),'%'), size = 6) + 
  annotate('text', x = n.yr+8, y = 4295, label = paste0(as.character((sum(hydro.df$frequency<4295)/n.yr)*100),'%'), size = 6) + 
  hydro.plot.settings

hydro.intensity <- ggplot() +
  geom_line(data = hydro.df, aes(x = year, y = intensity), lwd = 0.5) +
  geom_segment(aes(x = 0, xend = 100, y = 10712, yend = 10712), lwd = 1, lty = 'dashed') +
  geom_segment(aes(x = 0, xend = 100, y = 4295, yend = 4295), lwd = 1, lty = 'dashed') +
  labs(x = 'Simulation year', y = '', title = 'More intense') +
  theme_classic() +
  scale_x_continuous(expand = c(0,0), limits = c(0, 120), breaks = seq(0,100,20)) +
  annotate('text', x = n.yr+8, y = 10712, label = paste0(as.character((sum(hydro.df$intensity<10712)/n.yr)*100),'%'), size = 6) + 
  annotate('text', x = n.yr+8, y = 4295, label = paste0(as.character((sum(hydro.df$intensity<4295)/n.yr)*100),'%'), size = 6) + 
  hydro.plot.settings

ggarrange(hydro.contemporary,hydro.duration, hydro.frequency, hydro.intensity)


## FIGURE S13. Sensitivity to the CV of realized harvest rate ---------------------------------
load("cv_er_sa.RData")

cver.vio.df <- NULL
for(i in 1:20){
  assign(paste0("cver.sa.",str_pad(as.character(i), width = 2, pad = "0"),".df"), model_summary(get(paste0("cver.sa.",str_pad(as.character(i), width = 2, pad = "0")))))
  assign(paste0("cver.sa.",str_pad(as.character(i), width = 2, pad = "0"),".df.vio"), violin_df(get(paste0("cver.sa.",str_pad(as.character(i), width = 2, pad = "0"))), as.character(i)))
  cver.vio.df <- rbind(cver.vio.df, get(paste0("cver.sa.",str_pad(as.character(i), width = 2, pad = "0"),".df.vio")))
}

cver.tau.df <- rbind(cver.sa.01.df %>% mutate(climate='Contemporary', age_scen = 0.7),
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
age.scen.df1 <- data.frame(scenario = as.character(c(1,3,5,6,8,10,11,13,15,16,18,20)),
                           age_scen = as.character(c(0.7,1.7,2.7,0.9,1.9,2.9,1.1,2.1,3.1,1.3,2.3,3.3)))
cver.tau.vio.df <- cver.vio.df %>%
  filter(scenario %in% as.character(c(1,3,5,6,8,10,11,13,15,16,18,20))) %>%
  mutate(climate = ifelse(scenario %in% as.character(c(1,3,5)), 'Contemporary',
                   ifelse(scenario %in% as.character(c(6,8,10)), 'Longer duration',
                   ifelse(scenario %in% as.character(c(11,13,15)), 'More frequent',
                   'More intense')))) %>%
  left_join(., age.scen.df1, by = "scenario")
cver.tau.cv.df <- data.frame(climate_scenario = rep(c('Contemporary','Duration','Frequency','Intensity'), each=3),
                             age_struct = c(seq(0.7,2.7,by=1),seq(0.9,2.9,by=1),seq(1.1,3.1,by=1),seq(1.3,3.3,by=1)),
                             spawn_cv = c(cver.sa.01.df$spawn.cv, cver.sa.03.df$spawn.cv, cver.sa.05.df$spawn.cv, cver.sa.06.df$spawn.cv, cver.sa.08.df$spawn.cv, cver.sa.10.df$spawn.cv, cver.sa.11.df$spawn.cv, cver.sa.13.df$spawn.cv, cver.sa.15.df$spawn.cv, cver.sa.16.df$spawn.cv, cver.sa.18.df$spawn.cv, cver.sa.20.df$spawn.cv),
                             harvest_cv = c(cver.sa.01.df$harvest.cv, cver.sa.03.df$harvest.cv, cver.sa.05.df$harvest.cv, cver.sa.06.df$harvest.cv, cver.sa.08.df$harvest.cv, cver.sa.10.df$harvest.cv, cver.sa.11.df$harvest.cv, cver.sa.13.df$harvest.cv, cver.sa.15.df$harvest.cv, cver.sa.16.df$harvest.cv, cver.sa.18.df$harvest.cv, cver.sa.20.df$harvest.cv))
     
cver.eta.df <- rbind(cver.sa.02.df %>% mutate(climate='Contemporary', age_scen = 0.7),
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
age.scen.df2 <- data.frame(scenario = as.character(c(2,3,4,7,8,9,12,13,14,17,18,19)),
                           age_scen = as.character(c(0.7,1.7,2.7,0.9,1.9,2.9,1.1,2.1,3.1,1.3,2.3,3.3)))
cver.eta.vio.df <- cver.vio.df %>%
  filter(scenario %in% as.character(c(2,3,4,7,8,9,12,13,14,17,18,19))) %>%
  mutate(climate = ifelse(scenario %in% as.character(c(2,3,4)), 'Contemporary',
                   ifelse(scenario %in% as.character(c(7,8,9)), 'Longer duration',
                   ifelse(scenario %in% as.character(c(12,13,14)), 'More frequent',
                   'More intense')))) %>%
  left_join(., age.scen.df2, by = "scenario")
cver.eta.cv.df <- data.frame(climate_scenario = rep(c('Contemporary','Duration','Frequency','Intensity'), each=3),
                             age_struct = c(seq(0.7,2.7,by=1),seq(0.9,2.9,by=1),seq(1.1,3.1,by=1),seq(1.3,3.3,by=1)),
                             spawn_cv = c(cver.sa.02.df$spawn.cv, cver.sa.03.df$spawn.cv, cver.sa.04.df$spawn.cv, cver.sa.07.df$spawn.cv, cver.sa.08.df$spawn.cv, cver.sa.09.df$spawn.cv, cver.sa.12.df$spawn.cv, cver.sa.13.df$spawn.cv, cver.sa.14.df$spawn.cv, cver.sa.17.df$spawn.cv, cver.sa.18.df$spawn.cv, cver.sa.19.df$spawn.cv),
                             harvest_cv = c(cver.sa.02.df$harvest.cv, cver.sa.03.df$harvest.cv, cver.sa.04.df$harvest.cv, cver.sa.07.df$harvest.cv, cver.sa.08.df$harvest.cv, cver.sa.09.df$harvest.cv, cver.sa.12.df$harvest.cv, cver.sa.13.df$harvest.cv, cver.sa.14.df$harvest.cv, cver.sa.17.df$harvest.cv, cver.sa.18.df$harvest.cv, cver.sa.19.df$harvest.cv))
##
## CVER PLOTS
##
vio.plot.settings <- theme(legend.title = element_blank(), 
                           legend.position = 'none', 
                           axis.text.x = element_blank(), 
                           axis.ticks.x = element_blank(),
                           plot.title = element_text(hjust = 0.5),
                           text = element_text(size = 12),
                           plot.margin = unit(c(0.5,0,0,0.7),'cm'))
cv.plot.settings <- theme(legend.title = element_blank(), 
                          legend.position = 'none', 
                          text = element_text(size = 12), 
                          plot.margin = unit(c(0.5,0,0,0.7),'cm'))
cver.spawn.tau.vio.plot <- ggplot() +
  geom_violin(data = cver.tau.vio.df, aes(x = age_scen, y = spawn/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey75", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = 'Maturation') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  scale_x_discrete(expand = c(0,0)) +
  annotate('text', x = 2.5, y = 570, label = '[Early maturation]', size = 3) +
  annotate('text', x = 10.5, y = 570, label = '[Delayed maturation]', size = 3) +
  vio.plot.settings

cver.spawn.eta.vio.plot <- ggplot(data = cver.eta.vio.df) +
  geom_violin(aes(x = age_scen, y = spawn/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Spawner escapement (thousands)', title = 'Natural mortality') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  annotate('text', x = 2, y = 570, label = '[High mortality]', size = 3) +
  annotate('text', x = 11, y = 570, label = '[Low mortality]', size = 3) +
  vio.plot.settings

cver.spawnCV.tau.plot <- ggplot(data = cver.tau.cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.65, 0.8)) +
  cv.plot.settings

cver.spawnCV.eta.plot <- ggplot(data = cver.eta.cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of spawner escapement') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.65, 0.8)) +
  cv.plot.settings +
  theme(legend.position = c(0.8, 0.9))

#harvest
cver.harvest.tau.vio.plot <- ggplot() +
  geom_violin(data = cver.tau.vio.df, aes(x = age_scen, y = harvest/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey75", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = '') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 700), breaks = seq(0,700,100)) +
  scale_x_discrete(expand = c(0,0)) +
  vio.plot.settings

cver.harvest.eta.vio.plot <- ggplot(data = cver.eta.vio.df) +
  geom_violin(aes(x = age_scen, y = harvest/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Harvest (thousands)', title = '') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 700), breaks = seq(0,700,100)) +
  vio.plot.settings

cver.harvestCV.tau.plot <- ggplot(data = cver.tau.cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.7, 0.825), breaks = seq(0.7,0.825,0.025)) +
  cv.plot.settings

cver.harvestCV.eta.plot <- ggplot(data = cver.eta.cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of harvest') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.7, 0.825), breaks = seq(0.7,0.825,0.025)) +
  cv.plot.settings

ggarrange(cver.spawn.eta.vio.plot, cver.spawn.tau.vio.plot,
          cver.spawnCV.eta.plot, cver.spawnCV.tau.plot,
          cver.harvest.eta.vio.plot, cver.harvest.tau.vio.plot,
          cver.harvestCV.eta.plot, cver.harvestCV.tau.plot,
          nrow = 4, ncol = 2)


## FIGURE S14. Sensitivity to CV of recruitment stochasticity ------------------
load("cv_j_sa.RData")

cvj.vio.df <- NULL
for(i in 1:20){
  assign(paste0("cv.j.",str_pad(as.character(i), width = 2, pad = "0"),".df"), model_summary(get(paste0("cv.j.",str_pad(as.character(i), width = 2, pad = "0")))))
  assign(paste0("cv.j.",str_pad(as.character(i), width = 2, pad = "0"),".df.vio"), violin_df(get(paste0("cv.j.",str_pad(as.character(i), width = 2, pad = "0"))), as.character(i)))
  cvj.vio.df <- rbind(cvj.vio.df, get(paste0("cv.j.",str_pad(as.character(i), width = 2, pad = "0"),".df.vio")))
}

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

age.scen.df1 <- data.frame(scenario = as.character(c(1,3,5,6,8,10,11,13,15,16,18,20)),
                           age_scen = as.character(c(0.7,1.7,2.7,0.9,1.9,2.9,1.1,2.1,3.1,1.3,2.3,3.3)))

cv.j.tau.vio.df <- cvj.vio.df %>%
  filter(scenario %in% as.character(c(1,3,5,6,8,10,11,13,15,16,18,20))) %>%
  mutate(climate = ifelse(scenario %in% as.character(c(1,3,5)), 'Contemporary',
                   ifelse(scenario %in% as.character(c(6,8,10)), 'Longer duration',
                   ifelse(scenario %in% as.character(c(11,13,15)), 'More frequent',
                   'More intense')))) %>%
  left_join(., age.scen.df1, by = "scenario")

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

age.scen.df2 <- data.frame(scenario = as.character(c(2,3,4,7,8,9,12,13,14,17,18,19)),
                           age_scen = as.character(c(0.7,1.7,2.7,0.9,1.9,2.9,1.1,2.1,3.1,1.3,2.3,3.3)))
cv.j.eta.vio.df <- cvj.vio.df %>%
  filter(scenario %in% as.character(c(2,3,4,7,8,9,12,13,14,17,18,19))) %>%
  mutate(climate = ifelse(scenario %in% as.character(c(2,3,4)), 'Contemporary',
                   ifelse(scenario %in% as.character(c(7,8,9)), 'Longer duration',
                   ifelse(scenario %in% as.character(c(12,13,14)), 'More frequent',
                   'More intense')))) %>%
  left_join(., age.scen.df2, by = "scenario")

cv.j.eta.cv.df <- data.frame(climate_scenario = rep(c('Contemporary','Duration','Frequency','Intensity'), each=3),
                             age_struct = c(seq(0.7,2.7,by=1),seq(0.9,2.9,by=1),seq(1.1,3.1,by=1),seq(1.3,3.3,by=1)),
                             spawn_cv = c(cv.j.02.df$spawn.cv, cv.j.03.df$spawn.cv, cv.j.04.df$spawn.cv, cv.j.07.df$spawn.cv, cv.j.08.df$spawn.cv, cv.j.09.df$spawn.cv, cv.j.12.df$spawn.cv, cv.j.13.df$spawn.cv, cv.j.14.df$spawn.cv, cv.j.17.df$spawn.cv, cv.j.18.df$spawn.cv, cv.j.19.df$spawn.cv),
                             harvest_cv = c(cv.j.02.df$harvest.cv, cv.j.03.df$harvest.cv, cv.j.04.df$harvest.cv, cv.j.07.df$harvest.cv, cv.j.08.df$harvest.cv, cv.j.09.df$harvest.cv, cv.j.12.df$harvest.cv, cv.j.13.df$harvest.cv, cv.j.14.df$harvest.cv, cv.j.17.df$harvest.cv, cv.j.18.df$harvest.cv, cv.j.19.df$harvest.cv))

##
## CVJ PLOTS
##
vio.plot.settings <- theme(legend.title = element_blank(), 
                           legend.position = 'none', 
                           axis.text.x = element_blank(), 
                           axis.ticks.x = element_blank(),
                           plot.title = element_text(hjust = 0.5),
                           text = element_text(size = 12),
                           plot.margin = unit(c(0.5,0,0,0.7),'cm'))
cv.plot.settings <- theme(legend.title = element_blank(), 
                          legend.position = 'none', 
                          text = element_text(size = 12), 
                          plot.margin = unit(c(0.5,0,0,0.7),'cm'))
cv.j.spawn.tau.vio.plot <- ggplot() +
  geom_violin(data = cv.j.tau.vio.df, aes(x = age_scen, y = spawn/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey75", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = 'Maturation') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  scale_x_discrete(expand = c(0,0)) +
  annotate('text', x = 2.5, y = 570, label = '[Early maturation]', size = 3) +
  annotate('text', x = 10.5, y = 570, label = '[Delayed maturation]', size = 3) +
  vio.plot.settings

cv.j.spawn.eta.vio.plot <- ggplot(data = cv.j.eta.vio.df) +
  geom_violin(aes(x = age_scen, y = spawn/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Spawner escapement (thousands)', title = 'Natural mortality') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  annotate('text', x = 2, y = 570, label = '[High mortality]', size = 3) +
  annotate('text', x = 11, y = 570, label = '[Low mortality]', size = 3) +
  vio.plot.settings

cv.j.spawnCV.tau.plot <- ggplot(data = cv.j.tau.cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.65, 0.8)) +
  cv.plot.settings

cv.j.spawnCV.eta.plot <- ggplot(data = cv.j.eta.cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of spawner escapement') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.65, 0.8)) +
  cv.plot.settings +
  theme(legend.position = c(0.8, 0.9))

#harvest
cv.j.harvest.tau.vio.plot <- ggplot() +
  geom_violin(data = cv.j.tau.vio.df, aes(x = age_scen, y = harvest/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey75", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = '') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 700), breaks = seq(0,700,100)) +
  scale_x_discrete(expand = c(0,0)) +
  vio.plot.settings

cv.j.harvest.eta.vio.plot <- ggplot(data = cv.j.eta.vio.df) +
  geom_violin(aes(x = age_scen, y = harvest/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Harvest (thousands)', title = '') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 700), breaks = seq(0,700,100)) +
  vio.plot.settings

cv.j.harvestCV.tau.plot <- ggplot(data = cv.j.tau.cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.7, 0.825), breaks = seq(0.7,0.825,0.025)) +
  cv.plot.settings

cv.j.harvestCV.eta.plot <- ggplot(data = cv.j.eta.cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of harvest') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.7, 0.825), breaks = seq(0.7,0.825,0.025)) +
  cv.plot.settings

ggarrange(cv.j.spawn.eta.vio.plot, cv.j.spawn.tau.vio.plot,
          cv.j.spawnCV.eta.plot, cv.j.spawnCV.tau.plot,
          cv.j.harvest.eta.vio.plot, cv.j.harvest.tau.vio.plot,
          cv.j.harvestCV.eta.plot, cv.j.harvestCV.tau.plot,
          nrow = 4, ncol = 2)


## FIGURE S15. Sensitivity to mean NPGO effect -------------------
load("npgo_sa.RData")

npgo.vio.df <- NULL
for(i in 1:20){
  assign(paste0("npgo.",str_pad(as.character(i), width = 2, pad = "0"),".df"), model_summary(get(paste0("npgo.",str_pad(as.character(i), width = 2, pad = "0")))))
  assign(paste0("npgo.",str_pad(as.character(i), width = 2, pad = "0"),".df.vio"), violin_df(get(paste0("npgo.",str_pad(as.character(i), width = 2, pad = "0"))), as.character(i)))
  npgo.vio.df <- rbind(npgo.vio.df, get(paste0("npgo.",str_pad(as.character(i), width = 2, pad = "0"),".df.vio")))
}

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

age.scen.df1 <- data.frame(scenario = as.character(c(1,3,5,6,8,10,11,13,15,16,18,20)),
                           age_scen = as.character(c(0.7,1.7,2.7,0.9,1.9,2.9,1.1,2.1,3.1,1.3,2.3,3.3)))

npgo.tau.vio.df <- npgo.vio.df %>%
  filter(scenario %in% as.character(c(1,3,5,6,8,10,11,13,15,16,18,20))) %>%
  mutate(climate = ifelse(scenario %in% as.character(c(1,3,5)), 'Contemporary',
                   ifelse(scenario %in% as.character(c(6,8,10)), 'Longer duration',
                   ifelse(scenario %in% as.character(c(11,13,15)), 'More frequent',
                   'More intense')))) %>%
  left_join(., age.scen.df1, by = "scenario")

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

age.scen.df2 <- data.frame(scenario = as.character(c(2,3,4,7,8,9,12,13,14,17,18,19)),
                           age_scen = as.character(c(0.7,1.7,2.7,0.9,1.9,2.9,1.1,2.1,3.1,1.3,2.3,3.3)))
npgo.eta.vio.df <- npgo.vio.df %>%
  filter(scenario %in% as.character(c(2,3,4,7,8,9,12,13,14,17,18,19))) %>%
  mutate(climate = ifelse(scenario %in% as.character(c(2,3,4)), 'Contemporary',
                   ifelse(scenario %in% as.character(c(7,8,9)), 'Longer duration',
                   ifelse(scenario %in% as.character(c(12,13,14)), 'More frequent',
                   'More intense')))) %>%
  left_join(., age.scen.df2, by = "scenario")

npgo.eta.cv.df <- data.frame(climate_scenario = rep(c('Contemporary','Duration','Frequency','Intensity'), each=3),
                             age_struct = c(seq(0.7,2.7,by=1),seq(0.9,2.9,by=1),seq(1.1,3.1,by=1),seq(1.3,3.3,by=1)),
                             spawn_cv = c(npgo.02.df$spawn.cv, npgo.03.df$spawn.cv, npgo.04.df$spawn.cv, npgo.07.df$spawn.cv, npgo.08.df$spawn.cv, npgo.09.df$spawn.cv, npgo.12.df$spawn.cv, npgo.13.df$spawn.cv, npgo.14.df$spawn.cv, npgo.17.df$spawn.cv, npgo.18.df$spawn.cv, npgo.19.df$spawn.cv),
                             harvest_cv = c(npgo.02.df$harvest.cv, npgo.03.df$harvest.cv, npgo.04.df$harvest.cv, npgo.07.df$harvest.cv, npgo.08.df$harvest.cv, npgo.09.df$harvest.cv, npgo.12.df$harvest.cv, npgo.13.df$harvest.cv, npgo.14.df$harvest.cv, npgo.17.df$harvest.cv, npgo.18.df$harvest.cv, npgo.19.df$harvest.cv))

##
## NPGO PLOTS
##
vio.plot.settings <- theme(legend.title = element_blank(), 
                           legend.position = 'none', 
                           axis.text.x = element_blank(), 
                           axis.ticks.x = element_blank(),
                           plot.title = element_text(hjust = 0.5),
                           text = element_text(size = 12),
                           plot.margin = unit(c(0.5,0,0,0.7),'cm'))
cv.plot.settings <- theme(legend.title = element_blank(), 
                          legend.position = 'none', 
                          text = element_text(size = 12), 
                          plot.margin = unit(c(0.5,0,0,0.7),'cm'))
npgo.spawn.tau.vio.plot <- ggplot() +
  geom_violin(data = npgo.tau.vio.df, aes(x = age_scen, y = spawn/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey75", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = 'Maturation') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  scale_x_discrete(expand = c(0,0)) +
  annotate('text', x = 2.5, y = 570, label = '[Early maturation]', size = 3) +
  annotate('text', x = 10.5, y = 570, label = '[Delayed maturation]', size = 3) +
  vio.plot.settings

npgo.spawn.eta.vio.plot <- ggplot(data = npgo.eta.vio.df) +
  geom_violin(aes(x = age_scen, y = spawn/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Spawner escapement (thousands)', title = 'Natural mortality') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  annotate('text', x = 2, y = 570, label = '[High mortality]', size = 3) +
  annotate('text', x = 11, y = 570, label = '[Low mortality]', size = 3) +
  vio.plot.settings

npgo.spawnCV.tau.plot <- ggplot(data = npgo.tau.cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.65, 0.8)) +
  cv.plot.settings

npgo.spawnCV.eta.plot <- ggplot(data = npgo.eta.cv.df) +
  geom_point(aes(x = age_struct, y = spawn_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of spawner escapement') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.65, 0.8)) +
  cv.plot.settings +
  theme(legend.position = c(0.8, 0.9))

#harvest
npgo.harvest.tau.vio.plot <- ggplot() +
  geom_violin(data = npgo.tau.vio.df, aes(x = age_scen, y = harvest/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey75", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = '') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 700), breaks = seq(0,700,100)) +
  scale_x_discrete(expand = c(0,0)) +
  vio.plot.settings

npgo.harvest.eta.vio.plot <- ggplot(data = npgo.eta.vio.df) +
  geom_violin(aes(x = age_scen, y = harvest/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Harvest (thousands)', title = '') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 700), breaks = seq(0,700,100)) +
  vio.plot.settings

npgo.harvestCV.tau.plot <- ggplot(data = npgo.tau.cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.7, 0.825), breaks = seq(0.7,0.825,0.025)) +
  cv.plot.settings

npgo.harvestCV.eta.plot <- ggplot(data = npgo.eta.cv.df) +
  geom_point(aes(x = age_struct, y = harvest_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of harvest') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.7, 0.825), breaks = seq(0.7,0.825,0.025)) +
  cv.plot.settings

ggarrange(npgo.spawn.eta.vio.plot, npgo.spawn.tau.vio.plot,
          npgo.spawnCV.eta.plot, npgo.spawnCV.tau.plot,
          npgo.harvest.eta.vio.plot, npgo.harvest.tau.vio.plot,
          npgo.harvestCV.eta.plot, npgo.harvestCV.tau.plot,
          nrow = 4, ncol = 2)


## FIGURE S16. total escapement plots ----------
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

total.run.tau.vio.plot <- ggplot() +
  geom_violin(data = tau.vio.df, aes(x = age_scen, y = (spawn+harvest)/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey75", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = '', title = 'Maturation') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1100)) +
  scale_x_discrete(expand = c(0,0)) +
  annotate('text', x = 2.5, y = 1000, label = '[Early maturation]', size = 4) +
  annotate('text', x = 10.5, y = 1000, label = '[Delayed maturation]', size = 4) +
  vio.plot.settings

total.run.eta.vio.plot <- ggplot(data = eta.vio.df) +
  geom_violin(aes(x = age_scen, y = (spawn+harvest)/1000, fill = climate), draw_quantiles = 0.5) +
  scale_fill_manual(values = c("grey", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = '', y = 'Total run size (thousands)', title = 'Natural mortality') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1100)) +
  annotate('text', x = 2, y = 1000, label = '[High mortality]', size = 4) +
  annotate('text', x = 11, y = 1000, label = '[Low mortality]', size = 4) +
  vio.plot.settings

total.run.CV.tau.plot <- ggplot(data = tau.cv.df) +
  geom_point(aes(x = age_struct, y = totalrun_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = '') +
  # scale_x_continuous(breaks = seq(1,3), labels = c(expression(tau[3]~"= 0.99"), 'Base case', expression(tau[3]~"= 0.25"))) +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.5, 0.65)) +
  cv.plot.settings

total.run.CV.eta.plot <- ggplot(data = eta.cv.df) +
  geom_point(aes(x = age_struct, y = totalrun_cv, color = climate_scenario), size = 3) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9", "#009E73")) +
  theme_classic() +
  labs(x = 'Age structure scenario', y = 'CV of total run size') +
  scale_x_continuous(breaks = seq(1,3), labels = c('Low', 'Base case', 'High')) +
  scale_y_continuous(limits=c(0.5, 0.65)) +
  cv.plot.settings +
  theme(legend.position = c(0.8, 0.9))

totalrun.tau <- ggarrange(total.run.tau.vio.plot, total.run.CV.tau.plot, nrow=2, labels = c('b', 'd'))
totalrun.eta <- ggarrange(total.run.eta.vio.plot, total.run.CV.eta.plot, nrow=2, labels = c('a', 'c'))
totalrun.final <- ggarrange(totalrun.eta, totalrun.tau, ncol=2)

# 100-YEAR MODEL VALIDATION ---------------------------------------------------------------------------------------------
base.mod.df <- model_summary(mod.03)

# plots
sim.nums <- n.sim
base.mod1   <- base.mod #%>% filter(sim %in% sim.nums)
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














