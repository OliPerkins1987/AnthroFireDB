###############################################################################

### The following code is used to conduct EDA on the database of anthropogenic fire

### Author: Oli Perkins, 30 June 2020
### v5 14102020

###############################################################################

library(tidyverse)
library(ggplot2)
library(viridisLite)
library(maps)
library(xlsx)
library(openxlsx)

###############################

source('C:/Users/Oli/Documents/PhD/Model development/Analysis/Utility/General functions v5_robust 15102020.R')

###############################


### Setup

setwd('C:/Users/Oli/Documents/PhD/Model development/Data')
dbstring      <- 'Database v1_8b_ignitions 12102020.xlsx'
load.db()
Data_overview <- DB_overview()


#############################################################################################

### 1a) Summary stats by AFT

#############################################################################################


### Fire creation


Fire.use          <- plyr::rbind.fill(lapply(colnames(reported_fire)[c(10:19, 22:26)], 
                            function (x){summarise.behaviour(type = "Fire", 
                            behaviour = x, grouping = c("AFT"), escape.rm = T)}))

Fire.use$Intended <- ifelse(grepl('intended', tolower(Fire.use$Behaviour)), 'Intended', 'Actual')


### Suppression


Suppression.sum        <- plyr::rbind.fill(lapply(colnames(sup)[c(8, 10, 12)], function(x) {
                          summarise.behaviour(type = "Suppression", 
                          behaviour = x, grouping = "AFT")}))

Suppression.sum           <- Suppression.sum[, c(1, 7, 2, 5, 4, 6, 3)]
Suppression.sum[, c(3:7)] <- apply(Suppression.sum[, 3:7], 2, function(x) {ifelse(is.na(x), 0, x)}) 


### Policy


policy[, c(9, 13, 18)]    <- apply(policy[, c(9, 13, 18)], 2, as.factor)
policy.sum                <- plyr::rbind.fill(lapply(c("Incentives", "Fire.restricted", "Fire.banned"), 
                              function(x) {summarise.behaviour(type = "Policy", behaviour = x, 
                                  grouping = "AFT")}))

##############################################################################

### 2) Plots of database information

##############################################################################

### summary stats - by intention

dat.plot                  <- gather(Data_overview$Intention[, 2:ncol(Data_overview$Intention)])
dat.plot$`Fire Intention` <- Data_overview$Intention$Fire.intention 
dat.plot                  <- dat.plot %>% 
              filter(!tolower(`Fire Intention`) %in% c('accessibility', 
                    'charcoal production', 'cultural & spiritual', 
                  'fishing', 'forest management', 'vegetation management'))

dat.plot$Type <- c(rep(NA, times = 34), rep(rep(c('Intended', 'Actual'), 
                     each = 17), times = 6), rep(NA, times = 34))

dat.plot$Metric <- c(rep('Ignition Frequency', times = 34), rep(rep(c('Burned Area', 'Burned Area'), 
                      each = 17), times = 6), rep('Ignition Frequency', times = 34))

ggplot(dat.plot[!grepl('size', tolower(dat.plot$key)) & dat.plot$key != 'instances', ], 
       aes(x = `Fire Intention`, y = value, fill = key)) + 
  geom_col(position = position_dodge(), colour = 'black') + theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  facet_grid(.~Metric) + scale_y_sqrt(breaks = c(1,5, 10, 20, 50,100, 250)) + ylab('Number of data points')

### Data sources and availability? 
### Data sources and variance in numbers?

############################################################################################################

### 3) Fire use plots

############################################################################################################

### Active fire use

ggplot(Fire.use[grepl('burned', tolower(Fire.use$Behaviour)), ] %>% filter(!AFT %in% c('Cropping, Agroecology', 'Agroforestry, Subsistence-oriented')),
       aes(x = AFT, y = Combined.stat, fill = Behaviour)) + geom_col(position= position_dodge(), colour = 'black') + scale_colour_viridis_d() +
  theme_classic() + ylab("Ignitions km-2 year-1") + #scale_y_continuous(trans = 'log10', breaks = c(0.01, 0.1, 1, 5, 30)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) + 
  scale_fill_viridis_d() + facet_grid(Intended~., scales = 'free')


### Comparison with / without escaped?
Fire.use$`Escaped?`     <- F

Fire.escaped            <- plyr::rbind.fill(lapply(colnames(reported_fire)[c(10:19, 22:27)], 
                            function (x){summarise.behaviour(type = "Fire", 
                            behaviour = x, grouping = c("AFT"), escape.rm = F)}))

Fire.escaped$Intended   <- ifelse(grepl('intended', tolower(Fire.escaped$Behaviour)), 'Intended', 'Actual')
Fire.escaped$`Escaped?` <- T

Fire.both               <- plyr::rbind.fill(Fire.use, Fire.escaped)

ggplot(Fire.both[grepl('size', tolower(Fire.both$Behaviour)), ] %>% filter(!AFT %in% c('All', 'ND')),
       aes(x = AFT, y = Combined.stat, fill = `Escaped?`)) +
  geom_col(position = position_dodge(), colour = 'black') + facet_grid(Behaviour~Intended) + theme_classic() + ylab('BA %') +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) + scale_fill_viridis_d() + 
  scale_y_sqrt(breaks = c(1, 20, 50, 100, 250, 500, 1000, 2000))



### Suppression
Suppression.plot        <- gather(Suppression.sum[, 3:7])
Suppression.plot$AFT    <- rep(Suppression.sum$AFT, times = 5)
Suppression.plot$Metric <- rep(Suppression.sum$Behaviour, times  = 5)
Suppression.sum$Total   <- apply(Suppression.sum, 1, function(x) {sum(as.numeric(x[3:6]), na.rm = T)})
Suppression.plot$Total  <- rep(Suppression.sum$Total, times = 5)
Suppression.plot        <- Suppression.plot %>% 
                              filter(!tolower(AFT) %in% c("abandoned agricultural land", 
                              "agroforestry, market-oriented", "agroforestry, subsistence-oriented", 
                              'unoccupied', 'cropping, agroecology', 'nd', 'all', 'cattle, landless'))

ggplot(Suppression.plot[Suppression.plot$key != 'ND', ], aes(x = AFT, y = value / Total, fill = key)) + 
  geom_col(position = position_dodge(), colour = 'black') + ylab('Frequency') +
  facet_grid(Metric~.) + theme_classic()+ 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) + scale_fill_viridis_d()



#####################################################################################################

### 4) Maps

#####################################################################################################

landuse$Fire.development.stage[is.na(landuse$Fire.development.stage)] <- 'ND'
map.behaviour('Land use', ggcolour = 'Fire.development.stage', ggshape = 'Fire.development.stage')


map.behaviour('Fire', ggshape = 'Presence...Absence', ggcolour = 'Fire.intention', 
              choose = "AFT %in% c('Cropping, Swidden', 'Cattle, Extensive', 'Pastoralism, Migratory', 'Forestry, Traditional')")

map.behaviour('Fire', ggshape = 'Presence...Absence', ggcolour = 'Swidden', 
              choose = "(Fire.intention == 'Crop field preparation')")


map.behaviour('Suppression', ggshape = 'Fire.control.(0-3)', 
              ggcolour = "AFT", 
              choose = "!is.na(`Fire.control.(0-3)`) & `Fire.control.(0-3)` != 'ND'")


map.behaviour('Policy', ggcolour = 'AFT', 
              ggshape = 'Fire.banned', 
              choose = "!AFT %in% c('ND', 'All', 
   'Agroforestry, Market-oriented', 'Agroforestry, Subsistence-oriented', 'All', 'ND', 'Mixed cropping-livestock small holder, Market-oriented', 
              'Mixed cropping-livestock small holder, Subsistence-oriented')")



