###############################################################################

### The following code is used to conduct EDA on the database of anthropogenic fire

### Author: Oli Perkins, 30 June 2020

###############################################################################

library(tidyverse)
library(ggplot2)
library(viridisLite)
library(maps)
library(xlsx)
library(openxlsx)

###############################

setwd('yourwd')
source('General functions v2 06102020.R')

###############################


### Setup

dbstring      <- 'Database v1_8 05102020.xlsx'
load.db()
Data_overview <- DB_overview()


#############################################################################################

### 1) Fire creation summary by AFT

#############################################################################################


Fire.return       <- summarise.behaviour(type = "Fire", behaviour = "Fire.return.period.(years)", 
                                   grouping = "AFT")

Fire.return_byuse <- summarise.behaviour(type = "Fire", behaviour = "Fire.return.period.(years)", 
                                   grouping = c("AFT", "Fire.intention"))

Fire.min          <- summarise.behaviour(type = "Fire", behaviour = "Actual.fire.size.min.(ha)", 
                                         grouping = "AFT")

Fire.max          <- summarise.behaviour(type = "Fire", behaviour = "Actual.fire.size.max.(ha)", 
                                         grouping = "AFT")

Fire.mean         <- summarise.behaviour(type = "Fire", behaviour = "Actual.fire.size.mean.(ha)", 
                                         grouping = "AFT")

Fire.median       <- summarise.behaviour(type = "Fire", behaviour = "Actual.fire.size.median.(ha)", 
                                          grouping = "AFT")

Ignitions         <- summarise.behaviour(type = "Fire", behaviour = "Number.of.fires.(#.km2-1)", 
                                         grouping = "AFT")

BA.total          <- summarise.behaviour(type = "Fire", behaviour = "Actual.Burned.area.%.(total)", 
                                grouping = "AFT")

BA.LC             <- summarise.behaviour(type = "Fire", behaviour = "Actual.burned.area.%.(land.cover)", 
                                         grouping = "AFT")

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

dat.plot$Type <- c(rep(NA, times = 17), rep(rep(c('Intended', 'Actual'), 
                     each = 17), times = 6), rep(NA, times = 34))

dat.plot$Metric <- c(rep('Ignition Frequency', times = 17), rep(rep(c('Burned Area', 'Burned Area'), 
                      each = 17), times = 6), rep('Ignition Frequency', times = 34))

ggplot(dat.plot[!grepl('size', tolower(dat.plot$key)) & dat.plot$key != 'instances', ], aes(x = `Fire Intention`, 
                                                                                            y = value, fill = key)) + 
  geom_col(position = position_dodge(), colour = 'black') + theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  facet_grid(.~Metric) + scale_y_sqrt(breaks = c(1,5, 10, 20, 50,100, 250)) + ylab('Number of data points')


### plot by AFT

dat.plot                  <- gather(Data_overview$AFT[, 2:ncol(Data_overview$AFT)])
dat.plot$AFT              <- Data_overview$AFT$AFT
dat.plot                  <- dat.plot %>% 
                                filter(!tolower(AFT) %in% c("abandoned agricultural land", 
                                            "agroforestry, market-oriented", "agroforestry, subsistence-oriented", 
                                           'unoccupied', 'cropping, agroecology', 'nd', 'all'))

dat.plot$Type <- c(rep(NA, times = 16), rep(rep(c('Intended', 'Actual'), 
                                                each = 16), times = 6), rep(NA, times = 32))

dat.plot$Metric <- c(rep('Ignition Frequency', times = 16), rep(rep(c('Burned Area', 'Burned Area'), 
                                                                    each = 16), times = 6), rep('Ignition Frequency', times = 32))

ggplot(dat.plot[!grepl('size', tolower(dat.plot$key)) & dat.plot$key != 'instances', ], aes(x = AFT, 
      y = value, fill = key)) + 
  geom_col(position = position_dodge(), colour = 'black') + theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  facet_grid(.~Metric) + scale_y_sqrt(breaks = c(1,5, 10, 20, 50,100, 250)) + ylab('Number of data points')




ggplot(fire.return[fire.return$Fire.type %in% c('Human, deliberate', 
      'Human, escaped') & !fire.return$AFT %in% c('Abandoned agricultural land', 
   'Agroforestry, Market-oriented', 'Agroforestry, Subsistence-oriented', 'All', 'ND', 'Mixed cropping-livestock small holder, Market-oriented', 
   'Mixed cropping-livestock small holder, Subsistence-oriented'), ], 
       aes(x = AFT, y = return, fill = Fire.type)) + 
  geom_col(position = position_dodge(), colour = 'black') + theme_classic() + 
  scale_fill_viridis_d() + ylab('Fire return (Years)') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 



#####################################################################################################

### 3) Maps

#####################################################################################################

landuse$Fire.development.stage[is.na(landuse$Fire.development.stage)] <- 'ND'
map.behaviour('Land use', ggcolour = 'Fire.development.stage', ggshape = 'Fire.development.stage')


map.behaviour('Fire', ggshape = 'Presence...Absence', ggcolour = 'Fire.intention', 
              choose = "AFT %in% c('Cropping, Swidden', 'Cattle, Extensive', 'Pastoralism, Migratory', 'Forestry, Traditional')")

map.behaviour('Fire', ggshape = 'Presence...Absence', ggcolour = 'Fire.intention', 
              choose = "AFT %in% c('Cropping, Swidden', 'Cattle, Extensive', 'Pastoralism, Migratory', 'Forestry, Traditional') & Fire.intention %in% c('Pasture renewal', 'Rangeland management')")


map.behaviour('Suppression', ggcolour = 'AFT', 
              ggshape = "Fire.prevention..0.3.", 
              choose = "!AFT %in% c('ND', 'All', 
              'Agroforestry, Market-oriented', 'Agroforestry, Subsistence-oriented', 'All', 'ND', 'Mixed cropping-livestock small holder, Market-oriented', 
              'Mixed cropping-livestock small holder, Subsistence-oriented') & `Fire.prevention.(0-3)` != 'ND'")


map.behaviour('Policy', ggcolour = 'AFT', 
              ggshape = 'Fire.banned', 
              choose = "!AFT %in% c('ND', 'All', 
   'Agroforestry, Market-oriented', 'Agroforestry, Subsistence-oriented', 'All', 'ND', 'Mixed cropping-livestock small holder, Market-oriented', 
              'Mixed cropping-livestock small holder, Subsistence-oriented')")


####################################################################################################

### 4) Uncertainty in binned ranges

####################################################################################################


### Gather data and calculate uncertainties

uncert.frame <- list()

for(samp in 1:25) {
  
  dat <- reported_fire[, 
                       -which(colnames(reported_fire) %in% c(
                         "Actual.burned.area.(ha)", "Intended.burned.area.(ha)"))]
  
  for(i in 11:23) {
    
    
    ### Does this need fixing so that it bins based on the correct metric?
    
    dat[, i] <- as.numeric(ifelse(is.na(dat[, i]), 
                                  unlist(unbin(est_fire[, i], method = 'Random')), 
                                  dat[, i]))
    
  }
  
  dat <- dat %>% group_by(.dots = c('AFT')) %>% 
    select(11:23) %>%
    summarise_all(function(x) {mean(x[x!=0], na.rm = T)})
  
  uncert.frame[[samp]] <- dat
  
  
}

uncert.frame <- plyr::rbind.fill(uncert.frame)

### Calculate metrics

uncert.mean <- uncert.frame %>% group_by(.dots = c('AFT')) %>% 
  select(2:14) %>%
  summarise_all(mean, na.rm = T)

uncert.upper <- uncert.frame %>% group_by(.dots = c('AFT')) %>% 
  select(2:14) %>%
  summarise_all(quantile, 0.95, na.rm = T)

uncert.lower <- uncert.frame %>% group_by(.dots = c('AFT')) %>% 
  select(2:14) %>%
  summarise_all(quantile, 0.05, na.rm = T)

uncert.frame        <- rbind(uncert.mean, uncert.lower, uncert.upper)
uncert.frame$metric <- rep(c('Mean', 'Lower', 'Upper'), each = 19) 


### Plot data

ggplot(uncert.mean[-c(1:4, 16:18), ], aes(x = `AFT`, fill = `AFT`, y = `Actual.fire.size.mean.(ha)`)) + 
  geom_col(position = position_dodge(), colour = 'black') + scale_y_sqrt(breaks = c(2.5, 10, 25, 50, 100, 250, 500, 1000)) + 
  theme_classic() + scale_fill_viridis_d() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Fire size (ha)') +
  facet_grid(.~metric)

### long form

uncert.long     <- gather(uncert.frame[, 2:11])
uncert.long$AFT <- rep(uncert.frame$AFT, times = 10)
uncert.long$metric <- rep(c('Mean', 'Lower', 'Upper'), each = 42) 
uncert.long     <- uncert.long %>% filter(!AFT %in% c('ND', 'All', 'Abandoned agricultural land', 
                                                      'Agroforestry, Market-oriented', 'Agroforestry, Subsistence-oriented', 'All', 'ND', 'Mixed cropping-livestock small holder, Market-oriented', 
                                                      'Mixed cropping-livestock small holder, Subsistence-oriented', 'Forestry', 'Tourism manager', 'Urban planner'))

ggplot(uncert.long[!uncert.long$key %in% c("Actual.burned.area.%.(land.cover)", 
                                           "Intended.burned.area.%.(land.cover)"), ], aes(x = AFT, fill = key, y = value)) + 
  geom_col(position = position_dodge(), colour = 'black') + scale_y_sqrt(breaks = c(2.5, 10, 25, 50, 100, 250, 500, 1000), limits = c(0, 1500)) + 
  theme_classic() + scale_fill_viridis_d() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Fire size (ha)') #+
#facet_grid(.~metric)



