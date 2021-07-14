###############################################################################

### The following code is used to produce key reports and maps from DAFI

### Author: Oli Perkins, v2 17/12/2020

###############################################################################

library(tidyverse)
library(ggplot2)
library(viridisLite)
library(maps)
library(xlsx)
library(openxlsx)
library(devtools)

###############################

source_url('https://raw.githubusercontent.com/OliPerkins1987/AnthroFireDB/master/Utility/General_functions_v5_4_04052021.R')
source_url('https://raw.githubusercontent.com/OliPerkins1987/AnthroFireDB/master/Utility/Data_visualisation_v3_04052021.R')
source_url('https://raw.githubusercontent.com/OliPerkins1987/AnthroFireDB/master/Utility/Compress_database_v1_30112020.R')

###############################


### Setup

dbstring      <- 'DAFI.xlsx'
download.file('https://github.com/OliPerkins1987/AnthroFireDB/blob/master/Database/Database_v1_14_Nomenclature_04052021.xlsx?raw=true', 
              dbstring, mode = "wb", quiet = TRUE)
load.db(merge.stage = T)

table(Simplify.purpose()) # distribution of fire intentions

#############################################################################################

### 1a) Data overview by AFT

#############################################################################################

AFT_overview  <- DB_overview(simp = T, AFT.eval = T)

AFT_overview  <- pivot_longer(AFT_overview, cols = colnames(AFT_overview)[-1], names_to = 'AFT', 
                              values_to = 'value')

AFT_overview  <- AFT_overview %>% split(AFT_overview$AFT) %>% 
  lapply(function(x) {mutate(x, 'Proportion' = as.numeric(x$value) / sum(as.numeric(x$value), na.rm = T))}) %>%
  plyr::rbind.fill()


#############################################################################################

### 1b) Data overview by fire type

#############################################################################################


dat <- data.frame(DB_overview()$Purpose %>% filter(!is.na(`Fire purpose`)))

### sum intended and actual fire uses

for(i in seq(4, 14, 2)) {dat[, i] <- dat[, i] + dat[, i+1]}
dat <- dat[, c(1, 17, 2, 3, 4, 6, 8, 10, 12, 14, 16)]


#############################################################################################

### 2a) Summary stats by AFT

#############################################################################################

### Fire creation


Fire.use          <- plyr::rbind.fill(lapply(colnames(reported_fire)[c(11:20, 23:27)], 
                            function (x){summarise.behaviour(type = "Fire", 
                              behaviour = x, grouping = c('AFT', 'Fire purpose'), escape.rm = T)}))

Fire.use$Intended <- ifelse(grepl('intended', tolower(Fire.use$Behaviour)), 'Intended', 'Actual')


#####################################################

### By AFR
### !!! JM
### Add 'Fire purpose' to grouping arg to see fire use types by AFR

#####################################################

Fire.use          <- plyr::rbind.fill(lapply(colnames(reported_fire)[c(11:20, 23:27)], 
                      function (x){summarise.behaviour(type = "Fire", 
                        behaviour = x, grouping = c('Anthropogenic fire regime'), escape.rm = T)}))

Fire.use$Intended <- ifelse(grepl('intended', tolower(Fire.use$Behaviour)), 'Intended', 'Actual')


### Suppression


Suppression.sum           <- plyr::rbind.fill(lapply(colnames(sup)[c(8, 10, 12)], function(x) {
                                summarise.behaviour(type = "Suppression", 
                                  behaviour = x, grouping = "AFT")}))

Suppression.sum           <- Suppression.sum[, c(1, 7, 2, 5, 4, 6, 3)]
Suppression.sum[, c(3:7)] <- apply(Suppression.sum[, 3:7], 2, function(x) {ifelse(is.na(x), 0, x)}) 


### Policy


policy[, c(9, 13, 18)]    <- apply(policy[, c(9, 13, 18)], 2, as.factor)
policy.sum                <- plyr::rbind.fill(lapply(c("Incentives", "Fire restricted", "Fire banned"), 
                                                     function(x) {summarise.behaviour(type = "Policy", behaviour = x, 
                                                                                      grouping = "AFT")}))



#####################################################################################################

### 3) Plots

#####################################################################################################


plot.behaviour(metrics = colnames(reported_fire)[11:12], agg = T, Cropland = T, bin_width = 0.7, 
               scale_limits = c(0.00001, 100), log_scale = T)

plot.behaviour(metrics = colnames(reported_fire)[12:19], agg = F, Cropland = T, bin_width = 0.8,
               Xaxis_label = 'Fire size (ha)', log_scale = T, scale_limits = c(0.0001, 10000))

plot.behaviour(metrics = 'Fire return period (years)', agg = F, Cropland = T, 
               Xaxis_label = 'Fire return period (years)', log_scale = F, bin_width = 1.5, 
               scale_limits = c(0, 30))



#####################################################################################################

### 4) Maps

######################################################################################################

recordinfo$`Data Source` <- Simplify.source()
est_fire$`Fire use type` <- Simplify.purpose()
est_fire$Presence        <- est_fire$`Presence / Absence` == 'Presence'

map.behaviour('Land use', ggcolour = '`Data Source`', ggshape = '`Data Source`')

map.behaviour('Fire', ggshape = '`Fire use type`', ggcolour = '`Fire use type`', 
              choose = "`Fire use type` != 'Other' & `Presence / Absence` == 'Presence'")

map.behaviour('Suppression', ggshape = '`Fire control (0-3)`', 
              ggcolour = "AFT", 
              choose = "!is.na(`Fire control (0-3)`) & `Fire control (0-3)` != 'ND'")

map.behaviour('Policy', ggcolour = 'AFT', 
              ggshape = '`Fire banned`', 
              choose = "!AFT %in% c('ND', 'All', 
              'Agroforestry, Market-oriented', 'Agroforestry, Subsistence-oriented', 'All', 'ND', 'Mixed cropping-livestock small holder, Market-oriented', 
              'Mixed cropping-livestock small holder, Subsistence-oriented')")

map.behaviour.ras(dat.field='Data Source', ras.res=2, ras.function='count')

map.behaviour.ras(dat.field='Data Source', ras.res=2, ras.function='mode')

map.behaviour.ras(dat.field='Fire use type', ras.res=2, ras.function='mode')
