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

