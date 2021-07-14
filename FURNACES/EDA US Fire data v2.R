

library(tidyverse)
library(ggplot2)
library(raster)
library(rgdal)
library(readxl)


########################################################

### Load data

########################################################

source('C:/Users/Oli/Documents/PhD/Model development/DAFI Analysis/Utility/General_functions_v5_3_07122020.R')
setwd('C:/Users/Oli/Documents/PhD/Model development/Scoping/FURNACES Workshop/Processed data')

DAFI.fire        <- read.csv('Fires_DAFI.csv')[, -1]
DAFI.suppression <- read.csv('Suppression_DAFI.csv')[, -1]
USA.fires        <- read.csv('USA_fires.csv')[, -1]

################################################################################

### does mean / median fire size line up?

################################################################################

USA.fires$weighted.mean <- USA.fires$count * USA.fires$SIZECLASS

Firetype.size <- USA.fires %>% 
                  mutate(FIRETYPE = factor(ifelse(FIRETYPE == 'Prescribed fire','Prescribed', 
                      ifelse(FIRETYPE == 'Active/Suppressed', 'Suppressed', 'Other')))) %>%
                        group_by(.dots = c('STATE', 'Year', 'CAUSE', 'FIRETYPE')) %>%
                          summarise(BA = sum(weighted.mean, na.rm = T) * 1.18, 
                            Firesize = sum(weighted.mean, na.rm = T) / sum(count, na.rm = T))

DAFI.firesize <- DAFI.fire %>% 
                  pivot_longer(colnames(DAFI.fire)[6:20], 
                    values_to = 'Value', names_to =  'Metric') %>% 
                      mutate('Metric_Type' = ifelse(grepl('size', tolower(Metric)), 'Size', 
                        ifelse(grepl('number', tolower(Metric)), 'Count', 
                        ifelse(grepl('burned', tolower(Metric)), 'BA', NA)))) %>%
                         filter(!is.na(Metric_Type) & !is.na(Value))

DAFI.firesize <- DAFI.firesize %>% 
                  mutate(Weighted.value = Value * Count) %>%
                    group_by(.dots = c('State', 'Year', 'Fire.intention', 'Presence...Absence', 'Metric_Type')) %>%
                      summarise(Median_value = median(Value, na.rm = T), 
                                Value = sum(Weighted.value, na.rm = T) / sum(Count, na.rm = T)) %>% 
                        filter(Presence...Absence == 'Presence') %>% dplyr::select(-4)

