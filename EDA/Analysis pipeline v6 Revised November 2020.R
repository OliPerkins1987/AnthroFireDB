###############################################################################

### The following code is used to conduct EDA on the database of anthropogenic fire

### Author: Oli Perkins, 30 June 2020
### v6 27112020

###############################################################################

library(tidyverse)
library(ggplot2)
library(viridisLite)
library(maps)
library(readxl)
library(scales)

###############################

source('C:/Users/Oli/Documents/PhD/Model development/Analysis/Utility/General functions v5_3 07122020.R')
source('C:/Users/Oli/Documents/PhD/Model development/Analysis/Utility/Data visualisation v2 17122020.R')


###############################


### Setup

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/DAFI')
dbstring      <- 'Database v1_9_clean 07122020.xlsx'
load.db()
table(Simplify.intention()) # distribution of fire intentions


#############################################################################################

### 1a) Summary stats by AFT

#############################################################################################


### Fire creation


Fire.use          <- plyr::rbind.fill(lapply(colnames(reported_fire)[c(10:19, 22:26)], 
                            function (x){summarise.behaviour(type = "Fire", 
                            behaviour = x, grouping = c('AFT', 'Fire intention'), escape.rm = T)}))

Fire.use$Intended <- ifelse(grepl('intended', tolower(Fire.use$Behaviour)), 'Intended', 'Actual')


### Suppression


Suppression.sum        <- plyr::rbind.fill(lapply(colnames(sup)[c(8, 10, 12)], function(x) {
                          summarise.behaviour(type = "Suppression", 
                          behaviour = x, grouping = "AFT")}))

Suppression.sum           <- Suppression.sum[, c(1, 7, 2, 5, 4, 6, 3)]
Suppression.sum[, c(3:7)] <- apply(Suppression.sum[, 3:7], 2, function(x) {ifelse(is.na(x), 0, x)}) 


### Policy


policy[, c(9, 13, 18)]    <- apply(policy[, c(9, 13, 18)], 2, as.factor)
policy.sum                <- plyr::rbind.fill(lapply(c("Incentives", "Fire restricted", "Fire banned"), 
                              function(x) {summarise.behaviour(type = "Policy", behaviour = x, 
                                  grouping = "AFT")}))


##############################################################################

### 2) Plots of database meta information 

##############################################################################

########

### Intersection of preliminary AFTs and fire use types

########


AFT_overview  <- DB_overview(simp = T, AFT.eval = T)

AFT_overview  <- pivot_longer(AFT_overview, cols = colnames(AFT_overview)[-1], names_to = 'AFT', 
                              values_to = 'value')

AFT_overview  <- AFT_overview %>% split(AFT_overview$AFT) %>% 
                  lapply(function(x) {mutate(x, 'Proportion' = as.numeric(x$value) / sum(as.numeric(x$value), na.rm = T))}) %>%
                    plyr::rbind.fill()


######################################################

### Prepare for plotting

######################################################


AFT_overview$AFT[1:8]   <- rep('Conservationist', times = 8)
AFT_overview$AFT[33:48] <- c(rep('Monoculture, Market', times = 8), 
                                    rep('Monoculture, Subsistence', times = 8))

AFT_overview$AFT[81:88] <- rep('Hunter gatherer', times = 8)
AFT_overview$AFT[89:104]<- c(rep('Mixed smallholder, Market', times = 8), 
                               rep('Mixed smallholder, Subsistence', times = 8))

AFT_overview            <- AFT_overview[-c(105:112, 121:136), ]

AFT_overview$Fire.intention <- gsub('\\.', " ", AFT_overview$Var1)
AFT_overview$Fire.intention <- factor(AFT_overview$Fire.intention, levels = c('Arson', 
                                  'Crop field preparation', 'Crop residue burning', 
                                  'Pasture management', 'Vegetation clearance',
                                  'Hunter gatherer', 'Pyrome management'), ordered = T)

AFT_overview         <- AFT_overview[AFT_overview$Var1 != 'Other', ]

AFT_overview$Firedev <- factor(c(rep('Forestry', times = 7), rep('Livestock', times = 14), 
                          rep('Cropping', times = 28), rep('Forestry', times = 28),
                          rep('Livestock', times = 21)), 
                          levels = c('Cropping', 'Livestock', 'Forestry', ''), ordered = T)

AFT_overview$AFT     <- factor(AFT_overview$AFT, levels = c("Cropping, Swidden", 
                            "Monoculture, Subsistence", "Monoculture, Market", 
                            "Cropping, Intensive", "Pastoralism, Migratory", 
                            "Mixed smallholder, Subsistence", "Mixed smallholder, Market",
                            "Cattle, Extensive", "Cattle, Intensive", 
                            "Hunter gatherer", "Forestry, Small-scale", 
                            "Forestry, Industrial", "Conservationist",
                            "Fire suppression agent")) 


AFT_overview %>% filter(Fire.intention != 'Total' & !is.na(AFT)) %>%
  ggplot(aes(x = AFT, y = Fire.intention, fill = ifelse(Proportion < 0.01, NA, Proportion))) + 
  geom_tile(colour = 'black') + theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_viridis_b(breaks = c(0.05, 0.1, 0.25, 0.4, 0.6, 0.8), 
    na.value = 'white', name = "Proportion", trans = 'sqrt') +
  scale_x_discrete(name = "") + facet_grid(.~Firedev, scale="free") +
  scale_y_discrete(name = "") + theme(text = element_text(size=14))


####################################

### data overview tile

####################################

dat <- data.frame(DB_overview()$Intention %>% filter(!is.na(`Fire intention`)))

### sum intended and actual fire uses

for(i in seq(4, 14, 2)) {dat[, i] <- dat[, i] + dat[, i+1]}
dat <- dat[, c(1, 17, 2, 3, 4, 6, 8, 10, 12, 14, 16)]
dat <- pivot_longer(dat, cols = colnames(dat)[2:ncol(dat)],
                    names_to = 'Fire use metric', values_to = 'Count')

dat$`Fire use metric` <- factor(rep(c('Instances', 'Number of fires (land cover)','Number of fires (study area)', 'Min fire size', 'Max fire size', 
                          'Mean fire size', 'Median fire size', 'Burned area % (land cover)', 'Burned area % (study area)', 
                          'Fire return period'), times = length(unique(dat$Fire.intention))), 
                          levels = c('Instances', 'Number of fires (land cover)','Number of fires (study area)', 'Min fire size', 'Max fire size', 
                              'Mean fire size', 'Median fire size', 'Burned area % (land cover)', 'Burned area % (study area)', 
                              'Fire return period'), ordered = T)

ggplot(dat, 
       aes(x = `Fire use metric`, y = Fire.intention, fill = ifelse(Count == 0, NA, Count))) + 
  geom_tile(colour = 'black') + theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_viridis_b(breaks = c(0, 1, 5, 10, 25, 50, 100, 200, 500, 1000), trans = 'log',
  na.value = 'white', name = "Count", begin = 0, end = 1, option = 'D') +
  scale_x_discrete(name = "") + 
  scale_y_discrete(name = "") + theme(text = element_text(size=14))


### Plot with text

dat$Text                                        <- NA
dat$Text[dat$`Fire use metric` == 'Instances']  <- dat$Count[dat$`Fire use metric` == 'Instances']
dat$Count[dat$`Fire use metric` == 'Instances'] <- NA


ggplot(dat, 
       aes(x = `Fire use metric`, y = Fire.intention, fill = ifelse(Count == 0, NA, Count))) + 
  geom_tile(colour = 'black') + theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_viridis_b(breaks = c(0, 1, 5, 10, 25, 50, 100, 200, 400), trans = 'log',
                       na.value = 'white', name = "Count", begin = 0, end = 1, option = 'D') +
  scale_x_discrete(name = "") + geom_text(aes(label = round(Text, 1))) +
  scale_y_discrete(name = "") + theme(text = element_text(size=14))

  



###########################################################################

### 3) Analysis of anthropogenic pyrome

###########################################################################

plot.behaviour(metrics = colnames(reported_fire)[10:11], agg = T, Cropland = T, bin_width = 0.7, 
               scale_limits = c(0.00001, 100), log_scale = T)

plot.behaviour(metrics = colnames(reported_fire)[12:19], agg = F, Cropland = T, bin_width = 0.8,
               Xaxis_label = 'Fire size (ha)', log_scale = T, scale_limits = c(0.0001, 10000))

plot.behaviour(metrics = 'Fire return period (years)', agg = F, Cropland = T, 
    Xaxis_label = 'Fire return period (years)', log_scale = F, bin_width = 1.5, 
    scale_limits = c(0, 30))



##################################################

### Case-based analysis of arson

##################################################

arson <- reported_fire %>% filter(`Fire intention` == 'Arson' & `Presence / Absence` == 'Presence')
arson <- merge(arson, Landuse[, 1:7], by = 'Case Study ID', all.x = T)

arson.sum      <- plyr::rbind.fill(data.frame(table(arson$`Primary AFT`)), 
                          data.frame(table(arson$`Secondary.AFT`)), 
                          data.frame(table(arson$`Tertiary.AFT`)), 
                          data.frame(table(arson$`Other.AFT`))) %>% group_by(Var1) %>%
                            summarise('Cases' = sum(Freq, na.rm = T))

arson.conflict <- plyr::rbind.fill(data.frame(table(arson$`Primary AFT`[arson$`Land tenure` == 'Insecure'])), 
                                   data.frame(table(arson$`Secondary AFT`[arson$`Land tenure` == 'Insecure'])), 
                                   data.frame(table(arson$`Tertiary AFT`[arson$`Land tenure` == 'Insecure'])), 
                                   data.frame(table(arson$`Other AFT`[arson$`Land tenure` == 'Insecure']))) %>% group_by(Var1) %>%
                                    summarise('Cases' = sum(Freq, na.rm = T))

arson.sum           <- merge(arson.sum, arson.conflict, by = 'Var1', all.x = T)
colnames(arson.sum) <- c('Var1', 'All arson', 'Conflict arson')
arson.sum           <- gather(data.frame(apply(arson.sum[, 2:3], 2, function(x) {ifelse(is.na(x), 0, x)}))) %>% 
                        mutate('AFT' = rep(arson.sum$Var1, times= 2)) %>% filter(!AFT %in% c('Abandoned agricultural land', 
                          'Cattle, Intensive', 'Unoccupied'))

arson.sum$value     <- ifelse(arson.sum$key == 'All.arson', 
                          arson.sum$value / sum(arson.sum$value[arson.sum$key == 'All.arson']), 
                          arson.sum$value / sum(arson.sum$value[arson.sum$key != 'All.arson']))

arson.sum$key       <- sub('\\.', ' ', arson.sum$key)

arson.sum$AFT       <- ifelse(arson.sum$AFT %in% c(
                      'Cropping, Monoculture, Market-oriented', 
                      'Mixed cropping-livestock small holder, Market-oriented'), 'Market-oriented Small holder', 
                      ifelse(arson.sum$AFT %in% c('Mixed cropping-livestock small holder, Subsistence-oriented', 
                                'Cropping, Monoculture, Subsistence-oriented', 'Agroforestry, Subsistence-oriented'),
                             'Subsistence-oriented Small holder', 
                             ifelse(arson.sum$AFT == 'Forestry, Traditional', 'Hunter gatherer', 
                              ifelse(arson.sum$AFT %in% c('Tourism manager', 'Urban planner'), 'Urban resident' ,as.character(arson.sum$AFT)))))
                                               
                                 
arson.sum           <- arson.sum %>% group_by(.dots = c('key', 'AFT')) %>% 
                          summarise(value = sum(value))
                                                
                          
ggplot(arson.sum, aes(x = AFT, fill = key, y = value)) + theme_classic() +
  geom_col(position = position_dodge(), colour = 'black') +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) + 
  scale_fill_viridis_d() + ylab('Proportion of occurences') + theme(text = element_text(size=14))


############################################################################################################

### 4) Fire use plots

############################################################################################################

### Active fire use

Fire.use$Metric<- ifelse(grepl('median', Fire.use$Behaviour), 'Median fire size', 
                      ifelse(grepl('mean', Fire.use$Behaviour), 'Mean fire size', 
                      ifelse(grepl('min', Fire.use$Behaviour), 'Min fire size', 
                      ifelse(grepl('max', Fire.use$Behaviour), 'Max fire size', Fire.use$Behaviour))))


Fire.use$Fire.use.type <- Simplify.intention(Fire.use$`Fire intention`, Fire.use$AFT)

Fire.use$AFT.simp <- ifelse(grepl('subsistence', tolower(Fire.use$AFT)), 'Subsistence small-holder', 
                          ifelse(grepl('market', tolower(Fire.use$AFT)), 'Market-oriented small-holder',
                            Fire.use$AFT))

Fire.agg <- Fire.use %>% filter(grepl('size', Metric)) %>% 
              mutate('weight' = Combined.N * Combined.stat) %>% 
              mutate('AFT.simp' = ifelse(AFT.simp == 'Forestry, Traditional', 'Hunter gatherer', AFT.simp)) %>%
              group_by(.dots = c('AFT.simp', 'Fire.use.type', 'Metric')) %>%
                summarise('New stat' = sum(weight, na.rm = T) / sum(Combined.N, na.rm = T)) #%>%
                  #mutate('AFT' = factor(AFT.simp, levels = c('Cropping, Intensive', 'Cattle, Extensive', 
                  #'Market-oriented small-holder', 'Subsistence small-holder', 'Cropping, Swidden')))

 
ggplot(Fire.agg %>% filter(Fire.use.type == 'Pyrome management'), 
          aes(x = AFT.simp, y = `New stat`, fill = Metric)) + geom_col(position= position_dodge(), colour = 'black') + scale_colour_viridis_d() +
      theme_classic() + ylab("Fire size (ha)") + 
     scale_y_continuous(trans = pseudo_log_trans(base = 10), 
          breaks = c(1, 10, 100, 1000, 10000))  +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) + 
        scale_fill_viridis_d() +
        theme(text = element_text(size=20))


Fire.agg <- Fire.use %>% filter(grepl('size', Metric)) %>% 
     mutate('weight' = Combined.N * Combined.stat) %>% 
     mutate('AFT.simp' = ifelse(AFT.simp == 'Forestry, Traditional', 'Hunter gatherer', AFT.simp)) %>%
     group_by(.dots = c('AFT.simp', 'Fire.use.type', 'Metric')) %>%
     summarise('New stat' = sum(weight, na.rm = T) / sum(Combined.N, na.rm = T)) %>%
   mutate('AFT' = factor(AFT.simp, levels = c('Cropping, Intensive', 'Cattle, Extensive', 
   'Market-oriented small-holder', 'Subsistence small-holder', 'Cropping, Swidden')))
   
   
   
ggplot(Fire.agg %>% filter(!is.na(AFT) & !AFT %in% c('Hunter gatherer', 'Cropping, Agroecology', 'Pastoralism, Migratory')) %>%
         filter(tolower(Fire.use.type) == 'vegetation clearance'),
       aes(x = Metric, y = `New stat`, fill = AFT)) + 
  geom_col(position= position_dodge(), colour = 'black') + scale_colour_viridis_d() +
  theme_classic() + ylab("Fire size (ha)") + 
  scale_y_continuous(trans = pseudo_log_trans(base = 10), 
                     breaks = c(1, 10, 100, 1000, 10000)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) + 
  scale_fill_viridis_d() +
  theme(text = element_text(size=14))


##########################################################

### Policy

##########################################################

### Policy Plot

policy.sum$Fire.development.stage <- ifelse(policy.sum$AFT %in% c('Pastoralism, Migratory', 'Cropping, Swidden', 'Forestry, Traditional'), 'Pre-industrial', 
                                      ifelse(policy.sum$AFT %in% c('Cattle, Extensive', 'Forestry, Small-scale') | grepl('small', policy.sum$AFT), 'Transition', 
                                        ifelse(policy.sum$AFT %in% c('Cropping, Intensive', 'Forestry, Industrial', 'Cattle, Intensive'), 'Industrial', 
                                          ifelse(policy.sum$AFT %in% c('Urban planner', 'Tourism manager', 'Biodiversity conservation'), 'Post-industrial', NA))))

policy.sum$score   <- policy.sum$Reported.N * policy.sum$Reported.stat

policy.development <- policy.sum %>% filter(!is.na(Fire.development.stage)) %>%
                        group_by(.dots = c('Fire.development.stage', 'Behaviour')) %>% 
                          summarise('Reported.N' = sum(Reported.N, na.rm = T), 
                                    'Reported.stat' = sum(score, na.rm = T))

policy.development$Reported.stat          <- policy.development$Reported.stat / policy.development$Reported.N
policy.development$Fire.development.stage <- factor(policy.development$Fire.development.stage, 
                                                    levels = c('Pre-industrial', 'Transition', 
                                                      'Industrial', 'Post-industrial'), ordered = T)

policy.development$`Policy Type` <- ifelse(policy.development$Behaviour == 'Fire.banned', 'Fire Ban', 
                                           ifelse(policy.development$Behaviour == 'Fire.restricted', 'Fire Restricted', 
                                            ifelse(policy.development$Behaviour == 'Incentives', 'Incentives', NA)))

policy.development$Reported.N   <- policy.development$Reported.N * policy.development$Reported.stat


ggplot(policy.development, aes(x = Fire.development.stage, y = Reported.N , fill = `Policy Type`)) + 
  geom_col(position = position_dodge(), colour = 'black') + theme_classic() + ylab('Number of cases') + xlab('Fire development stage') +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) + scale_fill_viridis_d() + theme(text = element_text(size=14))
  

#####################################################################################################

### 4) Maps

#####################################################################################################

Landuse$`Fire development stage`[is.na(Landuse$`Fire development stage`)] <- 'ND'

recordinfo$`Data Source` <- Simplify.source()
est_fire$`Fire use type` <- Simplify.intention()

est_fire$Presence <- est_fire$`Presence / Absence` == 'Presence'

                                   

map.behaviour('Land use', ggcolour = '`Data Source`', ggshape = '`Data Source`')


map.behaviour('Fire', ggshape = '`Fire use type`', ggcolour = '`Fire use type`', 
              choose = "`Fire use type` != 'Other' & Presence == TRUE")


map.behaviour('Suppression', ggshape = '`Fire control (0-3)`', 
              ggcolour = "AFT", 
              choose = "!is.na(`Fire control (0-3)`) & `Fire control (0-3)` != 'ND'")


map.behaviour('Policy', ggcolour = 'AFT', 
              ggshape = '`Fire banned`', 
              choose = "!AFT %in% c('ND', 'All', 
   'Agroforestry, Market-oriented', 'Agroforestry, Subsistence-oriented', 'All', 'ND', 'Mixed cropping-livestock small holder, Market-oriented', 
              'Mixed cropping-livestock small holder, Subsistence-oriented')")


###########################################################

### 6) Fire suppression: WUI and TFK

###########################################################

Urban.key         <- Landuse[which(apply(Landuse[, 4:7], 1, function(x) {any(grepl('Urban', x))})), ]$`Case Study ID`

WUI.sum           <- plyr::rbind.fill(lapply(colnames(sup)[c(8, 10, 12)], function(x) {
                            summarise.behaviour(type = "Suppression", 
                              behaviour = x, grouping = "AFT", choose = '`Case Study ID` %in% Urban.key')}))

WUI.sum[, c(2:6)] <- apply(WUI.sum[, 2:6], 2, function(x) {ifelse(is.na(x), 0, x)}) 

Suppression.plot        <- gather(WUI.sum[, 2:6])
Suppression.plot$AFT    <- rep(WUI.sum$AFT, times = 5)
Suppression.plot$Metric <- rep(WUI.sum$Behaviour, times  = 5)
WUI.sum$Total           <- apply(WUI.sum, 1, function(x) {sum(as.numeric(x[2:6]), na.rm = T)})
Suppression.plot$Total  <- rep(WUI.sum$Total, times = 5)
Suppression.plot        <- Suppression.plot %>% 
                              filter(tolower(AFT) %in% c("abandoned agricultural land", 
                              "urban planner", "fire suppression agent", 
                              'forestry industrial', 'tourism manager'))

Suppression.plot$Metric <- ifelse(grepl('control', Suppression.plot$Metric), 'Control',
                            ifelse(grepl('extinction', Suppression.plot$Metric), 'Extinction',
                              ifelse(grepl('prevention', Suppression.plot$Metric), 'Prevention', 
                                NA)))

Suppression.plot$key   <- factor(ifelse(grepl('0', Suppression.plot$key), 'None',
                            ifelse(grepl('1', Suppression.plot$key), 'Limited',
                             ifelse(grepl('2', Suppression.plot$key), 'Moderate', 
                              ifelse(grepl('3', Suppression.plot$key), 'Intensive', NA)))), 
                              levels= c('None', 'Limited', 'Moderate', 'Intensive'), ordered = T)

Suppression.plot <- Suppression.plot %>% 
                      mutate('AFT' = ifelse(AFT == 'Urban planner', 'Urban resident', AFT))

ggplot(Suppression.plot[!is.na(Suppression.plot$key), ], aes(x = AFT, y = as.numeric(value) / Total, fill = key)) + 
  geom_col(position = position_dodge(), colour = 'black') + ylab('Frequency') +
  facet_grid(Metric~.) + theme_classic() + theme(text = element_text(size=20)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) + scale_fill_viridis_d()

############################################

### TFK

############################################

Preind.key            <- Landuse$`Case Study ID`[Landuse$`Fire development stage` == 'Pre-industrial']
TFK.sum               <- plyr::rbind.fill(lapply(colnames(sup)[c(8, 10, 12)], function(x) {
                                  summarise.behaviour(type = "Suppression", 
                                   behaviour = x, grouping = "AFT", choose = '`Case Study ID` %in% Preind.key')}))

TFK.sum               <- TFK.sum[, c(1, 7, 6, 2:5)]
TFK.sum[, c(3:7)]     <- apply(TFK.sum[, 3:7], 2, function(x) {ifelse(is.na(x), 0, x)}) 
TFK.sum$Preindustrial <- 'Pre-industrial'

other.sum             <- plyr::rbind.fill(lapply(colnames(sup)[c(8, 10, 12)], function(x) {
                           summarise.behaviour(type = "Suppression", 
                           behaviour = x, grouping = "AFT", choose = '!(`Case Study ID` %in% Preind.key)')}))

other.sum               <- other.sum[, c(1, 7, 2:6)]
other.sum[, c(3:7)]     <- apply(other.sum[, 3:7], 2, function(x) {ifelse(is.na(x), 0, x)}) 
other.sum$Preindustrial <- 'Industrial'

TFK.sum                 <- rbind(TFK.sum, other.sum)
TFK.sum                 <- data.frame(TFK.sum %>% filter(AFT %in% c(
                            'Forestry, Traditional', 'Pastoralism, Migratory', 'Cropping, Swidden', 
                            'Cropping, Monoculture, Subsistence-oriented')))

Suppression.plot        <- gather(TFK.sum[, 3:7])
Suppression.plot$AFT    <- rep(TFK.sum$AFT, times = 5)
Suppression.plot$Metric <- rep(TFK.sum$Behaviour, times  = 5)
TFK.sum$Total           <- apply(TFK.sum, 1, function(x) {sum(as.numeric(x[3:6]), na.rm = T)})
Suppression.plot$Total  <- rep(TFK.sum$Total, times = 5)
Suppression.plot$Preindustrial <- rep(TFK.sum$Preindustrial, times = 5)

Suppression.plot$Metric <- ifelse(grepl('control', Suppression.plot$Metric), 'Fire Control',
                            ifelse(grepl('extinction', Suppression.plot$Metric), 'Fire Extinction',
                            ifelse(grepl('prevention', Suppression.plot$Metric), 'Fire Prevention', 
                            NA)))

Suppression.plot$key   <- factor(ifelse(grepl('0', Suppression.plot$key), 'None',
                             ifelse(grepl('1', Suppression.plot$key), 'Limited',
                              ifelse(grepl('2', Suppression.plot$key), 'Traditional', 
                              ifelse(grepl('3', Suppression.plot$key), 'Intensive', NA)))), 
                              levels= c('None', 'Limited', 'Traditional', 'Intensive'), ordered = T)

Suppression.plot       <- Suppression.plot %>% mutate('AFT' = sub('-oriented', "", AFT)) %>%
                            mutate('AFT' = sub('Mixed cropping-livestock', 'Mixed', AFT)) %>%
                            mutate('AFT' = ifelse(AFT == 'Biodiversity conservation', 'Conservationist', 
                          ifelse(AFT == 'Forestry, Traditional', 'Hunter gatherer', AFT))) %>% 
                          mutate('AFT' = sub("Cropping, Swidden", "Swidden", AFT)) %>%
                            mutate('AFT' = sub("Cropping, Monoculture, Subsistence", "Subsistence small holder", AFT))  %>%
                            mutate('AFT' = factor(AFT, levels = c('Swidden', 
                                        'Subsistence small holder', 'Cropping,Monoculture, Market', 
                                        'Intensive', 'Pastoralism, Migratory', 'Cattle, Extensive',
                                        'Mixed small holder, Subsistence', 'Mixed small holder, Market', 
                                        'Cattle, Intensive', 'Hunter gatherer', 'Forestry, Small-scale', 
                                        'Forestry, Industrial', 'Conservationist', 'Fire suppression agent'), ordered = T))


ggplot(Suppression.plot[!is.na(Suppression.plot$key), ], aes(x = AFT, y = value, fill = key)) + 
  geom_col(position = position_dodge(), colour = 'black') + ylab('Count') +
  facet_grid(Preindustrial~Metric, scales = 'free_y') + theme_classic() + theme(text = element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) + scale_fill_viridis_d()


#####################################################################################################

### 7) Policy

#####################################################################################################

### Do bans cause less effective fire use?

Ban.key                       <- policy$`Case Study ID`[policy$`Fire banned` == 'Yes']
ban.sum                       <- plyr::rbind.fill(lapply(colnames(sup)[c(8, 10, 12)], function(x) {
                                  summarise.behaviour(type = "Suppression", 
                                    behaviour = x, grouping = "AFT", choose = '`Case Study ID` %in% Ban.key')}))

ban.sum               <- ban.sum[, c(1, 7, 6, 3, 4, 5, 2)]
ban.sum[, c(3:7)]     <- apply(ban.sum[, 3:7], 2, function(x) {ifelse(is.na(x), 0, x)}) 
ban.sum$Ban           <- 'Fire banned'


legal.sum               <- plyr::rbind.fill(lapply(colnames(sup)[c(8, 10, 12)], function(x) {
                          summarise.behaviour(type = "Suppression", 
                            behaviour = x, grouping = "AFT", choose = '!`Case Study ID` %in% Ban.key')}))

legal.sum               <- legal.sum[, c(1, 7, 2, 5, 4, 6, 3)]
legal.sum[, c(3:7)]     <- apply(legal.sum[, 3:7], 2, function(x) {ifelse(is.na(x), 0, x)}) 
legal.sum$Ban           <- 'Not banned'

ban.sum                 <- rbind(ban.sum, legal.sum)
ban.sum                 <- data.frame(ban.sum  %>% filter(AFT %in% c(
                            'Forestry, Traditional', 'Pastoralism, Migratory', 
                            'Cropping, Swidden', 'Cattle, Extensive')))


Suppression.plot        <- gather(ban.sum[, 3:7])
Suppression.plot$AFT    <- rep(ban.sum$AFT, times = 5)
Suppression.plot$Metric <- rep(ban.sum$Behaviour, times  = 5)
ban.sum$Total           <- apply(ban.sum, 1, function(x) {sum(as.numeric(x[3:6]), na.rm = T)})
Suppression.plot$Total  <- rep(ban.sum$Total, times = 5)
Suppression.plot$Ban    <- rep(ban.sum$Ban, times = 5)

Suppression.plot$Metric <- ifelse(grepl('control', Suppression.plot$Metric), 'Fire Control',
                              ifelse(grepl('extinction', Suppression.plot$Metric), 'Fire Extinction',
                                ifelse(grepl('prevention', Suppression.plot$Metric), 'Fire Prevention', 
                                                NA)))

Suppression.plot$key    <- factor(ifelse(grepl('0', Suppression.plot$key), 'None',
                                ifelse(grepl('1', Suppression.plot$key), 'Limited',
                                ifelse(grepl('2', Suppression.plot$key), 'Traditional', 
                                ifelse(grepl('3', Suppression.plot$key), 'Intensive', NA)))), 
                                 levels= c('None', 'Limited', 'Traditional', 'Intensive'), ordered = T)

Suppression.plot$AFT <- sub("Forestry, Traditional", "Hunter gatherer", Suppression.plot$AFT)

ggplot(Suppression.plot[!is.na(Suppression.plot$key) & Suppression.plot$Metric == 'Fire Control', ], 
       aes(x = AFT, y = value / Total, fill = key)) + 
  geom_col(position = position_dodge(), colour = 'black') + ylab('Proportion of reported behaviours') +
  facet_grid(Ban~.) + theme_classic() + theme(text = element_text(size=18)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) + scale_fill_viridis_d()

###########

### Policy change?

###########

policy.change <- policy[duplicated(policy[, c(1, 3, 4, 6)]), ]
policy.change <- policy.change %>% group_by(.dots = c('`Case Study ID`',"`Fire type`", "`Fire intention`","`Fire banned`")) %>%
                      summarise('ban' = n())
            

policy.change <- policy %>% filter(!is.na(`Study year`))



#########################################################################

### Data sources

#########################################################################

recordinfo$`Data Source` <- Simplify.source()


source.simp <- recordinfo %>% group_by(`Data Source`) %>% 
                summarise('Source' = n())

source.simp$`Data Source` <- factor(source.simp$`Data Source`, 
                              levels = c('Primary', 'Secondary', 'Remote sensing', 
                                         'Mixed', 'Literature review', 'Other'), 
                                      ordered = T)

ggplot(source.simp, aes(x = `Data Source`, y = Source)) + 
  geom_col(colour = 'black', fill = '#404788FF') +
  scale_fill_viridis_d() + theme_classic() + ylab('Number of case studies') + 
  theme(text = element_text(size=14)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) 
