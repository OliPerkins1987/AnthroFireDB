
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

setwd('C:/Users/Oli/Documents/PhD/Model development/Data')
dbstring   <- 'Database v1_4 23062020.xlsx'

######################################################################

### Read data

######################################################################

recordinfo <- read.xlsx(dbstring, sheet = 1, startRow = 1, colNames = TRUE)
recordinfo <- recordinfo[, 1:16]
recordinfo[, c(5, 6, 11, 14, 15)] <- apply(recordinfo[, c(5, 6, 10, 13, 15)], 2, 
                                           function(x) {ifelse(x == 'ND', NA, x)})

landuse    <- read.xlsx(dbstring, sheet = 2, startRow = 1, colNames = TRUE)
landuse[, c(2, 3, 5, 6, 7, 8:19)] <- apply(landuse[, c(2, 3, 5, 6, 7, 8:19)], 2, 
                                           function(x) {ifelse(x == 'ND', NA, x)})

reported_fire                   <- read.xlsx(dbstring, sheet = 3, startRow = 1, colNames = TRUE)
reported_fire[, c(6, 7, 10:28)] <- apply(reported_fire[, c(6, 7, 10:28)], 2, function(x) {ifelse(x == 'ND', NA, x)})
reported_fire[, c(10:25)]       <- apply(reported_fire[, c(10:25)], 2, function(x) {as.numeric(as.character(x))})
reported_fire[, c(26, 27)]      <- apply(reported_fire[, c(26, 27)], 2, function(x){as.factor(x)})

est_fire                        <- read.xlsx(dbstring, sheet = 4, startRow = 1, colNames = TRUE)
est_fire[, c(6, 7, 10:26)]      <- apply(est_fire[, c(6, 7, 10:26)], 2, function(x) {ifelse(x == 'ND', NA, x)})
est_fire[, c(10:25)]            <- apply(est_fire[, c(10:25)], 2, function(x){as.factor(x)})
est_fire                        <- est_fire[1:nrow(reported_fire), ]

sup    <- read.xlsx(dbstring, sheet = 5, startRow = 1, colNames = TRUE)
policy <- read.xlsx(dbstring, sheet = 6, startRow = 1, colNames = TRUE)

############################################################################################

### data checks

############################################################################################

which(reported_fire[, 26] != est_fire[, 24])
which(reported_fire[, 27] != est_fire[, 25])
table(gather(landuse[, c(4:7)])[, 2])
table(landuse$Land.tenure)

table(est_fire$AFT)
table(reported_fire$AFT)

#############################################################################################

### data cleaning

#############################################################################################

data_clean <- function(errors, fixes, data) {
  
  stopifnot(exprs = {length(errors) == length(fixes)})
  
  x <- data
  
  for(i in 1:length(errors)) {
  
  x <- apply(x, 2, function(y) {ifelse(y == errors[i], fixes[i], y)})
    
  }
  
  return(x)
  
}

landuse[, 4:7] <-  data_clean(c('Agro-forestry, subsistence-oriented', 'Agroforestry, market-oriented', 'Agroforestry, industrial', 
                                'Biodiversity Conservation', 'Cattle, Dairy', 'Cropping Swidden', 'Cattle, extensive', 
                                'Cropping, intensive', 'Cropping, Monoculture', 'Cropping, Monoculture, market-oriented', 
                                'Hunting, traditional', 'Pastroralism, migratory', 'Pasture, Extensive', 'Forestry, industrial', 
                                'Logging, commercial', 'Forestry, commercial', 'Cropping, Extensive', 'Agroforestry, subsistence-oriented', 
                                'Cattle Extensive', 'Cropping, swidden', 'Forestry Industrial', 'Forestry Commercial', 'Cropping, Plantation, Market-oriented'), 
                              c('Agroforestry, Subsistence-oriented', 'Agroforestry, Market-oriented', 'Agroforestry, Market-oriented', 
                                'Biodiversity conservation', 'Cattle, Extensive', 'Cropping, Swidden', 'Cattle Extensive', 
                                'Cropping, Intensive', 'Cropping, Monoculture, Market-oriented', 'Cropping, Monoculture, Market-oriented', 
                                'Forestry, Traditional', 'Pastoralism, migratory', 'Cattle, Extensive', 'Forestry Industrial', 'Forestry, Industrial', 
                                'Forestry, Industrial', 'Cropping, Monoculture, Subsistence-oriented', 'Agroforestry, Subsistence-oriented', 
                                'Cattle, Extensive', 'Cropping, Swidden', 'Forestry, Industrial', 'Forestry, Industrial', 'Cropping, Monoculture, Market-oriented'),
                              landuse[, 4:7]) 

reported_fire[, 5:6] <-  data_clean(c('Agro-forestry, subsistence-oriented', 'Agroforestry, market-oriented', 'Agroforestry, industrial', 
                                'Biodiversity Conservation', 'Cattle, Dairy', 'Cropping Swidden', 'Cattle, extensive', 
                                'Cropping, intensive', 'Cropping, Monoculture', 'Cropping, Monoculture, market-oriented', 
                                'Hunting, traditional', 'Pastroralism, migratory', 'Pasture, Extensive', 'Forestry, industrial', 
                                'Logging, commercial', 'Forestry, commercial', 'Cropping, Extensive', 'Agroforestry, subsistence-oriented', 
                                'Cattle Extensive', 'Cropping, swidden', 'Forestry Industrial', 'Forestry Commercial', 'Cropping, Plantation, Market-oriented'), 
                              c('Agroforestry, Subsistence-oriented', 'Agroforestry, Market-oriented', 'Agroforestry, Market-oriented', 
                                'Biodiversity conservation', 'Cattle, Extensive', 'Cropping, Swidden', 'Cattle Extensive', 
                                'Cropping, Intensive', 'Cropping, Monoculture, Market-oriented', 'Cropping, Monoculture, Market-oriented', 
                                'Forestry, Traditional', 'Pastoralism, migratory', 'Cattle, Extensive', 'Forestry Industrial', 'Forestry, Industrial', 
                                'Forestry, Industrial', 'Cropping, Monoculture, Subsistence-oriented', 'Agroforestry, Subsistence-oriented', 
                                'Cattle, Extensive', 'Cropping, Swidden', 'Forestry, Industrial', 'Forestry, Industrial', 'Cropping, Monoculture, Market-oriented'),
                              reported_fire[, 5:6]) 

est_fire[, 5:6] <-  data_clean(c('Agro-forestry, subsistence-oriented', 'Agroforestry, market-oriented', 'Agroforestry, industrial', 
                                      'Biodiversity Conservation', 'Cattle, Dairy', 'Cropping Swidden', 'Cattle, extensive', 
                                      'Cropping, intensive', 'Cropping, Monoculture', 'Cropping, Monoculture, market-oriented', 
                                      'Hunting, traditional', 'Pastroralism, migratory', 'Pasture, Extensive', 'Forestry, industrial', 
                                      'Logging, commercial', 'Forestry, commercial', 'Cropping, Extensive', 'Agroforestry, subsistence-oriented', 
                                      'Cattle Extensive', 'Cropping, swidden', 'Forestry Industrial', 'Forestry Commercial', 'Cropping, Plantation, Market-oriented'), 
                                    c('Agroforestry, Subsistence-oriented', 'Agroforestry, Market-oriented', 'Agroforestry, Market-oriented', 
                                      'Biodiversity conservation', 'Cattle, Extensive', 'Cropping, Swidden', 'Cattle Extensive', 
                                      'Cropping, Intensive', 'Cropping, Monoculture, Market-oriented', 'Cropping, Monoculture, Market-oriented', 
                                      'Forestry, Traditional', 'Pastoralism, migratory', 'Cattle, Extensive', 'Forestry Industrial', 'Forestry, Industrial', 
                                      'Forestry, Industrial', 'Cropping, Monoculture, Subsistence-oriented', 'Agroforestry, Subsistence-oriented', 
                                      'Cattle, Extensive', 'Cropping, Swidden', 'Forestry, Industrial', 'Forestry, Industrial', 'Cropping, Monoculture, Market-oriented'),
                                    est_fire[, 5:6]) 

#############################################################################################

### 1) Fire creation summary by AFT

#############################################################################################


###############################################

### Reported fire behaviour

###############################################


return        <- reported_fire %>% dplyr::group_by(AFT) %>% filter(`Fire.return.period.(years)` != 0) %>%
                    summarise('Fire return' = mean(`Fire.return.period.(years)`, na.rm = T))

size          <- reported_fire %>% dplyr::group_by(AFT) %>% filter(`Presence./.Absence` == 'Presence') %>%
  summarise('Intended Min'  =  mean(`Intended.fire.size.min.(ha)`, na.rm = T),
            'Actual Min'  =  mean(`Actual.fire.size.min.(ha)`, na.rm = T),
            'Intended Max'  =  mean(`Intended.fire.size.max.(ha)`, na.rm = T),
            'Actual Max'    = mean(`Actual.fire.size.max.(ha)`, na.rm = T),
            'Intended Mean' =  mean(`Intended.fire.size.mean.(ha)`, na.rm = T), 
            'Actual Mean'   = mean(`Actual.fire.size.mean.(ha)`, na.rm = T),
            'Intended Median' = mean(`Intended.fire.size.median.(ha)`, na.rm = T), 
            'Actual Median'   = mean(`Actual.fire.size.median.(ha)`, na.rm = T))

return_by_use <- reported_fire %>% dplyr::group_by(.dots = c('Fire.intention', 'AFT')) %>% 
                    filter(`Fire.return.period.(years)` != 0) %>%
                      summarise('Fire return' = mean(`Fire.return.period.(years)`, na.rm = T))

size_byuse    <- reported_fire %>% dplyr::group_by(.dots = c('Fire.intention', 'AFT')) %>% filter(`Presence./.Absence` == 'Presence') %>%
                    summarise('Intended Min'  =  mean(`Intended.fire.size.min.(ha)`, na.rm = T),
                              'Actual Min'  =  mean(`Actual.fire.size.min.(ha)`, na.rm = T),
                              'Intended Max'  =  mean(`Intended.fire.size.max.(ha)`, na.rm = T),
                              'Actual Max'    = mean(`Actual.fire.size.max.(ha)`, na.rm = T),
                              'Intended Mean' =  mean(`Intended.fire.size.mean.(ha)`, na.rm = T), 
                              'Actual Mean'   = mean(`Actual.fire.size.mean.(ha)`, na.rm = T),
                              'Intended Median' = mean(`Intended.fire.size.median.(ha)`, na.rm = T), 
                              'Actual Median'   = mean(`Actual.fire.size.median.(ha)`, na.rm = T))

###########################

### estimated fire use

##########################

Mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}

est_return          <- est_fire %>% dplyr::group_by(AFT) %>% filter(`Presence./.Absence` == 'Presence') %>%
  summarise('Fire return' = Mode(`Fire.return.period.(years)`))

est_size            <- est_fire %>% dplyr::group_by(AFT) %>% filter(`Presence./.Absence` == 'Presence') %>%
  summarise('Intended Min'  =  Mode(`Intended.fire.size.min.(ha)`),
            'Actual Min'  =  Mode(`Actual.fire.size.min.(ha)`),
            'Intended Max'  =  Mode(`Intended.fire.size.max.(ha)`),
            'Actual Max'    = Mode(`Actual.fire.size.max.(ha)`),
            'Intended Mean' =  Mode(`Intended.fire.size.mean.(ha)`), 
            'Actual Mean'   = Mode(`Actual.fire.size.mean.(ha)`),
            'Intended Median' = Mode(`Intended.fire.size.median.(ha)`), 
            'Actual Median'   = Mode(`Actual.fire.size.median.(ha)`))

est_size_byuse    <- est_fire %>% dplyr::group_by(.dots = c('Fire.intention', 'AFT')) %>% 
  filter(`Presence./.Absence` == 'Presence') %>%
  summarise('Intended Min'  =  Mode(`Intended.fire.size.min.(ha)`),
            'Actual Min'  =  Mode(`Actual.fire.size.min.(ha)`),
            'Intended Max'  =  Mode(`Intended.fire.size.max.(ha)`),
            'Actual Max'    = Mode(`Actual.fire.size.max.(ha)`),
            'Intended Mean' =  Mode(`Intended.fire.size.mean.(ha)`), 
            'Actual Mean'   = Mode(`Actual.fire.size.mean.(ha)`),
            'Intended Median' = Mode(`Intended.fire.size.median.(ha)`), 
            'Actual Median'   = Mode(`Actual.fire.size.median.(ha)`))

est_size_n    <- est_fire %>% dplyr::group_by(.dots = c('Fire.intention', 'AFT')) %>% 
  filter(`Presence./.Absence` == 'Presence') %>%
         summarise('Intended Mean' =  n(), 
                   'Actual Mean'   = n(),
                   'Intended Median' = n(), 
                   'Actual Median'   = n())



##############################################################################

### ignition density

##############################################################################


igs <- reported_fire %>% group_by(Case.study) %>% summarise('ignitions' = sum(`Number.of.fires.(#.km2-1)`, 
                                                                       na.rm = T))

mean(igs$ignitions[igs$ignitions > 0])
median(igs$ignitions[igs$ignitions > 0])

igs.est <- est_fire %>% filter(!`Number.of.fires.(#.km2-1)` %in% c('ND', NA, '0'))

igs.tab <- table(est_fire$`Number.of.fires.(#.km2-1)`)
igs.tab <- as.numeric(igs.tab)

### takes the mean value of the ignition density bins

(igs.tab[2]*0.005 + igs.tab[3]*0.018 + igs.tab[4]*0.0375 + igs.tab[5]*0.075 + igs.tab[6]*0.15 + igs.tab[7]*0.35 + igs.tab[8]*1.25 + igs.tab[9]*3.5)/sum(igs.tab[-1])


############################################################################

### Develop true uncertainty range from bins

############################################################################

uncertainty.range <- function(uncert.frame = est_size_frame, quants = c(0.025, 0.5, 0.975), 
                              colkey = 4:18) {

  ### function to get uncertainty range from estimates
  ### assumes values in bins are uniformly dist
  
  calc.uncert <- function(dat = uncert.frame, qs = quants, cols = colkey, iter = 500) {

    
    n       <- sum(as.numeric(dat))
    weights <- as.numeric(dat) / n

    cats        <- names(dat)[weights != 0]
    weights     <- weights[weights != 0]
    
    min.vals    <- as.numeric(sub("\\-.*", "", cats))
    max.vals    <- as.numeric(sub(".*-", "", cats))

    #print(paste(min.vals, max.vals, cats, weights), sep ='; ')
    
    samples     <- mapply(function(x, y, w) {runif(ceiling(iter*w), x, y)}, x = min.vals, y = max.vals, MoreArgs = list(w = weights))

    samples    <- unlist(samples)
    
    
    print(unlist(list(quantile(samples, qs[1], na.rm = TRUE), quantile(samples, qs[2], na.rm = TRUE), 
                      quantile(samples, qs[3], na.rm = TRUE))))
    
    return(unlist(list(quantile(samples, qs[1], na.rm = TRUE), quantile(samples, qs[2], na.rm = TRUE), 
           quantile(samples, qs[3], na.rm = TRUE))))
    
  }
  
  return(apply(uncert.frame[, colkey], 1, calc.uncert))
  
}


### plot uncertainty ranges

est_size_result <- est_size_frame[, 1:3]
est_size_result[, 4:6] <- data.frame(t(teg))

est_size_result <- est_size_result[!est_size_result$AFT %in% c('ND', NA) & !is.na(est_size_result$Fire.metric), ]

ggplot(est_size_result[est_size_result$Fire.metric == "Actual.fire.size.mean.(ha)", ], aes(x = AFT, fill = Fire.intention, y = X50.)) + 
  geom_col(position = position_dodge(), colour = 'black') + scale_y_sqrt(breaks = c(2.5, 10, 25, 50, 100, 250, 500, 1000)) + 
  theme_classic() + scale_fill_viridis_d() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Fire size (ha)') +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), width = 1, position = position_dodge())


######################################################################################################

### 2) Suppression and policy

######################################################################################################

sup %>% split(sup$AFT) %>% lapply(function(x) {table(x$`Fire.control.(0-3)`)})
sup %>% split(sup$AFT) %>% lapply(function(x) {table(x$`Fire.prevention.(0-3)`)})
sup %>% split(sup$AFT) %>% lapply(function(x) {table(x$`Fire.extinction.(0-3)`)})

policy %>% split(policy$AFT) %>% lapply(function(x) {table(x$Incentives)})
policy %>% split(policy$AFT) %>% lapply(function(x) {table(x$Fire.restricted)})
policy %>% split(policy$AFT) %>% lapply(function(x) {table(x$Fire.banned)})


#####################################################################################################

### 3) Maps

#####################################################################################################

map.behaviour <- function(type = c('Records', 'Land use', 'Fire', 'Suppression', 'Policy'), 
                          ggcolour = NULL, ggshape = NULL, choose = NULL) {
  
  require(tidyverse)
  require(ggplot2)
  
  ggcolour <- gsub(' ', '.', ggcolour)
  ggshape  <- gsub(' ', '.', ggshape)
  
  ggcolour <- gsub('(', '.', ggcolour, fixed = T)
  ggshape  <- gsub('(', '.', ggshape, fixed = T)
  
  ggcolour <- gsub(')', '.', ggcolour, fixed = T)
  ggshape  <- gsub(')', '.', ggshape, fixed = T)
  
  ## function to map attributes from the database
  
  ## Fire data uses discrete estimated bins
  
  ## choose should be a string containing the contents of a dplyr filter expression
  
  ## ggcolour and ggshape should be the intended column names of the mapping function
  
  if(type == 'Records') {
    
    dat <- recordinfo
    
  } else if(type == 'Land use') {
    
    dat <- merge(recordinfo, landuse, by = 'Case.Study.ID')
    
  } else if (type == 'Fire') {
  
    dat <- merge(recordinfo, est_fire, by = 'Case.Study.ID') 
  
  } else if (type == 'Suppression') {
    
    dat <- merge(recordinfo, sup, by = 'Case.Study.ID') 
    
  } else if (type =='Policy') {
    
    dat <- merge(recordinfo, policy, by = 'Case.Study.ID')
    
  } else {
    
    stop('Map type specified incorrectly')
    
  }
    
  #### Filter
  
  if (!is.null(choose)) {
  
  dat      <- data.frame(dat %>% filter(eval(parse(text = choose))))
  
  }
  
  dat$Latitude <- as.numeric(dat$Latitude)
  dat$Longitude<- as.numeric(dat$Longitude) 
  colnames(dat)<- gsub(' ', '.', colnames(dat))
  colnames(dat)<- gsub(')', '.', colnames(dat), fixed = T)
  colnames(dat)<- gsub('(', '.', colnames(dat), fixed = T)

   
  g <- ggplot(dat, aes_string(x = 'Longitude', y = 'Latitude', colour = ggcolour, shape = ggshape)) +
    geom_point(size = 1.5) + theme_classic() + borders() + scale_colour_viridis_d()
   
    print(g)

    return()
  
}

map.behaviour('Fire', ggcolour = 'Intended.fire.size.mean.(ha)', 
              ggshape = 'AFT', 
              choose = "AFT %in% c('Cropping, Swidden', 'Cattle, Extensive', 'Pastoralism, Migratory', 'Forestry, Traditional')")

map.behaviour('Policy', ggcolour = 'Fire.banned', 
              ggshape = 'AFT', 
              choose = "AFT %in% c('Cropping, Swidden', 'Cattle, Extensive', 'Pastoralism, Migratory', 'Forestry, Traditional')")

