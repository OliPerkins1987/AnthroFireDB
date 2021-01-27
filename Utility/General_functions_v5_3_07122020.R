test

###############################################################################

### Code contains functions to support analysis of DAFI, the global database anthropogenic fire impacts

### Author: Oli Perkins, 6 October 2020

###############################################################################


### Contents

# 1) Load data

# 2a) Convert continuous values to binned values (reported -> estimated fire)
# 2b) Convert binned values to continuous values (estimated -> reported fire)

#3 Produce report on data availability in DAFI (needs tidying)

#4 Code to reset numbering - for data import via 3rd party contributors


############################################################

### 1) Load data

############################################################



load.db <- function(r.notes = TRUE, env = .GlobalEnv) {

  rec                               <- read_excel(dbstring, sheet = 1)
  if(r.notes == TRUE){ rec          <- data.frame(rec[, 1:16])}
  rec[, c(5, 6, 11, 14, 15)]        <- apply(rec[, c(5, 6, 11, 14, 15)], 2, 
                                              function(x) {ifelse(x == 'ND', NA, x)})
  
  assign('recordinfo', rec, envir = env)
  
  LULC                              <- read_excel(dbstring, sheet = 2)
  LULC[, c(2, 3, 5, 6, 7, 8:19)]    <- apply(LULC[, c(2, 3, 5, 6, 7, 8:19)], 2, 
                                              function(x) {ifelse(x == 'ND', NA, x)})
  
  assign('Landuse', LULC, envir = env)
  
  fire                            <- read_excel(dbstring, sheet =  3)
  fire[, c(6, 7, 10:28)]          <- apply(fire[, c(6, 7, 10:28)], 2, function(x) {ifelse(x == 'ND', NA, x)})
  fire[, c(10:26)]                <- apply(fire[, c(10:26)], 2, function(x) {as.numeric(as.character(x))})
  fire[, c(27, 28)]               <- lapply(fire[, c(27, 28)], function(x){as.factor(x)})
  
  assign('reported_fire', fire, envir = env)
  
  est_fire                        <- read_excel(dbstring, sheet = 4)
  est_fire[, c(6, 7, 10:27)]      <- apply(est_fire[, c(6, 7, 10:27)], 2, function(x) {ifelse(x == 'ND', NA, x)})
  est_fire[, c(10:26)]            <- lapply(est_fire[, c(10:26)], function(x){as.factor(x)})
  est_fire                        <- est_fire[1:nrow(reported_fire), ]
  est_fire$`Fire intention`       <- gsub("&amp;", "&", est_fire$`Fire intention`)
  
  assign('est_fire', est_fire, envir = env)

  sup                             <-  read_excel(dbstring, sheet = 5)
  sup[, -c(11, 13, 14)]           <-  lapply(sup[, -c(11, 13, 14)],factor) 
  
  assign('sup', sup , envir = env)
  
  firepol                         <- read_excel(dbstring, sheet = 6)
  firepol[, 1:22]                 <- lapply(firepol[, 1:22], factor)
  
  assign('policy', firepol, envir = env)
  
  
}


##############################################################

### 2a) Convert continuous values to binned values in the Database

##############################################################

bin.vals <- function(data, metric = 'Size') {
  
  #metric should be either 'Size', 'Count', or 'BApct'
  
  catfunc <- function(x) {
    
    ### takes a numeric vector
    
    if(metric == 'Size') {
    
    y <- ifelse(x == 0, '0',
         ifelse(x <= 1, '0-1', 
         ifelse(x <= 2, '1-2', 
         ifelse(x <= 5, '2-5', 
         ifelse(x <= 10, '5-10', 
         ifelse(x <= 20 , '10-20', 
         ifelse(x <= 50, '20-50', 
         ifelse(x <= 100, '50-100',
         ifelse(x <= 200, '100-200',
         ifelse(x <= 500, '200-500',
         ifelse(x <= 1000, '500-1000', 
         ifelse(x <= 2500, '1000-2500',
         ifelse(x <= 5000, '2500-5000',
         ifelse(x <= 10000, '5000-10000',
         ifelse(x <= 25000, '10000-25000', 
         ifelse(x >25000, '25000-', 
    x))))))))))))))))
    
    } else if(metric == 'Count') {
        
      NA
      
    } else if(metric == 'BApct') {
        
      y <- ifelse(x == 0, '0', 
           ifelse(x <= 10, '0-10', 
           ifelse(x <= 20, '10-20', 
           ifelse(x <= 30, '20-30', 
           ifelse(x <= 40 , '30-40', 
           ifelse(x <= 50, '40-50', 
           ifelse(x <= 60, '50-60',
           ifelse(x <= 70, '60-70',
           ifelse(x <= 80, '70-80',
           ifelse(x <= 90, '80-90', 
           ifelse(x <= 100, '90-100',
                x)))))))))))
      
      }
    
    return(y)
    
    
    
    }
    
    return(catfunc(data))

  }
  

##############################################################

### 2b) Convert binned values to continuous

##############################################################


unbin <- function(data, metric = 'Size', method = 'Mean') {
  
  #metric should be either 'Size', 'Count', 'Return' or 'BApct'
  
  catfunc <- function(x) {
    
    ### takes a numeric vector

      top    <- sub(".*-", "", x)
      bottom <- sub("\\-.*", "", x)
      top    <- ifelse(bottom == '25000+', '100000', top)
      bottom <- ifelse(bottom == '25000+', '25000', bottom)

      if(method == 'Mean') {
        
        y <- mean(c(as.numeric(bottom), as.numeric(top)))
        
      } else if (method == 'Random') {
        
        y      <- runif(1, as.numeric(bottom), as.numeric(top))
        
      }

    return(y)
    
    
    
  }
  
  return(lapply(data, catfunc))
  
}


###############################################################

### 2c) Simplify fire intentions

###############################################################

Simplify.intention <- function(x = reported_fire$`Fire intention`, 
                               AFT = reported_fire$AFT) {
  
  y <- ifelse(x == 'Accesibility', 'Other', 
        ifelse(x == 'Accidental', 'Other', 
         ifelse(x == 'Arson', 'Arson', 
          ifelse(x == 'Charcoal production', 'Other', 
           ifelse(x == 'Conservation', 'Pyrome management', 
            ifelse(x == 'Crop field preparation', 'Crop field preparation', 
             ifelse(x == 'Crop residue burning', 'Crop residue burning', 
              ifelse(grepl('Cultural', x), 'Other', 
               ifelse(x == 'Domestic', 'Other', 
                ifelse(x == 'Fishing', 'Hunter gatherer',
                 ifelse(x == 'Forest clearance', 'Vegetation clearance', 
                  ifelse(x == 'Forest management', 'Crop residue burning', 
                   ifelse(x == 'Harvesting of NTFP', 'Hunter gatherer', 
                    ifelse(x == 'Hunting', 'Hunter gatherer', 
                     ifelse(x == 'Land clearance', 'Vegetation clearance', 
                      ifelse(x == 'Pasture renewal', 'Pasture management',
                       ifelse(x == 'Pest management' & AFT %in% c('Pastoralism, Migratory', 'Cattle, Extensive', 'Cattle, Intensive'), 'Pasture management', 
                        ifelse(x == 'Pyrome management', 'Pyrome management', 
                          ifelse(x == 'Rangeland management', 'Pasture management', 
                           ifelse(x == 'Vegetation management', 'Other', NA))))))))))))))))))))
  
  
  return(y) 
  
}

###############################################################

### 2c) Simplify data source

###############################################################

Simplify.source <- function(x = recordinfo$Data.Source.s.) {
  
  y <- ifelse(x == 'Primary', 'Primary', 
        ifelse(x == 'Primary, review', 'Mixed', 
        ifelse(x == 'Primary, remote sensing', 'Remote sensing', 
         ifelse(x == 'Primary, secondary, review', 'Mixed',
         ifelse(x == 'Primary, remote sensing, secondary', 'Mixed',
         ifelse(x == 'Primary, remote sensing, secondary, review', 'Mixed',
         ifelse(x == 'Remote sensing, secondary', 'Remote sensing',
         ifelse(x == 'Remote sensing, secondary ', 'Remote sensing', 
         ifelse(x== 'Primary, secondary', 'Mixed', 
         ifelse(x == 'Review', 'Literature review',
         ifelse(x == 'Other (see notes)', 'Other',
                                                  x)))))))))))
  return(y) 
  
}



###############################################################

#3) Data availability

### Produces a report on availability of data in DAFI by Fire intention and AFT

###############################################################

DB_overview <- function(simp = T, AFT.eval = F) {
  
  require(tidyverse)
  
  
  if(simp == T) {
    
    Simp.fire                  <- reported_fire
    Simp.fire$`Fire intention` <- Simplify.intention()
    Simp.est                   <- est_fire
    Simp.est$`Fire intention`  <- Simplify.intention()
    
    
  } else {
    
    
    Simp.fire <- reported_fire
    Simp.est  <- est_fire
    
  }
  
  
  ### intention summary
  
  if(AFT.eval == F) {
    
    N            <- Simp.fire[, -which(colnames(Simp.fire) %in% c(
      "Actual burned area (ha)", "Intended burned area (ha)"))] %>% group_by(`Fire intention`) %>% 
      filter(`Presence / Absence` == 'Presence') %>% summarise(Instances = n())
    
    
    metrics      <- Simp.fire[, -which(colnames(Simp.fire) %in% c("Actual burned area (ha)", "Intended burned area (ha)", "Study Year"))] %>% group_by(`Fire intention`) %>% 
                      summarise_if(is.numeric, function(x) {length(which(!is.na(x) & x != 0))}) 
    
    est_metrics  <- Simp.est %>% group_by(`Fire intention`) %>% 
      summarise_at(vars(`Number of fires, land cover (# km2-1 year-1)`:`Fire return period (years)`), 
                   function(x) {length(which(!is.na(x) & x != 0))})
    
    both_metrics <- metrics[, -1] + est_metrics[, -1]
    
    both_metrics$`Fire intention` <- metrics$`Fire intention`
    both_metrics                  <- both_metrics[, c(ncol(both_metrics), 1:ncol(both_metrics)-1)]
    both_metrics$instances        <- N$Instances
    
    db.sum <- list('Intention' = both_metrics)
    
    
    ### AFT summary
    
    if(simp == T) {
      
      Simp.fire <- Simp.fire %>% filter(!AFT %in% c('Abandoned agricultural land', 
                                                        'Agroforestry, Market-oriented', 'Agroforestry, Subsistence-oriented', 
                                                        'Cropping, Agroecology', 'Unoccupied', 'All') & !is.na(`Fire intention`))
      
      Simp.est  <- Simp.est %>% filter(!AFT %in% c('Abandoned agricultural land', 
                                                   'Agroforestry, Market-oriented', 'Agroforestry, Subsistence-oriented', 
                                                   'Cropping, Agroecology', 'Unoccupied', 'All') & !is.na(`Fire intention`))
      
    }
    
    
    N            <-   Simp.fire[, -which(colnames(Simp.fire) %in% c("Actual burned area (ha)", 
                                "Intended burned area (ha)"))] %>% group_by(AFT) %>% 
                                  filter(`Presence / Absence` == 'Presence') %>% 
                                  summarise(Instances = n())
    
    metrics      <-   Simp.fire[, -which(colnames(Simp.fire) %in% c(
                        "Actual burned area (ha)", "Intended burned area (ha)"))] %>% 
                          group_by(AFT) %>% 
                           summarise_if(is.numeric, function(x) {length(which(!is.na(x) & x != 0))}) 
    
    est_metrics  <- Simp.est %>% group_by(AFT) %>% 
                      summarise_at(vars(`Number of fires, land cover (# km2-1 year-1)`:`Fire return period (years)`), 
                         function(x) {length(which(!is.na(x) & x != 0))})
    
    both_metrics <- metrics[, -c(1, which(colnames(metrics) %in% c("Study Year", 
                    "Actual burned area.(ha)", "Intended burned area (ha)")))] + est_metrics[, -1]
    
    both_metrics$AFT       <- metrics$AFT
    both_metrics           <- both_metrics[, c(ncol(both_metrics), 1:ncol(both_metrics)-1)]
    both_metrics$instances <- N$Instances
    
    db.sum$AFT             <- both_metrics
    
  } else if(AFT.eval == T) {
    
    Simp.fire   <- Simp.fire %>% 
                     filter(`Fire type` %in% c('Human, deliberate',
                        'Human, escaped') & `Fire intention` != 'ND') 
    
    Simp.fire   <- Simp.fire %>% filter(!AFT %in% c('Abandoned agricultural land', 
                    'Agroforestry, Market-oriented', 'Agroforestry, Subsistence-oriented', 
                        'Cropping, Agroecology', 'Unoccupied', 'All'))
    
    AFT.actions <- plyr::rbind.fill(Simp.fire %>%
                    split(Simp.fire$AFT) %>% 
                    lapply(function(x) {mutate(x, 'AFTname' = unique(x$AFT)[1])}) %>% 
                    lapply(function(y) {data.frame(table(y$`Fire intention`), 
                     rep(y$AFTname[1], times = length(unique(y$`Fire intention`))))}))
    
    
    AFT.actions <- tidyr::pivot_wider(AFT.actions, names_from = colnames(AFT.actions)[3], 
                                       values_from = "Freq")
    
    
    AFT.actions[, 2:ncol(AFT.actions)] <- apply(AFT.actions[, 2:ncol(AFT.actions)], 
                                                2, function(x) {ifelse(is.na(x), 0, x)})
    
    db.sum <- AFT.actions
    
    
    
  }
  
  return(db.sum)
  
}


###########################################################################

### 4) Reset number in database

### useful for adding new rows from 3rd party contributors

##########################################################################

number.newrows <- function(newrows, data.type = NULL, base.case = NULL) {
  
  if (data.type == 'Fire') {
    
    base.case    <- max(tidyr::extract_numeric(reported_fire$Case.Study.ID[-newrows]), na.rm = T)
    
    case.studies <- paste0(tidyr::extract_numeric(reported_fire$Case.Study.ID[newrows]) + base.case,
                           stringr::str_extract(reported_fire$Case.Study.ID[newrows], "[a-z]+"))
    
    Fire.IDs     <- paste0(case.studies, sub(".*\\.", "", reported_fire$Fire.creation.ID[newrows]))
    
    return(data.frame('case.studies' = case.studies, "Fire.IDs" = Fire.IDs))
    
  } else if (data.type == 'Suppression') {
    
    case.studies <- paste0(tidyr::extract_numeric(sup$Case.Study.ID[newrows]) + base.case,
                           
                           
                           
                           
                           stringr::str_extract(sup$Case.Study.ID[newrows], "[a-z]+"))
    
    Sup.IDs     <- paste0(case.studies, sub(".*\\.", "", sup$Direct.fire.suppression.ID[newrows]))
    
    return(data.frame('case.studies' = case.studies, "Suppression.IDs" = Sup.IDs))
    
  }
  
  
} 




