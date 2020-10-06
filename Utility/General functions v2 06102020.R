###############################################################################

### Code contains functions to support analysis of DAFI, the global database anthropogenic fire impacts

### Author: Oli Perkins, 6 October 2020

###############################################################################


### Contents

# 1a) Load data
# 1b) Summarise data availability

# 2a) Convert continuous values to binned values (reported -> estimated fire)
# 2b) Convert binned values to continuous values (estimated -> reported fire)

# 3a) Summarise database information (produces dplyr tbl summary)
# 3b) Map database information (driven by ggplot2)


############################################################

### 1) Load data

############################################################



load.db <- function() {
  
  
  recordinfo <<- read.xlsx(dbstring, sheet = 1, startRow = 1, colNames = TRUE)
  recordinfo <<- recordinfo[, 1:16]
  recordinfo[, c(5, 6, 11, 14, 15)] <<- apply(recordinfo[, c(5, 6, 10, 13, 15)], 2, 
                                              function(x) {ifelse(x == 'ND', NA, x)})
  
  landuse    <<- read.xlsx(dbstring, sheet = 2, startRow = 1, colNames = TRUE)
  landuse[, c(2, 3, 5, 6, 7, 8:19)] <<- apply(landuse[, c(2, 3, 5, 6, 7, 8:19)], 2, 
                                              function(x) {ifelse(x == 'ND', NA, x)})
  
  reported_fire                   <<- read.xlsx(dbstring, sheet = 3, startRow = 1, colNames = TRUE)
  reported_fire[, c(6, 7, 10:28)] <<- apply(reported_fire[, c(6, 7, 10:28)], 2, function(x) {ifelse(x == 'ND', NA, x)})
  reported_fire[, c(10:25)]       <<- apply(reported_fire[, c(10:25)], 2, function(x) {as.numeric(as.character(x))})
  reported_fire[, c(26, 27)]      <<- apply(reported_fire[, c(26, 27)], 2, function(x){as.factor(x)})
  
  est_fire                        <<- read.xlsx(dbstring, sheet = 4, startRow = 1, colNames = TRUE)
  est_fire[, c(6, 7, 10:26)]      <<- apply(est_fire[, c(6, 7, 10:26)], 2, function(x) {ifelse(x == 'ND', NA, x)})
  est_fire[, c(10:25)]            <<- apply(est_fire[, c(10:25)], 2, function(x){as.factor(x)})
  est_fire                        <<- est_fire[1:nrow(reported_fire), ]
  
  sup    <<- read.xlsx(dbstring, sheet = 5, startRow = 1, colNames = TRUE)
  policy <<- read.xlsx(dbstring, sheet = 6, startRow = 1, colNames = TRUE)
  
  
}


###############################################################

#1b) Data availability

###############################################################

DB_overview <- function() {
  
  N <-   reported_fire[, -c(19, 20)] %>% group_by(Fire.intention) %>% 
    filter(`Presence./.Absence` == 'Presence') %>% summarise(Instances = n())
  
  
  metrics      <- reported_fire[, -c(19, 20)] %>% group_by(Fire.intention) %>% 
    summarise_if(is.numeric, 
                 function(x) {length(which(!is.na(x) & x != 0))}) 
  
  est_metrics  <- est_fire %>% filter(Fire.intention != 0) %>% group_by(Fire.intention) %>% 
    summarise_at(vars(`Number.of.fires.(#.km2-1)`:`Fire.return.period.(years)`), 
                 function(x) {length(which(!is.na(x) & x != 0))})
  
  both_metrics <- metrics[, -c(1, which(colnames(metrics) %in% c("Study.Year", 
                                                                 "Actual.burned.area.(ha)", "Intended.burned.area.(ha)")))] + est_metrics[, -1]
  
  both_metrics$Fire.intention <- metrics$Fire.intention
  both_metrics                <- both_metrics[, c(ncol(both_metrics), 1:ncol(both_metrics)-1)]
  both_metrics$instances <- N$Instances
  
  
  db.sum <- list('Intention' = both_metrics)
  
  N <-   reported_fire[, -c(19, 20)] %>% group_by(AFT) %>% 
    filter(`Presence./.Absence` == 'Presence') %>% summarise(Instances = n())
  
  metrics      <- reported_fire[, -c(19, 20)] %>% group_by(AFT) %>% 
    summarise_if(is.numeric, 
                 function(x) {length(which(!is.na(x) & x != 0))}) 
  
  est_metrics  <- est_fire %>% filter(Fire.intention != 0) %>% group_by(AFT) %>% 
    summarise_at(vars(`Number.of.fires.(#.km2-1)`:`Fire.return.period.(years)`), 
                 function(x) {length(which(!is.na(x) & x != 0))})
  
  both_metrics <- metrics[, -c(1, which(colnames(metrics) %in% c("Study.Year", 
                                                                 "Actual.burned.area.(ha)", "Intended.burned.area.(ha)")))] + est_metrics[, -1]
  
  both_metrics$AFT <- metrics$AFT
  both_metrics     <- both_metrics[, c(ncol(both_metrics), 1:ncol(both_metrics)-1)]
  both_metrics$instances <- N$Instances
  
  db.sum$AFT      <- both_metrics
  
  return(db.sum)
  
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
      top    <- ifelse(bottom == '25000' & top == '', '100000', top)
      
      if(method == 'Mean') {
        
        y <- mean(c(as.numeric(bottom), as.numeric(top)))
        
      } else if (method == 'Random') {
        
        y      <- runif(1, as.numeric(bottom), as.numeric(top))
        
      }

    return(y)
    
    
    
  }
  
  return(lapply(data, catfunc))
  
}




#############################################################################################

### 3a) Summarise database metrics

### Creates a dplyr tbl containing a summary of a given database value

#############################################################################################

summarise.behaviour <- function(type = c('Records', 'Land use', 'Fire', 'Suppression', 'Policy'), 
                                behaviour, grouping = 'Case.Study.ID', inc.Absence = F, choose = NULL) {
  

  #### Set up data
  
  require(tidyverse)
  require(ggplot2)
  
  #behaviour <- gsub(' ', '.', behaviour)
  #behaviour <- gsub('(', '.', behaviour, fixed = T)
  #behaviour <- gsub(')', '.', behaviour, fixed = T)
  
  ## function to summarise attributes from the database
  
  ## choose should be a string containing the contents of a dplyr filter expression
  
  ## behaviour and grouping should be the intended column names passed to dplyr::summarise and group_by_at respectively
  
  
  ##########################
  
  ### Select database segment
  
  ##########################
  
  if(type == 'Records') {
    
    dat <- recordinfo
    
  } else if(type == 'Land use') {
    
    dat <- landuse
    
  } else if (type == 'Fire') {
    
    dat <- list(reported_fire, est_fire)
    
    if (!behaviour %in% colnames(dat[[1]])) {

      stop('Behaviour must be a given column name')
      
    }
    
  } else if (type == 'Suppression') {
    
    dat <- sup 
    
  } else if (type =='Policy') {
    
    dat <- policy
    
  } else {
    
    stop("Argument 'type' specified incorrectly")
    
  }
  
  if (!behaviour %in% colnames(dat) & type != 'Fire') {
    
    stop('Behaviour must be a given column name')
    
  }
  
  
  #### Filter absence records?
  if(inc.Absence == F & type == 'Fire') {
    
    dat[[1]] <- dat[[1]][dat[[1]]$`Presence./.Absence` == 'Presence', ]
    
    dat[[2]] <- dat[[2]][dat[[2]]$`Presence./.Absence` == 'Presence', ]
    
  }
  
  
  ##################################
  
  ### Create summary data
  
  #################################
  
  if(type == 'Fire') {
    
    ### reported
    Quant.stat <- dat[[1]] %>% dplyr::group_by_at(grouping) %>% 
      filter(!is.na(base::get(behaviour))) %>%
      summarise('Reported.stat' = mean(base::get(behaviour), na.rm = T), 
                'Reported.N'    = n())
    
    ### estimated
    dat[[2]][, which(colnames(dat[[2]]) == behaviour)] <- unlist(
      unbin(dat[[2]][, which(colnames(dat[[2]]) == behaviour)]))
    
    Est.stat  <- dat[[2]] %>% dplyr::group_by_at(grouping) %>% 
      filter(!is.na(base::get(behaviour))) %>%
      summarise('Est.stat' = mean(base::get(behaviour), na.rm = T), 
                'Est.N'    = n())
    
    ### combine
    Results   <- merge(Quant.stat, Est.stat, by = grouping, 
                       all.x = T, all.y = T)
    
    Results$Combined <- ((Results$Reported.N * Results$Reported.stat) + 
                           (Results$Est.stat * Results$Est.N)) / (Results$Reported.N + Results$Est.N)
    
    
    
  } else {
    
    Results <- dat %>% dplyr::group_by_at(grouping) %>% 
      filter(!is.na(base::get(behaviour))) %>%
      summarise('Reported.stat' = mean(base::get(behaviour), na.rm = T), 
                'Reported.N'    = n())
    
    
  }
  
  
  return(Results)
  
  
}



#############################################################################################

### 3b) Map database metrics

### Creates a map plot of database values - driven by ggplot2

#############################################################################################


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
  
  ## ggcolour and ggshape should be the intended column names passed to ggplot shape and colour aesthetics
  
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
  colnames(dat)<- gsub('/', '.', colnames(dat))
  colnames(dat)<- gsub('-', '.', colnames(dat))
  
  
  g <- ggplot(dat, aes_string(x = 'Longitude', y = 'Latitude', colour = ggcolour, shape = ggshape)) +
    geom_point(size = 1.5) + theme_classic() + borders() + scale_colour_viridis_d() #+ scale_shape_manual(values = c(0, 3, 16))
  
  print(g)
  
  return(g)
  
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
