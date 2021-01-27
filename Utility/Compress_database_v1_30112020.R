
###########################################################

### This script contains functions to reduce & resample DAFI data
### for reasons of under / oversampling

###########################################################


library(tidyverse)
library(ggplot2)
library(viridisLite)
library(maps)
library(readxl)

###############################

source('C:/Users/Oli/Documents/PhD/Model development/Analysis/Utility/General functions v5_3 07122020.R')
source('C:/Users/Oli/Documents/PhD/Model development/Analysis/Utility/Data visualisation_v2_17122020.R')

###############################


### Setup

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/DAFI')
dbstring      <- 'Database v1_9_clean 07122020.xlsx'
load.db()



#######################################################################################

### 1) Compress function for multiple case study rows

### This function reweights the database away from studies which present a lot of cases
### This is to prevent biases in results due to oversampling of similar locations / systems


### !To do: allow function user to choose number of records threshold
### Currently compresses metrics for AFT/Fire intention / study, where n >4 to 4 synthetic rows


#######################################################################################


Compress.db <- function(type = 'Fire', inc.Absence = F) {

  require(tidyverse)
    
  ### find studies with lots of case studies, and filter by those based in one country
  
  study.key   <- as.numeric(names(table(recordinfo$Study.ID)[which(table(recordinfo$Study.ID) >= 10)]))
  
  n.countries <- lapply(split(recordinfo[which(recordinfo$Study.ID %in% study.key), ], 
                              recordinfo$Study.ID[which(recordinfo$Study.ID %in% study.key)]), 
                        function(x) {nrow(x) / length(table(x$Country))})
  
  study.key   <- study.key[unlist(n.countries) >= 10]
  remove(n.countries)
  
  
  if(!type %in% c('Fire', 'Suppression', 'Policy', 'Land use')) {
    
    stop("Argument 'type' specified incorrectly")
  }
  
  
  #############################
  
  ### Data prep for fire
  
  #############################
  
  if(type == 'Fire') {
    
    dat <- list(reported_fire, est_fire)
    
    ### remove absence records?
    
    if(inc.Absence == F & type == 'Fire') {
      
      dat[[1]] <- dat[[1]][dat[[1]]$`Presence / Absence` == 'Presence', ]
      
      dat[[2]] <- dat[[2]][dat[[2]]$`Presence / Absence` == 'Presence', ]
      
    }
    
    dat <- lapply(dat, function (x) x[readr::parse_number(x$`Case Study ID`) %in% study.key, ])
    
  }
  
  
  ##################################################
  
  ### Filter for potentially overweighted case studies
  
  ##################################################
  
  if (type == 'Fire') {
    
    overweighted <- list()
    
    overweighted$reported  <- split(dat[[1]][, c(1, 10:26)], readr::parse_number(dat[[1]]$`Case Study ID`))
    overweighted$reported  <- lapply(overweighted$reported, function(x) {apply(x, 2, function(y) {
                                length(which(y > 0))})})
    
    overweighted$estimated <- split(dat[[2]][, c(1, 10:24)], readr::parse_number(dat[[2]]$`Case Study ID`))
    overweighted$estimated <- lapply(overweighted$estimated, function(x) {
                                apply(x, 2, function(y) {try({
                                  length(which(unlist(unbin(y)) > 0))})})})
    
    overweighted           <- lapply(overweighted, function(x) {lapply(x, function (y) {
                                if(any(y[-1] > 4)) {y
                                   } else {NULL}})})
    
    overweighted           <- lapply(overweighted, function(x){ x[unlist(lapply(x, 
                                        function(y) {!is.null(y[[1]])}))]})
    
  
  
  
  
  ###################################################
  
  ### Explore for repeated AFT / fire intention combos
  
  ###################################################
  
  ### begin process of condensing data
  

  for(f in 1:length(dat)) {
  
  overweighted.AFT <- lapply(split(dat[[f]][, c(1, 3, 4, 5)], 
                              readr::parse_number(dat[[f]]$`Case Study ID`)), 
                                function(x) {data.frame(table(x[, c(2, 3, 4)]))})
  
  overweighted.AFT <- plyr::rbind.fill(lapply(1:length(overweighted.AFT), function(z) {
                              overweighted.AFT[[z]] %>% 
                                mutate('Study.ID' = names(overweighted.AFT)[z])}))[, c(5, 1:4)]
  
  overweighted.AFT <- overweighted.AFT %>% filter(Freq >= 5)

    ###################################
    ### filter data for relevant studies
    ###################################
    
    dat[[f]]         <- dat[[f]] %>% filter(readr::parse_number(`Case Study ID`) %in% overweighted.AFT$`Study.ID`)
    original.names   <- colnames(dat[[f]])
    dat[[f]]         <- split(dat[[f]], readr::parse_number(dat[[f]]$`Case Study ID`))
    names(dat[[f]])  <- unique(readr::parse_number(overweighted.AFT$Study.ID))
    
    ############################################################
    
    ### generate summary stats for overweighted data and replace
    
    ############################################################
    
    ### generate summary stats
    
    for(i in 1:length(overweighted[[f]])) {
      
      if(names(overweighted$reported)[i] %in% names(dat[[f]])) {
        
        dat.temp          <- data.frame(dat[[f]][which(names(dat[[f]]) == names(overweighted[[f]])[i])])
        colnames(dat.temp)<- original.names
        new.rows          <- list()
        
        ### key for AFT / fire intention combos
        
        dat.temp.filt <- overweighted.AFT %>% filter(Study.ID == names(overweighted[[f]])[i])
        
        ### loop through AFT / Fire intention combos that are overweighted
        
        for (r in 1:nrow(dat.temp.filt)) {
          
          
          dat.sum       <- unlist(lapply(2:length(overweighted[[f]][[i]]), function(j) {
            
            if(overweighted[[f]][[i]][j] > 4) {
              
              if(names(overweighted)[f] == 'reported') {
                
                sum.stat <- summary((dat.temp %>% filter(`Fire type` == dat.temp.filt$Fire.type[r] & 
                                                           `Fire intention` == dat.temp.filt$Fire.intention[r] &
                                                           AFT == dat.temp.filt$AFT[r]))[, j+8])
                
                sum.stat <- c(mean(sum.stat[1:2]), sum.stat[3], sum.stat[4], mean(sum.stat[5:6]))
                
              } else if(names(overweighted)[f] == 'estimated') {
                
                sum.stat <- sample(dat.temp[, j+8][!is.na(dat.temp[, j+8])], 
                                   4, replace = F)

              }
              
              ### random bootsrapped sample for n between 1-4
              
            } else if (overweighted[[f]][[i]][j] > 1) {
              
              sum.stat <- as.character(sample(dat.temp[, j+8][!is.na(dat.temp[, j+8])], 
                                 4, replace = T))
              
              sum.stat <- ifelse(sum.stat == 0, NA, sum.stat)
              
            } else {
              
              sum.stat <- rep(NA, times = 4)
            }
            
            return(as.character(sum.stat))
            
          })) 
          
          
          ########################################
          
          ### replace overweighted rows with new synthetic rows
          
          ########################################
          
          synthetic.rows  <- dat[[f]][which(names(dat[[f]]) == names(overweighted[[f]])[i])][[1]]
          synthetic.rows  <- synthetic.rows %>% filter(`Fire type` == as.character(dat.temp.filt$Fire.type[r]) & 
                                                                    `Fire intention` == as.character(dat.temp.filt$Fire.intention[r]) &
                                                                    AFT == as.character(dat.temp.filt$AFT[r]))
          
          synthetic.rows  <- synthetic.rows[sample.int(nrow(synthetic.rows), size = 4), ]
          

          if(names(overweighted)[f] == 'reported') {
            
            synthetic.rows[, 10:26] <- data.frame(matrix(dat.sum, nrow =4, ncol = length(dat.sum)/4))
            
          } else if(names(overweighted)[f] == 'estimated') {
            
            synthetic.rows[, 10:24] <- data.frame(matrix(dat.sum, nrow =4, ncol = length(dat.sum)/4))
            
          }
          
          ### delete overweighted rows
          
          dat[[f]][which(names(dat[[f]]) == names(
            overweighted[[f]])[i])][[1]] <- dat[[f]][which(
              names(dat[[f]]) == names(overweighted[[f]])[i])][[1]] %>% 
            filter(!(`Fire type` == dat.temp.filt$Fire.type[r] & 
                       `Fire intention` == dat.temp.filt$Fire.intention[r] &
                       AFT == dat.temp.filt$AFT[r]))
          
          ### append new
          
          dat[[f]][which(names(dat[[f]]) == names(
            overweighted[[f]])[i])][[1]]<- plyr::rbind.fill(dat[[f]][which(names(dat[[f]]) == names(
                                               overweighted[[f]])[i])][[1]], synthetic.rows)
          

        }
        
      } 
      
    }
   }
    
  } else if (type == 'Land use') {
    
    dat      <- Landuse
    dat.filt <- filter(dat, readr::parse_number(`Case Study ID`) %in% study.key)
    
    dat.filt <- dat.filt %>% split(readr::parse_number(dat.filt$`Case Study ID`)) %>% 
                                 lapply(function(x) x[sample.int(nrow(x), size = 5), ]) %>% 
                                    plyr::rbind.fill()
    
    dat     <- dat %>% filter(!readr::parse_number(`Case Study ID`) %in% study.key)
    dat     <- rbind(dat, dat.filt)
    dat     <- dat[order(readr::parse_number(dat$`Case Study ID`)), ]
    
    return(dat)
    
  }
  
  ###########################################################
  
  ### filter dat & compile with unaffected rows
  
  ###########################################################
  
  if (type == 'Fire') {
  
    dat     <- lapply(dat, function(z) {plyr::rbind.fill(z)})
    
    results <- list('reported' = reported_fire, 'estimated' = est_fire)
    
    if(inc.Absence == F) {
    
      overweighted.interaction <- interaction(overweighted.AFT[, 1:4])
      
      ##########################################
      ### compile synthetic rows and rest of data
      ##########################################
      
      results$reported <- results$reported %>% 
                           filter(!(interaction(readr::parse_number(`Case Study ID`), 
                            `Fire type`, `Fire intention`, AFT) %in% overweighted.interaction & `Presence / Absence` == 'Presence')) 
    
      results$reported <- rbind(results$reported, dat[[1]])
      results$reported <- results$reported[order(readr::parse_number(results$reported$`Case Study ID`)), ]
      
      
      results$estimated <- results$estimated %>% 
                            filter(!(interaction(readr::parse_number(`Case Study ID`), 
                             `Fire type`, `Fire intention`, AFT) %in% overweighted.interaction & `Presence / Absence` == 'Presence')) 
      
      results$estimated <- rbind(results$estimated, dat[[2]])
      results$estimated <- results$estimated[order(readr::parse_number(results$estimated$`Case Study ID`)), ]
      
    }
    
  }
    
  return(results)
  
}


## example use

#weighted.dat <- Compress.db()
#weighted.dat <- lapply(weighted.dat, function(x) {x[!duplicated(x), ]}) ## clear up why this produces dupes
#weight.LULC  <- Compress.db(type = 'Land use')


########################################################################

### 2) Function to merge estimated and reported fire data

########################################################################

merge.fire_data <- function(dat = list('reported' = reported_fire, 'estimated' = est_fire), unbin.method = 'Mean', 
                                       analysis.cols = 10:26) {
  
  for(i in analysis.cols) {
    
    for(j in 1:nrow(dat$reported)) {
      
      if(!colnames(dat$reported)[i] %in% c('Intended burned area (ha)', 'Actual burned area (ha)')) {
        
        if(is.na(dat$reported[j, i])) {
          
          row.key <- which(dat$estimated$`Fire creation ID` == dat$reported$`Fire creation ID`[j])[1]
          col.key <- which(colnames(dat$estimated) == colnames(dat$reported)[i])[1]
          val     <- dat$estimated[row.key, col.key]
          
          if(!is.na(as.numeric(val)) & as.numeric(val) != 0)  {  
            
            dat$reported[j, i] <- ifelse(is.na(as.numeric(val)), NA, unlist(unbin(val, method = unbin.method)))
            
          }
          
        }
        
      }
      
    }

  }
  
  return(dat$reported)
    
  }


##########################################################################

### 3) Resample database - for specific models / representativeness

##########################################################################

Resample.DAFI <- function(dat, criteria = list(), weights) {
  
  criteria           <- stringr::str_c(unlist(criteria), collapse = " ")
  
  sample.weight      <- ifelse(eval(parse(text = criteria)), weights[1], weights[2])
  samples            <- sample.int(nrow(dat), replace = TRUE, prob = sample.weight)
  dat                <- dat[samples, ]
  
  
  return(dat)
  
  
}


