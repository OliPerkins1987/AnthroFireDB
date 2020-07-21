

##########################################################################

### 1) Function to project fire size metrics using power law distribution

### This assumes the distribution of fire sizes is e^-x (Malamud 2005)
### Takes cases with a mean or median, and takes 99.9th quantiles as min and max

##########################################################################

estimate.size <- function(dat, metric = 'size', kind = 'estimated', 
                             actual = TRUE) {
  
  
  #######################################################################
  
  ### Function to make predictions on fire size based on exponential distribution
  
  ### Malamud et al., 2005
  
  ### https://en.wikipedia.org/wiki/Exponential_distribution
  
  #######################################################################
  
  dat.fill <- function(from, to) {
    
    ### Fill in meta data on prediction
    
    est_dat$Est.metric[nrow(est_dat)] <<- paste(ifelse(actual == TRUE,
                                                       'Actual', 'Intended'),to, sep = '.')
    
    est_dat$Based.on[nrow(est_dat)]   <<- paste(ifelse(actual == TRUE,
                                                       'Actual', 'Intended'),from, sep = '.')
    
    ##########################
    ### Calculate lambda
    ##########################
    
    
    if(kind == 'estimated') {
    
    if(from == 'mean') {
      
      lambda   <- 1/mean(as.numeric(c(gsub("\\-.*", "", ifelse(actual == FALSE, dat$`Intended.fire.size.mean.(ha)`[i], dat$`Actual.fire.size.mean.(ha)`[i])), 
                                      gsub(".*-", "", ifelse(actual == FALSE, dat$`Intended.fire.size.mean.(ha)`[i], dat$`Actual.fire.size.mean.(ha)`[i])))))
      
    } else if (from == 'median') {
      
      lambda   <- log(2) /mean(as.numeric(c(sub("\\-.*", "", ifelse(actual == FALSE, dat$`Intended.fire.size.median.(ha)`[i], dat$`Actual.fire.size.median.(ha)`[i])), 
                                            sub(".*-", "", ifelse(actual == FALSE, dat$`Intended.fire.size.median.(ha)`[i], dat$`Actual.fire.size.median.(ha)`[i])))))
      
    } else if (from == 'max') {
      
      lambda  <- -(log(0.01) /mean(as.numeric(c(sub("\\-.*", "", ifelse(actual == FALSE, dat$`Intended.fire.size.max.(ha)`[i], dat$`Actual.fire.size.max.(ha)`[i])), 
                                                sub(".*-", "", ifelse(actual == FALSE, dat$`Intended.fire.size.max.(ha)`[i], dat$`Actual.fire.size.max.(ha)`[i]))))))
      
    } else if (from == 'min') {
      
      lambda <- -(log(0.99) /mean(as.numeric(c(sub("\\-.*", "", ifelse(actual == FALSE, dat$`Intended.fire.size.min.(ha)`[i], dat$`Actual.fire.size.min.(ha)`[i])), 
                                               sub(".*-", "", ifelse(actual == FALSE, dat$`Intended.fire.size.min.(ha)`[i], dat$`Actual.fire.size.min.(ha)`[i]))))))
      
      }
      
    } else if (kind == 'reported') {
      
      if(from == 'mean') {
        
        lambda   <- 1/ ifelse(actual == FALSE, dat$`Intended.fire.size.mean.(ha)`[i], dat$`Actual.fire.size.mean.(ha)`[i])
        
      } else if (from == 'median') {
        
        lambda   <- log(2) /ifelse(actual == FALSE, dat$`Intended.fire.size.median.(ha)`[i], dat$`Actual.fire.size.median.(ha)`[i])
        
      } else if (from == 'max') {
        
        lambda  <- -(log(0.01) / ifelse(actual == FALSE, dat$`Intended.fire.size.max.(ha)`[i], dat$`Actual.fire.size.max.(ha)`[i]))
        
      } else if (from == 'min') {
        
        lambda <- -(log(0.99) / ifelse(actual == FALSE, dat$`Intended.fire.size.min.(ha)`[i], dat$`Actual.fire.size.min.(ha)`[i]))
        
      }
      
    }
    
    
    ##############################
    ### Make prediction
    ##############################
    
    if (to == 'mean') {
      
      return(1 / lambda)
      
    } else if (to == 'median') {
      
      return(log(2) / lambda)
      
    } else if (to == 'max') {
      
      return(qexp(0.99, lambda))
      
    } else if (to == 'min') {
      
      return(qexp(0.01, lambda))
      
    }
    
  }
  
  
  ####################################################################
  
  ### Function to update results frame as new rows are added
  
  ####################################################################
  
  update.rows <- function() {
    
    est_dat$Fire.creation.ID[nrow(est_dat)]  <<- dat$Fire.creation.ID[i]
    est_dat$Study.year[nrow(est_dat)]     <<- dat$Study.Year[i]
    est_dat$AFT[nrow(est_dat)]            <<- dat$AFT[i]
    est_dat$Fire.intention[nrow(est_dat)] <<- dat$Fire.intention[i]
    
    for (col in 1:length(key)) {
      
      est_dat[nrow(est_dat), 4+col] <<- key[col]
      
    }
    
    est_dat[nrow(est_dat)+1, ]            <<- NA
    update                                <<- TRUE
    
  }
  
  
  if(metric == 'size') {
    
    
    #####################################
    
    ### 1) Extract relevant data
    
    #####################################
    
    if(actual) {
    
      dat <- dat[apply(dat[, c(12, 14, 16, 18)], 1, function(x) {length(which(x > 0))}) >= 1, ]
    
    } else if (!actual) {
      
      dat <- dat[apply(dat[, c(11, 13, 15, 17)], 1, function(x) {length(which(x > 0))}) >= 1, ]
    }
    
    dat <- dat[dat$`Presence./.Absence` == 'Presence', ]
    
    
  }
  
  ### Set up results frame
  
  est_dat <- data.frame('Fire.creation.ID' = NA, 'AFT' = '', 'Fire intention' = '', 'Study.year' = '', 'Reported.Firemetric.1' = '', 
                        'Reported.Firemetric.2' = '', 'Reported.Firemetric.3' = '', 'Reported.Firemetric.4' = '',
                        'Reported.Firemetric.5' = '','Reported.Firemetric.6' = '','Based.on' = NA, 'Est.metric' = NA, 'Est.value' = NA)

    for(i in 1:nrow(dat)){
      
      if(i == 1) {
        
        est_dat[nrow(est_dat), ]                 <- NA
        est_dat$Fire.creation.ID[nrow(est_dat)]  <- dat$Fire.creation.ID[i]
        est_dat$Study.year[nrow(est_dat)]     <- dat$Study.Year[i]
        est_dat$AFT[nrow(est_dat)]            <- dat$AFT[i]
        est_dat$Fire.intention[nrow(est_dat)] <- dat$Fire.intention[i]
        
      }
      
      if(metric == 'size') {
        
        if(actual) {
        
        key <- colnames(dat)[c(12, 14, 16, 18)][which(!is.na(dat[i, c(12, 14, 16, 18)]))]
        
        } else if (!actual) {
          
        key <- colnames(dat)[c(11, 13, 15, 17)][which(!is.na(dat[i, c(11, 13, 15, 17)]))]
          
          
        }
        
        for (col in 1:length(key)) {
          
          est_dat[nrow(est_dat), 4+col] <- key[col]
          
        }
        
        ################################################
        
        ### 2) get information on what fire data is available
        
        ###############################################
        
        update     <- FALSE
        
        has.mean   <- any(TRUE %in% grepl('mean', est_dat[nrow(est_dat), 4:7]))
        has.median <- any(TRUE %in% grepl('median', est_dat[nrow(est_dat), 4:7]))
        has.min    <- any(TRUE %in% grepl('min', est_dat[nrow(est_dat), 4:7]))
        has.max    <- any(TRUE %in% grepl('max', est_dat[nrow(est_dat), 4:7]))

        
        #################################################################
        
        ### 3) Fill metrics
        
        ### Does not use max or min to extrapolate due to poor performance
        
        #################################################################
        
        ### use mean
        
        if(has.mean) {
        
          if(!has.median) {
          
            est_dat$Est.value[nrow(est_dat)]  <- dat.fill(from  = 'mean', to = 'median')
            update.rows()
          
          }
          
          if(!has.max) {
            
            est_dat$Est.value[nrow(est_dat)]  <- dat.fill(from  = 'mean', to = 'max')
            update.rows()
            
          }
          
          if(!has.min) {
            
            est_dat$Est.value[nrow(est_dat)]  <- dat.fill(from  = 'mean', to = 'min')
            update.rows()
            
          }
            
        }
        
        
        ### use median
        
        if(has.median) {
        
          if(!has.mean) {
            
            est_dat$Est.value[nrow(est_dat)]  <- dat.fill(from  = 'median', to = 'mean')
            update.rows()
            
          }
          
          if(!has.max) {
            
            est_dat$Est.value[nrow(est_dat)]  <- dat.fill(from  = 'median', to = 'max')
            update.rows()
            
          }
          
          if(!has.min) {
            
            est_dat$Est.value[nrow(est_dat)]  <- dat.fill(from  = 'median', to = 'min')
            update.rows()
            
          }
          
          
        }
        
        if(has.max & !has.median) {
          
          est_dat$Est.value[nrow(est_dat)]  <- dat.fill(from  = 'max', to = 'median')
          update.rows()
          
        }
        
        
        ##### Catch cases where no update was made

        if (update == FALSE) {
          
          est_dat$Fire.creation.ID[nrow(est_dat)]  <- dat$Fire.creation.ID[i]
          est_dat$Study.year[nrow(est_dat)]     <- dat$Study.Year[i]
          est_dat$AFT[nrow(est_dat)]            <- dat$AFT[i]
          est_dat$Fire.intention[nrow(est_dat)] <- dat$Fire.intention[i]
          
          for (col in 1:length(key)) {
            
            est_dat[nrow(est_dat), 4+col] <- key[col]
            
          }
          
          
          est_dat[nrow(est_dat)+1, ]                <- NA
          
        }
        
      }
      
    }
    
  return(est_dat)  
  
  
}



##########################################################

### merge and compare estimated outputs

##########################################################

### Visualisation

power.estimates <- estimate.size(reported_fire, actual = T, kind = 'reported')

power.sum <- power.estimates[-nrow(power.estimates), ] %>% 
  group_by(.dots = c('AFT', 'Fire.intention', 'Est.metric', 'Based.on')) %>%
    summarise('Mean_est' = median(Est.value, na.rm = T), 
              'Upper' = quantile(Est.value, 0.975, na.rm = T), 
              'Lower' = quantile(Est.value, 0.025, na.rm = T))

ggplot(power.sum[power.sum$Est.metric == 'Actual.median', ], aes(x = AFT, fill = Fire.intention, y = Mean_est)) + facet_grid(Based.on~.) +
  geom_col(position = position_dodge(), colour = 'black') + scale_y_sqrt(breaks = c(0, 2.5, 10, 100, 500, 1000, 2000)) + 
  theme_classic() + scale_fill_viridis_d() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Fire size (ha)') +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 1, position = position_dodge())


#####################################################################################################

### 1b) Simple power law extrapolation

### Code taken from overall extrapolation for application elsewhere

#####################################################################################################


powerlaw.extrapolate <- function(dat, from, to, actual = T) {
  

  ### Takes a row of a dataframe or named list as input
  ### assumes data has been converted to numeric via unbin
  
  ##########################
  ### Calculate lambda
  ##########################
  
  
     if(from == 'mean') {
      
      lambda   <- 1/ ifelse(actual == FALSE, dat[names(dat) == "Intended.fire.size.mean.(ha)"], dat[names(dat) == "Actual.fire.size.mean.(ha)"])
      
    } else if (from == 'median') {
      
      lambda   <- log(2) /ifelse(actual == FALSE, dat[names(dat) == "Intended.fire.size.median.(ha)"], dat[names(dat) == "Actual.fire.size.median.(ha)"])
      
    } 
  
  ##############################
  ### Make prediction
  ##############################
  
  if (to == 'mean') {
    
    return(1 / lambda)
    
  } else if (to == 'median') {
    
    return(log(2) / lambda)
    
  } else if (to == 'max') {
    
    return(qexp(0.99, lambda))
    
  } else if (to == 'min') {
    
    return(qexp(0.01, lambda))
    
  }
  
}

#####################################################################################################

### 2) Logical extrapolation of fire data

#####################################################################################################

get.igs <- function(data, type = 'reported', fill.metric = 'Ignitions',
                    method = 'BA+Firesize') {
  
  dat <- data
  
  if (type == 'estimated') {
    
    dat[, 10:23] <- apply(dat[, 10:23], 2, function(x) {unlist(unbin(x))})
    
  }
  
  
  if (fill.metric == 'Ignitions') {
    
    
    
    if(method == 'BA+Firesize') {
  
    dat <- dat[!is.na(dat$`Actual.fire.size.mean.(ha)`) | !is.na(dat$`Actual.fire.size.median.(ha)`), ]
    dat <- dat[!is.na(dat$`Actual.burned.area.%.(total)`) & dat$`Presence./.Absence` == 'Presence', ]
  
    }
    
  
  }
 
  if(fill.metric == 'Burned area') {
    
    if(method == 'Return') {
      
      dat           <- dat[!is.na(dat$`Fire.return.period.(years)`) & dat$`Presence./.Absence` == 'Presence', ]
      dat$estimated <- 1/dat$`Fire.return.period.(years)`
      
    }
    
  }
  
  return(dat)
   
}

dat <- get.igs(est_fire, fill.metric = 'Burned area', method = 'Return', type = 'estimated')
dat <- dat %>% group_by(.dots = c('AFT', 'Fire.type')) %>%
  summarise('estimate' = mean(estimated*100, na.rm = T))

dat <- merge(dat, area.burned, by = c('AFT', 'Fire.type'), 
                        all.x = T)

#### check quality

sd(dat$`Intended.burned.area.%.(land.cover)`[!is.na(dat$`Intended.burned.area.%.(land.cover)`)]) / 
  (Metrics::rmse(dat$`Intended.burned.area.%.(land.cover)`[!is.na(dat$`Intended.burned.area.%.(land.cover)`)], 
                 dat$estimated[!is.na(dat$`Intended.burned.area.%.(land.cover)`)]) + 
     sd(dat$`Intended.burned.area.%.(land.cover)`[!is.na(dat$`Intended.burned.area.%.(land.cover)`)]))


########################################################################

### 3) Data where regime and underlying behaviours are reported separately

########################################################################

#########################################################

### find cases

#########################################################

regime.behaviours <- list()


for (i in 1:length(unique(reported_fire$Case.Study.ID))) {
 
  regime.behaviours[[i]] <- list()

    tempdat <- reported_fire %>% 
    filter(Case.Study.ID == unique(reported_fire$Case.Study.ID)[i] & (Fire.type == 'All' | Fire.intention == 'All') & AFT %in% c('All', 'ND'))
  
  if (nrow(tempdat) >= 1) {
  
    ### add reported
    
  regime.behaviours[[i]][['regime']] <- data.frame(tempdat)

  tempdat <- reported_fire %>% 
    filter(Case.Study.ID == unique(reported_fire$Case.Study.ID)[i] & Fire.type != 'All' & Fire.intention != 'All' & !AFT %in% c('All', 'ND'))
     
  regime.behaviours[[i]][['behaviours']] <- data.frame(tempdat)
  
  regime.behaviours[[i]][['estimated.regime']]  <- est_fire %>% 
    filter(Case.Study.ID == unique(reported_fire$Case.Study.ID)[i] & (Fire.type == 'All' | Fire.intention == 'All') & AFT %in% c('All', 'ND'))
  
  regime.behaviours[[i]][['estimated.behaviours']]  <- est_fire %>% 
    filter(Case.Study.ID == unique(reported_fire$Case.Study.ID)[i] & Fire.type != 'All' & Fire.intention != 'All' & !AFT %in% c('All', 'ND'))
  
  
  } else {
    
    regime.behaviours[[i]]['regime']  <- NA
    regime.behaviours[[i]]['behaviours']  <- NA
  }
  
  
  
  
}

regime.behaviours <- regime.behaviours[unlist(lapply(regime.behaviours, 
                              function(x) {ifelse(!is.null(nrow(x$regime)), TRUE, FALSE)}))]


### conclusion - this needs to be done manually - relatively few cases, and should be 
### able to come up with consistent rules for extrapolation


