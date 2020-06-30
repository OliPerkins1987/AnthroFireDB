###################################################################################

### Bootstrapping algorithm for analysis of global anthropogenic fire database

### Author: Oli Perkins, 30062020

###################################################################################

### When bootstrapping data, we need to include AFT - 
### This means sampling the rows rather than the values?

### How are we including escaped vs deliberate?

### How do we deal with records giving specific (presence only) and general (quantified)?


find.firedata <- function(dat = dat.filt, sample.weights = list(), reported_bias = 0.25, 
                          nsamples = 50, boot.thresh = 10, index = key, result_cols) {
  
  
  ### Need to add presence / absence & intention to the sampling results
  
  ###########################################
  
  ### This function creates representative rows from the database 
  ### through bootstrap samples. Function arguments:
  
  ### sample.weights: a list of the sample weights that should be applied to the data, options:
  ### - 'geographic'
  ### - 'LC' (landcover)
  ### - 'climate' (not implemented)
  
  ### reported_bias: how much should reported values be preferred to estimates (0.25 = 25% bias)
  
  ### nsamples: how many samples of data for each fire intention should be taken?
  
  ### boot.thresh: a threshold for initial bootstrapping - if there are fewer values available than
  ### this figure, the data will be bootstrapped prior to the creation of representative rows
  
  ###########################
  ### needs implementing - choice of which criteria is used for grouping of fire behaviours
  ### Currently only avilable for fire intention
  ###########################
  
  ##########################################
  
  out.dat <- list()
  
  out.dat$Fire.intention       <- index
  out.dat$`Presence./.Absence` <- 'Presence'
  
  ### currently removes reported burned area (ha) - not enough vals and not consistent measure
  
  dat$reported <- dat$reported[, -c(which(colnames(dat$reported) %in% c("Actual.burned.area.(ha)", "Intended.burned.area.(ha)")))]
  result_cols  <- result_cols[!result_cols %in% c("Actual.burned.area.(ha)", "Intended.burned.area.(ha)")]
  
  for (fire.metric in 10:26) {
    
    ### get values & establish sampling frequency (estimated vs reported)
    
    pot.vals <- list(reported = dat$reported[which(dat$reported$`Presence./.Absence` != 'Absence' & !is.na(dat$reported[, fire.metric])), fire.metric], 
                     estimated = dat$estimated[which(dat$est$`Presence./.Absence` != 'Absence' & !is.na(dat$est[, fire.metric])), fire.metric])
    
    probs    <- length(pot.vals$reported)/(length(pot.vals$reported) + 
                                             length(pot.vals$estimated))
    
    probs    <- min(c(0.9, ifelse(probs < 1, probs/(1-reported_bias), probs)))
    
    ### Bootstrap small samples
    
    if (length(unlist(pot.vals)) < boot.thresh) {
      
      ### pass for now
      1+1
      
    }
    
    #####################################################
    
    ### Sample fire metric
    
    #####################################################
    
    if (length(unlist(pot.vals)) == 0) {
      
      
      #print(paste0('No values for ', colnames(dat$reported)[fire.metric])) 
      
      out.dat[[colnames(dat$reported)[fire.metric]]] <- rep(NA, times = nsamples)
      
    } else if(length(pot.vals$estimated) == 0) {
      
      
      out.dat[[colnames(dat$reported)[fire.metric]]]   <- sample(pot.vals$reported, nsamples, replace = T)
      
    } else if(length(pot.vals$reported)  == 0) {
      
      out.dat[[colnames(dat$reported)[fire.metric]]] <- sample(pot.vals$estimated, nsamples, replace = T)
      
      
    } else {
      
      for (samp in 1:nsamples) {
        
        mixed.samp <- character()
        
        mixed.samp <- c(mixed.samp, ifelse(runif(1) <= probs, 
                                           sample(pot.vals$reported, 1, replace = T), 
                                           sample(pot.vals$reported, nsamples, replace = T))) 
        
        
      }
      
      out.dat[[colnames(dat$reported)[fire.metric]]] <- mixed.samp
      
    }
    
  }
  
  out.dat <- lapply(out.dat, function(x) {x[1:nsamples]})
  out.dat <- data.frame(out.dat)
  colnames(out.dat) <- result_cols
  
  return(out.dat)
  
} 


########################################################################

### run boostrapping

########################################################################

###############################
### Add capability to include calculated metrics
##############################

iterations    <- 50
results.frame <- list()

set.seed(1987)

for (i in 1:length(unique(reported_fire$Fire.intention))) {
  
  ### filter data
  
  key       <- unique(reported_fire$Fire.intention)[i]
  
  dat.filt  <- list('reported' = reported_fire[reported_fire$Fire.intention == key, ], 
                    'estimated' = est_fire[est_fire$Fire.intention == key, ])
  
  for(iteration in 1:iterations){
    
    start.row <- dat.filt$reported[sample(1:nrow(dat.filt$reported), 1), c(4, 8, 10:28)]
    
    
    ### deal with absence
    
    if(start.row$`Presence./.Absence` == 'Absence') {
      
      start.row           <- start.row[, -c(which(colnames(start.row) %in% c("Actual.burned.area.(ha)", "Intended.burned.area.(ha)")))]
      
      
      if(length(results.frame) >= 1) {
        
        colnames(start.row) <- colnames(results.frame[[1]])
        
      }
      
      
      ### Should absence data be factored in as a spatial sample of presence %ct?
      
      results.frame[[length(results.frame) + 1]] <- start.row
      
    } else {
      
      
      if(length(results.frame) >= 1) {
        
        results.frame[[length(results.frame) + 1]]   <- find.firedata(nsamples = 1, result_cols = colnames(results.frame[[1]]))
        
      } else {
        
        results.frame[[length(results.frame) + 1]] <- find.firedata(nsamples = 1, result_cols = colnames(start.row))
        
      }
      
      
    }
    
  }
  
  
}

results.frame    <- plyr::rbind.fill(results.frame)


##########################################################################

### Function to project fire size metrics using power law distribution

### This assumes the distribution of fire sizes is e^-x (Malamud 2005)
### Takes cases with a mean or median, and takes 99.9th quantiles as min and max

##########################################################################

########################

### Need to sort out adding of rows to data frame so that it works

### Also, buckets should report buckets & numbers should report numbers

########################


estimate.metrics <- function(dat, metric = 'size', kind = 'estimated') {
  
  if(metric == 'size') {
    
    ### set up data frame
    dat <- dat[apply(dat[, 11:18], 1, function(x) {length(which(x > 0))}) >= 1, ]
    dat <- dat[dat$`Presence./.Absence` == 'Presence', ]
    
    
    ### Define function to update dataframe after metric is added
    
    update.rows <- function() {
      
      est_dat$Case.Study.ID[nrow(est_dat)]  <<- dat$Case.Study.ID[i]
      est_dat$Study.year[nrow(est_dat)]     <<- dat$Study.Year[i]
      est_dat$AFT[nrow(est_dat)]            <<- dat$AFT[i]
      est_dat$Fire.intention[nrow(est_dat)] <<- dat$Fire.intention[i]
      
      for (col in 1:length(key)) {
        
        est_dat[nrow(est_dat), 4+col] <<- key[col]
        
      }
      
      est_dat[nrow(est_dat)+1, ]            <<- NA
      update                                <<- TRUE
      
    }
    
  }
  
  est_dat <- data.frame('Case.Study.ID' = NA, 'AFT' = '', 'Fire intention' = '', 'Study.year' = '', 'Reported.Firemetric.1' = '', 
                        'Reported.Firemetric.2' = '', 'Reported.Firemetric.3' = '', 'Reported.Firemetric.4' = '',
                        'Reported.Firemetric.5' = '','Reported.Firemetric.6' = '','Est.metric' = NA, 'Est.value' = NA)
  
  
  if(kind == 'estimated') {
    
    for(i in 1:nrow(dat)){
      
      if(i == 1) {
        
        est_dat[nrow(est_dat), ]              <- NA
        est_dat$Case.Study.ID[nrow(est_dat)]  <- dat$Case.Study.ID[i]
        est_dat$Study.year[nrow(est_dat)]     <- dat$Study.Year[i]
        est_dat$AFT[nrow(est_dat)]            <- dat$AFT[i]
        est_dat$Fire.intention[nrow(est_dat)] <- dat$Fire.intention[i]
        
      }
      
      if(metric == 'size') {
        
        key <- colnames(dat)[11:18][which(!is.na(dat[i, 11:18]))]
        
        for (col in 1:length(key)) {
          
          est_dat[nrow(est_dat), 4+col] <- key[col]
          
        }
        
        ################################################
        
        ### estimate fire size distribution from values
        
        ###############################################
        
        update     <- FALSE
        
        has.mean   <- any(TRUE %in% grepl('mean', est_dat[nrow(est_dat), 4:7]))
        has.median <- any(TRUE %in% grepl('median', est_dat[nrow(est_dat), 4:7]))
        has.min    <- any(TRUE %in% grepl('min', est_dat[nrow(est_dat), 4:7]))
        has.max    <- any(TRUE %in% grepl('max', est_dat[nrow(est_dat), 4:7]))
        
        dat.kind <- ifelse(any(TRUE %in% grepl('Intended', est_dat[i, 4:7])) & 
                             !any(TRUE %in% grepl('Actual', est_dat[i, 4:7])), 'Intended', 
                           ifelse(any(TRUE %in% grepl('Actual', est_dat[i, 4:7])) & 
                                    !any(TRUE %in% grepl('Intended', est_dat[i, 4:7])), 
                                  'Actual', 'Mixed'))
        
        
        #######################################################################
        
        #https://en.wikipedia.org/wiki/Exponential_distribution
        
        #######################################################################
        
        dat.fill <- function(from, to) {
          
          ### come back to fix this
          
          if(dat.kind == 'Mixed') {
            
            return(NA)
          }
          
          est_dat$Est.metric[nrow(est_dat)] <<- paste(dat.kind, to, sep = '.')
          
          if(from == 'mean') {
            
            lambda   <- 1/mean(as.numeric(c(gsub("\\-.*", "", ifelse(dat.kind =='Intended', dat$`Intended.fire.size.mean.(ha)`[i], dat$`Actual.fire.size.mean.(ha)`[i])), 
                                         gsub(".*-", "", ifelse(dat.kind =='Intended', dat$`Intended.fire.size.mean.(ha)`[i], dat$`Actual.fire.size.mean.(ha)`[i])))))
            
          } else if (from == 'median') {
            
            lambda   <- log(2) /mean(as.numeric(c(sub("\\-.*", "", ifelse(dat.kind =='Intended', dat$`Intended.fire.size.mean.(ha)`[i], dat$`Actual.fire.size.mean.(ha)`[i])), 
                                        sub(".*-", "", ifelse(dat.kind =='Intended', dat$`Intended.fire.size.mean.(ha)`[i], dat$`Actual.fire.size.mean.(ha)`[i])))))
            
          } else if (from == 'max') {
            
            lambda  <- -(log(0.01) /mean(as.numeric(c(sub("\\-.*", "", ifelse(dat.kind =='Intended', dat$`Intended.fire.size.max.(ha)`[i], dat$`Actual.fire.size.max.(ha)`[i])), 
                          sub(".*-", "", ifelse(dat.kind =='Intended', dat$`Intended.fire.size.max.(ha)`[i], dat$`Actual.fire.size.max.(ha)`[i]))))))
            
          } else if (from == 'min') {
            
            lambda <- -(log(0.99) /mean(as.numeric(c(sub("\\-.*", "", ifelse(dat.kind =='Intended', dat$`Intended.fire.size.min.(ha)`[i], dat$`Actual.fire.size.min.(ha)`[i])), 
                                                     sub(".*-", "", ifelse(dat.kind =='Intended', dat$`Intended.fire.size.min.(ha)`[i], dat$`Actual.fire.size.min.(ha)`[i]))))))
            
          }
          
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
        
        ### mean to median
        
        if(has.mean & !has.median) {
        
        est_dat$Est.value[nrow(est_dat)]  <- dat.fill(from  = 'mean', to = 'median')
        update.rows()
          
        }
        
        
        ### mean from median
        
        if(has.median & !has.mean) {
        
        est_dat$Est.value[nrow(est_dat)]  <- dat.fill(from  = 'median', to = 'mean')
        update.rows()
        
        }
        
        ### median from max - max assumes values observed are at 99th%
        
        if(has.max & !has.median) {
        
          est_dat$Est.value[nrow(est_dat)]  <- dat.fill(from  = 'max', to = 'median')
          update.rows()
          
        }
        
        
        
        if (update == FALSE) {
          
          est_dat$Case.Study.ID[nrow(est_dat)]  <- dat$Case.Study.ID[i]
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
    
  
  }
  
  return(est_dat)  
  
  
}



##########################################################

### merge and compare estimated outputs

##########################################################

power.estimates <- estimate.metrics(est_fire)
est_extrap      <- est_fire
dict            <- list('Actual.median' = (which(colnames(est_extrap) == "Actual.fire.size.median.(ha)")), 
                        'Intended.median' = (which(colnames(est_extrap) == "Intended.fire.size.median.(ha)")))

for (i in 1:nrow(power.estimates)) {
  
  r <- which(est_extrap$Case.Study.ID == power.estimates$Case.Study.ID[i] & 
               est_extrap$AFT == power.estimates$AFT[i] & 
               est_extrap$Fire.intention == power.estimates$Fire.intention[i])
  
  est_extrap[r, as.numeric(dict[power.estimates$Est.metric[i]])] <- power.estimates$Est.value[i]
  
  print(i)
  
}



