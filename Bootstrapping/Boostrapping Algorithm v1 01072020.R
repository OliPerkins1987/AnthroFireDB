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


