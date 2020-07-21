###################################################################################

### Bootstrapping algorithm for analysis of global anthropogenic fire database

### Author: Oli Perkins, 30062020

###################################################################################

### When bootstrapping data, we need to include AFT - 
### This means sampling the rows rather than the values?

### How are we including escaped vs deliberate?

### How do we deal with records giving specific (presence only) and general (quantified)?


find.firedata <- function(dat = dat.filt, sample.weights = list(), reported_bias = 0.25, 
                          nsamples = 50, boot.thresh = 10, index = key, result_cols, start.case = '1a') {
  
  
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
  result_cols  <- result_cols[!result_cols %in% c("Case.Study.ID", "Actual.burned.area.(ha)", "Intended.burned.area.(ha)")]
  
  for (fire.metric in 10:26) {
    
    ### get values & establish sampling frequency (estimated vs reported)
    
    pot.vals <- list(reported = dat$reported[which(dat$reported$`Presence./.Absence` != 'Absence' & !is.na(dat$reported[, fire.metric])), c(1, fire.metric)], 
                     estimated = dat$estimated[which(dat$est$`Presence./.Absence` != 'Absence' & !is.na(dat$est[, fire.metric])), c(1, fire.metric)])
    
    probs    <- nrow(pot.vals$reported)/(nrow(pot.vals$reported) + 
                                           nrow(pot.vals$estimated))
    
    probs    <- min(c(0.9, ifelse(probs < 1, probs/(1-reported_bias), probs)))
    
    
    #############################################
    ### Bootstrap small samples
    #############################################
    
    if (length(unlist(pot.vals)) < boot.thresh) {
      
      ### pass for now
      1+1
      
    }
    
    #####################################################
    
    ### Get weights
    
    #####################################################
    
    if('Geographic' %in% names(sample.weights)) {
      
      w <- sample.weights$Geographic[[start.case]]
      
      pot.vals$reported$weights  <- w$distance[match(
                                    pot.vals$reported$Case.Study.ID, w$Case.Study.ID)]
      
      pot.vals$estimated$weights <- w$distance[match(
                                      pot.vals$estimated$Case.Study.ID, w$Case.Study.ID)] 
      
      ### deal with missing weights

      pot.vals$reported$weights <- ifelse(is.na(pot.vals$reported$weights), 
                                           0, pot.vals$reported$weights)
      
      pot.vals$estimated$weights <- ifelse(is.na(pot.vals$estimated$weights), 
                                           0, pot.vals$estimated$weights)
      
    }
    
    
    #####################################################
    
    ### Sample fire metric
    
    #####################################################
    
    ### Deal with cases with no data
    
    if (length(unlist(pot.vals)) == 0) {
      
      
      #print(paste0('No values for ', colnames(dat$reported)[fire.metric])) 
      
      out.dat[[colnames(dat$reported)[fire.metric]]] <- rep(NA, times = nsamples)
      
      
    } else if(length(unlist(pot.vals$estimated)) == 0) {
      
      # no estimated data
      
      if (length(sample.weights) > 0) {
      
      samp.rows <- sample(nrow(pot.vals$reported), nsamples, replace = T, prob = pot.vals$reported$weights)
      
      } else {
        
      samp.rows <- sample(nrow(pot.vals$reported), nsamples, replace = T)
        
      }
      
      out.dat[[colnames(dat$reported)[fire.metric]]]   <- pot.vals$reported[samp.rows, 2]
      
      
    } else if(length(unlist(pot.vals$reported))  == 0) {
      
      # no reported data
      
      if (length(sample.weights) > 0) {
      
      samp.rows <- sample(nrow(pot.vals$estimated), nsamples, replace = T, prob = pot.vals$estimated$weights)
        
      } else {
        
      samp.rows <- sample(nrow(pot.vals$estimated), nsamples, replace = T)
      
      }
      
      out.dat[[colnames(dat$reported)[fire.metric]]] <- unlist(unbin(pot.vals$estimated[samp.rows, 2], method = 'Random'))
      
      
    } else {
      
      ### both reproted and estimated data
      
      mixed.samp           <- character()
      
      #########################################
      
      ### do sampling
      
      #########################################
      
      for (samp in 1:nsamples) {

        samp.type  <- ifelse(runif(1) <= probs, 'reported', 'estimated')
        
        if (length(sample.weights) > 0) {
        
        samp.row   <- ifelse(samp.type == 'reported', 
                            sample(nrow(pot.vals$reported), 1, replace = T, prob = pot.vals$reported$weights),
                            sample(nrow(pot.vals$estimated), 1, replace = T, prob = pot.vals$estimated$weights))
          
        } else {
          
        samp.row   <- ifelse(samp.type == 'reported', 
                             sample(nrow(pot.vals$reported), 1, replace = T),
                             sample(nrow(pot.vals$estimated), 1, replace = T))
        
          }
        
        if(samp.type == 'reported') {mixed.samp <- c(mixed.samp, pot.vals$reported[samp.row, 2])
        } else {mixed.samp <- c(mixed.samp, unlist(unbin(pot.vals$estimated[samp.row, 2], method = 'Random')))} 
                                   
        
        }

      out.dat[[colnames(dat$reported)[fire.metric]]] <- mixed.samp
      
    }
    
  }
  
  out.dat <- lapply(out.dat, function(x) {x[1:nsamples]})
  out.dat <- data.frame(out.dat)
  colnames(out.dat) <- result_cols
  
  return(out.dat)
  
} 


#####################################################################

### 2) Make sample weights for bootstrapping function

#####################################################################

make.sampleweights <- function(dat = recordinfo, weight.type = 'Geographic') {
  
  if(weight.type == 'Geographic') {
    
    require(geosphere)
    
    ## split dataframe
    dat <- dat[, c(3,9, 10)]
    dat <- rep(data.frame(dat), times = length(unique(dat$Case.Study.ID)))
    
    key <- data.frame(dat)
    key <- split(colnames(key), rep(1:length(unique(dat$Case.Study.ID)), each = 3))
    
    dat  <- data.frame(dat)
    dat <- lapply(seq_along(key), function(x) {dat[, which(colnames(dat) %in% key[[x]])]})
    
    ### calculate weights
    
    weight.calc <- function(chunk, from = 1) {
      
      chunk$distance <- distGeo(matrix(data = c(as.numeric(rep(chunk[from, 3], times = nrow(chunk))), 
                                                as.numeric(rep(chunk[from, 2], times = nrow(chunk)))), ncol = 2, nrow = nrow(chunk)), 
                                matrix(data = c(as.numeric(chunk[, 3]), as.numeric(chunk[, 2])), ncol = 2, nrow = nrow(chunk)))
      
      
      chunk$distance <- 1 - (chunk$distance / max(chunk$distance, na.rm = T))
      
      return(chunk)
      
    }
    
    dat <- lapply(dat, weight.calc)
    
  }
  
  return(dat)
  
}

weights        <- make.sampleweights()
names(weights) <- unique(recordinfo$Case.Study.ID)
weights        <- lapply(weights, function(x) x[, c(1, 4)])



########################################################################

### 3) run boostrapping

########################################################################

###############################
### Add capability to include calculated metrics
##############################

iterations    <- 200
results.frame <- list()

set.seed(1988)

for (i in 1:length(unique(reported_fire$Fire.intention))) {
  
  ### filter data
  
  key       <- unique(reported_fire$Fire.intention)[i]
  
  dat.filt  <- list('reported' = reported_fire[reported_fire$Fire.intention == key, ], 
                    'estimated' = est_fire[est_fire$Fire.intention == key, ])
  
  for(iteration in 1:iterations){
    
    #print(paste0(iteration, ' of ', iterations, ' samples.'))
    
    start.row <- dat.filt$reported[sample(1:nrow(dat.filt$reported), 1), c(1, 4, 8, 10:28)]
    print(start.row$Case.Study.ID)
    
    ### deal with absence
    
    if(start.row$`Presence./.Absence` == 'Absence') {
      
      start.row           <- start.row[, -c(which(colnames(start.row) %in% c("Case.Study.ID", "Actual.burned.area.(ha)", "Intended.burned.area.(ha)")))]
      
      
      if(length(results.frame) >= 1) {
        
        colnames(start.row) <- colnames(results.frame[[1]])
        
      }
      
      
      
      
      ### Should absence data be factored in as a spatial sample of presence %ct?
      
      results.frame[[length(results.frame) + 1]] <- start.row
      
    } else {
      
      
      if(length(results.frame) >= 1) {
        
        results.frame[[length(results.frame) + 1]]   <- find.firedata(nsamples = 1, 
                                                              result_cols = colnames(results.frame[[1]]), 
                                                              start.case  = start.row$Case.Study.ID, 
                                                              sample.weights = list('Geographic' = weights))
        
      } else {
        
        results.frame[[length(results.frame) + 1]]   <- find.firedata(nsamples = 1, 
                                                              result_cols = colnames(start.row), 
                                                              start.case  = start.row$Case.Study.ID, 
                                                              sample.weights = list('Geographic' = weights))
        
      }
      
      
    }
    
  }
  
  print(i)
}

results.frame         <- plyr::rbind.fill(results.frame)
results.frame[, 3:16] <- apply(results.frame[, 3:16], 2, as.numeric)


  
