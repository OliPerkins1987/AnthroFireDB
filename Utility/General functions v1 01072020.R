

##############################################################

### Convert continuous values to binned values in the Database

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

### Convert binned values to continuous

##############################################################

unbin <- function(data, metric = 'Size', method = 'Mean') {
  
  #metric should be either 'Size', 'Count', 'Return' or 'BApct'
  
  catfunc <- function(x) {
    
    ### takes a numeric vector
    
    if(metric == 'Size') {
      
      if(method == 'Mean') {
      
          y <- ifelse(x == '0', 0,
            ifelse(x == '0-1', 0.5, 
            ifelse(x == '1-2', 1.5, 
            ifelse(x == '2-5', 3.5, 
            ifelse(x == '5-10', 7.5, 
            ifelse(x == '10-20', 15, 
            ifelse(x == '20-50', 35, 
            ifelse(x == '50-100', 75,
            ifelse(x == '100-200', 150,
            ifelse(x == '200-500', 350,
            ifelse(x == '500-1000', 750, 
            ifelse(x == '1000-2500', 1750,
            ifelse(x == '2500-5000', 3750,
            ifelse(x == '5000-10000', 7500,
            ifelse(x == '10000-25000', 17500, 
            ifelse(x == '25000-', 50000, 
            x))))))))))))))))
      
      
      } else if (method == 'Random') {
             
        top    <- sub(".*-", "", x)
        bottom <- sub("\\-.*", "", x)
        top    <- ifelse(bottom == '25000' & top == '', '100000', top)
        
        y      <- runif(1, as.numeric(bottom), as.numeric(top))
        
           }
      
        
      
      ### Fire Count
      
    } else if(metric == 'Count') {
      
      top    <- sub(".*-", "", x)
      bottom <- sub("\\-.*", "", x)
      top    <- ifelse(bottom == '25000' & top == '', '100000', top)
      
        if(method == 'Mean') {
        
          y <- mean(c(as.numeric(bottom), as.numeric(top)))
        
      } else if (method == 'Random') {
        
          y      <- runif(1, as.numeric(bottom), as.numeric(top))
        
      }
      
      
      ### Burned area Percentage
      
    } else if(metric == 'BApct') {
      
      top    <- sub(".*-", "", x)
      bottom <- sub("\\-.*", "", x)

      if(method == 'Mean') {
        
        y <- mean(c(as.numeric(bottom), as.numeric(top)))
        
      } else if (method == 'Random') {
        
        y      <- runif(1, as.numeric(bottom), as.numeric(top))
        
      }
      
      
      ### Fire return
      
    } else if(metric == 'Return') {
      
      top    <- sub(".*-", "", x)
      bottom <- sub("\\-.*", "", x)
      
      if(method == 'Mean') {
        
        y <- mean(c(as.numeric(bottom), as.numeric(top)))
        
      } else if (method == 'Random') {
        
        y      <- runif(1, as.numeric(bottom), as.numeric(top))
        
      }
    
    
    }
    
    return(y)
    
    
    
  }
  
  return(lapply(data, catfunc))
  
}


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


###########################################################################

### Reset number in database

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


################################

### check for errors in case studies

################################

grep(' ', landuse$Case.Study.ID)
grep(' ', recordinfo$Case.Study.ID)
grep(' ', reported_fire$Case.Study.ID)
grep(' ', sup$Case.Study.ID)
grep(' ', policy$Case.Study.ID)
