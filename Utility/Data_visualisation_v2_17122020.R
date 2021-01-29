###############################################################################

### The following code provides functions to drive key reports and maps from DAFI

### Author: Oli Perkins, 30 June 2020

###############################################################################




#############################################################################################

### 1) Summarise database metrics

### Creates a dplyr tbl containing a summary of a given database value

#############################################################################################

summarise.behaviour <- function(type = c('Records', 'Land use', 'Fire', 'Suppression', 'Policy'), choose = '',
                                sum_multi = c('Case Study ID', 'AFT', 'Fire intention', 'Actual land cover', 'Study Year'),
                                behaviour, grouping = 'Case Study ID', inc.Absence = F, escape.rm = F) {
  
  
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
    
    dat <- Landuse
    
  } else if (type == 'Fire') {
    
    dat <- list(reported_fire, est_fire)
    
    if (!behaviour %in% colnames(dat[[1]])) {
      
      stop('Behaviour must be a given column name')
      
    }
    
    if (escape.rm == T) {
      
      dat <- lapply(dat, function(x) {x[x$`Fire type` == 'Human, deliberate', ]})
      
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
    
    dat[[1]] <- dat[[1]][dat[[1]]$`Presence / Absence` == 'Presence', ]
    
    dat[[2]] <- dat[[2]][dat[[2]]$`Presence / Absence` == 'Presence', ]
    
  }
  
  if(choose != '') {
    
    dat      <- data.frame(dat %>% filter(eval(parse(text = choose))))
    behaviour<- sub("\\(", '.', behaviour)
    behaviour<- sub("\\)", '.', behaviour)
    behaviour<- sub(" ", ".", behaviour)
    behaviour<- sub(" ", ".", behaviour)
    behaviour<- sub("-", '.', behaviour)
    
  }
  
  
  ##################################
  
  ### Create summary data
  
  #################################
  
  if(type == 'Fire') {
    
    if(sum_multi[1] != FALSE & !grepl('size', behaviour)) {
      
      dat[[1]] <- dat[[1]] %>% dplyr::group_by_at(sum_multi) %>%
        summarise(!!behaviour := sum(base::get(behaviour), na.rm = T)) %>% 
        filter(base::get(behaviour) != 0)
      
      dat[[2]] <- dat[[2]] %>% dplyr::group_by_at(sum_multi) %>%
        summarise(!!behaviour := sum(unlist(unbin(base::get(behaviour))), na.rm = T)) %>%
        filter(base::get(behaviour) != 0)
      
    }
    
    
    ### reported
    Quant.stat <- dat[[1]] %>% dplyr::group_by_at(grouping) %>% 
      filter(!is.na(base::get(behaviour))) %>%
      summarise('Reported.stat' = mean(base::get(behaviour), na.rm = T), 
                'Reported.N'    = n())
    
    ### estimated
    
    if(sum_multi[1] == FALSE | grepl('size', behaviour)) {
      
      dat[[2]][, which(colnames(dat[[2]]) == behaviour)] <- unlist(
        unbin(data.frame(dat[[2]])[, which(colnames(dat[[2]]) == behaviour)]))
      
    }
    
    
    Est.stat  <- dat[[2]] %>% dplyr::group_by_at(grouping) %>% 
      filter(!is.na(base::get(behaviour))) %>%
      summarise('Est.stat' = mean(base::get(behaviour), na.rm = T), 
                'Est.N'    = n())
    
    ### combine
    Results               <- merge(Quant.stat, Est.stat, by = grouping, 
                                   all.x = T, all.y = T)
    
    Results$Combined.stat <- ((Results$Reported.N * Results$Reported.stat) + 
                                (Results$Est.stat * Results$Est.N)) / (Results$Reported.N + Results$Est.N)
    
    Results$Combined.stat <- ifelse(is.na(ifelse(is.na(Results$Combined.stat), Results$Reported.stat, 
                                                 Results$Combined.stat)), Results$Est.stat, ifelse(is.na(Results$Combined.stat), 
                                                                                                   Results$Reported.stat, Results$Combined.stat))
    
    Results$Combined.N    <- ifelse(is.na(Results$Reported.N + Results$Est.N), 
                                    Results$Reported.N, Results$Reported.N + Results$Est.N)
    
    Results$Combined.N    <- ifelse(is.na(Results$Combined.N), Results$Est.N, Results$Combined.N)
    
    
  } else if (type == 'Suppression') {
    
    Sup.counts     <- lapply(split(dat, dat[grouping]), 
                             function(x) {data.frame(table(x[behaviour]))})
    
    reps           <- as.numeric(table(unlist(lapply(names(Sup.counts), function(x) {rep(x, times = nrow(Sup.counts[x][[1]]))}))))
    Sup.names      <- unlist(sapply(1:length(reps), function(i){rep(names(Sup.counts)[i], times = reps[i])}))
    
    Sup.counts     <- plyr::rbind.fill(Sup.counts)
    Sup.counts$AFT <- as.character(Sup.names)
    colnames(Sup.counts)[colnames(Sup.counts) == 'Var1'] <- behaviour
    Results        <- pivot_wider(Sup.counts, names_from = all_of(behaviour), values_from = "Freq")
    
    
  } else if(type == 'Policy') {
    
    Results <- dat %>% dplyr::group_by_at(grouping) %>% 
      filter(!is.na(base::get(behaviour)) & base::get(behaviour) != 'ND') %>%
      summarise('Reported.stat' = length(which(grepl('yes', tolower(base::get(behaviour))))) / n(), 
                'Reported.N'    = n())
    
  } else {
    
    Results <- dat %>% dplyr::group_by_at(grouping) %>% 
      filter(!is.na(base::get(behaviour))) %>%
      summarise('Reported.stat' = mean(base::get(behaviour), na.rm = T), 
                'Reported.N'    = n())
    
    
  }
  
  
  Results$Behaviour <- behaviour
  
  return(Results)
  
  
}


#############################################################################################

### 2) Plot fire behaviour

#############################################################################################


plot.behaviour <- function(metrics = 'Actual fire size mean (ha)', agg = F, Cropland = T, bin_width = 0.5,
                           Xaxis_label = bquote('Number of fires ('*Km^-2~Year^-1*')'), log_scale = T, 
                           scale_limits = c(0, 30)) {
  
  
  ### filter and bind data
   
  if(agg == T) {
  
  rep.temp <- plyr::rbind.fill(lapply(metrics, function(x) {reported_fire %>% 
                filter(`Presence / Absence` == 'Presence') %>%
                 dplyr::group_by_at(c('Case Study ID', 'Fire type','Intended land cover', 'Study Year')) %>%
                  summarise(!!x := sum(base::get(x), na.rm = T)) %>% filter(base::get(x) != 0)}))
  
  est.temp <- plyr::rbind.fill(lapply(metrics, function(x) {est_fire %>% 
                filter(`Presence / Absence` == 'Presence') %>%
                  dplyr::group_by_at(c('Case Study ID', 'Fire type', 'Intended land cover', 'Study Year')) %>%
                    summarise(!!x := sum(unlist(unbin(base::get(x), method = 'Random')), na.rm = T)) %>% 
                      filter(base::get(x) != 0)}))
  
  
  } else {
    
    
    rep.temp <- reported_fire %>% filter(`Presence / Absence` == 'Presence' & grepl('human, deliberate', tolower(`Fire type`))) %>%
                  select(starts_with(metrics) | `Intended land cover` | `Fire type`) %>% mutate('Reported' = 'Reported') 
  
    est.temp <- data.frame(apply(data.frame(est_fire %>% filter(`Presence / Absence` == 'Presence' & grepl('human, deliberate', tolower(`Fire type`))) %>%
                  select(starts_with(metrics))), 2, function(x) {unlist(unbin(x, method = 'Random'))})) %>%
                    mutate('Intended land cover' = rep.temp$`Intended land cover`, 
                           'Fire type' = rep.temp$`Fire type`, 
                           'Reported' = 'Estimated')
    
    
  }
  
  
  colnames(est.temp)   <- colnames(rep.temp)
  dat.temp             <- rbind(rep.temp, est.temp)
  
  ### Combine relevant columns
  
  dat.temp             <- pivot_longer(dat.temp, cols = metrics,
                                        values_to = 'value', names_to = 'value_key')
  
  
  if(Cropland == T) {
  
  dat.temp$`Fire class` <- dat.temp$`Fire type`
  dat.temp$`Fire type`  <- ifelse(dat.temp$`Intended land cover` %in% c('Cropland', 'Secondary Vegetation'), 'Cropland', 'Broadcast')  
  
  
  p <- dat.temp %>% filter(`Fire class` == 'Human, deliberate') %>% 
    ggplot(aes(x = value, fill = `Fire type`)) + 
    geom_histogram(colour = 'black', binwidth = bin_width) + 
    theme_classic() +  xlab(Xaxis_label) + scale_fill_viridis_d() + 
    theme(text = element_text(size=14)) + scale_x_continuous(limits = scale_limits)
  
  
  } else {
    
    
  p <- dat.temp %>% filter(`Fire type` == 'Human, deliberate') %>% 
    ggplot(aes(x = value, fill = `value_key`)) + 
    geom_histogram(colour = 'black', binwidth = bin_width) + 
    theme_classic() +  xlab(Xaxis_label) + scale_fill_viridis_d() + 
    theme(text = element_text(size=14)) + scale_x_continuous(limits = scale_limits)
    
  }
  
  if(log_scale == T) {
    
  p <- p + scale_x_continuous(trans = scales::log_trans(), limits = scale_limits,
                    labels = function(x) sprintf("%g", x),
                         breaks = scales::log_breaks()) +  xlab(Xaxis_label)
    
    }

  
  return(p)
  
  
  
  
}
  
  

#############################################################################################

### 3) Map database metrics

### Creates a map plot of database values - driven by ggplot2

#############################################################################################


map.behaviour <- function(type = c('Records', 'Land use', 'Fire', 'Suppression', 'Policy'), 
                          ggcolour = NULL, ggshape = NULL, choose = NULL, point.size = 1.5) {
  
  require(tidyverse)
  require(ggplot2)

  
  ## function to map attributes from the database
  
  ## Fire data uses discrete estimated bins
  
  ## choose should be a string containing the contents of a dplyr filter expression
  
  ## ggcolour and ggshape should be the intended column names passed to ggplot shape and colour aesthetics
  
  if(type == 'Records') {
    
    dat <- recordinfo
    
  } else if(type == 'Land use') {
    
    dat <- merge(recordinfo, Landuse, by.x= 'Case.Study.ID', 
                 by.y = 'Case Study ID')
    
  } else if (type == 'Fire') {
    
    dat <- merge(recordinfo, est_fire, by.x= 'Case.Study.ID', 
                 by.y = 'Case Study ID') 
    
  } else if (type == 'Suppression') {
    
    dat <- merge(recordinfo, sup, by.x= 'Case.Study.ID', 
                 by.y = 'Case Study ID') 
    
  } else if (type =='Policy') {
    
    dat <- merge(recordinfo, policy, by.x= 'Case.Study.ID', 
                 by.y = 'Case Study ID')
    
  } else {
    
    stop('Map type specified incorrectly')
    
  }
  
  #### Filter
  
  if (!is.null(choose)) {
    
    dat      <- dat %>% filter(eval(parse(text = choose)))
    
  }
  
  dat$Latitude <- as.numeric(dat$Latitude)
  dat$Longitude<- as.numeric(dat$Longitude) 

  
  g <- ggplot(dat, aes_string(x = 'Longitude', y = 'Latitude', colour = ggcolour, shape = ggshape)) +
    geom_point(size = point.size, position='jitter') + theme_classic() + 
    borders() + scale_colour_viridis_d() + scale_shape_manual(values = c(18, 3, 16, 7, 8, 15, 19, 25))
  
  return(g)
  
}









