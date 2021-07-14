#load database with some manipulation
################################

load.db <- function(){
   
   dbstring      <- 'DAFI.xlsx'
   download.file('https://github.com/OliPerkins1987/AnthroFireDB/blob/master/Database/Database_v1_15.xlsx?raw=true', 
                 dbstring, mode = "wb", quiet = TRUE)
   
   env = .GlobalEnv
   
   #record info
   rec          <- read_excel(dbstring, sheet = 1)
   rec          <- data.frame(rec[, 1:16])
   rec[, c(5, 6, 11, 14, 15)]        <- apply(rec[, c(5, 6, 11, 14, 15)], 2, 
                                              function(x) {ifelse(x == 'ND', NA, x)})
   assign('record_info', rec, envir = env)
   
   #land-use  
   LULC                              <- read_excel(dbstring, sheet = 2)
   LULC[, c(2, 3, 5, 6, 7, 8:19)]    <- apply(LULC[, c(2, 3, 5, 6, 7, 8:19)], 2, 
                                              function(x) {ifelse(x == 'ND', NA, x)})
   assign('land_use', LULC, envir = env)
   
   #reported fire use
   fire                            <- read_excel(dbstring, sheet =  3)
   fire[, c(6, 7, 10:28)]          <- apply(fire[, c(6, 7, 10:28)], 2, function(x) {ifelse(x == 'ND', NA, x)})
   fire[, c(10:26)]                <- apply(fire[, c(10:26)], 2, function(x) {as.numeric(as.character(x))})
   fire[, c(27, 28)]               <- lapply(fire[, c(27, 28)], function(x){as.factor(x)})
   fire <- merge(land_use[, c(1, 3)], fire, by = 'Case Study ID', all.x = F, all.y = T)
   assign('rep_fire_use', fire, envir = env)
   
   #estimated fire use  
   est_fire_use                        <- read_excel(dbstring, sheet = 4)
   est_fire_use[, c(6, 7, 10:27)]      <- apply(est_fire_use[, c(6, 7, 10:27)], 2, function(x) {ifelse(x == 'ND', NA, x)})
   est_fire_use[, c(10:26)]            <- lapply(est_fire_use[, c(10:26)], function(x){as.factor(x)})
   est_fire_use                        <- est_fire_use[1:nrow(rep_fire_use), ]
   est_fire_use$`Fire purpose`         <- gsub("&amp;", "&", est_fire_use$`Fire purpose`)
   est_fire_use <- merge(land_use[, c(1, 3)], est_fire_use, by = 'Case Study ID', all.x = F, all.y = T)
   assign('est_fire_use', est_fire_use, envir = env)
   
   #suppression
   sup                             <-  read_excel(dbstring, sheet = 5)
   sup[, -c(11, 13, 14)]           <-  lapply(sup[, -c(11, 13, 14)],factor) 
   sup <- merge(land_use[, c(1, 3)], sup, by = 'Case Study ID', all.x = F, all.y = T)
   assign('suppression', sup , envir = env)
   
   #policy
   firepol                         <- read_excel(dbstring, sheet = 6)
   firepol[, 1:22]                 <- lapply(firepol[, 1:22], factor)
   firepol <- merge(land_use[, c(1, 3)], firepol, by = 'Case Study ID', all.x = F, all.y = T)
   assign('policy', firepol, envir = env)

}
################################

# Summarise database metrics
################################
summarise.behaviour <- function(temp.rep_fire_use, temp.est_fire_use, type = c('Records', 'Land use', 'Fire', 'Suppression', 'Policy'), choose = '',
                                sum_multi = c('Case Study ID', 'Anthropogenic fire regime', 'AFT', 'Fire purpose', 'Actual land cover', 'Study Year'),
                                behaviour, grouping = 'Case Study ID', inc.Absence = F, escape.rm = F) {
   
   
   ### 1) Summarise database metrics
   
   ### Creates a dplyr tbl containing a summary of a given database value
   
   #### Set up data
   
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
      
      dat <- record_info
      
   } else if(type == 'Land use') {
      
      dat <- land_use
      
   } else if (type == 'Fire') {
      
      dat <- list(temp.rep_fire_use, temp.est_fire_use)
      
      if (!behaviour %in% colnames(dat[[1]])) {
         
         stop('Behaviour must be a given column name')
         
      }
      
      if (escape.rm == T) {
         
         dat <- lapply(dat, function(x) {x[x$`Fire type` == 'Human, deliberate', ]})
         
      }
      
   } else if (type == 'Suppression') {
      
      dat <- suppression 
      
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
      
      #add mode function to summarise https://stackoverflow.com/a/38194749
      
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
################################

# Simplify suppression
################################
Simplify.suppression <- function(x){
   
   y <- ifelse(x == 'Fire control (0-3)', 'Control', 
               ifelse(x == 'Fire prevention (0-3)', 'Prevention', 
                      'Extinction'))
   return(y)
   
}
################################


# Simplify fire intentions
################################
Simplify.purpose <- function(x = reported_fire$`Fire purpose`, 
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
################################


# Convert binned values to continuous
################################


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
################################

