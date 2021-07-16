# Box plot (fire use)
################################
box.fireuse.regime <- function(temp.rep_fire_use, temp.est_fire_use, metric = 'fire size mean (ha)', boxtype='box', estimated = T, reported = T,
                               log_scale = F, cropland = T, scale_limits = c(0, 1000)) {
   
   
   
   n_fun <- function(x){
      return(data.frame(y = y.lim, label = paste0(length(x))))
   }
   
   temp.data <- function(input.data=temp.rep_fire_use, reported='Reported', metric=metric){
      
      dat.temp <- input.data %>% 
         filter(`Presence / Absence` == 'Presence' & grepl('human, deliberate', tolower(`Fire type`))) %>%
         dplyr::select(contains(metric) | `Anthropogenic fire regime` | `Fire purpose`) %>% 
         mutate('Reported' = reported) %>%
         pivot_longer(cols = contains(metric), values_to = 'value', names_to = 'measure') %>% 
         dplyr::select(-measure)
      
      return(dat.temp)
   }
   
   
   if(reported) {
      if(length(dplyr::select(temp.rep_fire_use, contains(metric))) > 0){
         
         rep.temp <- temp.data(temp.rep_fire_use, 'Reported', metric)
         
      } else {
         print("No reported data")
         reported <- F
      }
   }
   
   if(estimated){
      if(length(dplyr::select(temp.est_fire_use, contains(metric))) > 0){
         
         est.temp <- temp.data(temp.est_fire_use, 'Estimated', metric)
         
         est.temp$value <- apply(data.frame(est.temp$value), 2, function(x) {unlist(unbin(x, method = 'Random'))})
         
         est.temp$value[is.nan(est.temp$value)] <- NA 
         
      } else {
         print("No estimated data")
         estimated <- F
      }
   }
   
   if(reported || estimated){
      
      if(reported && !estimated){ 
         dat.temp <- rep.temp 
         plot.title <- "Reported Data"
      }
      if(estimated && !reported){ 
         dat.temp <- est.temp 
         plot.title <- "Estimated Data"
      }
      if(reported && estimated) {
         colnames(est.temp)   <- colnames(rep.temp)
         dat.temp             <- rbind(rep.temp, est.temp)
         plot.title <- "Reported and Estimated Data"
      } 
      
      
      dat.temp <- dat.temp[!is.na(dat.temp$`Anthropogenic fire regime`),]
      dat.temp <- dat.temp[!is.na(dat.temp$value),]
      dat.temp$`Anthropogenic fire regime` <- factor(dat.temp$`Anthropogenic fire regime`, 
                                                     levels = c("Pre-industrial","Transition",
                                                                "Industrial","Post-industrial"))
      
      if(cropland){
         dat.temp$`Fire type`  <- ifelse(dat.temp$`Fire purpose` %in% c('Crop field preparation', 'Crop residue burning'), 'Cropland', 'Broadcast')  
         
         p <- dat.temp %>% 
            ggplot(aes(y = value, x = `Anthropogenic fire regime`, fill=`Fire type`))
      }
      else {
         p <- dat.temp %>% 
            ggplot(aes(y = value, x = `Anthropogenic fire regime`))
      }
      
      if(boxtype=='box'){
         p <- p + 
            geom_boxplot() +
            ylab(metric) + scale_fill_viridis_d(alpha=0.5) +
            theme(text = element_text(size=14)) 
      }
      
      if(boxtype=='violin'){
         
         p <- p + 
            geom_violin() +
            ylab(metric) + scale_fill_viridis_d(alpha=0.5) +
            theme(text = element_text(size=14)) 
      }
      
      if(log_scale == T) {
         
         y.lim <- log(scale_limits[2])
         
         p <- p + scale_y_continuous(trans = scales::log_trans(), limits = scale_limits,
                                     labels = function(x) sprintf("%g", x),
                                     breaks = scales::log_breaks()) +  ylab(metric) +
            stat_summary(fun.data = n_fun, geom = "text",position = position_dodge(0.7))
         
      } else {
         
         y.lim <- scale_limits[2]
         
         p <- p + scale_y_continuous(limits = scale_limits) + 
            stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(0.7))
      }
      
      p <- p + ggtitle(plot.title)
      
      return(p)
   } else{ print ("One of estimated or reported must be TRUE") }
}
################################


# Box plot (fire size)
################################
box.FS.regime <- function(dat.temp, metric = 'mean', boxtype='box', estimated = T, reported = T,
                          log_scale = F, cropland = T, scale_limits = c(0, 1000)) {
   
   n_fun <- function(x){
      return(data.frame(y = y.lim, label = paste0(length(x))))
   }
   
   dat.temp <- dat.temp %>% filter(grepl(metric, Behaviour))
   dat.temp <- dat.temp[!is.na(dat.temp$`Anthropogenic fire regime`),]
   dat.temp$`Anthropogenic fire regime` <- factor(dat.temp$`Anthropogenic fire regime`, 
                                                  levels = c("Pre-industrial","Transition",
                                                             "Industrial","Post-industrial"))
   
   if(reported || estimated){
      
      if(reported && !estimated){
         value <- 'Reported.stat'
         plot.title <- "Reported Data"
      }
      if(estimated && !reported){
         value <- 'Est.stat'
         plot.title <- "Estimated Data"
      }
      if(reported && estimated) {
         value <- "Combined.stat"
         plot.title <- "Reported and Estimated Data"
      } 
      
      if(cropland){
         dat.temp$`Fire type`  <- ifelse(dat.temp$`Fire purpose` %in% c('Crop field preparation', 'Crop residue burning'), 'Cropland', 'Broadcast')
         
         p <- dat.temp %>% 
            ggplot(aes(y = .data[[value]], x = `Anthropogenic fire regime`, fill=`Fire type`))
      }
      else {
         p <- dat.temp %>% 
            ggplot(aes(y = .data[[value]], x = `Anthropogenic fire regime`))
      }
      
      if(boxtype=='box'){
         p <- p +
            geom_boxplot(alpha=0.8) +
            ylab(paste0(metric, " fire size (ha)")) + 
            scale_fill_manual(values=c(vir_10[7], vir_10[10])) +
            #scale_fill_viridis_d(alpha=0.8,option='inferno') +
            theme(text = element_text(size=14))
      }
      
      if(boxtype=='violin'){
         p <- p +
            geom_violin() +
            ylab(paste0(metric, " fire size (ha)")) + scale_fill_viridis_d(alpha=0.8,option='inferno') +
            theme(text = element_text(size=14))
      }
      
      if(log_scale == T) {
         
         y.lim <- log(scale_limits[2])
         
         p <- p + scale_y_continuous(trans = scales::log_trans(), limits = scale_limits,
                                     labels = function(x) sprintf("%g", x),
                                     breaks = scales::log_breaks()) +  ylab(paste0(metric," fire size (ha)")) +
            stat_summary(fun.data = n_fun, geom = "text",position = position_dodge(0.7))
         
      } else {
         
         y.lim <- scale_limits[2]
         
         p <- p + scale_y_continuous(limits = scale_limits) + 
            stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(0.7))
      }
      
      #p <- p + ggtitle(plot.title)
      
      return(p)
   } else{ print ("One of estimated or reported must be TRUE") }
}
################################


#Box plot (burned area)
################################
box.BA.regime <- function(dat.temp=burned.area,metric = 'burned area (ha)', boxtype='box', 
                          log_scale = T, cropland = T, scale_limits = c(0.001, 1000000000)) {
   
   
   n_fun <- function(x){
      return(data.frame(y = y.lim, label = paste0(length(x))))
   }
   
   
   dat.temp <- dat.temp %>%
      drop_na(`Anthropogenic fire regime`) %>%
      mutate(`Anthropogenic fire regime` = factor(`Anthropogenic fire regime`, levels = c("Pre-industrial","Transition","Industrial","Post-industrial"))) %>%
      filter(Combined.stat > 0.01) %>%
      filter(grepl(metric,Behaviour,fixed=TRUE)) 
   
   if(cropland){
      dat.temp$`Fire type`  <- ifelse(dat.temp$`Fire purpose` %in% c('Crop field preparation', 'Crop residue burning'), 'Cropland', 'Broadcast')  
      
      p <- dat.temp %>% 
         ggplot(aes(y = Combined.stat, x = `Anthropogenic fire regime`, fill=`Fire type`))
   }
   else {
      p <- dat.temp %>% 
         ggplot(aes(y = Combined.stat, x = `Anthropogenic fire regime`))
   }
   
   if(boxtype=='box'){
      p <- p + 
         geom_boxplot(alpha=0.8) +
         ylab(metric) + 
         scale_fill_manual(values=c(vir_10[7], vir_10[10])) +
         #scale_fill_viridis_d(alpha=0.5) +
         theme(text = element_text(size=14)) 
   }
   
   if(boxtype=='violin'){
      
      p <- p + 
         geom_violin(alpha=0.8) +
         ylab(metric) + 
         scale_fill_manual(values=c(vir_10[7], vir_10[10])) +
         #scale_fill_viridis_d(alpha=0.5) +
         theme(text = element_text(size=14)) 
   }
   
   if(log_scale == T) {
      
      y.lim <- log(scale_limits[2])
      
      p <- p + scale_y_continuous(trans = scales::log_trans(), limits = scale_limits,
                                  labels = function(x) sprintf("%g", x),
                                  breaks = scales::log_breaks()) +  ylab(metric) +
         stat_summary(fun.data = n_fun, geom = "text",position = position_dodge(0.7))
      
   } else {
      
      y.lim <- scale_limits[2]
      
      p <- p + scale_y_continuous(limits = scale_limits) + 
         stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(0.7))
   }
   
   return(p)
}
################################


# Bar plot
################################
bar.purpose.regime <- function(temp.rep_fire_use, temp.est_fire_use, bartype='fill', estimated = T, reported = T, umode=T) {
   
   ####  utility function
   UniqueMode <- function(x) {
      #combo from https://stackoverflow.com/a/38194749 and https://stackoverflow.com/a/49570596
      ux <- unique(x)                   #create list of unique values
      tbl <-   tabulate(match(x, ux))   #count number of each unique values
      maxima <- which(tbl == max(tbl))  #return list of unique values that are most commonly found
      
      #if there is more than one most commonly found unique value
      if(length(maxima) > 1){
         mm <- ux[sample(maxima, 1)]  #pick one of the most commonly found unique values at random
      } else {
         mm <- ux[tbl==max(tbl)]    #otherwise, select the single most commonly found value
      }
      return(mm)
   }
   
   #manip data with distinct (with ties first found is reported as mode)
   mode_data <- function(input.data=rep.temp, report='Reported'){
      
      dat.temp <- input.data %>%
         filter(`Presence / Absence` == 'Presence' & grepl('human, deliberate', tolower(`Fire type`))) %>%
         dplyr::select(Purpose | `Anthropogenic fire regime` | `Fire creation ID`) %>%
         distinct(`Fire creation ID`, .keep_all = TRUE) %>%
         mutate('Reported' = report) 
      
      return(dat.temp)
   }
   
   #manip data with UniqueMode function
   mode_data_unique <- function(input.data=rep.temp, report='Reported'){
      
      dat.temp <- input.data %>%
         filter(`Presence / Absence` == 'Presence' & grepl('human, deliberate', tolower(`Fire type`))) %>%
         dplyr::select(Purpose | `Anthropogenic fire regime` | `Fire creation ID`) %>%
         group_by(`Fire creation ID`, `Anthropogenic fire regime`) %>%
         summarise(Purpose = UniqueMode(Purpose)) %>%
         mutate('Reported' = report) 
      
      return(dat.temp)
   }
   
   
   if(reported) {
      if(length(dplyr::select(temp.rep_fire_use, `Fire purpose`)) > 0){
         
         rep.temp <- temp.rep_fire_use
         rep.temp$Purpose = Simplify.purpose(temp.rep_fire_use$`Fire purpose`, AFT = temp.rep_fire_use$AFT)
         
         if(umode) { rep.temp <- mode_data_unique(rep.temp, 'Reported') 
         } else { rep.temp <- mode_data(rep.temp, 'Reported') }
         
      } else {
         print("No reported data")
         reported <- F
      }
   }
   
   if(estimated){
      if(length(dplyr::select(temp.est_fire_use, `Fire purpose`)) > 0){
         
         est.temp <- temp.est_fire_use
         est.temp$Purpose = Simplify.purpose(x=temp.est_fire_use$`Fire purpose`, AFT=temp.est_fire_use$AFT)
         
         if(umode) { est.temp <- mode_data_unique(est.temp, 'Estimated')
         } else { est.temp <- mode_data(est.temp, 'Estimated') }
         
      } else {
         print("No estimated data")
         estimated <- F
      }
   }
   
   if(reported || estimated){
      
      if(reported && !estimated){ 
         dat.temp <- rep.temp 
         #plot.title <- "Reported Data"
      }
      if(estimated && !reported){ 
         dat.temp <- est.temp 
         #plot.title <- "Estimated Data"
      }
      if(reported && estimated) {
         colnames(est.temp)   <- colnames(rep.temp)
         dat.temp             <- rbind(rep.temp, est.temp)
         #plot.title <- "Reported and Estimated Data"
      } 
      
      
      dat.temp <- dat.temp[!is.na(dat.temp$Purpose),]
      dat.temp$Purpose <- factor(dat.temp$Purpose, 
                                 levels = c('Crop field preparation',
                                            'Crop residue burning',
                                            'Pasture management',
                                            'Hunter gatherer',
                                            'Vegetation clearance',
                                            'Pyrome management',
                                            'Arson','Other'
                                 )) 
      
      dat.temp <- dat.temp[!is.na(dat.temp$`Anthropogenic fire regime`),]
      dat.temp$`Anthropogenic fire regime` <- factor(dat.temp$`Anthropogenic fire regime`, 
                                                     levels = c("Pre-industrial","Transition",
                                                                "Industrial","Post-industrial"))
      
      p <- dat.temp %>% 
         ggplot(aes(x = `Anthropogenic fire regime`, fill=Purpose))
      
      if(bartype=='dodge') {
         p <- p + geom_bar(position="dodge") 
      } else if(bartype=='fill') {
         
         p <- p + geom_bar(position="fill") +
            ylab("Proportion")
         
      } else {
         p <- p + geom_bar(position="stack")
      }
      
      
      p <- p + 
         #ggtitle(plot.title) +
         scale_fill_viridis_d(option = 'inferno') 
      
      return(p)
   } else{ print ("One of estimated or reported must be TRUE") }
}
################################


# map behaviour (raster)
################################
map.behaviour.ras <- function(dat.field, ras.res=2, ras.function=c('count','mode')){
  
   
   ### 4) Map database metrics RASTER
   
   ### Author: James Millington, 03 Feb 2021
   
   ### Creates a raster map plot of database values - driven by levelplot
   
   ### Currently count and mode fnctions available for rasterizing
   
   ### Currently only tested for 'Fire use type' and 'Data Source' fields 
    
   require(raster)
   require(rasterVis)
   require(latticeExtra)
   require(maps) 
   require(maptools)
   
   #dat.field = 'Fire purpose'
   #ras.function='mode'
   #ras.res=2
   
   ###
   ### Create raster functions here, passed to fun in rasterize
   ### These might be actual function definitions as for 'mode'
   ### Or they might be one of the special 'character values' as detailed in rasterize help
   ###  
   
   if(ras.function == 'mode'){
      #function for mode from https://www.tutorialspoint.com/r/r_mean_median_mode.htm
      ras.func <- function(v,...) {
         uniqv <- unique(v) 
         uniqv[which.max(tabulate(match(v, uniqv)))]
      }
   }  else if(ras.function == 'count') {
      ras.func <- 'count'
   } else {
      stop('ras.function specified incorrectly')
   }
   
   
   ###
   #Create and manipulate data 
   ###
   fdat <- merge(record_info, est_fire_use, by.x= 'Case.Study.ID',
                 by.y = 'Case Study ID') 
   
   #filter the missing value
   fdat <- fdat[fdat$Longitude != "ND",]
   
   ###
   ###Rasterizing requires a numeric field s
   ###So convert field to numeric if needed
   if(dat.field=='Fire purpose'){
      #list of fire use types in the order we want to present
      flist <-  c('Crop field preparation',
                  'Crop residue burning',
                  'Pasture management',
                  'Hunter gatherer',
                  'Vegetation clearance',
                  'Pyrome management',
                  'Arson','Other') 
      
      fdat$`Fire purpose` <- Simplify.purpose(x=fdat$`Fire purpose`, AFT=fdat$`Anthropogenic fire regime`)
   }
   
   if(dat.field=='Data.Source.s.'){
      #list of data sources in the order we want to present
      flist <-  c('Primary',
                  'Secondary',
                  'Remote sensing',
                  'Literature review',
                  'Mixed',
                  'Other'
      ) 
      
      fdat <- fdat %>%
         mutate(Data.Source.s.=ifelse(grepl(',',Data.Source.s.), 'Mixed', as.character(Data.Source.s.))) %>%
         mutate(Data.Source.s.=ifelse(Data.Source.s.=='Review', 'Literature review', as.character(Data.Source.s.))) %>%
         mutate(Data.Source.s.=ifelse(Data.Source.s.=='Other (see notes)', 'Other', as.character(Data.Source.s.)))
      
   }
   
   if(dat.field=='Anthropogenic fire regime'){
      #list of afrs in the order we want to present
      flist <-  c('Pre-industrial',
                  'Transition',
                  'Industrial',
                  'Post-industrial'
      )   
   }
   
   #sequence of length flist
   fseq <- seq(length(flist),1,-1)
   
   #convert dat.field column to values (using flist)
   fdat['DATn'] <- fseq[match(fdat[[dat.field]], flist)]
   
   ###
   ###Make data spatial
   ###
   xy <- fdat[,c("Longitude","Latitude")]
   xy <- as.data.frame(lapply(xy, as.numeric)) 
   xy <- drop_na(xy)
   
   fdat.sp <- SpatialPointsDataFrame(coords = xy, data = fdat,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
   
   ###
   ###Create empty raster for rasterising
   ###
   worldRas <- raster(xmn = -180,   # set minimum x coordinate
                      xmx = 180,    # set maximum x coordinate
                      ymn = -60,     # set minimum y coordinate
                      ymx = 80,     # set maximum y coordinate
                      res = c(ras.res,ras.res)) # resolution in c(x,y) direct
   
   worldRas[] <- seq(from = 1, to = ncell(worldRas),by = 1)
   
   ###
   ###Rasterize 
   ###
   fRas <- rasterize(x=xy,y=worldRas,field=fdat$DATn,fun=ras.func)
   
   
   ###
   ###Plot
   ###
   
   #if function is returning a categorical
   if(ras.function=='mode'){
      #ratify for categories
      fRas <- ratify(fRas)     #tell R that the values are categorical                    
      rat <- levels(fRas)[[1]]    #apply the levels (i.e. categories) 
      rat <- cbind(rat, rev(flist))    #set the names of categories
      #rat[[dat.field]] <- flist     #set the names of categories
      levels(fRas) <- rat         #apply the named categories to the raster
   }
   
   #basemap
   countries <- map("world", plot=FALSE) 
   countries <- map2SpatialLines(countries, proj4string = CRS("+proj=longlat"))
   
   #plot
   plt <- rasterVis::levelplot(fRas, margin = FALSE, main=paste0(dat.field, " (", ras.function, ")"),col.regions=inferno(20, direction=-1)) +
      latticeExtra::layer(sp.lines(countries), under=T, theme = simpleTheme(lwd=0.5, alpha.line=0.5),
                          data=list(countries=countries))
   
   return(plt)
   
}
################################


# histograms
################################
hist.fire.regime <- function(dat.temp=burned.area, metric = 'burned area', actual=F, bin_width = 1, 
                             log_scale = T, cropland = T, regime=F, vertical=F, scale_limits = c(0.01, 10000000000)) {
   
   dat.temp <- dat.temp %>%
      drop_na(`Anthropogenic fire regime`) %>%
      mutate(`Anthropogenic fire regime` = factor(`Anthropogenic fire regime`, levels = c("Pre-industrial","Transition","Industrial","Post-industrial"))) %>%
      filter(Combined.stat > 0.01) %>%
      filter(grepl(metric,Behaviour,fixed=TRUE)) 
   
   if(actual){
      dat.temp <- filter(dat.temp, grepl("Actual",Behaviour))
   }
   
   if(cropland == T) {
      #split broadcast vs cropland
      dat.temp <- drop_na(dat.temp, `Fire purpose`)
      dat.temp$`Fire type`  <- ifelse(dat.temp$`Fire purpose` %in% c('Crop field preparation', 'Crop residue burning'), 'Cropland', 'Broadcast')   
      
      p <- dat.temp %>%
         ggplot(aes(x = Combined.stat, fill = `Fire type`)) +
         theme_classic() + xlab(metric) + theme(text = element_text(size=14)) 
      
      if(bin_width > 0){
         #bar histogram 
         p <- p + 
            geom_histogram(colour = 'black', binwidth = bin_width, alpha=0.8) +
            scale_fill_manual(values=c(vir_10[7], vir_10[10]))
         #scale_fill_viridis_d()
      } else {
         #density plot (for KDE)
         p <- p +  geom_density(alpha=0.8) + 
            scale_fill_manual(values=c(vir_10[7], vir_10[10])) 
         #scale_fill_viridis_d(alpha=0.5) 
      }
   } else {
      #don't split cropland vs broadcast
      p <- dat.temp %>%
         ggplot(aes(x = Combined.stat)) + 
         theme_classic() + xlab(metric) + theme(text = element_text(size=14)) 
      
      if(bin_width > 0){
         #bar histogram
         p <- p + geom_histogram(colour = 'black', binwidth = bin_width, alpha=0.8) +
            scale_fill_manual(values=c(vir_10[7], vir_10[10]))
         #scale_fill_viridis_d()
      } else {
         #density plot (for KDE)  
         p <- p + geom_density(alpha=0.8) + 
            scale_fill_manual(values=c(vir_10[7], vir_10[10]))
         #scale_fill_viridis_d(alpha=0.5) 
      }
   }
   
   if(regime){
      p <- p + facet_grid(`Anthropogenic fire regime`~.)
   }
   
   if(log_scale == T) {
      p <- p + scale_x_continuous(trans = scales::log_trans(), limits = scale_limits,
                                  labels = function(x) sprintf("%g", x),
                                  breaks = scales::log_breaks())
   } else {
      p <- p + scale_x_continuous(limits = scale_limits) 
   }
   
   if(vertical) {
      p <- p + coord_flip()
   }
   
   return(p)
}
################################


