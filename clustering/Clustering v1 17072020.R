

#####################################################################################

### Analysis of boostrapped database data

### Code by Oli Perkins 17072020

#####################################################################################

#########################################

### 1) Clean and prepare data

#########################################

### min from mean / median

results.frame$estimate                    <- apply(results.frame[, 3:16], 1, 
                                function(x) {powerlaw.extrapolate(x, 'mean', 'min')})

results.frame$`Actual.fire.size.min.(ha)` <- ifelse(is.na(results.frame$`Actual.fire.size.min.(ha)`),
                                                    results.frame$estimate, results.frame$`Actual.fire.size.min.(ha)`)

results.frame$estimate                    <- apply(results.frame[, 3:16], 1, 
                                                   function(x) {powerlaw.extrapolate(x, 'median', 'min')})

results.frame$`Actual.fire.size.min.(ha)` <- ifelse(is.na(results.frame$`Actual.fire.size.min.(ha)`),
                                                    results.frame$estimate, results.frame$`Actual.fire.size.min.(ha)`)

### max from mean / median


results.frame$estimate                    <- apply(results.frame[, 3:16], 1, 
                                                   function(x) {powerlaw.extrapolate(x, 'mean', 'max')})

results.frame$`Actual.fire.size.max.(ha)` <- ifelse(is.na(results.frame$`Actual.fire.size.max.(ha)`),
                                                    results.frame$estimate, results.frame$`Actual.fire.size.max.(ha)`)

results.frame$estimate                    <- apply(results.frame[, 3:16], 1, 
                                                   function(x) {powerlaw.extrapolate(x, 'median', 'max')})

results.frame$`Actual.fire.size.max.(ha)` <- ifelse(is.na(results.frame$`Actual.fire.size.max.(ha)`),
                                                    results.frame$estimate, results.frame$`Actual.fire.size.max.(ha)`)

### median from mean & vice versa

results.frame$estimate                     <- apply(results.frame[, 3:16], 1, 
                                                   function(x) {powerlaw.extrapolate(x, 'mean', 'median')})

results.frame$`Actual.fire.size.median.(ha)` <- ifelse(is.na(results.frame$`Actual.fire.size.median.(ha)`),
                                                    results.frame$estimate, results.frame$`Actual.fire.size.median.(ha)`)

results.frame$estimate                     <- apply(results.frame[, 3:16], 1, 
                                                   function(x) {powerlaw.extrapolate(x, 'median', 'mean')})

results.frame$`Actual.fire.size.mean.(ha)` <- ifelse(is.na(results.frame$`Actual.fire.size.mean.(ha)`),
                                                       results.frame$estimate, results.frame$`Actual.fire.size.mean.(ha)`)



### clean up character columns

results.frame                      <- results.frame[, -ncol(results.frame)]
results.frame$Fire.intention       <- factor(results.frame$Fire.intention)
results.frame$`Presence./.Absence` <- factor(results.frame$`Presence./.Absence`)
results.frame$Fire.season.start    <- factor(results.frame$Fire.season.start)
results.frame$Fire.season.end      <- factor(results.frame$Fire.season.end )
results.frame$Fire.ignition.pattern<- factor(results.frame$Fire.ignition.pattern)


##############################################

### Filter NAs

##############################################

lapply(split(results.frame, results.frame$Fire.intention), function(x) {
  
  apply(x, 2, function(y) {length(which(is.na(y[y != 0])))})
  
})

### Fire return for absence

results.frame$`Fire.return.period.(years)` <- ifelse(results.frame$`Presence./.Absence` == 'Absence', 
                                                    99, results.frame$`Fire.return.period.(years)`)

### Fire return for land clearance fires

results.frame$`Fire.return.period.(years)` <- ifelse(results.frame$Fire.intention %in% c(
                                                      'Forest clearance', 'Land clearance'), 
                                                     49, results.frame$`Fire.return.period.(years)`)


######################################################

### Do clustering

######################################################

cluster.frame <- results.frame %>% filter(Fire.intention %in% c(
  'Conservation', 'Crop field preparation', 'Crop residue burning', 'Hunting', 
  'Forest clearance', 'Land clearance', 'Rangeland management', 'Pasture renewal'
)) %>% select(c(5, 7, 9, 11, 16)) %>% filter(`Fire.return.period.(years)` != 99)

cluster.frame <- data.frame(apply(cluster.frame, 2, function(x) {x/max(x, na.rm = T)}))

clust                 <- kmeans(cluster.frame[!is.na(cluster.frame$Actual.fire.size.min..ha.), ], 
                                5, iter.max = 15, nstart = 50)
cluster.frame         <- cluster.frame[!is.na(cluster.frame$Actual.fire.size.min..ha.), ]
cluster.frame$cluster <- clust$cluster
cluster.frame$Intention<- (results.frame %>% filter(Fire.intention %in% c(
  'Conservation', 'Crop field preparation', 'Crop residue burning', 'Hunting', 
  'Forest clearance', 'Land clearance', 'Rangeland management', 'Pasture renewal'
))%>% filter(`Fire.return.period.(years)` != 99 & !is.na(`Actual.fire.size.min.(ha)`)))$Fire.intention

clust.result <- lapply(unique(cluster.frame$Intention), 
       function(i) {table(cluster.frame$cluster[cluster.frame$Intention == i])})

names(clust.result) <- unique(cluster.frame$Intention)
clust.result


#########################################################

### Multiple numbers of clusters

#########################################################

cluster.frame <- results.frame %>% filter(Fire.intention %in% c(
  'Conservation', 'Crop field preparation', 'Crop residue burning', 'Hunting', 
  'Forest clearance', 'Land clearance', 'Rangeland management', 'Pasture renewal'
)) %>% select(c(5, 7, 9, 11, 16)) %>% filter(`Fire.return.period.(years)` != 99)

cluster.frame <- data.frame(apply(cluster.frame, 2, function(x) {x/max(x, na.rm = T)}))

k.max <- 15
wss   <- sapply(1:k.max, 
              function(k){kmeans(cluster.frame[!is.na(cluster.frame$Actual.fire.size.min..ha.), ], k, nstart=50,iter.max = 15 )$tot.withinss})

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


res <- data.frame(lapply(1:5, function(x) {clust$centers[, x]*max(cluster.frame[, x], na.rm = T)}))
colnames(res) <- colnames(clust$centers)

write.csv(res, 'Kmeans1stresult.csv', row.names = F)
