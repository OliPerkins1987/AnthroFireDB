DAFI Paper
================
James Millington
2022-06-29

-   [Overview](#overview)
    -   [Setup](#setup)
-   [2. Materials and Methods](#2-materials-and-methods)
    -   [2.3 Database Summary and
        Analysis](#23-database-summary-and-analysis)
-   [3. Results](#3-results)
    -   [3.1 Geographic Distribuion](#31-geographic-distribuion)
        -   [Figure 1](#figure-1)
    -   [3.2. Fire Use](#32-fire-use)
        -   [3.2.1. Fire Purpose](#321-fire-purpose)
        -   [Table 2](#table-2)
        -   [Figure 2](#figure-2)
        -   [Figure 3](#figure-3)
    -   [3.3 Fire Suppression](#33-fire-suppression)
        -   [Figure 4](#figure-4)
    -   [3.4 Fire Policy](#34-fire-policy)
        -   [Figure 5](#figure-5)
-   [4. Discussion](#4-discussion)
    -   [4.1 Improving the quality of anthropogenic fire
        data](#41-improving-the-quality-of-anthropogenic-fire-data)
    -   [4.2 Modelling and observing anthropogenic fire
        regimes](#42-modelling-and-observing-anthropogenic-fire-regimes)
        -   [Figure 6](#figure-6)
    -   [4.3. Categorising Anthropogenic Fire Uses and
        Regimes](#43-categorising-anthropogenic-fire-uses-and-regimes)

# Overview

Analysis of DAFI Fire Use variables by Anthropogenic Fire Regime. Also
distinguishing Broadcast vs Cropland fires.

All derived values are shown in context of the text in which they are
used.

## Setup

Load packages, data and supporting functions. Code not shown in rendered
document (i.e. html, pdf, etc) but can be viewed in original .rmd file.
Session Info shown for package versions.

``` r
source("manip-script.r", local = knitr::knit_global())
source("plots-script.r", local = knitr::knit_global())
```

``` r
sessionInfo()
```

    ## R version 4.1.3 (2022-03-10)
    ## Platform: x86_64-pc-linux-gnu (64-bit)
    ## Running under: Linux Mint 20.3
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
    ## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_GB.UTF-8          LC_NUMERIC=C                 
    ##  [3] LC_TIME=en_GB.UTF-8           LC_COLLATE=en_GB.UTF-8       
    ##  [5] LC_MONETARY=en_GB.UTF-8       LC_MESSAGES=en_GB.UTF-8      
    ##  [7] LC_PAPER=en_GB.UTF-8          LC_NAME=en_GB.UTF-8          
    ##  [9] LC_ADDRESS=en_GB.UTF-8        LC_TELEPHONE=en_GB.UTF-8     
    ## [11] LC_MEASUREMENT=en_GB.UTF-8    LC_IDENTIFICATION=en_GB.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] rnaturalearthdata_0.1.0 rnaturalearth_0.1.0     sf_0.9-7               
    ##  [4] scales_1.1.1            readxl_1.4.0            devtools_2.4.3         
    ##  [7] usethis_2.1.6           openxlsx_4.2.5          xlsx_0.6.5             
    ## [10] maps_3.4.0              viridisLite_0.4.0       forcats_0.5.1          
    ## [13] stringr_1.4.0           dplyr_1.0.8             purrr_0.3.4            
    ## [16] readr_2.1.2             tidyr_1.2.0             tibble_3.1.6           
    ## [19] ggplot2_3.3.5           tidyverse_1.3.1        
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] fs_1.5.2           lubridate_1.8.0    httr_1.4.2         rprojroot_2.0.3   
    ##  [5] tools_4.1.3        backports_1.4.1    utf8_1.2.2         R6_2.5.1          
    ##  [9] KernSmooth_2.23-20 DBI_1.1.2          colorspace_2.0-3   sp_1.4-6          
    ## [13] withr_2.5.0        tidyselect_1.1.2   prettyunits_1.1.1  processx_3.5.3    
    ## [17] compiler_4.1.3     cli_3.3.0          rvest_1.0.2        xml2_1.3.3        
    ## [21] desc_1.4.1         classInt_0.4-3     callr_3.7.0        proxy_0.4-26      
    ## [25] digest_0.6.29      rmarkdown_2.13     pkgconfig_2.0.3    htmltools_0.5.2   
    ## [29] sessioninfo_1.2.2  dbplyr_2.1.1       fastmap_1.1.0      rlang_1.0.2       
    ## [33] rstudioapi_0.13    generics_0.1.2     jsonlite_1.8.0     zip_2.2.0         
    ## [37] magrittr_2.0.3     Rcpp_1.0.8.3       munsell_0.5.0      fansi_1.0.3       
    ## [41] lifecycle_1.0.1    stringi_1.7.6      yaml_2.3.5         brio_1.1.3        
    ## [45] pkgbuild_1.3.1     grid_4.1.3         crayon_1.5.1       lattice_0.20-45   
    ## [49] haven_2.4.3        xlsxjars_0.6.1     hms_1.1.1          knitr_1.38        
    ## [53] ps_1.6.0           pillar_1.7.0       pkgload_1.2.4      reprex_2.0.1      
    ## [57] glue_1.6.2         evaluate_0.15      remotes_2.4.2      modelr_0.1.8      
    ## [61] vctrs_0.4.0        tzdb_0.3.0         testthat_3.1.4     cellranger_1.1.0  
    ## [65] gtable_0.3.0       assertthat_0.2.1   cachem_1.0.6       xfun_0.30         
    ## [69] broom_0.7.12       e1071_1.7-9        class_7.3-20       rJava_1.0-6       
    ## [73] memoise_2.0.1      units_0.8-0        ellipsis_0.3.2

# 2. Materials and Methods

## 2.3 Database Summary and Analysis

``` r
ns <- n_distinct(record_info$`Study.ID`)
ncs <- n_distinct(record_info$`Case.Study.ID`)

pubs <- record_info %>%
  group_by(Study.Type) %>%
  summarise(n = n()) %>%
  mutate(perc = 100 * n / sum(n))
pubs
```

    ## # A tibble: 4 × 3
    ##   Study.Type     n   perc
    ##   <chr>      <int>  <dbl>
    ## 1 Academic    1707 94.4  
    ## 2 Government    48  2.65 
    ## 3 NGO           50  2.76 
    ## 4 Other          4  0.221

``` r
aca <- round(filter(pubs, Study.Type=='Academic')$perc,0)
govngo <- round(sum(filter(pubs, Study.Type=='NGO' | Study.Type=='Government')$perc),0)

datsrc <- record_info %>%
  mutate(Data.Source.s.=ifelse(grepl(',',Data.Source.s.), 'Mixed', as.character(Data.Source.s.))) %>%
  mutate(Data.Source.s.=ifelse(Data.Source.s.=='Review', 'Literature review', as.character(Data.Source.s.))) %>%
  group_by(Data.Source.s.) %>%
  summarise(n = n()) %>%
  mutate(perc = 100 * n / sum(n))

datsrc
```

    ## # A tibble: 6 × 3
    ##   Data.Source.s.        n  perc
    ##   <chr>             <int> <dbl>
    ## 1 Literature review   118  6.52
    ## 2 Mixed               432 23.9 
    ## 3 Other (see notes)    60  3.32
    ## 4 Primary             712 39.4 
    ## 5 Remote sensing       68  3.76
    ## 6 Secondary           419 23.2

``` r
src.fs <- round(filter(datsrc, Data.Source.s.=='Primary')$perc,0)
src.idr <- round(filter(datsrc, Data.Source.s.=='Secondary')$perc,0)
src.rs <- round(filter(datsrc, Data.Source.s.=='Remote sensing')$perc,0)
src.lr <- round(filter(datsrc, Data.Source.s.=='Literature review')$perc,0)
src.oth <- round(filter(datsrc, Data.Source.s.=='Other (see notes)')$perc,0)
src.mix <- round(filter(datsrc, Data.Source.s.=='Mixed')$perc,0)

#sum(datsrc$perc)
```

At the time of publication DAFI contains data from 1809 human
fire‐related case studies collated from 504 sources.

Data were overwhelmingly from academic publications (94% of case
studies) but were also from reports produced by governments and NGOs
(5%).

Case studies used data exclusively from Field Studies (39% of case
studies), Institutional Data Repositories (23%), Literature Review (7%),
Remote Sensing (4%) or a combination (24%) of these data source types.
The remaining case studies (\~3%) used expert elicitation, media
reports, archival research and other reports as sources.

``` r
#number of case studies
ncs.rfire <- n_distinct(rep_fire_use$`Case Study ID`)

#count of incomplete case studies by variable
incsvar.rfire <- rep_fire_use %>%
  dplyr::select(-Notes, -Calculations) %>%
  group_by(`Case Study ID`) %>%
  summarise_each(funs(sum(is.na(.)))) %>%
  mutate_all(funs(ifelse(.>1,1,.))) %>%
  summarise_all(sum)

#count of missing values across all variables
mval.rfire <- incsvar.rfire %>%
  dplyr::select(-`Case Study ID`) %>%
  sum()

#total number of possible values
tval.rfire <- ncs.rfire * dim(incsvar.rfire)[2] - 1   #-1 to ignore the Case Study ID

#% missing values
tval.rfire.perc <- round(100* mval.rfire/ tval.rfire,0)

#count of incomplete case studies by variable for QUANTITATIVE vars only
incsvar.rfire.q <- rep_fire_use %>%
  dplyr::select_if(names(.)=="Case Study ID" | sapply(., is.numeric)) %>%
  dplyr::select(-`Study Year`) %>%
  group_by(`Case Study ID`) %>%
  summarise_each(funs(sum(is.na(.)))) %>%
  mutate_all(funs(ifelse(.>1,1,.))) %>%
  summarise_all(sum)

#count of missing values across all variables
mval.rfire.q <- incsvar.rfire.q %>%
  dplyr::select(-`Case Study ID`) %>%
  sum()

#total number of possible values
tval.rfire.q <- ncs.rfire * dim(incsvar.rfire.q)[2] - 1   #-1 to ignore the Case Study ID

#% missing values
tval.rfire.q.perc <- round(100* mval.rfire.q / tval.rfire.q,0)
```

Reflecting the fragmented nature of the anthropogenic fire literature,
no case study contained data on all variables and data across all DAFI
fields are sparse. For example, for reported fire use data, 60% of
values were missing, rising to 82% when only quantitative variables are
considered.

``` r
lu.AFR <- land_use %>% 
  rename(AFR = `Anthropogenic fire regime`) %>%
  count(AFR, .drop=FALSE) %>%
  drop_na()

cs.AFR <- sum(lu.AFR$n)
```

Furthermore, we were able to define an AFR for only 1605 of the 1809
case studies (Pre-industrial 261, Transition 850, Industrial 300,
Post-industrial 194) as land use information is not always provided or
available. The incompleteness of the database means that the number of
values used in analyses below varies depending on which aspects of
anthropogenic fire are being examined.

``` r
afrsrc <- merge(record_info, land_use, by.x= 'Case.Study.ID', by.y = 'Case Study ID')

afrsrc <- afrsrc %>% 
  mutate(Data.Source.s.=ifelse(grepl(',',Data.Source.s.), 'Mixed', as.character(Data.Source.s.))) %>%
  mutate(Data.Source.s.=ifelse(Data.Source.s.=='Review', 'Literature review', as.character(Data.Source.s.))) %>%
  dplyr::select(Data.Source.s., `Anthropogenic fire regime`)

table(afrsrc)
```

    ##                    Anthropogenic fire regime
    ## Data.Source.s.      Industrial Post-industrial Pre-industrial Transition
    ##   Literature review         46              41              6         20
    ##   Mixed                     63              37             44        278
    ##   Other (see notes)          3               1             17         36
    ##   Primary                   31              50            188        431
    ##   Remote sensing            20               7              6         30
    ##   Secondary                137              58              0         55

``` r
afrsrc.s <- summary(table(afrsrc))
afrsrc.p <- round(prop.table(table(afrsrc),2),2)
```

# 3. Results

## 3.1 Geographic Distribuion

When DAFI data sources are mapped spatially (Figure 1) we find a
prevalence of case studies using institutional data in Europe and North
America versus a dominance of field studies in Asia and Africa.

### Figure 1

``` r
srcmap <- map.behaviour.ras('Data.Source.s.', ras.function='mode')
srcmap
```

![](DAFI-Paper_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
afrmap <- map.behaviour.ras('Anthropogenic fire regime', ras.function='mode')
afrmap
```

![](DAFI-Paper_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
fumap <- map.behaviour.ras('Fire purpose', ras.function='mode')
fumap
```

![](DAFI-Paper_files/figure-gfm/unnamed-chunk-5-3.png)<!-- --> The
spatial distribution of AFRs (Figure 1b) indicates a similar
distribution to data sources, with a prevalence of Industrial and
Post-Industrial regimes in Europe and North America versus a dominance
of Transition and Pre-Industrial regimes in Asia and Africa. A
Chi-square test for the association between AFR and data source
indicates non-randomness (Chi-sq = 608, df = 15, p \< 0.001) and Primary
sources dominate case studies of Pre-industrial (72% of case studies)
and Transition (51%) regimes, whereas Secondary sources are the most
frequent data sources for case studies of Industrial (46%) and
Post-industrial (30%) regimes.

## 3.2. Fire Use

### 3.2.1. Fire Purpose

``` r
rep_FUS <- rep_fire_use
est_FUS <- est_fire_use

rep_FUS$`Fire purpose` <- Simplify.purpose(rep_fire_use$`Fire purpose`, AFT = rep_fire_use$AFT)
est_FUS$`Fire purpose` <- Simplify.purpose(est_fire_use$`Fire purpose`, AFT = est_fire_use$AFT)

#summarise grouped (sum) by case study, AFR and fire use
fs.cs.afr <- plyr::rbind.fill(lapply(colnames(rep_fire_use)[c(13:20)],
                                     function (x){summarise.behaviour(temp.rep_fire_use=rep_FUS,temp.est_fire_use=est_FUS,
                                                                      type = "Fire", behaviour = x,
                                                                      sum_multi = F,
                                                                      grouping = c('Case Study ID', 'Anthropogenic fire regime','Fire purpose'),
                                                                      inc.Absence = F, escape.rm = T)}))
```

    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.

``` r
#summarise grouped (sum) by case study, AFR and fire use
ba.cs.afr <- plyr::rbind.fill(lapply(colnames(rep_fire_use)[c(21:26)],
                                     function (x){summarise.behaviour(temp.rep_fire_use=rep_FUS,temp.est_fire_use=rep_FUS,
                                                                      type = "Fire", behaviour = x,
                                                                      sum_multi=F,
                                                                      grouping = c('Case Study ID', 'Anthropogenic fire regime','Fire purpose'),
                                                                      inc.Absence = F, escape.rm = T)}))
```

    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime'. You can override using the `.groups` argument.

``` r
#DAFI Records %
fu.count <- rep_FUS %>%
  drop_na(`Fire purpose`) %>%
  tally()

fu.perc <- rep_FUS %>%
  drop_na(`Fire purpose`) %>%
  group_by(`Fire purpose`) %>%
  summarise(n=n()) %>%
  mutate(fuperc = 100* n / as.numeric(fu.count))

#% that the seven main fire use tyoe compose
fu.contrib <- round(sum(fu.perc$fuperc) - fu.perc$fuperc[fu.perc$`Fire purpose` == 'Other'],0)
```

Overall, 20 anthropogenic fire uses were identified during literature
review, but many were closely related (e.g., ‘pasture renewal’ and
‘rangeland management’). After such similar types were combined (see
Supplementary Materials), seven dominant fire uses emerged (Table 2)
each with more than 100 instances in the database and accounting for 93%
of fire instance records.

### Table 2

#### DAFI records

``` r
fu.perc
```

    ## # A tibble: 8 × 3
    ##   `Fire purpose`             n fuperc
    ##   <chr>                  <int>  <dbl>
    ## 1 Arson                    118   3.48
    ## 2 Crop field preparation   700  20.6 
    ## 3 Crop residue burning     590  17.4 
    ## 4 Hunter gatherer          226   6.66
    ## 5 Other                    223   6.57
    ## 6 Pasture management       434  12.8 
    ## 7 Pyrome management        628  18.5 
    ## 8 Vegetation clearance     474  14.0

#### Mean size (ha)

``` r
#summarise
fs.fp <- plyr::rbind.fill(lapply(colnames(rep_fire_use)[17],
                                     function (x){summarise.behaviour(temp.rep_fire_use=rep_FUS,temp.est_fire_use=est_FUS,
                                                                      type = "Fire", behaviour = x,
                                                                      sum_multi = F,
                                                                      grouping = c('Fire purpose'),
                                                                      inc.Absence = F, escape.rm = T)}))
fu.sizemn <- fs.fp %>%
  dplyr::select(`Fire purpose`, `Combined.stat`)

fu.sizemn 
```

    ##             Fire purpose Combined.stat
    ## 1                  Arson     3.6000000
    ## 2 Crop field preparation     0.7899407
    ## 3   Crop residue burning     3.9358281
    ## 4        Hunter gatherer     2.1251000
    ## 5                  Other     1.3344167
    ## 6     Pasture management    33.9394000
    ## 7      Pyrome management   357.2023810
    ## 8   Vegetation clearance     9.1709286
    ## 9                   <NA>    24.4153846

#### Mean burned area (% LS)

``` r
#summarise
ba.fp        <- plyr::rbind.fill(lapply(colnames(rep_FUS)[c(23:27)],
                                        function (x){summarise.behaviour(temp.rep_fire_use=rep_FUS,temp.est_fire_use=est_FUS,
                                                                         type = "Fire", sum_multi = FALSE,
                                                                         behaviour = x, grouping = c('Fire purpose'), 
                                                                         escape.rm = T)}))
ba.fp$Intended <- ifelse(grepl('intended', tolower(ba.fp$Behaviour)), 'Intended', 'Actual')

ba.fp.i <- ba.fp %>%
  filter(grepl('land cover', Behaviour)) %>%
  filter(Intended == 'Intended') %>%
  rbind(c('Hunter gatherer',rep(0,6),'Intended burned area % (land cover)','Intended')) %>%
  mutate(Combined.stat = as.numeric(Combined.stat), 
         Combined.N = as.numeric(Combined.N)) %>%
  arrange(`Fire purpose`)
  
ba.fp.a <- ba.fp %>%
  filter(grepl('land cover', Behaviour)) %>%
  filter(Intended == 'Actual') %>%
  mutate(Combined.stat = as.numeric(Combined.stat), 
         Combined.N = as.numeric(Combined.N)) %>%
  drop_na(`Fire purpose`) %>%
  arrange(`Fire purpose`)

ba.fp.lc <- round(((ba.fp.i$Combined.stat * ba.fp.i$Combined.N) + (ba.fp.a$Combined.stat * ba.fp.a$Combined.N)) / (ba.fp.i$Combined.N + ba.fp.a$Combined.N),1)

ba.fp.lc <- cbind.data.frame(ba.fp.i$`Fire purpose`, ba.fp.lc)

ba.fp.lc
```

    ##   ba.fp.i$`Fire purpose` ba.fp.lc
    ## 1 Crop field preparation     12.8
    ## 2   Crop residue burning     22.8
    ## 3        Hunter gatherer      9.1
    ## 4     Pasture management     32.1
    ## 5      Pyrome management      8.9
    ## 6   Vegetation clearance      6.6

#### Return period (years)

``` r
#summarise
rp.fp <- plyr::rbind.fill(lapply(colnames(rep_FUS)[27],
                                     function (x){summarise.behaviour(temp.rep_fire_use=rep_FUS,temp.est_fire_use=est_FUS,
                                                                      type = "Fire", behaviour = x,
                                                                      sum_multi = F,
                                                                      grouping = c('Fire purpose'),
                                                                      inc.Absence = F, escape.rm = T)}))
dplyr::select(rp.fp, `Fire purpose`, Combined.stat) 
```

    ##             Fire purpose Combined.stat
    ## 1                  Arson      3.000000
    ## 2 Crop field preparation      9.797941
    ## 3   Crop residue burning      1.481539
    ## 4        Hunter gatherer      4.329545
    ## 5                  Other      3.260000
    ## 6     Pasture management      3.046744
    ## 7      Pyrome management      5.680000
    ## 8                   <NA>      5.376667

#### Escaped %

For fires uses with complete data

``` r
#summarise
igs <-  plyr::rbind.fill(lapply(colnames(rep_FUS)[c(11:12)],
                                  function (x){summarise.behaviour(temp.rep_fire_use=rep_FUS,temp.est_fire_use=est_FUS,
                                                                   type = "Fire", sum_multi =  FALSE,
                                                                   behaviour = x, grouping = c('Fire type', 'Fire purpose'), 
                                                                   inc.Absence = F, escape.rm = F)}))
```

    ## `summarise()` has grouped output by 'Fire type'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'Fire type'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'Fire type'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'Fire type'. You can override using the
    ## `.groups` argument.

``` r
igs.e <- igs %>% 
  filter(`Fire type` == 'Human, escaped' & !is.na(`Fire purpose`) & grepl('land cover',Behaviour)) %>%
  filter(`Fire purpose` != 'Hunter gatherer') %>%
  mutate(Combined.stat = as.numeric(Combined.stat), 
         Combined.N = as.numeric(Combined.N)) %>%
  arrange(`Fire purpose`)

igs.d <- igs %>% 
  filter(`Fire type` == 'Human, deliberate' & !is.na(`Fire purpose`) & grepl('land cover',Behaviour)) %>%
  filter(!(`Fire purpose` %in% c('Arson','Pyrome management','Vegetation clearance'))) %>%
  mutate(Combined.stat = as.numeric(Combined.stat), 
         Combined.N = as.numeric(Combined.N)) %>%
  arrange(`Fire purpose`) 

esc.n <- rep_FUS %>% 
  filter(`Fire type` == 'Human, escaped'  & !is.na(`Fire purpose`)) %>%
  group_by_at(.vars = c('Fire purpose', 'Fire type')) %>%
  summarise(count = n()) %>%
  filter(`Fire purpose` %in% c('Crop field preparation', 'Crop residue burning', 'Pasture management')) %>%
  arrange(`Fire purpose`)
```

    ## `summarise()` has grouped output by 'Fire purpose'. You can override using the
    ## `.groups` argument.

``` r
del.n <-  rep_FUS %>% 
  filter(`Fire type` == 'Human, deliberate'  & !is.na(`Fire purpose`)) %>%
  group_by_at(.vars = c('Fire purpose', 'Fire type')) %>%
  summarise(count = n()) %>%
  filter(`Fire purpose` %in% c('Crop field preparation', 'Crop residue burning', 'Pasture management')) %>%
  arrange(`Fire purpose`)
```

    ## `summarise()` has grouped output by 'Fire purpose'. You can override using the
    ## `.groups` argument.

``` r
escaped <- 
round(100*(esc.n$count / (esc.n$count + del.n$count)) * (igs.e$Combined.stat / (igs.e$Combined.stat + igs.d$Combined.stat)),2)

escaped <- cbind.data.frame(del.n$`Fire purpose`, escaped)
escaped
```

    ##     del.n$`Fire purpose` escaped
    ## 1 Crop field preparation    0.06
    ## 2   Crop residue burning    0.01
    ## 3     Pasture management    5.01

For less complete fire uses, use global (non-cropland) means with
overall counts

``` r
#summarise
igs.o <-  plyr::rbind.fill(lapply(colnames(rep_FUS)[c(11:12)],
                                  function (x){summarise.behaviour(temp.rep_fire_use=rep_FUS,temp.est_fire_use=est_FUS,
                                                                   type = "Fire", sum_multi =  FALSE,
                                                                   behaviour = x, grouping = c('Fire type', 'Fire purpose'), 
                                                                   escape.rm = F)}))
```

    ## `summarise()` has grouped output by 'Fire type'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'Fire type'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'Fire type'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'Fire type'. You can override using the
    ## `.groups` argument.

``` r
igs.o.d <- igs.o %>% filter(`Fire type` == 'Human, deliberate' & !is.na(`Fire purpose`)) %>%
  filter(grepl('total', Behaviour)) %>%
  filter(!(`Fire purpose` %in% c('Crop field preparation', 'Crop residue burning'))) %>%
  filter(`Fire purpose` != 'Other')

denom <- sum(igs.o.d$Combined.stat*igs.o.d$Combined.N) / sum(igs.o.d$Combined.N)


igs.o.e <- igs.o %>% filter(`Fire type` == 'Human, escaped' & !is.na(`Fire purpose`)) %>%
  filter(!(`Fire purpose` %in% c('Crop field preparation', 'Crop residue burning'))) %>%
  filter(grepl('total', Behaviour)) %>%
  filter(`Fire purpose` != 'Other')

numer <-sum(igs.o.e$Combined.stat*sum(igs.o.e$Combined.N)) / sum(igs.o.e$Combined.N)

esc.n.g <- rep_FUS%>% 
  filter(`Fire type` == 'Human, escaped'  & !is.na(`Fire purpose`)) %>%
  group_by_at(.vars = c('Fire purpose', 'Fire type')) %>%
  filter(!(`Fire purpose` %in% c('Crop field preparation', 'Crop residue burning', 'Pasture management', 'Other', 'Arson'))) %>%
  summarise(count = n())
```

    ## `summarise()` has grouped output by 'Fire purpose'. You can override using the
    ## `.groups` argument.

``` r
del.n.g <- rep_FUS%>% 
  filter(`Fire type` == 'Human, deliberate'  & !is.na(`Fire purpose`)) %>%
  group_by_at(.vars = c('Fire purpose', 'Fire type')) %>%
  filter(!(`Fire purpose` %in% c('Crop field preparation', 'Crop residue burning', 'Pasture management', 'Other', 'Arson'))) %>%
  summarise(count = n())
```

    ## `summarise()` has grouped output by 'Fire purpose'. You can override using the
    ## `.groups` argument.

``` r
escaped.g <- 
round(100*(esc.n.g$count / (esc.n.g$count + del.n.g$count)) * (numer / denom),2)

escaped.g <- cbind.data.frame(del.n.g$`Fire purpose`, escaped.g)
escaped.g
```

    ##   del.n.g$`Fire purpose` escaped.g
    ## 1        Hunter gatherer      1.10
    ## 2      Pyrome management      0.06
    ## 3   Vegetation clearance      0.95

### Figure 2

``` r
a <- filter(rep_fire_use, `Presence / Absence` == 'Presence')
b <- filter(est_fire_use, `Presence / Absence` == 'Presence')

fig2 <- bar.purpose.regime(a, b, bartype='fill')
```

    ## `summarise()` has grouped output by 'Fire creation ID'. You can override using
    ## the `.groups` argument.
    ## `summarise()` has grouped output by 'Fire creation ID'. You can override using
    ## the `.groups` argument.

``` r
fig2 <- fig2 + theme(axis.text.x=element_text(color = "black", size=9),
                     axis.text.y=element_text(color = "black", size=9)) +
  labs(fill = "Fire Use", x = 'Anthropogenic Fire Regime')
#ggsave(fig2, filename='Figure2_600.png', width=14, height=10, units='cm', dpi=600)

fig2_alt <- bar.purpose.regime(rep_fire_use, est_fire_use, bartype='dodge')
```

    ## `summarise()` has grouped output by 'Fire creation ID'. You can override using
    ## the `.groups` argument.
    ## `summarise()` has grouped output by 'Fire creation ID'. You can override using
    ## the `.groups` argument.

``` r
fig2
```

![](DAFI-Paper_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
fig2_alt
```

![](DAFI-Paper_files/figure-gfm/unnamed-chunk-14-2.png)<!-- --> \###
3.2.2. Physical Characteristics

``` r
#summarise
igs <- plyr::rbind.fill(lapply(colnames(rep_FUS)[c(11:12)],
                      function (x){summarise.behaviour(temp.rep_fire_use=rep_FUS,temp.est_fire_use=est_FUS,
                                                       type = "Fire", behaviour = x, 
                                                       grouping = c('Case Study ID'), escape.rm = T)}))
```

    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime', 'AFT', 'Fire purpose', 'Actual land cover'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime', 'AFT', 'Fire purpose', 'Actual land cover'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime', 'AFT', 'Fire purpose', 'Actual land cover'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime', 'AFT', 'Fire purpose', 'Actual land cover'. You can override using the
    ## `.groups` argument.

``` r
igs.med <- round(median(igs$Combined.stat),2)
igs.mean <- round(sum(igs$Combined.stat * igs$Combined.N) / sum(igs$Combined.N, na.rm = T),2)
```

Data in DAFI show that deliberate anthropogenic fires occur (where
present) at a median and mean rate of 0.08 and 1.53 km-2 year-1
respectively.

``` r
### Fire Return Period - not annualised
FR <- plyr::rbind.fill(lapply(colnames(rep_FUS)[c(27)], 
                               function (x){summarise.behaviour(temp.rep_fire_use=rep_FUS,temp.est_fire_use=est_FUS,
                                                                type = "Fire", behaviour = x, 
                                                                grouping = c('Case Study ID'), escape.rm = T)}))
```

    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime', 'AFT', 'Fire purpose', 'Actual land cover'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'Case Study ID', 'Anthropogenic fire
    ## regime', 'AFT', 'Fire purpose', 'Actual land cover'. You can override using the
    ## `.groups` argument.

``` r
ri.mean <- round(sum(FR$Combined.stat * FR$Combined.N) / sum(FR$Combined.N, na.rm = T),2)

### median FR - use all values for more accuracy (not possible for igs using aggregation)
all_fire <- rep_FUS
all_fire$`Fire return period (years)` <- ifelse(is.na(all_fire$`Fire return period (years)`), 
                                                unlist(unbin(est_FUS$`Fire return period (years)`)), 
                                                all_fire$`Fire return period (years)`)

all_fire <- all_fire %>% filter(`Fire type` == 'Human, deliberate' & `Presence / Absence` == 'Presence')

ri.med <- round(median(all_fire$`Fire return period (years)`, na.rm = T),2)
```

Similarly, fire return intervals are typically short, with median of 3
and mean of 6.4 years.

### Figure 3

``` r
fs_mean_kde <- hist.fire.regime(dat.temp=fs.cs.afr,metric = 'fire size mean', bin_width = -1, actual = F, log_scale=T, regime=F, vertical=T, scale_limits = c(0.1, 10000)) 
#ggsave('fs_mean_kde.pdf', plot=fs_mean_kde, height=14, width=9, units='cm')
fs_mean_kde
```

![](DAFI-Paper_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
fs_mean_box <- box.FS.regime(fs.cs.afr, metric = 'mean', boxtype='box', 
                   estimated = T, reported = T, log_scale = T, cropland = T, scale_limits = c(0.1, 10000))
#ggsave('fs_mean_box.pdf', plot=fs_mean_box, height=14, width=16, units='cm')
fs_mean_box 
```

![](DAFI-Paper_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

``` r
ba_ha_box <- box.BA.regime(dat.temp=ba.cs.afr,metric = 'burned area (ha)', boxtype='box', 
              log_scale = T, cropland = T, scale_limits = c(0.01, 1000000000)) 
#ggsave('ba_ha_box.pdf', plot=ba_ha_box, height=14, width=16, units='cm')
ba_ha_box
```

![](DAFI-Paper_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
ba_ha_kde <- hist.fire.regime(dat.temp=ba.cs.afr,metric = 'burned area (ha)', bin_width = -1, log_scale=T, regime=F, vertical=T,  scale_limits = c(0.01, 1000000000)) 
#ggsave('ba_ha_kde.pdf', plot=ba_ha_kde, height=14, width=9, units='cm')
ba_ha_kde
```

![](DAFI-Paper_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

## 3.3 Fire Suppression

### Figure 4

``` r
#suppression at case study level
#
#only count CaseStudy once for each AFR-Suppression-Effort combination
#don't use pivot_longer as that results in double-counting

cleanEffort <- function(supTbl, supLabel){
  
  rsupTbl <- supTbl %>%
    setNames(c("Case","AFR","Effort")) %>%
    drop_na() %>%
    filter(Effort != 'ND') %>%
    mutate(Effort = recode(Effort, `0`='None',`1`='Limited',`2`='Moderate',`3`='Intensive')) %>%
    group_by_all %>%
    summarise(n= n()) %>%
    dplyr::select(-n) %>%
    mutate(Suppression = supLabel)
  
  return(rsupTbl)
  
}

sup.c <- cleanEffort(dplyr::select(suppression, 1,2,9),'Control')
```

    ## `summarise()` has grouped output by 'Case', 'AFR'. You can override using the
    ## `.groups` argument.

``` r
sup.p <- cleanEffort(dplyr::select(suppression, 1,2,11),'Prevention')
```

    ## `summarise()` has grouped output by 'Case', 'AFR'. You can override using the
    ## `.groups` argument.

``` r
sup.e <- cleanEffort(dplyr::select(suppression, 1,2,13),'Extinction')
```

    ## `summarise()` has grouped output by 'Case', 'AFR'. You can override using the
    ## `.groups` argument.

``` r
c.sup <- rbind(sup.c, sup.p, sup.e)  

c.sup.c <- c.sup %>% 
  ungroup() %>%
  mutate(Effort = factor(Effort, levels = c("None","Limited","Moderate","Intensive"))) %>%
  mutate(AFR = factor(AFR, levels = c("Pre-industrial","Transition","Industrial","Post-industrial"))) %>% 
  mutate(Suppression = factor(Suppression,levels = c("Control","Prevention","Extinction"))) %>%
  count(AFR, Suppression, Effort,.drop=FALSE)

#dodge
fig4 <- c.sup.c %>%
  ggplot(aes(x = AFR, y=n)) +
  geom_bar(aes(fill=Effort),position='dodge',stat="identity") +
  facet_grid(Suppression~.) +
  scale_fill_viridis_d(option = 'inferno') +
  xlab("Anthropogenic Fire Regime") +
  ylab("Count") +
  theme(axis.text.y=element_text(color = "black", size=9),
        axis.text.x=element_text(color = "black", size=9))
#ggsave(fig4, filename='Figure4_600.png', width=14, height=8.5, units='cm', dpi=600)

#fill
fig4_alt <- c.sup.c %>%
  ggplot(aes(x = AFR, y=n)) +
  geom_bar(aes(fill=Effort),position='fill',stat="identity") +
  facet_grid(Suppression~.) +
  scale_fill_viridis_d(option = 'inferno') +
  xlab("Anthropogenic Fire Regime") +
  ylab("Count")

fig4
```

![](DAFI-Paper_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
fig4_alt
```

![](DAFI-Paper_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

## 3.4 Fire Policy

    ## `summarise()` has grouped output by 'Anthropogenic fire regime'. You can
    ## override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Anthropogenic fire regime'. You can
    ## override using the `.groups` argument.
    ## `summarise()` has grouped output by 'Anthropogenic fire regime'. You can
    ## override using the `.groups` argument.

### Figure 5

``` r
#policy at case study level 
pol.cs <- policy %>%
  dplyr::select(1,2,4,10:17,19:22) %>%
  rename('Case'=1,'AFR' = 2) %>%
  mutate_at(vars(-1,-2,-3), funs(case_when(grepl("Yes", ., ignore.case=TRUE) ~ "Yes"))) %>%
  dplyr::select(-`Fire type`)

#create 'Other' Rationale for when Type = Yes but individual rationale is not specified
pol.cs <- pol.cs %>% 
  rename('Unknown incentives' = 'Incentives') %>%
  rename('Unknown restriction' = 'Fire restricted') %>%
  rename('Unknown ban' = 'Fire banned') %>%
  mutate(`Unknown incentives` = if_else(`Unknown incentives` == "Yes" & is.na(`Economic incentives`) &
                                          is.na(`Environmental incentives`) & is.na(`Health incentives`), "Yes", NA_character_)) %>%
  mutate(`Unknown restriction` = if_else(`Unknown restriction` == "Yes" & is.na(`Economic restriction`) &
                                          is.na(`Environmental restriction`) & is.na(`Health restriction`), "Yes", NA_character_)) %>%
  mutate(`Unknown ban` = if_else(`Unknown ban` == "Yes" & is.na(`Economic ban`) &
                                          is.na(`Environmental ban`) & is.na(`Health ban`), "Yes", NA_character_))

pol.cs <- pol.cs %>%
  group_by_all %>%
  summarise(n= n()) %>%
  ungroup()
```

    ## `summarise()` has grouped output by 'Case', 'AFR', 'Unknown incentives',
    ## 'Economic incentives', 'Environmental incentives', 'Health incentives',
    ## 'Unknown restriction', 'Economic restriction', 'Environmental restriction',
    ## 'Health restriction', 'Unknown ban', 'Economic ban', 'Environmental ban'. You
    ## can override using the `.groups` argument.

``` r
#go long
pol.cs <-pivot_longer(pol.cs, cols=c(`Unknown incentives`:`Health ban`), names_to="Rationale-Type")

#create new columns for Rationale and Type then set factors and levels
pol.cs <- pol.cs %>%
  separate(`Rationale-Type`, c("Rationale", "Type"), sep=" ") %>%
  mutate(Type = recode_factor(Type, incentives='Incentive',restriction='Restriction',ban='Ban')) %>%
  mutate(Rationale = factor(Rationale, levels=c("Environmental","Economic","Health","Unknown"))) %>%
  mutate(AFR = factor(AFR, levels = c("Pre-industrial","Transition","Industrial","Post-industrial")))

#drop NAs and count         
pol.cs <- pol.cs %>% 
  drop_na(value) %>%
  drop_na(AFR) %>%
  count(AFR, Type, Rationale,.drop=FALSE)

#https://stackoverflow.com/a/12104207
rationale_names <- list(
  'Environmental'='Environment',
  'Economic'='Economic',
  'Health'='Health',
  'Unknown'='Unknown'
)

rationale_labeller <- function(variable,value){
  return(rationale_names[value])
}

#dodge
fig5 <- pol.cs %>%
  ggplot(aes(x = AFR, y=n)) +
  geom_bar(aes(fill=Type),position='dodge',stat="identity") +
  facet_grid(Rationale~., labeller=rationale_labeller) +
  scale_fill_viridis_d(option = 'inferno') +
  xlab("Anthropogenic Fire Regime") +
  ylab("Count") +
  theme(axis.text.y=element_text(color = "black", size=9),
        axis.text.x=element_text(color = "black", size=9),
        strip.text = element_text(size = 8))
```

    ## Warning: The labeller API has been updated. Labellers taking `variable` and
    ## `value` arguments are now deprecated. See labellers documentation.

``` r
ggsave(fig5, filename='Figure5_600.png', width=14, height=8.5, units='cm', dpi=600)

#plot stacked
fig5_alt <- pol.cs %>%
  ggplot(aes(x = AFR, y=n)) +
  geom_bar(aes(fill=Type),position='fill',stat="identity") +
  facet_grid(Rationale~.) +
  scale_fill_viridis_d(option = 'inferno') +
  xlab("Anthropogenic Fire Regime") +
  ylab("Count") 

fig5
```

![](DAFI-Paper_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
fig5_alt
```

![](DAFI-Paper_files/figure-gfm/unnamed-chunk-22-2.png)<!-- -->

# 4. Discussion

## 4.1 Improving the quality of anthropogenic fire data

``` r
ba.rep <- ba.cs.afr %>%
      drop_na(`Anthropogenic fire regime`) %>%
      filter(Combined.stat > 0.01) %>%
      filter(grepl('burned area (ha)',Behaviour,fixed=TRUE)) 

fs.rep  <- fs.cs.afr %>%
      drop_na(`Anthropogenic fire regime`) %>%
      filter(Combined.stat > 0.01) %>%
      filter(grepl('mean',Behaviour,fixed=TRUE))

ba.rep <- round(100 * length(ba.rep[,1]) / n_distinct(record_info$`Case.Study.ID`),1)
fs.rep <- round(100 * length(fs.rep[,1]) / n_distinct(record_info$`Case.Study.ID`),1)
```

we find that in case studies, burned area metrics are reported less
frequently than fire size metrics (6.5% of case studies vs 17.1%).

## 4.2 Modelling and observing anthropogenic fire regimes

### Figure 6

``` r
#https://datascience.blog.wzb.eu/2019/04/30/zooming-in-on-maps-with-sf-and-ggplot2/
##https://ggplot2.tidyverse.org/reference/ggsf.html
require(sf)
require(rnaturalearth)
require(rnaturalearthdata)
worldmap <- ne_countries(scale = 'medium', type = 'map_units',returnclass = 'sf')

dat <- right_join(record_info, est_fire_use, by=c("Case.Study.ID"='Case Study ID'))

dat$Latitude <- as.numeric(dat$Latitude)
dat$Longitude<- as.numeric(dat$Longitude) 

fsdat <- dat %>% 
  filter(grepl('Crop',`Fire purpose`)) %>%
  drop_na(`Fire season start`) %>%
  mutate(`Fire season start` = factor(`Fire season start`, levels = month.name)) %>%
  mutate(`Fire quarter` = factor(case_when(
    `Fire season start` == 'December' | `Fire season start` == 'January' | `Fire season start` == 'February' ~ 'DJF',
    `Fire season start` == 'March' | `Fire season start` == 'April' | `Fire season start` == 'May' ~ 'MAM',
    `Fire season start` == 'June' | `Fire season start` == 'July' | `Fire season start` == 'August' ~ 'JJA',
    `Fire season start` == 'September' | `Fire season start` == 'October' | `Fire season start` == 'November' ~ 'SON'), 
    levels= c('DJF','MAM','JJA','SON')))

shapes = c(21,23) 
inset_x <- c(70, 95)
inset_y <- c(20, 33)

#hack described here: https://github.com/tidyverse/ggplot2/issues/3909#issuecomment-602253881
myrect <- 
  list(
    cbind(
      c(inset_x[1], inset_x[2], inset_x[2], inset_x[1], inset_x[1]), 
      c(inset_y[1], inset_y[1], inset_y[2], inset_y[2], inset_y[1])
    )
  ) %>%
  st_polygon() %>%
  st_sfc(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#world plot
w <- ggplot() +
  geom_sf(data = worldmap, fill="beige", colour='grey',size=0.1) +
  geom_sf(data = myrect,
  #geom_rect(xmin = 50, xmax = 70, ymin = 50, ymax = 70, 
     fill = NA, colour = "red", size = 0.5) +
  coord_sf(xlim = c(-170, 180), ylim = c(-60, 85), expand = FALSE) +
  geom_point(data=fsdat,aes(x = Longitude, y = Latitude, shape = `Fire purpose`, fill= `Fire quarter`),
             colour='black', size = 0.75, stroke=0.1, alpha=0.9) + 
  theme(
    panel.background = element_rect(fill = "azure",colour = "black",size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(linetype='blank'),
    panel.grid.minor = element_line(linetype='blank'),
    legend.key = element_rect(fill = "white", colour = "white")
  ) +
  scale_fill_viridis_d(option='inferno') + 
  guides(fill=guide_legend(override.aes=list(shape=22))) +
  scale_shape_manual(values=shapes) 

#north India inset
i <- ggplot() + 
  geom_sf(data = worldmap, fill='beige', colour='grey') + 
  coord_sf(xlim = inset_x, ylim = inset_y, expand = FALSE) +
  geom_point(data=fsdat, aes(x = Longitude, y = Latitude, shape = `Fire purpose`, fill= `Fire quarter`),
             colour='black', size = 2, stroke=0.5, alpha=0.9) +
    theme(
    panel.background = element_rect(fill = "azure",colour = "red",size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(linetype='blank'),
    panel.grid.minor = element_line(linetype='blank'),
    legend.key = element_rect(fill = "white", colour = "white")
  ) +
  scale_fill_viridis_d(option='inferno') + 
  guides(fill=guide_legend(override.aes=list(shape=22))) +
  scale_shape_manual(values=shapes) 
```

``` r
w
```

![](DAFI-Paper_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
i
```

![](DAFI-Paper_files/figure-gfm/unnamed-chunk-26-2.png)<!-- -->

``` r
count21ha <- rep_FUS %>%
  filter(`Actual fire size mean (ha)` > 21) %>%
  filter(`Presence / Absence` == 'Presence') %>%
  filter(`Fire type` == 'Human, deliberate') %>%
  nrow(.)
  
count0ha <- rep_FUS %>%
  filter(`Actual fire size mean (ha)` > 0) %>%
  filter(`Presence / Absence` == 'Presence') %>%
  filter(`Fire type` == 'Human, deliberate') %>%
  nrow(.)  

perc21 <- 100 - (100 * as.numeric(count21ha) / as.numeric(count0ha))

count100ha <- rep_FUS %>%
  filter(`Actual fire size mean (ha)` > 100) %>%
  filter(`Presence / Absence` == 'Presence') %>%
  filter(`Fire type` == 'Human, deliberate') %>%
  nrow(.)
  
perc100 <- 100 - (100 * as.numeric(count100ha) / as.numeric(count0ha))
```

We find that 60% of records in DAFI for mean size of deliberately
started fires are \<21 ha, suggesting many anthropogenic fires will not
be detected.

Use of Sentinel‐2 derived fire data (at 0.04 ha spatial resolution) has
shown how MODIS‐derived products may underestimate burned area by up to
80% across Africa and that fires \<100 ha are critically important for
characterizing landscape fire on a global scale \[102\]. Our results
from DAFI support this; 79% of mean fire size records for deliberately
started fires are \<100ha, and cropland fires in pre-industrial and
transition regimes are generally smaller compared to fires ignited to
burn across landscapes more broadly (Figure 3a).

We also find a discrepancy between the median density for deliberate
anthropogenic fires in DAFI of 0.08 (Section 3.2.2) and the median value
suggested by the MODIS‐derived global fire atlas of less than 0.01 fires
km−2 year−1 \[44\]. The mean density in DAFI of 1.53 km−2 year−1
indicates a skewed distribution and the DAFI‐MODIS discrepancy is most
acute in regions of intensive crop residue burning in tightly packed
fields.

## 4.3. Categorising Anthropogenic Fire Uses and Regimes

``` r
fs.fp <- plyr::rbind.fill(lapply(colnames(rep_fire_use)[13:20],
                                     function (x){summarise.behaviour(temp.rep_fire_use=rep_FUS,temp.est_fire_use=est_FUS,
                                                                      type = "Fire", behaviour = x,
                                                                      grouping = c('Fire purpose'),
                                                                      inc.Absence = F, escape.rm = T)})) %>%
  filter(`Fire purpose` %in% c('Hunter gatherer', 'Pasture management')) %>%
  filter((grepl('max', Behaviour) | grepl('min', Behaviour)) & grepl('Actual', Behaviour))

hg.min <- filter(fs.fp, `Fire purpose` == 'Hunter gatherer' & grepl('min', Behaviour))$Combined.stat
hg.max <- filter(fs.fp, `Fire purpose` == 'Hunter gatherer' & grepl('max', Behaviour))$Combined.stat

pas.min <- filter(fs.fp, `Fire purpose` == 'Pasture management' & grepl('min', Behaviour))$Combined.stat
pas.max <- filter(fs.fp, `Fire purpose` == 'Pasture management' & grepl('max', Behaviour))$Combined.stat
```

In particular, hunting-gathering fire sizes vary more than pasture
managament fires, with sizes ranging from 1.4 ha to 8345 ha compared to
4.4 ha to 244.8 ha.
