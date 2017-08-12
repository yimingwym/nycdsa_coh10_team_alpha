library(data.table)
setwd('~/Downloads/kaggle_proj')
prop <- fread('properties_2016.csv', stringsAsFactors = F)
head(prop)
nrow(prop)
trs <- read.csv('train_2016_v2.csv')
saled <- right_join(prop, trs, by = 'parcelid')
head(saled)
saled$date = with(saled, as.POSIXct(transactiondate))

numeric_features <- names(saled)[sapply(saled, is.numeric)]
corr <- cor(saled %>% 
                      select(one_of(numeric_features, "logerror")), 
                    method = "pearson", 
                    use = "pairwise.complete.obs")

corrplot(corr, method = "color", tl.cex = 0.2)

length(numeric_features)
ncol(prop)
colnames(saled)

#####
airconditioningtypeid `mean(logerror)` `median(logerror)` `n()`
1                     1       0.01285241             0.0070 26668
2                    11       0.02629365             0.0227    63
3                    13       0.01546552             0.0070  1833
4                     3       0.09170000             0.0917     1
5                     5       0.01568233             0.0020   215
6                     9       0.01000000             0.0100     1
7                    NA       0.01070144             0.0050 61494
#####
architecturalstyletypeid `mean(logerror)` `median(logerror)` `n()`
1                       10       0.06390000             0.0639     1
2                        2       0.01225455             0.0090    11
3                       21       0.01447500             0.0260     8
4                        3       0.04622500             0.0313     4
5                        7       0.01641357             0.0030   221
6                        8       0.01591875             0.0208    16
7                       NA       0.01144176             0.0060 90014
#####
data_sale$tax_delinquency_year[data_sale$tax_delinquency_year == 6] = 2006
data_sale$tax_delinquency_year[data_sale$tax_delinquency_year == 7] = 2007
data_sale$tax_delinquency_year[data_sale$tax_delinquency_year == 8] = 2008
data_sale$tax_delinquency_year[data_sale$tax_delinquency_year == 9] = 2009
data_sale$tax_delinquency_year[data_sale$tax_delinquency_year == 10] = 2010
data_sale$tax_delinquency_year[data_sale$tax_delinquency_year == 11] = 2011
data_sale$tax_delinquency_year[data_sale$tax_delinquency_year == 12] = 2012
data_sale$tax_delinquency_year[data_sale$tax_delinquency_year == 13] = 2013
data_sale$tax_delinquency_year[data_sale$tax_delinquency_year == 14] = 2014
data_sale$tax_delinquency_year[data_sale$tax_delinquency_year == 15] = 2015
data_sale$tax_delinquency_year[data_sale$tax_delinquency_year == 99] = 1999

aircon <- sum(is.na(data_sale$aircon))/nrow(data_sale)
num_bathroom <- sum(is.na(data_sale$num_bathroom))/nrow(data_sale)
framing <- sum(is.na(data_sale$framing))/nrow(data_sale)
area_firstfloor_finished <- sum(is.na(data_sale$area_firstfloor_finished))/nrow(data_sale)
area_base <- sum(is.na(data_sale$area_base))/nrow(data_sale)
heating <- sum(is.na(data_sale$heating))/nrow(data_sale)
area_lot <- sum(is.na(data_sale$area_lot))/nrow(data_sale)
num_pool <- sum(is.na(data_sale$num_pool))/nrow(data_sale)
pool_with_spa <- sum(is.na(data_sale$pool_with_spa))/nrow(data_sale)
pool_without_spa <- sum(is.na(data_sale$pool_without_spa))/nrow(data_sale)
area_patio <- sum(is.na(data_sale$area_patio))/nrow(data_sale)
area_shed <- sum(is.na(data_sale$area_shed))/nrow(data_sale)
tax_building <- sum(is.na(data_sale$tax_building))/nrow(data_sale)
tax_year <- sum(is.na(data_sale$tax_year))/nrow(data_sale)
tax_delinquency_year <- sum(is.na(data_sale$tax_delinquency_year))/nrow(data_sale)
month <- sum(is.na(data_sale$month))/nrow(data_sale)
logerror <- sum(is.na(data_sale$logerror))/nrow(data_sale)

# region, census tract
# aircon column missingness: 0.68 (airconditioning type)
#     Central    Wall Unit    Yes     Evaporative Cooler     None     Refrigeration  NA's 
#     26668       63          1833    1                      215      1              61494

# (not sure remove) area_base missingness: 0.99 (Base unfinished and finished area) (impute as 0, creat another tag column)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 257    1112    2028    2303    3431    7224   89854

# (not sure remove) area_firstfloor_finished: 0.92 (Size of the finished living area on the 
#                                  first (entry) floor of the home)
# Min. 1st Qu.  Median  Mean    3rd Qu.   Max.    NA's 
# 44     938    1244    1348    1614      7625   83419 

# (proposed) area_lot : 0.11 (Area of the lot in square feet) (can random impute)
# Min.  1st Qu.  Median  Mean    3rd Qu.   Max.      NA's 
# 167   5703     7200    29110   11686     6971010   10150 

# (not sure remove) area_patio: 0.97 (Patio in  yard)
#   Min.   1st Qu.  Median    Mean    3rd Qu.    Max.     NA's 
#   25.0   180.0    259.5     310.1   384.0      2678.0   87629 

# (not sure remove) area_shed: 0.99 (Storage shed/building in yard)
# Min.   1st Qu.  Median    Mean    3rd Qu.    Max.     NA's 
# 18.0   100.0    159.0     311.7   361.0      1366.0   90180

# (not sure remove) framing: 0.99 (The building framing type (steel frame, wood frame, concrete/brick)) (drop)
#   Buildings having wood or wood and steel frames    NA's 
#   16                                                90259 
# most are missing, all non missing values are not fireproof

# (proposed) heating: 0.37 ( Type of home heating system) (can random impute) (factorise, level the NAs, impute after)
# Baseboard    Gravity    Heat Pump    Hot Water    None    other    radient     central    solar    yes     Forced air     Floor/Wall    NA's 
# 13           2          1            1            76      2         25          38303      97       1071   970            15519         34195 

# Forced air heating is probably the type of central heating most commonly installed in North America
# floor/ wall like radient
# heat pump : lower than -5 would be difficult to heat
# gravity: fuel combustion like, not comfortable

# logerror : 0

# month: 0

# num_bathroom: 0

# (finished) num_pool: 0.80 ( Number of pools on the lot (if any))
#      Min.    1st Qu. Median  Mean    3rd Qu. Max.  NA's 
#      1       1       1       1       1       1     72374 
# can remove this column, information has been provided from pool_with/without_spa

# (finished) pool_with_spa: 0.98 (I defiened the name)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1       1       1       1       1       1      89071
# most missing, seems all 1
# 
data_sale$pool_with_spa_imputed <- sapply(data_sale$pool_with_spa, 
                                             function(x) ifelse(is.na(x), 0, x))
summary(data_sale$pool_with_spa_imputed)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.00000 0.00000 0.00000 0.01334 0.00000 1.00000 

data_sale$pool_with_spa_imputed_tag <- sapply(data_sale$pool_with_spa,
                                                 function(x) ifelse(is.na(x), 1, 0))
data_sale$pool_with_spa_imputed_tag <- as.factor(as.character(data_sale$pool_with_spa_imputed_tag))
summary(data_sale$pool_with_spa_imputed_tag)
0     1 
1204 89071 

# (finished) pool_without_spa: 0.81 ( Pool without hot tub)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    1       1       1       1       1       1       73578 
# 80% missing, all one
# set 0 to all NAs, created a new tag column indicate wether originally NA or not.
data_sale$pool_without_spa_imputed <- sapply(data_sale$pool_without_spa, 
                                             function(x) ifelse(is.na(x), 0, x))
summary(data_sale$pool_without_spa_imputed)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.000   0.000   0.000   0.185   0.000   1.000 
data_sale$pool_without_spa_imputed_tag <- sapply(data_sale$pool_without_spa,
                                              function(x) ifelse(is.na(x), 1, 0))
data_sale$pool_without_spa_imputed_tag <- as.factor(as.character(data_sale$pool_without_spa_imputed_tag))
summary(data_sale$pool_without_spa_imputed_tag)
0     1 
16697 73578 
# (proposed) tax_building: 0.004 (The assessed value of the built structure on the parcel) (can random impute)
#    Min.  1st Qu.  Median    Mean     3rd Qu.    Max.        NA's 
#    100   81245    132000    180093   210534     9948100     380 

# (finished) tax_delinquency_year: 0.98 (Year for which the unpaid propert taxes were due )
#  1999  2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  NA's 
#  1     3     8     24    63    89    85    154   210   628   518   88492
impute_try$tax_delinquency_year_imputed_tag <- sapply(impute_try$tax_delinquency_year_imputed, 
                                                      function(x) ifelse(x == 'NA', 1, 0))
impute_try$tax_delinquency_year_imputed_tag <- as.factor(as.character(impute_try$tax_delinquency_year_imputed_tag))

# tax_year: 0

# impute NA in tax_delinquency_year as 'NA', I assume those variables have no violation on tax
library(missForest)
tax_building_mod <- missForest(data_sale$tax_building)

[1] "aircon"                       "num_bathroom"                 "framing"                      "area_firstfloor_finished"    
[5] "area_base"                    "heating"                      "area_lot"                     "num_pool"                    
[9] "pool_with_spa"                "pool_without_spa"             "area_patio"                   "area_shed"                   
[13] "tax_building"                 "tax_year"                     "tax_delinquency_year"         "month"                       
[17] "logerror"                     

# ncol(data_before): 17 (13)
# rule 1: remove all column have missingness larger than 90%: area_base (0.99), area_firstfloor_finished (0.92), 
#                                                             area_patio (0.97), area_shed (0.99), framing (0.99)
# rule 2: imputed tax_delinquency_year (0.98), pool_with_spa (0.98), pool_without_spa (0.81)
#         in ~_imputed with indicator column ~_imputed_tag (1: imputed; 0: no need to impute)
#         then remove tax_delinquency_year, pool_with_spa, pool_without_spa
#         specifically: tax_delinquency_year, impute NA in tax_delinquency_year as 'NA', 
#                       I assume those variables have no violation on tax
#                       pool_with_spa/pool_without_spa
#                       I assume those NA indicate there's no pool_with_spa/ pool_without_spa
#                       So, set 0 to all NAs (not sure if it's reasonable, need discuss)
# rule 3: remove num_pool, the information have been provided by pool_with_spa and pool_without_spa
# rule 4: factorise all needed columns

[1] "aircon"                           "num_bathroom"                     "heating"                         
[4] "area_lot"                         "tax_building"                     "tax_year"                        
[7] "month"                            "logerror"                         "tax_delinquency_year_imputed"    
[10] "pool_without_spa_imputed"         "pool_without_spa_imputed_tag"     "pool_with_spa_imputed"           
[13] "pool_with_spa_imputed_tag"        "tax_delinquency_year_imputed_tag"
# ncol(data_now): 14 (4)

# next: combine all other columns, impute less missingness (imputable) columns:
#       aircon, missingness: 0.68 (Airconditioning type)
#       area_lot, missingness : 0.11 (Area of the lot in square feet)
#       heating, missingness: 0.37 (Type of home heating system)
#       tax_building, missingness: 0.004 (The assessed value of the built structure on the parcel)

numeric_features <- names(impute_try)[sapply(impute_try, is.numeric)]
corr <- cor(impute_try %>% 
              select(one_of(numeric_features, "logerror")), 
            method = "pearson", 
            use = "pairwise.complete.obs")
corrplot(corr, method = "color", tl.cex = 1)

# summary(saled$regionidneighborhood)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
6952   46736  118887  190646  274800  764167   54263 

# summary(saled$regionidcity)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
3491   12447   25218   33761   45457  396556    1803 


ggplot(data.frame(logerror = saled$logerror, deck= is.na(saled$buildingclasstypeid)), 
                  aes(x=logerror, color=deck, fill=deck)) +
  geom_line(stat="density")

[1] "aircon"                 "framing"                "heating"               
[4] "month"                  "logerror"               "rawcensustractandblock"
[7] "censustractandblock"    "region_city"            "region_county"         
[10] "region_neighbor"        "region_zip"             "lat"                   
[13] "lon"

data_sale$fips <- sapply(data_sale$rawcensustractandblock, function(x) substr(x, 1, 4))
data_sale$blockid <- sapply(data_sale$rawcensustractandblock, function(x) substr(x, 5, 8))
# from    rawcensustractandblock, created one column
#         fips_blockid
# removed rawcensustractandblock, censustractandblock
# removed region_county, exactly the same with fips (3 counties)
# removed region_neighbor (duplicated information),
#         region_city (not accurate, doesn't make sense)
# removed framing column, 99% missing, only one level in available observations
# imputed region_zip, based on its relationship with fips_blockid
# imputed aircon, heating using missForest, added _imp_tags
# trying imputation with MICE

> summary(as.factor(as.character(for_impute$aircon)))
1    11    13     3     5     9  NA's 
26668    63  1833     1   215     1 61494 

airconfun <- function(x) {
  if (x <= 2) {
    return(1)
  }
  else if (x > 2 & x <= 4) {
    return(3)
  }
  else if (x > 4 & x <= 7) {
    return(5)
  }
  else if (x > 7 & x <= 10) {
    return(9)
  }
  else if (x > 10 & x <= 12) {
    return(11)
  }
  else if (x > 12) {
    return(13)
  }
}


> summary(as.factor(as.character(for_impute$heating)))
    1    10    11    12    13    14    18     2    20    24     6     7  NA's 
13     2     1     1    76     2    25 38303    97  1071   970 15519 34195 

heatingfun <- function(x) {
  if (x <= 1.5) {
    return(1)
  }
  else if (x > 1.5 & x <= 4) {
    return(2)
  }
  else if (x > 4 & x <= 6.5) {
    return(6)
  }
  else if (x > 6.5 & x <= 8.5) {
    return(7)
  }
  else if (x > 8.5 & x <= 10.5) {
    return(10)
  }
  else if (x > 10.5 & x <= 11.5) {
    return(11)
  }
  else if (x > 11.5 & x <= 12.5) {
    return(12)
  }
  else if (x > 12.5 & x <= 13.5) {
    return(13)
  }
  else if (x > 13.5 & x <= 16) {
    return(14)
  }
  else if (x > 16 & x <= 19) {
    return(18)
  }
  else if (x > 19 & x <= 22) {
    return(20)
  }
  else if (x > 22) {
    return(24)
  }
}

















