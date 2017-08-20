library(dplyr)
library(ggplot2)
library(data.table)


setwd('~/Downloads/kaggle_proj')

properties <- fread('properties_2016.csv')
transactions <- fread('train_2016_v2.csv')

properties <- properties %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)

transactions <- transactions %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)

try_properties <- properties

del <- sapply(try_properties, function(x) sum(is.na(x)) / nrow(try_properties) > 0.75)
del <- c(1:ncol(try_properties))[del]
try_properties <- try_properties %>% select(-del)

try_properties$rawcensustractandblock <- as.character(try_properties$rawcensustractandblock)
try_properties$fips_blockid <- sapply(try_properties$rawcensustractandblock, function(x) substr(x, 1, 11))
try_properties$rawcensustractandblock <- NULL



#########remove all NA properties########################
# I don't plan to train them (11437 obs.)
# I plan to add mean logerror to those obs.`in prediction
#########################################################
head(try_properties %>% filter(is.na(latitude)), 20)
prop <- try_properties %>% filter(!is.na(latitude))
all_na_prop <- try_properties %>% filter(is.na(latitude))
summary(prop)

prop_ori_backup <- try_properties %>% filter(!is.na(latitude))



######### do more feature engineering ###################
num_na <- sapply(prop, function(x) sum(is.na(x)) / nrow(prop))
na_df <- data.frame(cols = colnames(prop), perc_na = num_na, stringsAsFactors = F, row.names = NULL)
ggplot(data = na_df, aes(x = reorder(cols, perc_na), y = perc_na)) + 
      geom_bar(stat = 'identity') + ylim(c(0, 1)) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))


unique(prop$aircon) # should keep numeric, MICE can handle discrete numeric 72% missing
#      1       3       5       9      11      12      13    NA's 
# 742364       7    8795      19    1818      59   58457 2162261 
prop$aircon <- ifelse(prop$aircon %in% c(3, 9, 12), 13, prop$aircon)

unique(prop$num_garage)
# 0       1       2       3       4       5       6       7       8       9      10      11      12 
# 14005  177579  660462   19634    8495    1705     575     266     181     127      78      62      41 
# 13      14      15      16      17      18      19      20      21      24      25    NA's 
# 16      14       9       3       4       1       3       2       2       2       1 2090513 
unique(prop$area_garage) # could impute 0, or there_might be garage
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.0   324.0   441.0   383.8   494.0  7749.0 2090513 
#                                              0: 196752
# > summary(prop$area_total_calc)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1    1213    1572    1827    2136  952576   44128 
# > na_garage_prop <- prop %>% filter(is.na(num_garage))
# > summary(na_garage_prop$area_total_calc)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1    1174    1522    1788    2055  952576   44082 

##########
# same distribution between two garage columns, need mice imputation
##########
unique(prop$region_neighbor)
### too many missing, information in fips_block id or county, don't need this
prop$region_neighbor <- NULL

unique(prop$heating)
# 1       2       6       7      10      11      12      13      14      18      19      20      21 
# 262 1156830   27480  595453      39      16      25    1342      41     586       1    3216       3 
# 24    NA's 
#   21107 1167379 

# (proposed) heating: 0.37 ( Type of home heating system) (can random impute)
# Baseboard    Gravity    Heat Pump    Hot Water    None    other    radient     central    solar    yes     Forced air     Floor/Wall    NA's 
# 13           2          1            1            76      2         25          38303      97       1071   970            15519         34195 

# Forced air heating is probably the type of central heating most commonly installed in North America
# floor/ wall like radient
# heat pump : lower than -5 would be difficult to heat
# gravity: fuel combustion like, not comfortable

#########
#plan for heating: 1, 10, 11, 12, 24, 19 -> 14 (other)
#                  18, 21 -> 7(floor/wall radient)
#                  6 -> 2 (central)
prop_backup <- prop

prop$heating <- ifelse(prop$heating %in% c(1, 10, 11, 12, 24, 19), 14, prop$heating)
prop$heating <- ifelse(prop$heating %in% c(18, 21), 7, prop$heating)
prop$heating <- ifelse(prop$heating == 6, 2, prop$heating)
summary(as.factor(prop$heating))
# 2       7      13      14      20    NA's 
# 1184310  596042    1342   21491    3216 1167379 

unique(prop$quality)
#assessment point
#       1       2       3       4       5       6       7       8       9      10      11      12    NA's 
#   69467       2       6  692160      21      94 1133238      51      25   39713      19    3692 1035292 
# we can randomly impute points in this column, mice or random
unique(prop$num_unit)
# those properties without num_unit has same area_total_calc distribution with
# those properties with num_unit
# I plan to do the imputation, MICE or random select

summary(prop$area_lot)
# plan MICE or RANDOM impute

# many outliers, especially huge properties
summary(prop$area_live_finished)
# same with area_total_calc, delete
prop$area_live_finished <- NULL

summary(prop$num_bathroom_calc)
# completely same information with num_bathroom, delete
prop$num_bathroom_calc <- NULL
prop$num_bath <- NULL
prop$censustractandblock <- NULL

rm(prop_backup)
prop_backup <- prop

summary(prop$tax_land)
# > cor(tax$tax_building, tax$tax_property)
# [1] 0.8069155
# > cor(tax$tax_land, tax$tax_property)
# [1] 0.8216457
# plan: delete
prop$tax_land <- NULL

summary(prop$region_city)

summary(prop$build_year)
##### MICE impute

summary(prop$area_total_calc)
##### MICE impute

summary(prop$tax_building)
# > cor(tax$tax_building, tax$tax_property)
# [1] 0.8069155
# > cor(tax$tax_land, tax$tax_property)
# [1] 0.8216457
# plan: delete
prop$tax_building <- NULL

summary(prop$tax_total)
##### sum of other tax columns while more incompleteness, delete
prop$tax_total <- NULL

summary(prop$tax_property)
# plan: MICE impute or RANDOM

summary(prop$region_zip)
prop$region_zip <- ifelse(prop$region_zip == 399675, 99675, prop$region_zip)

summary(prop$num_room)
# MICE impute

summary(prop$num_bathroom)
# MICE impute

summary(prop$num_bedroom)
# MICE impute

summary(prop$tax_year)
# MICE impute

summary(prop$zoning_property)
# first plan, delete column, too complicated
# 995151 MISSING, 5K levels
prop$zoning_property <- NULL


sum(prop$zoning_landuse_county == '')
# 840 missing, 241 levels, to factor

unique(as.factor(prop$zoning_landuse))
# very clean column, beautiful! to factor

summary(prop$tax_delinquency)
# sensitive, plan remove
prop$tax_delinquency <- NULL

summary(prop$region_county)
# clean county information

summary(prop$longitude)
summary(prop$latitude)

#plan scale the longitude and latitude, squeeze more information!
prop$lon <- (prop$longitude - min(prop$longitude)) / (max(prop$longitude) - min(prop$longitude))
prop$lat <- (prop$latitude - min(prop$latitude)) / (max(prop$latitude) - min(prop$latitude))

prop$latitude <- NULL
prop$longitude <- NULL


########flag related
summary(prop$flag_tub)
prop$flag_tub <- ifelse(prop$flag_tub == '', 'false', prop$flag_tub)
prop$flag_fireplace <- ifelse(prop$flag_fireplace == '', 'false', prop$flag_fireplace)

summary(prop$fips)
# same with region_county, remove
prop$fips <- NULL

prop$num_bedroom <- as.integer(prop$num_bedroom)
prop$heating <- as.integer(prop$heating)
prop$build_year <- as.integer(prop$build_year)
prop$num_room <- as.integer(prop$num_room)
prop$region_zip <- as.integer(prop$region_zip)
prop$zoning_landuse <- as.factor(prop$zoning_landuse)
prop$zoning_landuse_county <- as.factor(prop$zoning_landuse_county)
prop$flag_tub <- as.factor(prop$flag_tub)
prop$region_county <- as.factor(prop$region_county)
prop$flag_fireplace <- as.factor(prop$flag_fireplace)
prop$fips_blockid <- as.factor(prop$fips_blockid)

