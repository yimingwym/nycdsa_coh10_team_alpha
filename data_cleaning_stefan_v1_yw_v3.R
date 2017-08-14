library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)
library(mice)

############################################################################################
###################### Stephan, creates cleaned, imputation free data ######################
############################################################################################
#
#   data loading and renaming
#


properties <- fread('properties_2016.csv')
transactions <- fread('train_2016_v2.csv')
#sample_submission <- fread('./data/sample_submission.csv')

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

#properties <- properties %>% 
#  mutate(tax_delinquency = ifelse(tax_delinquency=="Y",1,0),
#         flag_fireplace = ifelse(flag_fireplace=="Y",1,0),
#         flag_tub = ifelse(flag_tub=="Y",1,0))

sales = as.data.table(
  transactions %>%
    inner_join(properties, by="id_parcel")
)

clean = data.table(id_parcel = sales$id_parcel, date=as.Date(sales$date), logerror=sales$logerror)
imp = data.table(id_parcel = sales$id_parcel)



################################################
##  build_year

imp$build_year_isna = is.na(sales$build_year)
clean$build_year = ifelse(imp$build_year_isna, 0, sales$build_year)


################################################
##  garagetotalsqft (area_garage)
##  num_garage
##
##  Total number of square feet of all garages on lot including an attached garage.

imp$num_garage_isna = is.na(sales$num_garage)
clean$num_garage = ifelse(is.na(sales$num_garage), 0, sales$num_garage)

imp$area_garage_isna = is.na(sales$area_garage)
clean$area_garage = ifelse(is.na(sales$area_garage), 0, sales$area_garage)

# calc avg. garage size
avg_garage_area = (properties %>% 
                     filter(num_garage>0 & area_garage>0) %>% 
                     summarize(sum(area_garage)/sum(num_garage)))[1,1]

print(paste0("avg. garage area: ", avg_garage_area))

imp$num_garage_1 = clean$area_garage>0 & clean$num_garage==0

clean$num_garage_1 = ifelse(
  imp$num_garage_1, 
  1, 
  clean$num_garage
)

imp$area_garage_1 = clean$num_garage>0 & clean$area_garage==0

clean$area_garage_1 =  ifelse(
  imp$area_garage_1,
  clean$num_garage * avg_garage_area,
  clean$area_garage
)     

################################################
## area_basement
## area_patio
## area_shed
## area_lot
## area_firstfloor_finished
## area_total_calc
## area_base
## area_live_finished
## area_liveperi_finished
## area_total_finished
## area_unknown

imp$area_basement_isna = is.na(sales$area_basement)
clean$area_basement = ifelse(is.na(sales$area_basement), 0, sales$area_basement)

imp$area_patio_isna = is.na(sales$area_patio)
clean$area_patio = ifelse(is.na(sales$area_patio), 0, sales$area_patio)

imp$area_shed_isna = is.na(sales$area_shed)
clean$area_shed = ifelse(is.na(sales$area_shed), 0, sales$area_shed)

imp$area_lot_isna = is.na(sales$area_lot)
clean$area_lot = ifelse(is.na(sales$area_lot), 0, sales$area_lot)

imp$area_firstfloor_finished_isna = is.na(sales$area_firstfloor_finished)
clean$area_firstfloor_finished = ifelse(is.na(sales$area_firstfloor_finished), 0, sales$area_firstfloor_finished)

imp$area_total_calc_isna = is.na(sales$area_total_calc)
clean$area_total_calc = ifelse(is.na(sales$area_total_calc), 0, sales$area_total_calc)

imp$area_base_isna = is.na(sales$area_base)
clean$area_base = ifelse(is.na(sales$area_base), 0, sales$area_base)

imp$area_live_finished_isna = is.na(sales$area_live_finished)
clean$area_live_finished = ifelse(is.na(sales$area_live_finished), 0, sales$area_live_finished)

imp$area_liveperi_finished_isna = is.na(sales$area_liveperi_finished)
clean$area_liveperi_finished = ifelse(is.na(sales$area_liveperi_finished), 0, sales$area_liveperi_finished)

imp$area_total_finished_isna = is.na(sales$area_total_finished)
clean$area_total_finished = ifelse(is.na(sales$area_total_finished), 0, sales$area_total_finished)

imp$area_unknown_isna = is.na(sales$area_unknown)
clean$area_unknown = ifelse(is.na(sales$area_unknown), 0, sales$area_unknown)

imp$area_no_lot_and_no_total = imp$area_lot_isna & imp$area_total_calc_isna

#imp$_isna = is.na(sales$)
#clean$ = ifelse(is.na(sales$), 0, sales$)

################################################
##  poolsizesum (area_pool)
##  num_pool
##  pooltypeid2
##  pooltypeid7
##  pooltypeid10
##
##  Total square footage of all pools on property.

imp$num_pool_isna = is.na(sales$num_pool)
clean$num_pool = ifelse(is.na(sales$num_pool), 0, sales$num_pool)

imp$area_pool_isna = is.na(sales$area_pool)
clean$area_pool = ifelse(is.na(sales$area_pool), 0, sales$area_pool)

# calculate avg pool area
avg_pool_area = (properties %>% 
                   filter(num_pool>0 & area_pool>0) %>% 
                   summarize(sum(area_pool)/sum(num_pool)))[1,1]

print(paste0("avg. pool area: ", avg_pool_area))


imp$num_pool_1 = clean$area_pool>0 & clean$num_pool==0

clean$num_pool_1 = ifelse(
  imp$num_pool_1, 
  1, 
  clean$num_pool
)

imp$area_pool_1 = clean$num_pool>0 & clean$area_pool==0

clean$area_pool_1 =  ifelse(
  imp$area_pool_1,
  clean$num_pool * avg_pool_area,
  clean$area_pool
)     

imp$pooltypeid2_isna = is.na(sales$pooltypeid2)
clean$pooltypeid2 = ifelse(imp$pooltypeid2_isna, 0, sales$pooltypeid2)

imp$pooltypeid7_isna = is.na(sales$pooltypeid7)
clean$pooltypeid7 = ifelse(imp$pooltypeid7_isna, 0, sales$pooltypeid7)

imp$pooltypeid10_isna = is.na(sales$pooltypeid10)
clean$pooltypeid10 = ifelse(imp$pooltypeid10_isna, 0, sales$pooltypeid10)


################################################
##  region_city
##  region_neighbor
##  region_county
##  region_zip
##

imp$region_city_isna = is.na(sales$region_city)
clean$region_city = as.integer(as.factor(ifelse(imp$region_city_isna, 0, sales$region_city)))

imp$region_neighbor_isna = is.na(sales$region_neighbor)
clean$region_neighbor = as.integer(as.factor(ifelse(imp$region_neighbor_isna, 0, sales$region_neighbor)))

imp$region_county_isna = is.na(sales$region_county)
clean$region_county = as.factor(ifelse(imp$region_county_isna, 0, sales$region_county))

imp$region_zip_isna = is.na(sales$region_zip)
clean$region_zip = as.integer(as.factor(ifelse(imp$region_zip_isna, 0, sales$region_zip)))

################################################
##  zoning_landuse_county
##  zoning_landuse
##  zoning_property

# no missing values
clean$zoning_landuse_county = as.integer(as.factor(sales$zoning_landuse_county))

# no missing values
clean$zoning_landuse = as.factor(sales$zoning_landuse)

# no missing values
clean$zoning_property = as.integer(as.factor(sales$zoning_property))

################################################
##  num_room
##  num_unit
##  num_story
##  num_bathroom
##  num_bedroom
##  num_bathroom_calc
##  num_bath
##  num_75_bath
##  num_fireplace

#num_room is always set
clean$num_room = sales$num_room

imp$num_story_isna = is.na(sales$num_story)
clean$num_story = as.factor(ifelse(imp$num_story_isna, 0, sales$num_story))

imp$num_unit_isna = is.na(sales$num_unit)
clean$num_unit = ifelse(imp$num_unit_isna, 0, sales$num_unit)

imp$num_bathroom_isna = is.na(sales$num_bathroom)
clean$num_bathroom = ifelse(imp$num_bathroom_isna, 0, sales$num_bathroom)

imp$num_bedroom_isna = is.na(sales$num_bedroom)
clean$num_bedroom = ifelse(imp$num_bedroom_isna, 0, sales$num_bedroom)

imp$num_bathroom_calc_isna = is.na(sales$num_bathroom_calc)
clean$num_bathroom_calc = ifelse(imp$num_bathroom_calc_isna, 0, sales$num_bathroom_calc)

imp$num_bath_isna = is.na(sales$num_bath)
clean$num_bath = ifelse(imp$num_bath_isna, 0, sales$num_bath)

imp$num_75_bath_isna = is.na(sales$num_75_bath)
clean$num_75_bath = ifelse(imp$num_75_bath_isna, 0, sales$num_75_bath)

imp$num_fireplace_isna = is.na(sales$num_fireplace)
clean$num_fireplace = ifelse(imp$num_fireplace_isna, 0, sales$num_fireplace)

imp$num_fireplace_1 = is.na(sales$num_fireplace) & sales$flag_fireplace=="true"
clean$num_fireplace = ifelse(imp$num_fireplace_1, 1, clean$num_fireplace)

################################################
##  tax_total
##  tax_building
##  tax_land
##  tax_property
##  tax_year
##  tax_delinquency
##  tax_delinquency_year
##

imp$tax_total_isna = is.na(sales$tax_total)
clean$tax_total = ifelse(imp$tax_total_isna, 0, sales$tax_total)

imp$tax_building_isna = is.na(sales$tax_building)
clean$tax_building = ifelse(imp$tax_building_isna, 0, sales$tax_building)

imp$tax_land_isna = is.na(sales$tax_land)
clean$tax_land = ifelse(imp$tax_land_isna, 0, sales$tax_land)

imp$tax_property_isna = is.na(sales$tax_property)
clean$tax_property = ifelse(imp$tax_property_isna, 0, sales$tax_property)

# tax_year has always the same value; drop it;

clean$tax_delinquency = ifelse(sales$tax_delinquency=="Y", 1, 0)

imp$tax_delinquency_year_isna = is.na(sales$tax_delinquency_year)
clean$tax_delinquency_year = ifelse(imp$tax_delinquency_year_isna, 0, sales$tax_delinquency_year)

#imp$_isna = is.na(sales$)
#clean$ = ifelse(imp$_isna, 0, sales$)

################################################
##  material
##  deck
##  quality
##  framing
##  story
##  heating
##  aircon
##  architectural_style
##  flag_tub

clean$material = ifelse(is.na(sales$material), 0 , sales$material)

clean$material = factor(clean$material, 0:18, labels=c('NA', 'Adobe', 'Brick', 'Concrete Block', 'Concrete', 'Dome', 'Frame', 
                                                       'Heavy', 'Log', 'Light', 'Metal', 'Manufactured', 'Mixed', 'Masonry', 
                                                       'Other', 'Steel', 'Stone', 'Tilt-Up', 'Wood'))

clean$deck = as.factor(ifelse(is.na(sales$deck), 0, sales$deck))

clean$quality = as.factor(ifelse(is.na(sales$quality), 0, sales$quality))

clean$framing = as.factor(ifelse(is.na(sales$framing), 0, sales$framing))

clean$story = as.factor(ifelse(is.na(sales$story), 0, sales$story))

clean$heating = as.factor(ifelse(is.na(sales$heating), 0, sales$heating))

clean$aircon = as.factor(ifelse(is.na(sales$aircon), 0, sales$aircon))

clean$architectural_style = as.factor(ifelse(is.na(sales$architectural_style), 0, sales$architectural_style))

clean$flag_tub = ifelse(sales$flag_tub=="true", 1, 0)


################################################
##  longitude
##  latitude

clean$longitude = sales$longitude
clean$latitude = sales$latitude

################################################
##  censustractandblock
##  

clean$censustractandblock = as.integer(as.factor(sales$censustractandblock))

############################################################################################
############################################################################################
############################################################################################


# missing_values <- clean %>% summarize_each(funs(sum(is.na(.))/n()))
# 
# missing_values <- gather(missing_values, key="feature", value="missing_pct")
# missing_values %>% 
#   ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
#   geom_bar(stat="identity",fill="red")+
#   coord_flip()+theme_bw()
# 
############################################################################################
################### Yiming, updates, adds fips_blockid column in clean data ################
############################################################################################

##### I don't find rawcensustractandblock in clean, which is a very informative column.

##### Introduce and extract the blockid from rawcensustractandblock column in sales
##### For example: 60371066.461001: '6037'(County id or so called 'fips')
#####                               '1066.46' (block id, 4 digits before and 2 digits after dot)
#####                               omit the rest digits

clean$rawcensustractandblock <- sales$rawcensustractandblock
clean$fips_blockid <- sapply(clean$rawcensustractandblock, function(x) substr(x, 1, 11))
clean$rawcensustractandblock <- NULL

##### add NA tag column of censustractandblock in imp, set NAs in clean$censustractandblock to 0
imp$censustractandblock_isna = is.na(sales$censustractandblock)
clean$censustractandblock = sales$censustractandblock
clean$censustractandblock[imp$censustractandblock_isna] <- 0

# #### before mice impute
# data_sale_mice <- clean %>% select(aircon, heating, logerror, latitude, longitude, fips_blockid)
# data_sale_mice$region_zip <- sales$region_zip
# data_sale_mice$aircon_isna <- is.na(data_sale_mice$aircon)
# data_sale_mice$heating_isna <- is.na(data_sale_mice$heating)
# data_sale_mice$region_zim_isna <- is.na(data_sale_mice$region_zip)
# data_sale_mice$region_zip[data_sale$region_zip == 399675] <- 99675
# 
# ##### imputation train
# mice_mod <- mice(data_sale_mice, method='rf') 
# mice_output <- complete(mice_mod)
# 
# ##### imputation
# data_sale_mice$airconimp <- mice_output$aircon
# data_sale_mice$heatingimp <- mice_output$heating
# data_sale_mice$region_zip <- mice_output$region_zip
# ##### The mice imputation seems perfect! histograms matches perfectly!
# ##### Much better than the result (aircon, heating) from missForest
