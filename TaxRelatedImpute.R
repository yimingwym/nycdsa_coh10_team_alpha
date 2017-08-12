# Importing Data
properties <- fread(input = "https://s3.us-east-2.amazonaws.com/aws-emr-dedicated/data/zillow/properties_2016.csv", 
                    na.strings = "") 
## convert lat/lon
properties <- properties %>%
  mutate(latitude = latitude/1e6, longitude = longitude/1e6)
train <- fread(input = "https://s3.us-east-2.amazonaws.com/aws-emr-dedicated/data/zillow/train_2016_v2.csv",
               na.strings = "")

## train_data: train inner_join properties 
train_data <- train %>% 
  mutate(year=year(transactiondate),
         month=month(transactiondate)) %>%
  dplyr::select(-transactiondate) %>%
  inner_join(properties, by="parcelid") 


#Renaming
train_data <- train_data %>% rename(
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


#Tax impute
library(mice)
pattern=md.pattern(train_data)
df=data.frame(pattern)
df$count=rownames(pattern)

taxcols=train_data %>%  dplyr::select(id_parcel,tax_total, #taxvaluedollarcnt,
                      tax_building, #= structuretaxvaluedollarcnt,
                      tax_land, #=landtaxvaluedollarcnt,
                      tax_property, # = taxamount,
                      tax_year, #= assessmentyear,
                      tax_delinquency, # = taxdelinquencyflag,
                      tax_delinquency_year # = taxdelinquencyyear,
) 


#convert ID to character
taxcols$id_parcel=as.character(taxcols$id_parcel)

#summary
summary(taxcols)

#tax_delinquency
#????drop, overlap with tax_delinquency_year
#taxcols$tax_delinquency=as.factor(taxcols$tax_delinquency)
#convert to 0/1 factors
taxcols$tax_delinquency=as.factor(ifelse(is.na(taxcols$tax_delinquency),'0','1'))
  
#drop tax year column  
taxcols= taxcols %>% dplyr::select(-tax_year)

#tax_delinquency_year
#taxcols$tax_delinquency_year=as.factor(taxcols$tax_delinquency_year)
taxcols %>% group_by(tax_delinquency_year) %>% count()

ggplot(data.frame(logerror = train_data$logerror, deck=as.factor(train_data$tax_delinquency_year)), aes(x=logerror, color=deck, fill=deck)) +
  geom_line(stat="density") +
  theme_bw()

#impuate NA as 0
taxcols$tax_delinquency_year=ifelse(is.na(taxcols$tax_delinquency_year)
                                    ,0,taxcols$tax_delinquency_year)


#plot pattern
library(mice)
library(ggplot2)
aggr(taxcols, labels=names(taxcols), cex.axis=.5,
     gap=3, ylab=c("Missing data","Pattern"))

a=taxcols[is.na(taxcols$tax_building),]


