library(rpart)
library(lubridate) 
library(ade4)

all_data = clean %>% inner_join(imp, by="id_parcel")

# GOBAL SETTING
complexity_split_threshold = 0.001

important_variables = character()

##############################################################################
#
#   investigate region_zip unfold
#
t = select(all_data, region_zip)
t$region_zip = as.factor(t$region_zip)
df_region_zip = data.frame(sapply(acm.disjonctif(t), as.factor))

df_region_zip$logerror = all_data$logerror

train_data = df_region_zip[train_inx, ]
test_data = df_region_zip[-train_inx, ]

testModel= rpart(logerror ~ ., data = train_data, method='anova', control = rpart.control(cp = complexity_split_threshold))
prediction = predict(testModel, test_data)

median(abs(test_data$logerror - prediction))

important_variables = c(important_variables, names(testModel$variable.importance))

##############################################################################
#
#   investigate region_county unfold
#
t = select(all_data, region_county)
t$region_county = as.factor(t$region_county)
df_region_county = data.frame(sapply(acm.disjonctif(t), as.factor))

df_region_county$logerror = all_data$logerror

train_data = df_region_county[train_inx, ]
test_data = df_region_county[-train_inx, ]

testModel= rpart(logerror ~ ., data = train_data, method='anova', control = rpart.control(cp = complexity_split_threshold))
prediction = predict(testModel, test_data)

median(abs(test_data$logerror - prediction))
summary(testModel)

important_variables = c(important_variables, names(testModel$variable.importance))

##############################################################################
#
#   investigate region_neighbor unfold
#
t = select(all_data, region_neighbor)
t$region_neighbor = as.factor(t$region_neighbor)
df_region_neighbor = data.frame(sapply(acm.disjonctif(t), as.factor))

df_region_neighbor$logerror = all_data$logerror

train_data = df_region_neighbor[train_inx, ]
test_data = df_region_neighbor[-train_inx, ]

testModel= rpart(logerror ~ ., data = train_data, method='anova', control = rpart.control(cp = complexity_split_threshold))
prediction = predict(testModel, test_data)

median(abs(test_data$logerror - prediction))
summary(testModel)

important_variables = c(important_variables, names(testModel$variable.importance))

##############################################################################
#
#   investigate region_city unfold
#
t = select(all_data, region_city)
t$region_city = as.factor(t$region_city)
df_region_city = data.frame(sapply(acm.disjonctif(t), as.factor))

df_region_city$logerror = all_data$logerror

train_data = df_region_city[train_inx, ]
test_data = df_region_city[-train_inx, ]

testModel= rpart(logerror ~ ., data = train_data, method='anova', control = rpart.control(cp = complexity_split_threshold))
prediction = predict(testModel, test_data)

median(abs(test_data$logerror - prediction))
summary(testModel)

important_variables = c(important_variables, names(testModel$variable.importance))

##############################################################################
#
#   investigate zoning_landuse_county unfold
#
t = select(all_data, zoning_landuse_county)
t$zoning_landuse_county = as.factor(t$zoning_landuse_county)
df_zoning_landuse_county = data.frame(sapply(acm.disjonctif(t), as.factor))

df_zoning_landuse_county$logerror = all_data$logerror

train_data = df_zoning_landuse_county[train_inx, ]
test_data = df_zoning_landuse_county[-train_inx, ]

testModel= rpart(logerror ~ ., data = train_data, method='anova', control = rpart.control(cp = complexity_split_threshold))
prediction = predict(testModel, test_data)

median(abs(test_data$logerror - prediction))
summary(testModel)
# zoning_landuse_county.10 is important

important_variables = c(important_variables, names(testModel$variable.importance))

##############################################################################
#
#   investigate zoning_landuse unfold
#
t = select(all_data, zoning_landuse)
t$zoning_landuse = as.factor(t$zoning_landuse)
df_zoning_landuse = data.frame(sapply(acm.disjonctif(t), as.factor))

df_zoning_landuse$logerror = all_data$logerror

train_data = df_zoning_landuse[train_inx, ]
test_data = df_zoning_landuse[-train_inx, ]

testModel= rpart(logerror ~ ., data = train_data, method='anova', control = rpart.control(cp = complexity_split_threshold))
prediction = predict(testModel, test_data)

median(abs(test_data$logerror - prediction))
summary(testModel)

important_variables = c(important_variables, names(testModel$variable.importance))

##############################################################################
#
#   investigate zoning_property unfold
#
t = select(all_data, zoning_property)
t$zoning_property = as.factor(t$zoning_property)
df_zoning_property = data.frame(sapply(acm.disjonctif(t), as.factor))

df_zoning_property$logerror = all_data$logerror

train_data = df_zoning_property[train_inx, ]
test_data = df_zoning_property[-train_inx, ]

testModel= rpart(logerror ~ ., data = train_data, method='anova', control = rpart.control(cp = complexity_split_threshold))
prediction = predict(testModel, test_data)

median(abs(test_data$logerror - prediction))
summary(testModel)
# important: zoning_property.93
