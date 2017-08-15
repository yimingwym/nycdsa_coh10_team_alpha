# try to grow the tree incrementally; i.e. start with one column and test all, take the 
# one with the smallest error; then add another column and keep the one with results in the
# smallest overall error; stop if the error doesn't decrease anymore.
#

library(rpart)
library(lubridate) 
library(ade4)

all_data = clean %>% inner_join(imp, by="id_parcel")

all_data$month_factor = as.factor(month(all_data$date))
all_data$month = month(all_data$date)
all_data$num_garage_fac=as.factor(all_data$num_garage)
all_data$num_garage_1_fac=as.factor(all_data$num_garage_1)
all_data$num_pool_fac=as.factor(all_data$num_pool)
all_data$num_pool_1_fac=as.factor(all_data$num_pool_1)
all_data$num_room_fac=as.factor(all_data$num_room)
all_data$num_story_fac=as.factor(all_data$num_story)
all_data$num_unit_fac=as.factor(all_data$num_unit)
all_data$num_bathroom_fac=as.factor(all_data$num_bathroom)
all_data$num_bedroom_fac=as.factor(all_data$num_bedroom)
all_data$num_bath_fac=as.factor(all_data$num_bath)
all_data$num_75_bath_fac=as.factor(all_data$num_75_bath)
all_data$num_fireplace_fac=as.factor(all_data$num_fireplace)

all_data$is_zoning_landuse_county.10 = as.factor(all_data$zoning_landuse_county == 10)

set.seed(2344)
train_inx = sample(1:nrow(all_data), nrow(all_data)*0.7)

#all_data$date = as.numeric(all_date$date)
#selectedColNames = colnames(imp)
#selectedColNames = selectedColNames[-which(selectedColNames %in% c("id_parcel", "logerror"))]

selectedColNames = character()

remainingColNames = colnames(all_data)
remainingColNames = remainingColNames[-which(remainingColNames %in% c("id_parcel", "logerror", "fips_blockid", selectedColNames))]

bestError = 1000
while (length(remainingColNames>0))
{
  minError = 1000
  bestCol = ""
  for (rn in remainingColNames){
    print(rn)
    testColNames = c(selectedColNames, rn)
    
    train_data = all_data[train_inx, c(testColNames, "logerror")]
    test_data = all_data[-train_inx, c(testColNames, "logerror")]
    
    #train_data = filter(train_data, logerror>-0.5 & logerror<0.5)
    
    testModel= rpart(logerror ~ ., data = train_data, method='anova', control = rpart.control(cp = 0.0001))

    prediction = predict(testModel, test_data)
    err = median(abs(test_data$logerror - prediction))
    if (err<minError){
      minError = err
      bestCol = rn
    }
    
    print(err)
  }

  
  if (bestError>minError){
    bestError = minError
    print(paste0("new bestError: ", as.character(bestError), collapse = ""))
    print(paste0("selected column: ", bestCol, collapse = ""))
  }
  else
    break
  
  selectedColNames = c(selectedColNames, bestCol)
  remainingColNames = remainingColNames[-which(remainingColNames==bestCol)]
}

cat("best error: ", bestError, "\n")
cat("selected columns: ", selectedColNames, "\n")



