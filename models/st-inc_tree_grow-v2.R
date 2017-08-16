# try to grow the tree incrementally; i.e. start with one column and test all, take the 
# one with the smallest error; then add another column and keep the one with results in the
# smallest overall error; stop if the error doesn't decrease anymore.
#

library(rpart)
library(lubridate) 
library(ade4)

all_data = cbind(clean, imp[,-1])


all_data$logerror_q3 = NULL

#remove outlier
all_data = all_data[logerror>-0.8 & logerror<0.8]

#all_data$is_zoning_landuse_county.10 = as.factor(all_data$zoning_landuse_county == 10)

set.seed(2344)
train_inx = sample(1:nrow(all_data), nrow(all_data)*0.7)

#all_data$date = as.numeric(all_date$date)
#selectedColNames = colnames(imp)
#selectedColNames = selectedColNames[-which(selectedColNames %in% c("id_parcel", "logerror"))]

selectedColNames = character()

remainingColNames = colnames(all_data)
remainingColNames = remainingColNames[-which(remainingColNames %in% c("id_parcel", "logerror", "fips_blockid", selectedColNames))]

complexity_threshold = 0.001
complexity_threshold_dec = 0
bestError = 1000
while (length(remainingColNames>0))
{
  minError = 1000
  bestCol = ""
  for (rn in remainingColNames){
    print(rn)
    testColNames = c(selectedColNames, rn)
    
    train_data = all_data[train_inx, c(testColNames, "logerror"), with=F]
    test_data = all_data[-train_inx, c(testColNames, "logerror"), with=F]
    
    #train_data = filter(train_data, logerror>-0.5 & logerror<0.5)
    
    testModel= rpart(logerror ~ ., data = train_data, method='anova', control = rpart.control(cp = complexity_threshold))

    prediction = predict(testModel, test_data)
    prediction = prediction[!is.na(prediction)]
    err = median(abs(test_data$logerror - prediction))
    if (err<minError){
      print(paste0("new bestError: ", as.character(err), collapse = ""))
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
  {
    complexity_threshold_dec = complexity_threshold_dec + 1
    if (complexity_threshold_dec>10)
      break
    cat("no best found, reduce complexity threshold: ", complexity_threshold_dec, "\n")
    complexity_threshold = complexity_threshold -0.0005
  }
  
  selectedColNames = c(selectedColNames, bestCol)
  remainingColNames = remainingColNames[-which(remainingColNames==bestCol)]
}

cat("best error: ", bestError, "\n")
cat("selected columns: ", selectedColNames, "\n")

cat("baseline median abs err", median(abs(mean(all_data$logerror)-all_data$logerror)), "\n")
cat("baseline mean abs err", mean(abs(mean(all_data$logerror)-all_data$logerror)), "\n")


