# try to grow the tree incrementally; i.e. start with one column and test all, take the 
# one with the smallest error; then add another column and keep the one with results in the
# smallest overall error; stop if the error doesn't decrease anymore.
#

library(rpart)

all_data = clean %>% inner_join(imp, by="id_parcel")
set.seed(2344)
train_inx = sample(1:nrow(all_data), nrow(all_data)*0.7)

#all_data$date = as.numeric(all_date$date)
#selectedColNames = colnames(imp)
#selectedColNames = selectedColNames[-which(selectedColNames %in% c("id_parcel", "logerror"))]

selectedColNames = character()

remainingColNames = colnames(all_data)
remainingColNames = allColNames[-which(remainingColNames %in% c("id_parcel", "logerror", selectedColNames))]

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
    
    testModel= rpart(logerror ~ ., data = train_data, method='anova')

    prediction = predict(testModel, test_data)
    err = median(abs(test_data$logerror - prediction))
    if (err<minError){
      minError = err
      bestCol = rn
    }
    
    print(err)
  }
  selectedColNames = c(selectedColNames, bestCol)
  remainingColNames = remainingColNames = allColNames[-which(remainingColNames==bestCol)]
  
  if (bestError>minError)
    bestError = minError
  else
    break
}

print("best error:")
bestError
print("selected columns:")
selectedColNames

# NO COLUMN CAN BE FOUND !!!!!
