
library(randomForest)

all_data = cbind(clean, imp[,-1])
all_data$logerror_q3 = NULL

col_names = colnames(all_data)
rm_names = c("id_parcel", "fips_blockid", "date")

cn = col_names[(!(col_names %in% rm_names))]

# train / test data split
set.seed(2344)
train_inx = sample(1:nrow(all_data), nrow(all_data)*0.8)
train_data = all_data[train_inx, cn, with=F]
test_data = all_data[-train_inx, cn, with=F]



set.seed(100)
Sys.time()
tree.rand = randomForest(logerror ~ ., data = train_data, importance = TRUE, sampsize=10000, ntree=500, do.trace=T)
Sys.time()

importance(tree.rand)
varImpPlot(tree.rand)

tree.pred = predict(tree.rand, test_data)

err = test_data$logerror - tree.pred
cat("median abs err: ", median(abs(err)), "\n")
cat("mean abs err: ", mean(abs(err)), "\n")
cat("mean square err: ", mean(err^2), "\n")
cat("baseline median abs err", median(abs(mean(all_data$logerror)-test_data$logerror)), "\n")
cat("baseline mean abs err", mean(abs(mean(all_data$logerror)-test_data$logerror)), "\n")
cat("baseline mean square err: ", mean((mean(all_data$logerror)-test_data$logerror)^2), "\n")







makePrediction <- function(model, newdata, months, labels, dates) {
  predictions <- newdata[, "parcelid", drop=FALSE]
  for(i in 1:length(months)) {
    cat("month: ", months[i], "\n")
    newdata$month <- months[i]
    newdata$month_factor = factor(newdata$month, levels = levels(train_data$month_factor))
    newdata$date = as.Date(dates[i])
    predictions[, labels[i]] <- predict(model, newdata = newdata)
  }
  cat("write submission data to disk\n")
  write.csv(x = predictions, file = "submission_randForest_1.csv", 
            quote = FALSE, row.names = FALSE)
  return(predictions)
}

newdata = cbind(clean_prop, imp_prop[,-1])
newdata$parcelid = newdata$id_parcel
newdata$date = as.Date("2016-06-11") # impute the median

s = makePrediction(tree.rand, newdata = newdata, months = c(10),#, 11, 12, 10, 11, 12), 
               labels = c("201610", "201611", "201612", "201710", "201711", "201712"), 
               dates = c("2016-10-15", "2016-11-15", "2016-12-15", "2017-10-15", "2017-11-15", "2017-12-15") )



levels(train_data$month_factor)

colnames(newdata)[!colnames(newdata) %in% colnames(train_data)]

colnames(train_data)[!colnames(train_data) %in% colnames(newdata)]







