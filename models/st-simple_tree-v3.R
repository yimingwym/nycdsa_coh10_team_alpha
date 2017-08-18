library(rpart)

all_data = cbind(clean, imp[,-1])
all_data$logerror_q3 = NULL

col_names = colnames(all_data)
rm_names = c("id_parcel", "fips_blockid", "date")

cn = col_names[(!(col_names %in% rm_names))]


# train / test data split
set.seed(2344)
train_inx = sample(1:nrow(all_data), nrow(all_data)*0.7)
train_data = all_data[train_inx, cn, with=F]
test_data = all_data[-train_inx, cn, with=F]

# create model & train
tree = rpart(logerror ~ ., data = train_data, method="anova", cp=0.0001)

rsq.rpart(tree)


tree.pred = predict(tree, test_data)

err = test_data$logerror - tree.pred
cat("median abs err: ", median(abs(err)), "\n")
cat("mean abs err: ", mean(abs(err)), "\n")
cat("baseline median abs err", median(abs(mean(all_data$logerror)-all_data$logerror)), "\n")
cat("baseline mean abs err", mean(abs(mean(all_data$logerror)-test_data$logerror)), "\n")

cps = seq(0, 0.01, 0.01/100)
mae = numeric(length(cps))
for (i in (1:length(cps))) {
  tree_prune <- prune(tree, cp = cps[i])
  
  tree.pred = predict(tree_prune, test_data)
  err = test_data$logerror - tree.pred
  mae[i] = mean(abs(err^2))
}
plot(cps, mae)
bestcp = cps[which(mae == min(mae))][1]

# measuring the median abs error yields no result:
# the best tree is the one with one node => rpart couldn't find a tree which enhance the result.
# the measuring of the squared mean error yields the cp 0.0013



#################################################
# create submission

# build "best" tree
tree.best = rpart(logerror ~ ., data = all_data[, cn, with=F], method="anova", cp=0.0013)


makePrediction <- function(model, newdata, months, labels, dates) {
  predictions <- newdata[, "parcelid", drop=FALSE]
  for(i in 1:length(months)) {
    cat("month: ", months[i], "\n")
    newdata$month <- months[i]
    newdata$month_factor = factor(newdata$month, levels = levels(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
    predictions[, labels[i]] <- predict(model, newdata = newdata)
  }
  cat("write submission data to disk\n")
  write.csv(x = predictions, file = "submission_simple_tree_v3.csv", 
            quote = FALSE, row.names = FALSE)
  return(predictions)
}


newdata = cbind(clean_prop, imp_prop[,-1])
newdata$logerror_q3 = NULL

newdata$parcelid = newdata$id_parcel

subm = makePrediction(tree.best, newdata = newdata, months = c(10),#, 11, 12, 10, 11, 12), 
                   labels = c("201610", "201611", "201612", "201710", "201711", "201712"), 
                   dates = c("2016-10-15", "2016-11-15", "2016-12-15", "2017-10-15", "2017-11-15", "2017-12-15") )


