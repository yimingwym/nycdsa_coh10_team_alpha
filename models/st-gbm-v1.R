

# setwd("C:/Projects/NYCDS/Projects/030 Machine Learning/data")
# load(file="clean.dat")
# load(file="imp.dat")
# load(file="clean_prop.dat")
# load(file="imp_prop.dat")


library(gbm)
library(data.table)

all_data = cbind(clean, imp[,-1])
all_data$logerror_q3 = NULL

# convert logical to factor
log_cols = colnames(all_data)[sapply(all_data, class) == "logical"]
for (log_col in log_cols) {
  all_data[[log_col]] = as.factor(all_data[[log_col]])
}

col_names = colnames(all_data)
rm_names = c("id_parcel", "fips_blockid", "date")

cn = col_names[(!(col_names %in% rm_names))]

# train / test data split
set.seed(2344)
train_inx = sample(1:nrow(all_data), nrow(all_data)*0.7)
train_data = all_data[train_inx, cn, with=F]
test_data = all_data[-train_inx, cn, with=F]
#laplace: 0.03167543
#adaboost
#gaussian: 0.03489639

NUM_TREES = 500
INTERACTION_DEPTH = 10

fit.gbm = gbm(logerror ~ ., 
    data=train_data, 
    distribution = "laplace", 
    n.trees = NUM_TREES, 
    shrinkage = 0.01,
    interaction.depth = INTERACTION_DEPTH, 
    n.minobsinnode = 500,
    verbose=T)

ntree = c(seq(5, NUM_TREES, 1), NUM_TREES)
err = numeric(length(ntree))

for(i in (1:length(ntree))) {
  pred = predict(fit.gbm, test_data, ntree[i])
  err[i] = median(abs(test_data$logerror - pred))
}
plot(ntree, err)

bestTreeNr = ntree[which(err == min(err))[1]]

pred = predict(fit.gbm, test_data, bestTreeNr)

err = test_data$logerror - pred
cat("median abs err: ", median(abs(err)), "\n")
# cat("mean abs err: ", mean(abs(err)), "\n")
# cat("mean square err: ", mean(err^2), "\n")
cat("baseline median abs err", median(abs(mean(all_data$logerror)-test_data$logerror)), "\n")
# cat("baseline mean abs err", mean(abs(mean(all_data$logerror)-test_data$logerror)), "\n")
# cat("baseline mean square err: ", mean((mean(all_data$logerror)-test_data$logerror)^2), "\n")

# median abs err on test: 0.03161633

####################################################################################################
#
# create submit


newdata = cbind(clean_prop, imp_prop[,-1])
newdata$logerror_q3 = NULL

newdata$parcelid = newdata$id_parcel


months = c(10, 11, 12, 10, 11, 12)
labels = c("201610", "201611", "201612", "201710", "201711", "201712")

predictions <- newdata[, "parcelid", drop=FALSE]
for(i in 1:length(months)) {
  cat("month: ", months[i], "\n")
  newdata$month <- months[i]
  newdata$month_factor = factor(newdata$month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
  #newdata$month_factor = as.factor(newdata$month)
  predictions[, labels[i]] <- predict(fit.gbm, newdata, bestTreeNr) 
}

write.csv(x = predictions, file = "submission_gbm_v1.csv", 
          quote = FALSE, row.names = FALSE)






