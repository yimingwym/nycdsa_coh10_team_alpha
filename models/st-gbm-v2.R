

# setwd("C:/Projects/NYCDS/Projects/030 Machine Learning/data")
# load(file="gbm_clean.dat")
# load(file="gbm_imp.dat")
# load(file="gbm_clean_prop.dat")
# load(file="gbm_imp_prop.dat")


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


NUM_TREES = 350
INTERACTION_DEPTH = 20

fit.gbm = gbm(logerror ~ ., 
    data=train_data, 
    distribution = "laplace", 
    n.trees = NUM_TREES, 
    shrinkage = 0.01,
    interaction.depth = INTERACTION_DEPTH, 
    n.minobsinnode = 100,
    #cv.folds = 10,
    n.cores = 7,
    verbose=T)

# laplace: 0.03211207

#n.new.trees
#nTrain
#cv.folds

# Currently available options are "gaussian" (squared error), 
# "laplace" (absolute loss), "tdist" (t-distribution loss), 
# "bernoulli" (logistic regression for 0-1 outcomes), 
# "huberized" (huberized hinge loss for 0-1 outcomes), 
# "multinomial" (classification when there are more than 2 classes), 
# "adaboost" (the AdaBoost exponential loss for 0-1 outcomes), 
# "poisson" (count outcomes), "coxph" (right censored observations), 
# "quantile", or "pairwise" (ranking measure using the LambdaMart algorithm).


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

#tree depth 20: mae = 0.0315192
#tree depth 17: mae = 0.03156282
#tree depth 13: mae = 0.03158281


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

write.csv(x = predictions, file = "submission_gbm_v2.csv", 
          quote = FALSE, row.names = FALSE)






