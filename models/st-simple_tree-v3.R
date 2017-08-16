library(rpart)

#all_data = cbind(clean, imp[,-1])
#all_data$logerror_q3 = NULL

col_names = colnames(all_data)
rm_names = c("id_parcel", "fips_blockid")

cn = col_names[(!(col_names %in% rm_names))]


# train / test data split
set.seed(2344)
train_inx = sample(1:nrow(all_data), nrow(all_data)*0.7)
train_data = all_data[train_inx, cn, with=F]
test_data = all_data[-train_inx, cn, with=F]

# create model & train
tree.1 = rpart(logerror ~ ., data = train_data, method="anova", cp=0.002)

tree.pred = predict(tree.1, test_data)

err = test_data$logerror - tree.pred
cat("median abs err: ", median(abs(err)), "\n")
cat("mean abs err: ", mean(abs(err)), "\n")
cat("baseline median abs err", median(abs(mean(all_data$logerror)-all_data$logerror)), "\n")
cat("baseline mean abs err", mean(abs(mean(all_data$logerror)-all_data$logerror)), "\n")


summary(tree.1)
