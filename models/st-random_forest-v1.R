
# load(file="clean.dat")
# load(file="imp.dat")
# load(file="clean_prop.dat")
# load(file="imp_prop.dat")

library(randomForest)
library(data.table)
library(dplyr)

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

saveRDS(tree.rand, file = "randForest_v1_initTree.dat")
tree.rand = readRDS(file = "randForest_v1_initTree.dat")






