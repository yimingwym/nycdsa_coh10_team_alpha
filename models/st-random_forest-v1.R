
library(randomForest)

all_data = cbind(clean, imp[,-1])
all_data$logerror_q3 = NULL

col_names = colnames(all_data)
rm_names = c("id_parcel", "fips_blockid")

cn = col_names[(!(col_names %in% rm_names))]

# train / test data split
set.seed(2344)
train_inx = sample(1:nrow(all_data), nrow(all_data)*0.8)
train_data = all_data[train_inx, cn, with=F]
test_data = all_data[-train_inx, cn, with=F]

set.seed(100)
Sys.time()
tree.rand = randomForest(logerror ~ ., data = train_data, importance = TRUE)
Sys.time()


oob.err = numeric(15)
fits = vector("list",15)
for (mtry in 13:13) {
  fits[[mtry]] = randomForest(logerror ~ ., data = train_data, mtry = mtry, maxNodes=2, ntree=500)
  #oob.err[mtry] = fit$mse[500] # the last mse gives the error of the whole model; running mean before
  cat("We're performing iteration", mtry, "\n")
}









