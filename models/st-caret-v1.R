# setwd("C:/Projects/NYCDS/Projects/030 Machine Learning/data")
# load(file="clean.dat")
# load(file="imp.dat")
# load(file="clean_prop.dat")
# load(file="imp_prop.dat")


library(caret)
library(data.table)

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



# Machine Learning
## Data splitting based on the outcome
set.seed(123)
trainIndex <- createDataPartition(train_data$logerror, 
                                  p = .75, 
                                  list = FALSE, 
                                  times = 1)
## training set
subTrain <- train_data[ trainIndex,]
## testing set
subTest  <- train_data[-trainIndex,]

## define metric - MAE
maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "DENFIS"
  mae_score
}

## cross validation
## 1. random hyperparameter
rdmSearch <- trainControl(method = "cv",
                          number = 3,
                          summaryFunction = maeSummary,
                          search = "random")

gbmFit <- train(logerror ~ .,
                data = subTrain, 
                method = "gbm", 
                #preProcess = c("center", "scale"),
                metric = "MAE",
                maximize = FALSE,
                tuneLength = 1,
                trControl = rdmSearch,
                verbose = TRUE)
plot(gbmFit)
gbmFit$bestTune
## 2. grid search
gridSearch <- trainControl(method = "cv",
                           number = 3,
                           summaryFunction = maeSummary)

gbmGrid <-  expand.grid(interaction.depth = 3, 
                        n.trees = c(100), 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

gbmFit2 <- train(logerror ~ .,
                 data = subTrain, 
                 method = "bstTree", 
                 #preProcess = c("center", "scale"),
                 metric = "MAE",
                 maximize = FALSE,
                 tuneGrid = gbmGrid,
                 trControl = gridSearch,
                 verbose = TRUE)
gbmFit2
## parameters
# plot(gbmFit2)
gbmImp <- varImp(gbmFit2, scale = FALSE)
plot(gbmImp, top = 20)

# Prediction
predict(gbmFit2, newdata = subTrain)
