# preliminary xgb model done 

library(dplyr)
library(xgboost)

xgb_sales <- sales

ctypes <- sapply(xgb_sales, class)
cidx <- which(ctypes %in% c("character", "factor"))

for(i in cidx){
  xgb_sales[[i]] <- as.integer(factor(xgb_sales[[i]]))
}


for_xgboost_train <- xgb_sales %>% select(-id_parcel, -date)

for_xgboost_train <- for_xgboost_train[1:80000, ]

for_xgboost_test <- xgb_sales %>% select(-id_parcel, -date, -logerror)

for_xgboost_test <- for_xgboost_test[80001 : 90275, ]

dtrain <- xgb.DMatrix(data.matrix(for_xgboost_train %>% select(-logerror)), 
                      label=for_xgboost_train$logerror, 
                      missing=NA)

dval <- xgb.DMatrix(data.matrix(for_xgboost_test), 
                    missing=NA)

param <- list(  objective           = "reg:linear",
                booster             = "gbtree",
                eval_metric         = "mae",
                eta                 = 0.1, 
                max_depth           = 8, 
                subsample           = 0.5,
                colsample_bytree    = 0.5,
                min_child_weight    = 4,
                maximize            = FALSE
)

bstSparse <- xgb.train(param,
                       data = dtrain, 
                       nrounds               = 1000, 
                       verbose               = 1,
                       print_every_n         = 10L
                       )

pred <- predict(bstSparse, dval)

ori <- sales$logerror[80001 : 90275]
error <- pred - ori
res <- sum(error^2)
tot <- sum((ori - mean(ori))^2)
rsqr <- 1 - res / tot
rsqr
#### test error R^2: -1.96


########################
# try clean
########################


xgb_sales <- clean

ctypes <- sapply(xgb_sales, class)
cidx <- which(ctypes %in% c("character", "factor"))

for(i in cidx){
  xgb_sales[[i]] <- as.integer(factor(xgb_sales[[i]]))
}


for_xgboost_train <- xgb_sales %>% select(-id_parcel, -date)

for_xgboost_train <- for_xgboost_train[1:80000, ]

for_xgboost_test <- xgb_sales %>% select(-id_parcel, -date, -logerror)

for_xgboost_test <- for_xgboost_test[80001 : 90275, ]

dtrain <- xgb.DMatrix(data.matrix(for_xgboost_train %>% select(-logerror)), 
                      label=for_xgboost_train$logerror, 
                      missing=NA)

dval <- xgb.DMatrix(data.matrix(for_xgboost_test), 
                    missing=NA)

param <- list(  objective           = "reg:linear",
                booster             = "gbtree",
                eval_metric         = "mae",
                eta                 = 0.1, 
                max_depth           = 8, 
                subsample           = 0.5,
                colsample_bytree    = 0.5,
                min_child_weight    = 4,
                maximize            = FALSE
)

bstSparse <- xgb.train(param,
                       data = dtrain, 
                       nrounds               = 1000, 
                       verbose               = 1,
                       print_every_n         = 10L
)

pred <- predict(bstSparse, dval)

ori <- xgb_sales$logerror[80001 : 90275]
error <- pred - ori
res <- sum(error^2)
tot <- sum((ori - mean(ori))^2)
rsqr <- 1 - res / tot
rsqr
#test error R^2: 0.16


###############
# imp
###############


imp$date <- sales$date
imp$logerror <- sales$logerror

xgb_sales <- imp

ctypes <- sapply(xgb_sales, class)
cidx <- which(ctypes %in% c("character", "factor"))

for(i in cidx){
  xgb_sales[[i]] <- as.integer(factor(xgb_sales[[i]]))
}


for_xgboost_train <- xgb_sales %>% select(-id_parcel, -date)

for_xgboost_train <- for_xgboost_train[1:80000, ]

for_xgboost_test <- xgb_sales %>% select(-id_parcel, -date, -logerror)

for_xgboost_test <- for_xgboost_test[80001 : 90275, ]

dtrain <- xgb.DMatrix(data.matrix(for_xgboost_train %>% select(-logerror)), 
                      label=for_xgboost_train$logerror, 
                      missing=NA)

dval <- xgb.DMatrix(data.matrix(for_xgboost_test), 
                    missing=NA)

param <- list(  objective           = "reg:linear",
                booster             = "gbtree",
                eval_metric         = "mae",
                eta                 = 0.1, 
                max_depth           = 8, 
                subsample           = 0.5,
                colsample_bytree    = 0.5,
                min_child_weight    = 4,
                maximize            = FALSE
)

bstSparse <- xgb.train(param,
                       data = dtrain, 
                       nrounds               = 1000, 
                       verbose               = 1,
                       print_every_n         = 10L
)

pred <- predict(bstSparse, dval)

ori <- xgb_sales$logerror[80001 : 90275]
error <- pred - ori
res <- sum(error^2)
tot <- sum((ori - mean(ori))^2)
rsqr <- 1 - res / tot
rsqr

# test error R^2: -1.708695

# preliminary model is done, next: tune the model and try on different data