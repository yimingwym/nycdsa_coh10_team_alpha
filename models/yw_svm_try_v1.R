# I only tested 1000*0.7 rows in here. took me around 10 min.
# R squre = 0.08 << 1, median abs error = 0.036
# I'll re-run this code with modified full training set & Gaussian kernel.

library(e1071)

raw_data <- imp
raw_data$logerror <- clean$logerror

train_idx <- sample(1 : nrow(raw_data), 0.7 * nrow(raw_data))
train_data <- raw_data[train_idx, ]
test_data <- raw_data[-train_idx, ]

tuneResult <- tune(svm, logerror ~ .-id_parcel,  data = train_data,
                   kernel = 'radial',
                   ranges = list(cost = 10^(seq(-1, 1.5, length = 10)),
                                 gamma = 10^(seq(-2, 1, length = 10))))
print(tuneResult)
# Draw the tuning graph
plot(tuneResult)

tuned_mdl <- tuneResult$best.model
ypred <- predict(tuned_mdl, train_data)
error <- ypred - train_data$logerror
median(abs(error))
# result before with default kernel, 700 observations: [1] 0.03617779

res <- sum(error^2)
tot <- sum((train_data$logerror - mean(train_data$logerror))^2)
rsqr <- 1 - res / tot
rsqr
##### result before with default kernel, 700 observations: 
#     R^2 = 0.08, which means shit!
##### possible causes: limited observations in training set (only 700)
#                      need to change kernel to radial (gaussian kernel)
#                      other
