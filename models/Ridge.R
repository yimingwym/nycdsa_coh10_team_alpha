*****RIDGE_1******

#Applying ridge leaving out factors (as these create sparsity in the data as well as hard to interpret)

summary(clean)
unique(clean$tract)
library(ISLR)
library(glmnet)

as.factor(clean$fips)

x = model.matrix(logerror ~ . -id_parcel -material -deck -quality -framing -story -heating -aircon -architectural_style -censustractandblock -fips_blockid -census -tract_block -census - tract_block -tract -latitude2 - longitude2, clean)[, -1] #Dropping the intercept column.
y = clean$logerror

set.seed(0)
train = sample(1:nrow(x), 7*nrow(x)/10)
test = (-train)
y.test = y[test]

length(train)/nrow(x)
length(y.test)/nrow(x)

grid = 10^seq(5, -2, length = 100)

ridge.models.train = glmnet(x[train, ], y[train], alpha = 0, lambda = grid)

set.seed(0)
cv.ridge.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 0, nfolds = 10)
plot(cv.ridge.out, main = "Ridge Regression\n")
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge
log(bestlambda.ridge)

ridge.bestlambdatrain = predict(ridge.models.train, s = bestlambda.ridge, newx = x[test, ])
mean((ridge.bestlambdatrain - y.test)^2)
median(abs(ridge.bestlambdatrain - y.test))


# Sum of Squares Total and Error
sst <- sum(y.test^2)
sse <- sum((ridge.bestlambdatrain - y.test)^2)

# R squared
rsq <- 1 - sse / sst
rsq
#> [1] 0.008

*****GROUP_LASSO*****
#Another solution as opposed to leaving them out is grouping (Applying Group Lasso). 
  #Group Lasso does not consider every individual dummy variable but rather determines whether certain variables should be left out as a whole

set.seed(0)
train = sample(1:nrow(x), 7*nrow(x)/10)
test = (-train)
y.test = y[test]


grid = 10^seq(5, -5, length = 100)

lasso.models = glmnet(x[train, ], y[train], alpha = 1, lambda = grid,standardize=TRUE, type.multinomial = c("grouped"))

plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10, lambda = grid)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)

2.

lasso.bestlambdatrain = predict(lasso.models, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2)

lasso.bestlambda = predict(cv.lasso.out, s = bestlambda.lasso, newx = x)
mean((lasso.bestlambda - y)^2)


predict(cv.lasso.out, type = "coefficients", s = bestlambda.lasso)

mean((lasso.bestlambda - y)^2)

# Sum of Squares Total and Error
sst <- sum(y^2)
sse <- sum((lasso.bestlambda - y)^2)

# R squared
rsq <- 1 - sse / sst
rsq
#> [1] 0.0119


