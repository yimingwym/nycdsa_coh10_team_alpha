################################################################################
#    MODEL          LASSO REGRESSION 
#    DATA           MyData_mice.csv
################################################################################
library(RCurl)
imp <- getURL("https://raw.githubusercontent.com/yimingwym/nycdsa_coh10_team_alpha/master/MyData_mice.csv")
imp <- read.csv(text=imp)
imp=imp[-1]
summary(imp)

#replace remaining na with 0
imp$aircon=ifelse(is.na(imp$aircon),0,imp$aircon) #type of aircon is 0
imp$heating=ifelse(is.na(imp$heating),0,imp$heating) #type of heating is 0
imp$area_total_finished.1=ifelse(is.na(imp$area_total_finished.1),0,imp$area_total_finished.1)
imp$area_total_calc.1=ifelse(is.na(imp$area_total_calc.1),0,imp$area_total_calc.1)
imp$flag_tub=ifelse(is.na(imp$flag_tub),0,imp$flag_tub)
imp$flag_fireplace=ifelse(is.na(imp$flag_fireplace),0,imp$flag_fireplace)
imp$story=ifelse(is.na(imp$story),0,imp$story)

#fator columns
imp$flag_fireplace=as.factor(imp$flag_fireplace)
imp$flag_tub=as.factor(imp$flag_tub)
imp$tax_delinquency=as.factor(imp$tax_delinquency)

# #rename
# colnames(imp)[c(30,31,34)]=c("id_parcel_1","area_total_calc_1","area_total_finished_1")
# colnames(imp)[48]="id_parcel_2"

#drop tag column
imp=imp[-c(9,10,11,12)]

#1
#Creating the data matrices for the glmnet() function.
x = model.matrix(logerror ~ ., imp)[, -1]
y = imp$logerror

#Creating training and test sets with an 70-30 split, respectively.
library(ISLR)
library(Matrix)
set.seed(0)
train = sample(1:nrow(x), 7*nrow(x)/10)
test = (-train)
y.test = y[test]

#Values of lambda over which to check.
grid = 10^seq(5, -5, length = 100)

#Fitting the lasso regression
lasso.models = glmnet(x[train, ], y[train], alpha = 1, lambda = grid,standardize=TRUE)

plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

#cvalidate for best lambda
set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10, lambda = grid)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)

#The error seems to be reduced with a log lambda value of around -8.489329; this
#corresponts to a lambda value of about 0.0002056512.

lasso.bestlambdatrain = predict(lasso.models, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2)
#The test MSE is about 0.02758232.


lasso.bestlambda = predict(lasso.out, s = bestlambda.lasso, newx = x)
mean((lasso.bestlambda - y)^2)

#The overall MSE is about 0.02578685. but a little bit lower because of the way in which the model was
#fit using the data at hand.

#predict(ridge.out, type = "coefficients", s = bestlambda.ridge)
predict(lasso.out, type = "coefficients", s = bestlambda.lasso)
#mean((ridge.bestlambda - y)^2)
mean((lasso.bestlambda - y)^2)

#Both models appear to perform in a similar manner. Both the test MSEs and the
#overall MSEs were approximately the same. Ultimately, the ridge regression MSE
#was slightly lower than that of the lasso. Although this might be due to random
#variability among our dataset, if we are strictly talking about predictive
#power, we might choose to move forth with the ridge regression in this scenario.
#On the other hand, the final lasso regression model "kicked out" the lcp
#variable altogether (setting its coefficient value to 0). If we are particularly
#interested in dimension reduction and variable selection, we might choose to
#move forth with the lasso regression.