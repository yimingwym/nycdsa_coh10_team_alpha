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

#1
#Creating the data matrices for the glmnet() function.
x = model.matrix(logerror ~ ., imp)[, -1]
y = imp$logerror

#Creating training and test sets with an 80-20 split, respectively.
set.seed(0)
train = sample(1:nrow(x), 8*nrow(x)/10)
test = (-train)
y.test = y[test]

#Values of lambda over which to check.
grid = 10^seq(5, -2, length = 100)

#Fitting the lasso regression. Alpha = 1 for lasso regression.
lasso.models = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)

plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

#The coefficients all seem to shrink towards 0 as lambda gets quite large. All
#coefficients seem to go to 0 once the log lambda value gets to about 0. We
#note that in the lasso regression scenario, coefficients are necessarily set
#to exactly 0.

set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10, lambda = grid)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)

#The error seems to be reduced with a log lambda value of around -3.3027; this
#corresponts to a lambda value of about 0.038. This is the value of lambda
#we should move forward with in our future analyses.

lasso.bestlambdatrain = predict(lasso.models, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2)

#The test MSE is about 0.506.

lasso.out = glmnet(x, y, alpha = 1)
predict(lasso.out, type = "coefficients", s = bestlambda.lasso)

#The coefficients have been shrunken towards 0; most notably, the lcp variable
#has dropped out first and has a coefficient of exactly 0. Other variables like
#gleason, pgg45, and age have pretty small coefficient values as well. Similar
#to the ridge regression scenario, the svi, lweight, and lcavol all have the
#largest coefficient estimates.

lasso.bestlambda = predict(lasso.out, s = bestlambda.lasso, newx = x)
mean((lasso.bestlambda - y)^2)

#The overall MSE is about 0.45996. Again, this is similar to the test MSE we
#found above, but a little bit lower because of the way in which the model was
#fit using the data at hand.

predict(ridge.out, type = "coefficients", s = bestlambda.ridge)
predict(lasso.out, type = "coefficients", s = bestlambda.lasso)
mean((ridge.bestlambda - y)^2)
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