library(rpart)

all_data = cbind(clean, imp[,-1])

col_names = colnames(all_data)
rm_names = c("id_parcel", "fips_blockid", "logerror")

cn = col_names[(!(col_names %in% rm_names))]


# train / test data split
set.seed(2344)
train_inx = sample(1:nrow(all_data), nrow(all_data)*0.7)
train_data = all_data[train_inx, cn, with=F]
test_data = all_data[-train_inx, cn, with=F]

# create model & train
tree.1 = rpart(logerror_q3 ~ ., data = train_data, method="class", cp=0.001)

tree.pred = predict(tree.1, test_data)

t = table(data.frame(real=test_data$logerror_q3, prediction=quartil_pred))
t

acc = (t[1,1]+t[2,2]+t[3,3]) / sum(t)
cat("accuracy: ", acc, "\n")

under_estimating= t[2,1] + t[3,1] + t[3,2]
over_estimating = t[1,2] + t[1,3] + t[2,3]

# q=quantile(clean$logerror, c(1/3, 2/3))
# q1_mean = mean(all_data[all_data$logerror<=q[1]]$logerror)
# q2_mean = mean(all_data$logerror)
# q3_mean = mean(all_data[all_data$logerror>q[2]]$logerror)

# logerror_mean = mean(all_data$logerror)
# 
# # correction factor to balance over and under estimating 
# delta = (under_estimating * logerror_mean - over_estimating * logerror_mean) / (logerror_mean+under_estimating)
# 
# q2_mean = logerror_mean
# q1_mean = q2_mean-delta
# q3_mean = q2_mean+delta
# 
# quartil_pred = apply(tree.pred, 1, function(r) which(r==max(r)))
# logerror_estimate = ifelse(quartil_pred==1, q1_mean,
#                            ifelse(quartil_pred==2, q2_mean, q3_mean))
# 
# err = all_data[-train_inx,,]$logerror - logerror_estimate
# median(abs(err))
# 
# median(abs(logerror_mean-all_data$logerror))
# mean(abs(logerror_mean-all_data$logerror))

summary(tree.1)
