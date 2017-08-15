library(rpart)

all_data = clean %>% inner_join(imp, by="id_parcel")

col_names = colnames(all_data)
rm_names = c("id_parcel", "fips_blockid")

cn = col_names[(!(col_names %in% rm_names))]


# train / test data split
set.seed(2344)
train_inx = sample(1:nrow(all_data), nrow(all_data)*0.7)
train_data = all_data[train_inx, cn]
test_data = all_data[-train_inx, cn]

# create model & train
tree.1 = rpart(logerror ~ ., data = train_data, method="anova")

tree.pred = predict(tree.1, test_data)

err = test_data$logerror - tree.pred
median(abs(err))

summary(tree.1)
