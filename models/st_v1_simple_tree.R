library(tree)

all_data = clean %>% inner_join(imp, by="id_parcel")

cn = c(colnames(imp), "logerror")

# train / test data split
set.seed(2344)
train_inx = sample(1:nrow(all_data), nrow(all_data)*0.7)
train_data = all_data[train_inx, cn]
test_data = all_data[-train_inx, cn]

# create model & train
tree.1 = tree(logerror ~ .-id_parcel, data = train_data, split='gini')

tree.pred = predict(tree.1, test_data)

err = test_data$logerror - tree.pred
median(abs(err))
