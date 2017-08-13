
library(tree)
library(randomForest)

all_data = clean %>% inner_join(imp, by="id_parcel")
set.seed(234)
train_inx = sample(1:nrow(all_data), nrow(all_data)/140)

cn = colnames(all_data)
cn=cn[1:3]

train_data = all_data[1:10,] 
train_data$date = as.numeric(train_data$date)

tree.1 = tree(logerror ~ .-id_parcel, split = "gini", data = train_data)
summary(tree.carseats)

#The output shows the variables actually used within the tree, the number of
#terminal nodes, the residual mean deviance based on the Gini index, and
#the misclassification error rate.

#Plotting the classification tree.
plot(tree.carseats)
text(tree.carseats, pretty = 0) #Yields category names instead of dummy 


set.seed(0)
rf.boston = randomForest(logerror ~ .-id_parcel, data = train_data, importance = TRUE)
rf.boston

