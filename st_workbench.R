library(rpart)
library(randomForest)
library(stats)
library(flexclust)

all_data = cbind(clean, imp[,-1])

#col_names = colnames(imp)

all_data$num_story = as.numeric(all_data$num_story)

col_names= c(
  "tax_total",
  "tax_building",
  "tax_land",
  "tax_property",
  "tax_delinquency",
  "tax_delinquency_year"
)


rm_names = c("id_parcel")

cn = col_names[(!(col_names %in% rm_names))]
set.seed(123)
clust_data = all_data[,cn, with=F]
perfs = numeric()

clust_models = vector("list", 100)
for (k in 2:50){
  km_model = kcca(clust_data, k=k, kccaFamily("kmeans"))
  clust_models[[k]] = km_model
  clust_pred =  predict(km_model, clust_data)
  h.df = data.frame(logerror=all_data$logerror, is.na_clust = as.factor(clust_pred))
  tree = rpart(logerror ~ is.na_clust, data=h.df, method="anova", cp=0.001)
  cat(k, ": ", tree$variable.importance, " nodes:", nrow(tree$frame) ,"\n")
  
  #cat("k=", k, " perf=", perf, "\n")
}

clust_pred =  predict(clust_models[[49]], clust_data)
h.df = data.frame(logerror=all_data$logerror, is.na_clust = as.factor(clust_pred))
tree = rpart(logerror ~ is.na_clust, data=h.df, method="anova", cp=0.01)
p = predict(tree, h.df)
mean(abs(h.df$logerror-p))
median(abs(h.df$logerror-p))

nrow(tree$frame)

err_mean = numeric(100)
err_median = numeric(100)
for (k in 33:50){
  km_model = clust_models[[k]]
  clust_pred =  predict(km_model, clust_data)
  h.df = data.frame(logerror=all_data$logerror, is.na_clust = as.factor(clust_pred))
  tree = rpart(logerror ~ is.na_clust, data=h.df, method="anova", cp=0.001)
  cat(k, ": ", tree$variable.importance, " nodes:", nrow(tree$frame) ,"\n")

  p = predict(tree, h.df)
  err_mean[k] = mean(abs(h.df$logerror-p))
  err_median[k] = median(abs(h.df$logerror-p))
  
    
  #cat("k=", k, " perf=", perf, "\n")
}




##############################################################
cl1 = kcca(clust_data, k=11, kccaFamily("kmeans"))


clust_pred =  predict(cl1, clust_data)

h.df = data.frame(logerror=clean$logerror, is.na_clust = clust_pred)
tree = rpart(logerror ~ is.na_clust, data=h.df, method="anova", cp=0.001)
tree



##########################################################

maxLong = max(all_data$longitude)
minLong = min(all_data$longitude)
maxLat = max(all_data$latitude)
minLat = min(all_data$latitude)

