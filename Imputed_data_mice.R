library(RCurl)
merged_data <- getURL("https://raw.githubusercontent.com/yimingwym/nycdsa_coh10_team_alpha/master/merged_data.csv")
merged_data <- read.csv(text=merged_data)
merged_data=merged_data[-1]


##### imputation train
library(mice)
mice_mod <- mice(merged_data, method='rf')
mice_output <- mice::complete(mice_mod)

##### imputation
data_sale_mice$airconimp <- mice_output$aircon
data_sale_mice$heatingimp <- mice_output$heating
data_sale_mice$region_zip <- mice_output$region_zip

####write dataset output from mice impute
write.csv(mice_output, file = "~/Desktop/MyData_mice.csv")

dim(merged_data)
dim(mice_output)
#90275 * 54

#check missingness
propmiss <- function(merged_data) {
  m <- sapply(merged_data, function(x) {
    data.frame(
      nmiss=sum(is.na(x)), 
      n=length(x), 
      propmiss=sum(is.na(x))/length(x)
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d[order(d$propmiss), ])
}

#imputation status
a=propmiss(merged_data)
b=propmiss(mice_output)
b$n_imputed=a$nmiss-b$nmiss
b$origin_nmiss=a$nmiss
b$perc_imp=round((b$n_imputed/b$origin_nmiss) *100,2) #percentage of na imputed with MICE
b

#distribution check
hist(merged_data$aircon)
hist(mice_output$aircon)

hist(merged_data$heating)
hist(mice_output$heating)

hist(merged_data$area_total_finished.1)
hist(mice_output$area_total_finished.1)

hist(merged_data$area_total_calc.1)
hist(mice_output$area_total_calc.1)

hist(merged_data$tax_delinquency_year)
hist(mice_output$tax_delinquency_year)

hist(merged_data$tax_property)
hist(mice_output$tax_property)



