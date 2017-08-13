##### I found I might made a mistake on the fips_blockid, here's the updated version on create new fips_blockid column
##### The information is extracted from rawcensustractandblock, you can run the code on your computer.
##### After the extraction, the rawcensustractandblock column is removed because we don't need to use it any more.

library(dplyr)
##############
merged_data <- read.csv('merged_data.csv')
merged_data$X <- NULL
length(unique(merged_data$rawcensustractandblock))
length(unique(merged_data$fips_blockid))
##### Now, extract the blockid from rawcensustractandblock
##### For example: 60371066.461001: '6037'(County id or so called 'fips')
#####                               '1066.46' (block id, 4 digits before and 2 digits after dot)
#####                               omit the rest digits
merged_data$rawcensustractandblock <- as.character(merged_data$rawcensustractandblock)
merged_data$fips_blockid <- sapply(merged_data$rawcensustractandblock, function(x) substr(x, 1, 11))
merged_data$rawcensustractandblock <- NULL