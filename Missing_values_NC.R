
#architecturalstyletypeid - convert to factor add level for NA

properties[,c('architectural_style')] <- lapply(properties[,c('architectural_style')], factor, exclude=NULL)


#quality - convert to factor add level for NA

properties[,c('quality')] <- lapply(properties[,c('quality')], factor, exclude=NULL)


#area_total_finished - impute based on area_total_calc, impute missingness with package

properties = properties %>% mutate(area_total_finished = ifelse(is.na(area_total_finished),area_total_calc, area_total_finished))

#num_bathroom_calc/num_75_bath/num_bath/num_bathroom - impute based on num_bathroom (already equal, only not for the NA's)
#convert to int

properties[,c('num_bathroom_calc')] <- lapply(properties[,c('num_bathroom_calc')], as.integer)
properties[,c('num_bathroom')] <- lapply(properties[,c('num_bathroom')], as.integer)


properties = properties %>% mutate(num_bathroom_calc= ifelse(is.na(num_bathroom_calc), num_bathroom, num_bathroom_calc))
properties = properties %>% mutate(num_75_bath= ifelse(is.na(num_75_bath), 0, num_75_bath))
properties = properties %>% mutate(num_bath= ifelse(is.na(num_bath), num_bathroom, num_bath))


#area_total_calc -impute at random OR DELETE outliers

#finishedsquarefeet13 - remove, only 33 TRUE and high multicollinearity - DROP

properties <- select(properties, -area_liveperi_finished)

#fips - no missingness

#num_fireplace - impute with 0 (mnar)

properties = properties %>% mutate(num_fireplace= ifelse(is.na(num_fireplace), 0, num_fireplace))

#flag_tub - convert to 1&0, input 1 if true, zero if else

properties = properties %>% mutate(., flag_tub = ifelse(flag_tub=="true",1,0))

properties[,c('flag_tub')] <- lapply(properties[,c('flag_tub')], factor, exclude=NULL)


#pooltypeid10 - spa or hottub? impute with 0 

properties$pooltypeid10 = ifelse(is.na(properties$pooltypeid10),0,1)

properties[,c('pooltypeid10')] <- lapply(properties[,c('pooltypeid10')], factor, exclude=NULL)


#rawcensustractandblock - no missingness

#story - remove, however does imply having a basement - delete

properties <- select(properties, -story)

#threequarterbathnbr - impute at 0.every of these houses has a bathroom, hence it is expected that these are indeed 0 


#unitcnt - convert to factor and level for na

properties[,c('unitcnt')] <- lapply(properties[,c('unitcnt')], factor, exclude=NULL)

levels(properties$unitcnt)

#yearbuilt - impute at random
 

#flag_fireplace - remove, has no extra info over fireplacecnt
properties <- select(properties, -flag_fireplace)


