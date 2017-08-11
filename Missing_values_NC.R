#architecturalstyletypeid - remove 

properties <- select(properties, -architectural_style)

#quality - missing values relate to newer houses. In addition, newer houses have also lower mean scores - impute random

properties %>% mutate(., as.factor(properties$quality))
plyr::count(is.na(properties$quality))

#area_total_finished - impute based on area_total_calc

properties = properties %>% mutate(area_total_finished = ifelse(is.na(area_total_finished),area_total_calc, area_total_finished))

#num_bathroom_calc - impute based on num_bathroom (already equal, only not for the NA's)

properties %>% mutate(num_bathroom_calc= ifelse(is.na(num_bathroom_calc), num_bathroom, num_bathroom_calc))


#area_total_calc -remove outliers 

plyr::count(is.na(properties$area_total_calc))

#finishedsquarefeet13 - remove, only 33 TRUE and high multicollinearity
properties <- select(properties, -area_liveperi_finished)

summary(properties2$area_total_finished)

#fips - no missingness

#num_fireplace - impute with 0 (mnar)

properties %>% mutate(num_fireplace= ifelse(is.na(num_fireplace), 0, num_fireplace))

#flag_tub - convert to 1&0, input 1 if true, zero if else


properties$flag_tub = ifelse(is.na(properties$flag_tub),0,1)


#pooltypeid10 - spa or hottub? impute with 0 


properties$pooltypeid10 = ifelse(is.na(properties$pooltypeid10),0,1)


#rawcensustractandblock - no missingness

#story - remove, however does imply having a basement 

properties <- select(properties, -story)

#threequarterbathnbr - impute at 0.every of these houses has a bathroom, hence it is expected that these are indeed 0 


#unitcnt - impute based on number of stories or mode. 
plyr::count(is.na(properties$unitcnt))

properties %>% group_by(., unitcnt, numberofstories) %>% summarise(., total=n()) %>% filter(., is.na(unitcnt))

#yearbuilt - remove outliers, also have 0 rooms and 0 batrooms, outliers
plyr::count(is.na(properties$yearbuilt))

properties %>% dplyr::filter(., is.na(properties$yearbuilt)) 
properties_yearbuilt %>% group_by(., roomcnt) %>% summarise(., total=n()) 

#fireplaceflag - remove, has no extra info over fireplacecnt
properties <- select(properties, -story)


