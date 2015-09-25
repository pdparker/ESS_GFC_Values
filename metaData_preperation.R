##### setup ####
##Libraries
lib <- c("tidyr", "dplyr")
lapply(lib, library, character.only = TRUE)
#Find directory path and set it as working. Use bash calls to find location of directory
#if on windows you are on your own but maybe "dir \ess \s" might work
if(Sys.info()[1] != "Darwin"){message("Only tested on Mac. Should also work on all unix systems also")}
#Dynamically locate directory
path <- system("locate /ess | grep /ess$",intern = TRUE)
setwd(path)
#Load the data
load("multilevelData/metaData25092015.rda")
metaData <- tmp[,c(2,10:36)]
rm(tmp)
##### Reorientation of Data #####
wideArregrate <- metaData %>% group_by(cntry) %>%  summarise_each(funs(mean)) 
unEmployment <- gather(wideArregrate[,c(1, 23:28)], "year", "unemployment", c_unraall_2002:c_unraall_2012)
unEmployment$year <- gsub("(.+)([0-9]{4})", "\\2", unEmployment$year)
save(unEmployment,file = "multilevelData/unEmployment_COUNTRY.rda")