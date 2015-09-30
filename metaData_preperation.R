##### setup ####
##Libraries
lib <- c("tidyr", "dplyr", "plyr")
lapply(lib, library, character.only = TRUE)
#Find directory path and set it as working. Use bash calls to find location of directory
#if on windows you are on your own but maybe "dir \ess \s" might work
if(Sys.info()[1] != "Darwin"){message("Only tested on Mac. Should work on all unix systems also")}
#Dynamically locate directory
path <- system("locate /ess | grep /ess$",intern = TRUE)
setwd(path)
#Load the data
load("multilevelData/metaData_CountryLevel30092015.rda")
extraction <- grep("c_(gini|soexgni|unraall)_20(02|04|06|08|10|12)", names(tmp))
metaData <- tmp[,c(1,extraction)]
#rm(tmp)
##### Reorientation of Data #####
longMetaData <- metaData %>% 
	gather(key, value, -cntry) %>%
	extract(key, c("question", "year"), "(c_[a-z]+)_([0-9]+)") %>%
	spread(question, value)

##### Add Replacement rates ####
load("multilevelData/replacement30092015.rda")
replacement$cntry <- factor(replacement$cntry)
longMetaData$year <- as.numeric(longMetaData$year)
longMetaData$cntry <- gsub("[[:blank:]]","", longMetaData$cntry)

longMetaData <- merge(longMetaData, replacement, by = c("year", "cntry"), all.x = TRUE, all.y = FALSE)
names(replacement)

save(longMetaData, file = "longMetaData_COUNTRY.rda")

