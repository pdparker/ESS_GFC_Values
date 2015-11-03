####setup####
#Find directory path and set it as working. Use bash calls to find location of directory
#if on windows you are on your own but maybe "dir \ess \s" might work
if(Sys.info()[1] != "Darwin"){message("Only tested on Mac. Should also work on all unix systems also")}
#Dynamically locate directory
path <- system("locate /ess | grep /ess$",intern = TRUE)
setwd(path)
#load required libraries
libs <- list("lavaan", "survey", "Amelia", "foreign", "dplyr", "car", "isco88conversion")
lapply(libs, library, character.only = TRUE)
#find all integrated databases
integratedDataLocation <- list.files(".",pattern = "integrated",recursive = TRUE, full.names = TRUE)
dates <- gsub("(.+)([0-9]{4})(.*)", "\\2", integratedDataLocation)

load(integratedDataLocation[[1]])
####Extract required Data####
#Common variable names across all rounds
requiredVariableNames <- c("dweight", "essround", "cntry", "agea","gndr", 
						   "ipcrtiv", "imprich","ipeqopt", "ipshabt","impsafe","impdiff",
						   "ipfrule", "ipudrst","ipmodst", "ipgdtim","impfree","iphlppl",
						   "ipsuces", "ipstrgv","ipadvnt", "ipbhprp","iprspot","iplylfr",
						   "impenv" , "imptrad","impfun")
#Loop to extract data
integratedData <- list()
for (i in seq_along(integratedDataLocation)){
	cat("loading dataset: ", integratedDataLocation[[i]], "\n")
	load(integratedDataLocation[[i]])
	if(i == 6){requiredVariableNames2 = c(requiredVariableNames, 'isco08')
	}else{requiredVariableNames2 = c(requiredVariableNames, 'iscoco')}
	integratedData[[i]] <- x[,requiredVariableNames2]
	names(integratedData)[i] <- dates[i]
	rm(x)
	cat("completed\n")
}

names(integratedData[[6]])[27] <- "iscoco"
integratedData[[6]]$iscoco <- isco08to88(integratedData[[6]]$iscoco)
#Merge all data into single data frame
mergedIntegratedData <- do.call(rbind.data.frame, integratedData)
rm(integratedData)
mergedIntegratedData$ISEI <- convert(as.character(mergedIntegratedData$iscoco), type = "ISEI")
#Export to SPSS
write.foreign(mergedIntegratedData, "mergedData.txt", "mergedData.sps", package="SPSS")

#### Create Value Scales ####
##Bad Cases
# There is a standard to remove cases based on consistent response or missing on more than 5 items
	# I find this dubious but have included it for now
missingSixOrMore <- which(rowSums(is.na(mergedIntegratedData[,4:24])) > 5)
constantResponse <- apply(mergedIntegratedData[,4:24], 1,function(x) max(table(x))) > 16
constantResponse <-which(constantResponse)
intersection <- union(constantResponse, missingSixOrMore)
cat("Bad cases account for: ", length(intersection)/nrow(mergedIntegratedData)*100, "% of cases")
#Subset out 'BAD' cases
mergedIntegratedData <- mergedIntegratedData[-intersection, ]
##MRAT calculation
mergedIntegratedData$mrat <- rowMeans(mergedIntegratedData[, 4:24],na.rm = TRUE)
##Raw score calculation
mergedIntegratedData$conformity_RAW <- rowMeans(mergedIntegratedData[, c("ipfrule", "ipbhprp")],na.rm = TRUE)
mergedIntegratedData$tradition_RAW <- rowMeans(mergedIntegratedData[, c("ipmodst", "imptrad")],na.rm = TRUE)
mergedIntegratedData$benevolence_RAW <- rowMeans(mergedIntegratedData[, c("iphlppl", "iplylfr")],na.rm = TRUE)
mergedIntegratedData$universalism_RAW <- rowMeans(mergedIntegratedData[, c("ipeqopt", "ipudrst", "impenv")],na.rm = TRUE)
mergedIntegratedData$selfDirection_RAW <- rowMeans(mergedIntegratedData[, c("ipcrtiv", "impfree")],na.rm = TRUE)
mergedIntegratedData$stimulation_RAW <- rowMeans(mergedIntegratedData[, c("impdiff", "ipadvnt")],na.rm = TRUE)
mergedIntegratedData$headonism_RAW <- rowMeans(mergedIntegratedData[, c("ipgdtim", "impfun")],na.rm = TRUE)
mergedIntegratedData$achievement_RAW <- rowMeans(mergedIntegratedData[, c("ipshabt", "ipsuces")],na.rm = TRUE)
mergedIntegratedData$power_RAW <- rowMeans(mergedIntegratedData[, c("ipshabt", "ipsuces")],na.rm = TRUE)
mergedIntegratedData$security_RAW <- rowMeans(mergedIntegratedData[, c("imprich", "iprspot")],na.rm = TRUE)
##Ipsatised Scores
mergedIntegratedData$conformity_CENTER <- mergedIntegratedData$conformity_RAW - mergedIntegratedData$mrat
mergedIntegratedData$tradition_CENTER <- mergedIntegratedData$tradition_RAW - mergedIntegratedData$mrat
mergedIntegratedData$benevolence_CENTER <- mergedIntegratedData$benevolence_RAW - mergedIntegratedData$mrat
mergedIntegratedData$universalism_CENTER <- mergedIntegratedData$universalism_RAW - mergedIntegratedData$mrat
mergedIntegratedData$selfDirection_CENTER <- mergedIntegratedData$selfDirection_RAW - mergedIntegratedData$mrat
mergedIntegratedData$stimulation_CENTER <- mergedIntegratedData$stimulation_RAW - mergedIntegratedData$mrat
mergedIntegratedData$headonism_CENTER <- mergedIntegratedData$headonism_RAW - mergedIntegratedData$mrat
mergedIntegratedData$achievement_CENTER <- mergedIntegratedData$achievement_RAW - mergedIntegratedData$mrat
mergedIntegratedData$power_CENTER <- mergedIntegratedData$power_RAW - mergedIntegratedData$mrat
mergedIntegratedData$security_CENTER <- mergedIntegratedData$security_RAW - mergedIntegratedData$mrat


##### Merge with meta data ####
load("longMetaData_COUNTRY.rda")
#add year
mergedIntegratedData$year <- recode(mergedIntegratedData$essround, "1 = 2002; 2 = 2004; 3 = 2006; 4 = 2008; 5 = 2010; 6 = 2012")
analysisData <- merge(mergedIntegratedData, longMetaData, by = c("year", "cntry"))


####checks ####
# Extract common countries across datasets
commonCNT <- unique(longMetaData$cntry)[unique(longMetaData$cntry) %in% unique(analysisData$cntry)]
#If any FALSE then there is a mistake
# Checked 30 Sep 2015: No FALSE
#Check gini
tapply(analysisData$c_gini, list(analysisData$cntry, analysisData$year), mean, na.rm=TRUE)[commonCNT,] ==
tapply(longMetaData$c_gini, list(longMetaData$cntry, longMetaData$year), unique)[commonCNT,]
#Check social expenditure
tapply(analysisData$c_soexgni, list(analysisData$cntry, analysisData$year), mean, na.rm=TRUE)[commonCNT,] ==
	tapply(longMetaData$c_soexgni, list(longMetaData$cntry, longMetaData$year), unique)[commonCNT,]
#Check unemployment
tapply(analysisData$c_unraall, list(analysisData$cntry, analysisData$year), mean, na.rm=TRUE)[commonCNT,] ==
	tapply(longMetaData$c_unraall, list(longMetaData$cntry, longMetaData$year), unique)[commonCNT,]
#Check retirement replacement
tapply(analysisData$replacement, list(analysisData$cntry, analysisData$year), mean, na.rm=TRUE)[c("CH","GB", "NO"),] ==
	tapply(longMetaData$replacement, list(longMetaData$cntry, longMetaData$year), unique)[c("CH","GB", "NO"),]


#### Export ####
save(analysisData, file = "analysisData.rda")

# ####Preliminary Exploration####
# valueMeans <- 	ddply(mergedIntegratedData, .(essround, cntry),
# 		  				function(x) sapply(x[,c(2:3,26:46)], 
# 		  				   function(z) weighted.mean(z, x$dweight, na.rm=TRUE)
# 		  				   )
# 		  		)
# 
# valueMeans$year <- recode(valueMeans$essround, "1 = 2002; 2 = 2004; 3 = 2006; 4 = 2008; 5 = 2010; 6 = 2012")
# #Add unemployment
# load("longMetaData_COUNTRY.rda")
# valueMeans <- merge(valueMeans, longMetaData, all.x = TRUE)
# #### Interactive Plot ####
# 
# M <- gvisMotionChart(valueMeans_unemployment, 'cntry', 'year', xvar = 'essround', yvar = 'conformity_CENTER',
# 					 sizevar = "unemployment", options=list(state='{"iconKeySettings":[{"key":{"dim0":"ES"},"trailStart":"2002"},{"key":{"dim0":"GB"},"trailStart":"2002"},{"key":{"dim0":"GR"}}],"nonSelectedAlpha":0.4,"yZoomedDataMax":3.461256388,"time":"2002","playDuration":15000,"xLambda":1,"xZoomedIn":false,"iconType":"BUBBLE","orderedByY":false,"xZoomedDataMin":1009843200000,"yZoomedIn":false,"xZoomedDataMax":1325376000000,"uniColorForNonSelected":true,"xAxisOption":"_TIME","sizeOption":"23","yLambda":1,"yZoomedDataMin":2.167843688,"duration":{"timeUnit":"Y","multiplier":1},"yAxisOption":"3","dimensions":{"iconDimensions":["dim0"]},"orderedByX":false,"showTrails":true,"colorOption":"_UNIQUE_COLOR"};'))
# #plot(M)
# capture.output(print(M, 'chart'), file = "VALUESchart.html")




