####setup####
#Find directory path and set it as working. Use bash calls to find location of directory
#if on windows you are on your own but maybe "dir \ess \s" might work
if(Sys.info()[1] != "Darwin"){message("Only tested on Mac. Should also work on all unix systems also")}
#Dynamically locate directory
path <- system("locate /ess | grep /ess$",intern = TRUE)
setwd(path)
#load required libraries
libs <- list("lavaan", "survey", "Amelia", "foreign", "dplyr")
lapply(libs, library, character.only = TRUE)
#find all integrated databases
integratedDataLocation <- list.files(".",pattern = "integrated",recursive = TRUE, full.names = TRUE)
dates <- gsub("(.+)([0-9]{4})(.*)", "\\2", integratedDataLocation)
####Extract required Data####
#Common variable names across all rounds
requiredVariableNames <- c("dweight", "essround", "cntry",
						   "ipcrtiv", "imprich","ipeqopt", "ipshabt","impsafe","impdiff",
						   "ipfrule", "ipudrst","ipmodst", "ipgdtim","impfree","iphlppl",
						   "ipsuces", "ipstrgv","ipadvnt", "ipbhprp","iprspot","iplylfr",
						   "impenv" , "imptrad","impfun")
#Loop to extract data
integratedData <- list()
for (i in seq_along(integratedDataLocation)){
	cat("loading dataset: ", integratedDataLocation[[i]], "\n")
	load(integratedDataLocation[[i]])
	integratedData[[i]] <- x[,requiredVariableNames]
	names(integratedData)[i] <- dates[i]
	rm(x)
	cat("completed\n")
}
#Merge all data into single data frame
mergedIntegratedData <- do.call(rbind.data.frame, integratedData)
rm(integratedData)
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

####Preliminary Exploration####
valueMeans <- 	ddply(mergedIntegratedData, .(essround, cntry),
		  				function(x) sapply(x[,27:46], 
		  				   function(z) weighted.mean(z, x$dweight, na.rm=TRUE)
		  				   )
		  		)

valueMeans$year <- recode(valueMeans$essround, "1 = 2002; 2 = 2004; 3 = 2006; 4 = 2008; 5 = 2010; 6 = 2012")
#Add unemployment
load("multilevelData/unEmployment_COUNTRY.rda")
valueMeans_unemployment <- merge(valueMeans, unEmployment, all.x = TRUE)
#### Interactive Plot ####
library(googleVis)
library(car)

#op <- options(gvis.plot.tag='chart')
M <- gvisMotionChart(valueMeans_unemployment, 'cntry', 'year', xvar = 'essround', yvar = 'conformity_CENTER',
					 sizevar = "unemployment", options=list(state='{"yZoomedIn":false,"dimensions":{"iconDimensions":["dim0"]},"time":"2002","xZoomedDataMax":6,"orderedByX":false,"xAxisOption":"2","colorOption":"_UNIQUE_COLOR","xZoomedDataMin":1,"iconKeySettings":[{"key":{"dim0":"ES"},"trailStart":"2002"},{"key":{"dim0":"GB"},"trailStart":"2002"}],"yAxisOption":"3","yZoomedDataMax":3.461256388,"showTrails":true,"playDuration":15000,"xLambda":1,"xZoomedIn":false,"iconType":"BUBBLE","duration":{"timeUnit":"Y","multiplier":1},"sizeOption":"23","yZoomedDataMin":2.167843688,"uniColorForNonSelected":true,"nonSelectedAlpha":0.4,"orderedByY":false,"yLambda":1};'))
#plot(M)
capture.output(print(M, 'chart'), file = "VALUESchart.html")

