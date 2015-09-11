##libraries
library(lavaan)
library(lavaan.survey)
setwd( "~/ess/2008/" )
#Extract Countries from file names
countries <- grep("[A-Z]", list.dirs(), value = TRUE)
countries <- gsub("^.{2}", "", countries)

#Create loop to extract and merge all countries data
dataIntegrated <- list()
#Temp until I figure out design matrix issues
#how does probs have missing values!!!
countries <- c("ES")
t <- 1
for (i in seq_along(countries)){
	if(file.exists(file.path(getwd(), countries[i], "ESS4__SDDF.rda"))){
		cat("processing country: ",countries[i], "\n")
		#load main data
		tmpFilePath <- file.path(getwd(), countries[i], "ESS4.rda")
		load(tmpFilePath)
		tmpData1 <- x
		#load design file
		tmpFilePath <- file.path(getwd(), countries[i], "ESS4__SDDF.rda")
		load(tmpFilePath)
		tmpData2 <- x
		tmpMerge <- merge( tmpData1 , tmpData2)
		#Test all rows maintained
		if( nrow( tmpData1 ) == nrow( tmpMerge ) | nrow( tmpData2 ) == nrow( tmpMerge ) ){
			tmpMerge$country <- countries[i]
			tmpMerge$psu <- paste( countries[i] , tmpMerge$psu , sep="-" )
			dataIntegrated[[t]] <- tmpMerge
			names(dataIntegrated)[t] <- countries[i]
			t <- t + 1
			cat("processed\n")
		}else{cat("ERROR in country: ", countries[i],"\n")}
	}else{cat("no design matrix in:", countries[i],"\n");next()}
}

# Join all countries with PSUs
ess4.m <- do.call(rbind, dataIntegrated)
