#set working directory
setwd( "~/ess/" )
#Adjusted from
# source_url( "https://raw.github.com/ajdamico/asdfree/master/European%20Social%20Survey/replication.R" , prompt = FALSE , echo = TRUE )

library(lmerTest)		# allows random effects modeling
library(survey)		# load survey package (analyzes complex design surveys)

# you'll need this for the replication scripts below.
options( survey.lonely.psu = "adjust" )
# this setting matches the MISSUNIT option in SUDAAN
#### Integrated sample ####
#load integrated sample
load( "./2012/integrated.rda" )
# The weighted data is slightly different to that reported online but here I use all countries
weighted.mean(x$happy,w = x$dweight, na.rm=TRUE)
#Fixed Effects Model
ESS5_cnt <- split(x, x$cntry)
fixEfMean <-sapply(ESS5_cnt, function(y) lm(happy ~ 1, weights = dweight, data = y)$coef)
#Random Effects Model
M1 <- lmer(happy ~ 1 + (1|cntry), weights = dweight, data = x)
ranEfMean<- ranef(M1)$cntry+fixef(M1)
#Identical to the first decimal place but both different to 
cbind(fix = round(fixEfMean,3), rand = round(ranEfMean,3))

# load france's round two sample design data file (sddf)..
load( "./2012/FR/ESS6__SDDF.rda" )
# ..and immediately save it to a more appropriately-named object
ess6.fr.sddf <- x

# load france's round two main data file..
load( "./2012/FR/ESS6.rda" )
# ..and immediately save it to a more appropriately-named object
ess6.fr <- x

# merge these two files together, creating a merged object..
ess6.fr.m <- merge( ess6.fr , ess6.fr.sddf )
# ..and immediately check that all record counts match up
stopifnot( nrow( ess6.fr ) == nrow( ess6.fr.m ) & nrow( ess6.fr.sddf ) == nrow( ess6.fr.m ) )

# construct one complex-sample survey design..
ess6.fr.design <- 
	svydesign(
		ids = ~psu ,
		strata = ~stratify ,
		probs = ~prob ,
		data = ess6.fr.m
	)
#Mean is identical to that taken from fixed effect and random effect model to first decimal place.
# Identical to fixed effect model further out from that.
svymean( ~happy , ess6.fr.design , deff = TRUE, na.rm=TRUE )
svytable(~happy, ess6.fr.design, Ntotal = 100)
