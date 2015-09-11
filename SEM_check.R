#based on http://www.ssoar.info/ssoar/bitstream/handle/document/6669/ssoar-2008-3-davidov_et_al-bringing_values_back_in_the.pdf

#Load libraries
library(lavaan)
#set working directory
setwd( "~/ess/2008/" )
#load integrated data
load("integrated.rda")
#provide sample design
#this is not really ideal but given not every country has a design matrix
	# this will have to do for now
ess4.design <- 
	svydesign(
		ids = ~idno,
		weights = ~dweight, 
		data = x
	)

modelSyntax <- " 
Universalism =~ ipeqopt + ipudrst + impenv
Benevolence  =~ iphlppl + iplylfr

Tradition    =~ ipmodst + imptrad
Conformity   =~ ipfrule + ipbhprp 
Security     =~ impsafe + ipstrgv
"

Mtotal <- cfa(modelSyntax, data = x, group = "cntry") 
Mtotal.survey <- lavaan.survey( Mtotal , ess4.design )
fitmeasures(Mtotal.survey)
standardizedsolution(Mtotal.survey)
