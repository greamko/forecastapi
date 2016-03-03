library("jsonlite")
library("forecast")

setContentType("application/json")

#Array of availivle api versions. All folders with same name shoul exist in project folder
apiVersions <- c(
	"0.0.1"
	)

clientData <- fromJSON(POST$data)

#Checking client ai version. Setting last version in case of typo or empty
if(exists('clientData$version')){
	if(match(clientData$version, apiVersions)){
		clientApiVersion <- userData$version
	}
}
if(!exists('clientApiVersion')){
	clientApiVersion <- apiVersions[length(apiVersions)]
}

#Include api code
workingDir <- paste(getwd(), clientApiVersion, sep = "/")

setwd(workingDir)

source("index.R")
