library("jsonlite")
library("forecast")

setContentType("application/json")

#Array of availivle api versions. All folders with same name shoul exist in project folder
#TODO change to dynamic list of folders 
apiVersions <- c(
	"0.0.1",
	"0.0.2"
	)

clientData <- fromJSON(rawToChar(receiveBin(length=8192)))
clientData[clientData==""] <- NULL

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

execute(clientData)
