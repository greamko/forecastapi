library("jsonlite")
library("forecast")

setContentType("application/json")

tryCatch({
  output <- list()
  outputData <- list()
  errors <- list()
  
  uriData <- unlist(strsplit(SERVER$uri, '/'))
  
  uriData <- uriData[uriData != ""]
  
  if(is.null(POST$data)){
    stop("Sorry, but no data in POST")
  }
  
  userData <- fromJSON(POST$data)
  
  
  if(is.null(userData$ts)){
    stop("Sorry, but no data in Time Series (ts)")
  }
  
  tsIn <- as.numeric(userData$ts)

  if(!is.null(userData$frequency)){
  	tsIn <- ts(tsIn, frequency = userData$frequency)
  }
  else{
  	tsIn <- ts(tsIn,  frequency = findfrequency(tsIn))
  }
  
  
  if(tolower(uriData[1]) == 'arima'){

  	if(!is.null(userData$factors)){
  		if(!is.null(userData$sfactors)){
  			model <- arima(tsIn, order = unlist(userData$factors), seasonal=list(order = unlist(userData$sfactors)))
  		}
  		else{
  			model <- arima(tsIn, order = unlist(userData$factors))
  		}
  	}
  	else{    
	    model <- auto.arima(tsIn)
	}
    
	if(!is.null(userData$horizon)){
	  fitModel <- forecast(model, h=userData$horizon);
	}
	else{
	  fitModel <- forecast(model);
	}
    
    outputData$mean <- fitModel$mean

    outputData$horizon <- length(fitModel$mean)
    
    outputData$method <- fitModel$method
    
    outputData$fitted <- fitModel$fitted
    
    outputData$lower <- list('80%' = fitModel$lower[,1], '95%' = fitModel$lower[,2])
    
    outputData$upper <- list('80%' = fitModel$upper[,1], '95%' = fitModel$upper[,2]	)
    
    outputData$frequency <- frequency(tsIn)
    
    output <- list(data = outputData, errors = errors)
    cat(toJSON(output, pretty = TRUE))
    
  }
}, error = function(err){ 
	output <- list(data = outputData, errors = err$message)
    cat(toJSON(output, pretty = TRUE))
	return()
}

)