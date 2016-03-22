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
  			model <- Arima(
          tsIn, 
          order = unlist(userData$factors), 
          seasonal=list(order = unlist(userData$sfactors)),
          xreg = userData$xreg,
          include.mean = userData$include.mean,
          include.drift = userData$include.drift,
          include.constant = userData$include.constant,
          xreg = userData$xreg,
          lambda = userData$lambda,
          transform.pars = userData$transform.pars,
          fixed = userData$fixed,
          init = userData$init,
          method = userData$method,
          optim.control = userData$optim.control,
          kappa = userData$kappa,
          )
  	}
  	else{    
	    model <- auto.arima(tsIn)
	  }
  }
  else if(tolower(uriData[1]) == 'ets'){

    model <- ets(
      tsIn, 
      model = userData$model, 
      damped = userData$damped, 
      alpha = userData$alpha, 
      beta = userData$beta, 
      gamma = userData$gamma, 
      phi = userData$phi,
      additive.only = userData$additive.only,
      lower = userData$lower,
      upper = userData$upper,
      opt.crit  = userData$opt.crit,
      nmse  = userData$nmse,
      allow.multiplicative.trend  = userData$allow.multiplicative.trend
      )
    
  }
  else{
    stop("method not found")
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
    
}, error = function(err){ 
	output <- list(data = outputData, errors = err$message)
    cat(toJSON(output, pretty = TRUE))
	return()
}

)