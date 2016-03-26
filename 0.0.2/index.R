library("jsonlite")
library("forecast")

setContentType("application/json")

execute <- function(userData){
  tryCatch({
    
    output <- list()
    outputData <- list()
    errors <- list()
    
    uriData <- unlist(strsplit(SERVER$uri, '/'))
    
    uriData <- uriData[uriData != ""]
    
    # if(is.null(POST)){
    #   stop("Sorry, but no data in POST")
    # }
    
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
        funArgs = list(tsIn, order = unlist(userData$factors))
        if(!is.null(userData$sfactors)){
          funArgs <- c(funArgs, list(seasonal=list(order = unlist(userData$sfactors))))
        }
        if(length(userData$include.mean)>0){
          funArgs <- c(funArgs, list(include.mean=userData$include.mean))
        }
        if(length(userData$include.drift)>0){
          funArgs <- c(funArgs, list(include.drift=userData$include.drift))
        }
        if(length(userData$include.constant)>0){
          funArgs <- c(funArgs, list(include.constant=userData$include.constant))
        }
        if(length(userData$xreg)>0){
          funArgs <- c(funArgs, list(xreg=userData$xreg))
        }
        if(length(userData$lambda)>0){
          funArgs <- c(funArgs, list(lambda=userData$lambda))
        }
        if(length(userData$transform.pars)>0){
          funArgs <- c(funArgs, list(transform.pars=userData$transform.pars))
        }
        if(length(userData$fixed)>0){
          funArgs <- c(funArgs, list(fixed=userData$fixed))
        }
        if(length(userData$init)>0){
          funArgs <- c(funArgs, list(init=userData$init))
        }
        if(length(userData$method)>0){
          funArgs <- c(funArgs, list(method=userData$method))
        }
        if(length(userData$optim.control)>0){
          funArgs <- c(funArgs, list(optim.control=userData$optim.control))
        }
        if(length(userData$kappa)>0){
          funArgs <- c(funArgs, list(kappa=userData$kappa))
        }
        model <- do.call(Arima, funArgs)
      }
      else{    
        model <- auto.arima(tsIn)
      }
    }
    else if(tolower(uriData[1]) == 'ets'){
      funArgs = list(tsIn)
      if(length(userData$model)>0){
        funArgs <- c(funArgs, list(model=userData$model))
      }
      if(length(userData$damped)>0){
        funArgs <- c(funArgs, list(damped=userData$damped))
      }
      if(length(userData$alpha)>0){
        funArgs <- c(funArgs, list(alpha=userData$alpha))
      }
      if(length(userData$beta)>0){
        funArgs <- c(funArgs, list(beta=userData$beta))
      }
      if(length(userData$gamma)>0){
        funArgs <- c(funArgs, list(gamma=userData$gamma))
      }
      if(length(userData$phi)>0){
        funArgs <- c(funArgs, list(phi=userData$phi))
      }
      if(length(userData$additive.only)>0){
        funArgs <- c(funArgs, list(additive.only=userData$additive.only))
      }
      if(length(userData$lower)>0){
        funArgs <- c(funArgs, list(lower=userData$lower))
      }
      if(length(userData$upper)>0){
        funArgs <- c(funArgs, list(upper=userData$upper))
      }
      if(length(userData$opt.crit)>0){
        funArgs <- c(funArgs, list(opt.crit=userData$opt.crit))
      }
      if(length(userData$nmse)>0){
        funArgs <- c(funArgs, list(nmse=userData$nmse))
      }
      if(length(userData$allow.multiplicative.trend)>0){
        funArgs <- c(funArgs, list(allow.multiplicative.trend=userData$allow.multiplicative.trend))
      }
      model <- do.call(ets, funArgs)      
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
    
    outputData$lower <- c('80%' = fitModel$lower[,1], '95%' = fitModel$lower[,2])
    
    outputData$upper <- c('80%' = fitModel$upper[,1], '95%' = fitModel$upper[,2] )
    
    outputData$frequency <- frequency(tsIn)
    
    output <- list(data = outputData, errors = errors)
    cat(toJSON(output, pretty = TRUE))
    
  }, error = function(err){ 
    output <- list(data = outputData, errors = err$message)
    cat(toJSON(output, pretty = TRUE))
    return()
  }
  
  )
}