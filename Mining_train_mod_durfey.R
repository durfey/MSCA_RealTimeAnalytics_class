setwd("//Users/rdurfey/R_misc/RealTime/ProjectMaterials_Mining")

library(nloptr)

datapath<-"."
dataMatrix <- as.matrix(read.table(file=paste(datapath,'MiningTraining2016.csv',sep="/"),header=T, sep = ","))
eventMoments <- dataMatrix[,1]
head(dataMatrix)

# change the time scale to minutes
eventMoments <- eventMoments/60000000
plot(eventMoments, c(1:length(eventMoments)), col = 'blue', 
     type="s",ylab="n_events",xlab="time (minutes)",
     lwd=2,xlim=c(0,max(eventMoments)),ylim=c(0,length(eventMoments)))

dt <- 0.05
w <- 18 # experiment with window width to estimate intensity (was 10)
n <- 52 # experiment with number of observations of intensity in the model (was 50)
t0 <- ((n+w)*dt) # earliest time when regression can be fitted
i0 <- (findInterval(t0, eventMoments) + 1) # earliest event number 

regression <- function(tc, logRate, regressionTimes, returnError){
  # tc - time of collapse
  # logRate - logarithm of intensity calculated at times t_i, i= 1,...,n
  # regressionTimes - sequence (tc-t_i), i= 1,...,n
  # returnError - logical flag, TRUE if function is used in a minimization procedure; then it returns mean-squared residual
  #               if FALSE return coefficient p, the slope, of the model    
  
  logregressionTimes <- log(tc-regressionTimes)
  linModel <- lm(logRate ~ logregressionTimes)
  if(returnError){ #if we use this function in a minimization procedure
    err <- sqrt(mean(linModel$residuals^2))
    res <- err    
  }
  else{ #we will need the p coefficient after minimization
    p <- linModel$coefficients[2]
    names(p) <- NULL
    res <- p
  }
  return(res)
}


# try to run for loop to get params out that'll be used to predict collapse later
# set initial params that'll be used in for loop
Z <- length(eventMoments)
intensities <- rep(NA, (Z-i0))
currTime <- rep(NA, (Z-i0))
tc <- rep(NA, (Z-i0))
p <- rep(NA, (Z-i0))
idx <- 1

for (i in 1:(Z-i0)) {
  currTime[idx] <- eventMoments[i0]
  tGrid <- seq(currTime[idx] - t0 + dt, currTime[idx], by=dt) # grid at t0
  #head(tGrid)
  
  eventsNumber <- findInterval(tGrid, eventMoments) 
  #eventsNumber  # eventsNumber: vector of cumulative counts of events in w+n time intervals in tGrid
  
  #cbind(tGrid=tGrid,eventsNumber=eventsNumber,events=head(eventMoments,(w+n)))[1:16,]
  
  N <- length(tGrid)
  intensity <- eventsNumber[(w+1):N] - eventsNumber[1:(N-w)]
  intensity <- intensity / (dt*w) # events per minute
  x <- length(intensity)
  intensities[idx] <- intensity[x] # save intensities
  times.ti <- tGrid[(N-n+1):N] # times.ti contains t_i, i=1,...n
  # Use pmax(x, 0.1) to avoid log(0)
  logIntensity <- log(pmax(intensity, 0.1))
  
  res <- nloptr(x0=currTime[idx]+1, eval_f=regression, lb=currTime[idx]+0.1, ub=currTime[idx]+10, 
                opts=list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel" = 1e-04),
                logRate=logIntensity, regressionTimes=times.ti, returnError=TRUE)
  
  tc[idx] <- res$solution
  #print(paste0('tc: ', tc[idx]))
  p[idx] <- regression(res$solution, logIntensity, times.ti, FALSE)
  #print(paste0('pp: ', p[idx]))
  i0 <- i0 + 1
  idx <- idx + 1
}

resTable <- as.data.frame(cbind(currTime=currTime, pp=p, intensities = intensities , timeToShock = tc-currTime))
head(resTable,15)

# adding alarm signal -- the last reading in the training set before collapse is at 18.77, so our window for alarm is in the 1-2 minutes before that
resTable <- cbind(resTable, alarm_signal = (rep(0, nrow(resTable))))
resTable$alarm_signal <- replace(resTable$alarm_signal, resTable$currTime > (18.77 - 2) 
                                 & resTable$currTime < (18.77 -1), 1)
resTable

log.mod <- glm(alarm_signal~pp+intensities+timeToShock, data=resTable, family=binomial("logit"))
summary(log.mod)

log.mod.predict <- predict(log.mod, resTable, type="response")
as.data.frame(cbind(resTable, log.mod.predict))

save(list="log.mod",file = "mining_log_model.Rdata")


