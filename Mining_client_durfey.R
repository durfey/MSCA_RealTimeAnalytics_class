source("Mining_connection.R")
library("nloptr")
load("mining_log_model.Rdata")

# global vars
incoming_signals_counter <- 0                                   # incoming signals event counter
outgoing_signals_counter <- 0                                   # outgoing signals event counter
BUF_SIZE <- 10000                                               # we create buffers in advance:
inc_signals <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)))     # dataframe for incoming signals 
out_signals <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)))     # dataframe for outgoing signals

eventMoments <- rep(NA, BUF_SIZE)         # time spans from i-th signal and the first signal in minutes, eventMoments[1] = 0 
initial_timestamp <- Sys.time()     # start-of-program timestamp
plot_timestamp <- initial_timestamp # we use plot_timestamp to draw the plot once in a second

# parameters for the simple solution 
# W <- 0.5                # window size for intensity estimating
# eventRate_barrier <- 10 # when intensity exceeds this barrier then we send alarm signal!

#####################################
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
###############################################


# user defined handler
## no arguments
## returns:
#### logical vector of unit length which value is TRUE when we want to send signal back to server
## !! Note that only the first outgoing signal makes sence! Others will be ignored by server.

# This simple code is given as an example only. You can tune "eventRate_barrier" parameter for the
# workshop data, but it won't give you the best result on the test data.
# We will send an alarm signal if estimated event rate exceeds eventRate_barrier.
new_event_handler <- function() {
  now <- Sys.time()
  if(incoming_signals_counter < 0.5){
    initial_timestamp <<- now
  }
  # time in minutes from the start of the stream 
  t <- as.double(difftime(now, initial_timestamp, unit='min'))
  # log event if necessary
  message("EVENT at ", now)
  message("t = ", t)
  # update inc_signals dataframe (append last value):
  incoming_signals_counter <<- incoming_signals_counter + 1
  inc_signals[incoming_signals_counter,] <<- list(now)
  eventMoments[incoming_signals_counter] <<- t
 
#################################################   
  #print(eventMoments[0:incoming_signals_counter])
  send_signal <- FALSE
  
  dt <- 0.05
  w <- 18 # experiment with window width to estimate intensity
  n <- 52 # experiment with number of observations of intensity in the model
  t0 <- ((n+w)*dt) # earliest time when regression can be fitted
  #t0
  i0 <- (findInterval(t0, eventMoments[1:incoming_signals_counter]) + 1) # earliest event number.
  #i0
  
  Numevents = length(eventMoments[1:incoming_signals_counter])
  
  for (i in 0:(Numevents-i0)) {
    currTime <<- eventMoments[incoming_signals_counter]    #build system for linear model
    tGrid <- seq(currTime - t0 + dt, currTime, by=dt) # grid at t0. This = total # of times necessary for building the system.
    eventsGrid <- findInterval(tGrid, eventMoments[1:incoming_signals_counter])
    
    N <- length(tGrid)
    intensity <- eventsGrid[(w+1):N] - eventsGrid[1:(N-w)]
    intensity <- intensity / (dt*w)  # events per minute
    times.ti <- tGrid[(N-n+1):N] # times.ti contains t_i, i=1,...n 
    # Use pmax(x, 0.1) to avoid log(0). 
    logIntensity <- log(pmax(intensity, 0.1))
    
    res <- nloptr(x0=currTime+1,
                  eval_f=regression,     #function here
                  lb=currTime + 0.1,     #Range for optimization is currTime+0.1 - currTime+10
                  ub=currTime + 10,
                  opts=list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel" = 1e-04),
                  logRate= logIntensity,
                  regressionTimes=times.ti,
                  returnError=TRUE)
    
    pp<-regression(res$solution, logIntensity, times.ti, FALSE)
    intensities<-intensity[length(intensity)]
    timeToShock<-res$solution-currTime
    
    event_data <- list(pp,intensities,timeToShock)
    data.grid <- as.data.frame(event_data)
    
  }
  
  names(data.grid)<-c("pp", "intensities", "timeToShock")
  
  # now use the loaded log.mod to predict a probability & send a signal if it's large enough
  pred<-predict(log.mod, data.grid, type="response")
  message("probability: ", pred)
  
  if(pred >= 0.8)
  {
    tSet <- c(t-w,t)
    X <- eventMoments[!is.na(eventMoments)]
    eventsBeforeMoments <- findInterval(tSet, X)
    intensity <- eventsBeforeMoments[2] - eventsBeforeMoments[1] #events number between "t-w" and "t"
    intensity <- intensity/w # events per minute
    #      send_signal <- (intensity > eventRate_barrier) & (outgoing_signals_counter <= 0)
    if(outgoing_signals_counter==0)
      send_signal <- TRUE
  }
  
  
############### stock code, not to be used ######################  
  
#   if(t > W)
#   {
#     tSet <- c(t - W, t) #current time interval (t_i-1,t_i) 
#     X <- eventMoments[!is.na(eventMoments)]
#     eventsBeforeMoments <- findInterval(tSet, X)
#     intensity <- eventsBeforeMoments[2] - eventsBeforeMoments[1] #events number between "t-w" and "t"
#     intensity <- intensity/W # events per minute
#     send_signal <- (intensity > eventRate_barrier) & (outgoing_signals_counter <= 0)
#   }
#   
  
  
  if (send_signal) {
    # update out_signals dataframe (append last value):
    outgoing_signals_counter <<- outgoing_signals_counter + 1
    out_signals[outgoing_signals_counter,] <<- list(now)
  }
  
  Draw()
  
  return( send_signal )
}


# plot once in a second
Draw <- function()
{
    now <- Sys.time();
    if (difftime(now, plot_timestamp, unit='sec') >= 1) {
        plot_timestamp <<- now;
        if (incoming_signals_counter > 0) {
            t <- difftime(inc_signals$time[1:incoming_signals_counter], initial_timestamp, unit='min');
            plot(x=t, y=1:length(t), 
                 xlim=c(0, difftime(now, initial_timestamp, unit='min')),
                 type='s', xlab='time (minutes)', ylab='n_events');
            
            if (outgoing_signals_counter > 0) {
                # draw outgoing signal (only the first one)
                abline(v=difftime(out_signals$time, initial_timestamp, unit='min')[1],
                       col='red', lwd=2);
            }
        }
    }
}


# server options
host <- "datastream.ilykei.com"
port <- 30004
login <- "durfey@uchicago.edu"
password <- "rnVGOrGr"
stream_name <- "Mining"
catch_handler_errors <- TRUE  # we recommend using TRUE during the test and FALSE during homework
# make connection with your personal handler
result <- Connect(host, port, login, password, stream_name, new_event_handler, catch_handler_errors)

# remove empty values from buffers
inc_signals <- inc_signals[!is.na(inc_signals$time),]
out_signals <- out_signals[!is.na(out_signals$time),]
eventMoments <- eventMoments[1:incoming_signals_counter]
alarmTime <- as.double(difftime(out_signals[1], inc_signals[1] , unit='min'))
message("alarmTime = ", alarmTime)

# after all you can dump your data/results and analyze it later
dump(c("inc_signals", "out_signals", "result"), file = "results.txt")

