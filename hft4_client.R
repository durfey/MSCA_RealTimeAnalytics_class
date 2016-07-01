setwd("//Users/rdurfey/R_misc/RealTime/ProjectMaterials_HFT4")

#!/usr/bin/env Rscript
source("hft4_connection.R")

usePackage('hawkes')

# global vars
event_counter  <- 0
BUF_SIZE <- 10000                                                # we create buffer in advance:
events <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),           # dataframe for new trades and our signals 
                     trade_price=as.numeric(rep(NaN, BUF_SIZE)),
                     trade_size=as.numeric(rep(NaN, BUF_SIZE)),
                     pch=as.numeric(rep(NaN, BUF_SIZE)),         # price changes
                     lambda=as.numeric(rep(NaN, BUF_SIZE)),      # hawkes intensity
                     signal=as.numeric(rep(NaN, BUF_SIZE)) 
)

initial_timestamp <- Sys.time()      # start-of-program timestamp
plot_timestamp <- initial_timestamp  # we use plot_timestamp to draw the plot once in a second

timeHorizon <- 60  # in seconds

# you can adjust these parameters: ----> 740 & 0.96 gave lowest training penalty
width <- 740  # window width in events
confidenceLevel = 0.96    # VaR level



# user defined handler
## arguments:
#### -- trade_price - latest ES trade price
#### -- trade_size  - corresponding ES trade size
## returns:
#### list(send_signal,value) where 
#### -- list$send_signal - TRUE when we desire to send msg back to server and FALSE otherwise
#### -- list$value is worst price estimate with time horizon (i.e. current price minus DVaR)
event_handler <- function(trade_price, trade_size) {
    now <- Sys.time()
    # log event if you want
    #message("EVENT at ", now, ", trade_price=", trade_price)
    
    # update events dataframe (append last value except for 'lambda' and 'signal'):
    event_counter <<- event_counter + 1
    events[event_counter,] <<- list(now, trade_price, trade_size, 
                                    ifelse(event_counter>1, trade_price - events$trade_price[event_counter-1], 0),  # price change
                                    NaN,  # lambda
                                    NaN)  # signal
    
    # initial return vars
    send_signal <- FALSE
    value <- NaN
    
    if ( event_counter > 10 ) {
        i0 <- max(2, event_counter - width + 1)  # i0:event_counter are window indices
        params <- fit_hawkes( difftime(events$time[i0:event_counter], events$time[i0-1], unit='sec') )$par
        mu <- params[1]
        alpha <- params[2]
        beta <- params[3]
        # hawkes intensity formula:
        lambda <- mu + alpha * sum( exp( -beta * ( as.numeric(difftime(now, events$time[i0:event_counter], unit='sec')) ) ) )
        events$lambda[event_counter] <<- lambda
        sigma <- sd( events$pch[i0:event_counter] )
        DVaR <- -sigma*sqrt(lambda*timeHorizon/2)*log(1-confidenceLevel)
        send_signal <- TRUE
        value <- trade_price - DVaR
    }
    
    # update events dataframe (only signal):
    events$signal[event_counter] <<- value
    
    Draw()  # once in a second
    
    # return list of 2 objects
    return( list(send_signal=send_signal, value=value) )
}


# plot once in a second
Draw <- function()
{
    now <- Sys.time()
    if (difftime(now, plot_timestamp, unit='sec') >= 1) {
        plot_timestamp <<- now;
        if (event_counter > 0) {
            good_events <- events[1:event_counter,]
            par(mar = c(5,5,2,5))
            # draw price chart
            t <- difftime(good_events$time, initial_timestamp, unit="sec")
            plot(x=t, y=good_events$trade_price, 
                 type='s', col = 'blue',
                 xlab='time (seconds)', ylab='price',
                 xlim = c(0, tail(t,1)),
                 ylim = c(min(good_events$signal, good_events$trade_price, na.rm = T), 
                          max(good_events$signal, good_events$trade_price, na.rm = T)) 
                 )
            # draw predictions
            lines(x=t, y=good_events$signal,
                  type='s', col = 'red'
                  )
            par(new = T)
            # draw hawkes intensity
            plot(x=t, y=good_events$lambda,
                 type='l', col = 'green',
                 axes=F, xlab=NA, ylab=NA, cex=1.2,
                 xlim = c(0, tail(t,1)),
                 ylim = c(0, max(c(1,good_events$lambda), na.rm = T) )
                 )
            axis(side = 4)
            mtext(side = 4, line = 3, 'intensity')
            legend("bottomleft",
                   legend=c("price", "risk forecast\nfor next 60 sec", "intensity"),
                   lty=c(1,1,1), col=c("blue", "red", "green"))
        }
    }
}



## Fit hawkes process functions :

negloglik_hawkes <- function(params, history) {
    # params == c(mu, alpha, beta)
    return(likelihoodHawkes(params[1], params[2], params[3], history))
}

fit_hawkes <- function(times) {
    ui <- rbind(diag(3), c(0,-1,1))  # constraints: mu,alpha,beta >= 0 && alpha <= beta
    init_par <- c(1, 0.1, 1)         # init params
    constrOptim(init_par, negloglik_hawkes, grad=NULL,
                ui = ui, ci=0, history = times)
}


# server options
host <- "datastream.ilykei.com"
port <- 30104
login <- "durfey@uchicago.edu"
password <- "rnVGOrGr"
stream_name <- "hft4"
catch_handler_errors <- TRUE  # we recommend using TRUE during the test and FALSE during homework
# make connection with your personal handler
result <- Connect(host, port, login, password, stream_name, event_handler, catch_handler_errors)

# remove empty values from buffers
events <- events[!is.na(events$time),]

# after all you can dump your data/results and analyze it later
dump( c("events", "result"), file = "results.txt")

# save this iteration of sourced file output
this_output<-data.frame(width, confidenceLevel, this_penalty)
write.table(this_output,file="./outputs.txt",append=TRUE,col.names=FALSE,row.names = FALSE)

# # save plot
# png(filename=paste0('./plots/width_',width,'_cf_',confidenceLevel,'.png'))
# dev.off()
dev.copy(png,file=paste0('./plots/width_',width,'_cf_',confidenceLevel,'.png'))
dev.off()