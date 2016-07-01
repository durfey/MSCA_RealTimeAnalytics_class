setwd("//Users/rdurfey/R_misc/RealTime/ProjectMaterials_RevenueManagement")
#!/usr/bin/env Rscript
source("Yield_connection.R")
#load("./calc_intensities.Rdata")
load("./fare_mean_var.Rdata") # load fare means & variances
library(RM2)
load("./yield_mean_mod.Rdata")
load("./yield_var_mod.Rdata")

# global vars
BUF_SIZE <- 1000 # we create buffers in advance
ticket_counter <- 0
sold_tickets <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),
                           price=as.numeric(rep(NA, BUF_SIZE)) )
price_changes_counter <- 0
price_changes <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),
                            price=as.numeric(rep(NA, BUF_SIZE)) )
initial_timestamp <- Sys.time()      # start-of-program timestamp
plot_timestamp <- initial_timestamp  # we use plot_timestamp to draw the plot once in a second

#init_price <- 100
# add initial price into price_changes dataframe:
price_changes_counter <- 1
price_changes[price_changes_counter,] <- list(time=initial_timestamp, price=init_price)



# user defined handler is expected to have two arguments:
### 1) sold (logical vector of unit length) - TRUE when ticket was sold. Otherwise the event is just a heartbeat.
### 2) price (numeric vector of unit length) - sold price (when sold==TRUE)
## and to return a list of 2 variables:
### 1) lst$change_price (logical vector of unit length) - TRUE when operator desires to change ticket price
### 2) lst$price (numeric vector of unit length) - new ticket price

# intensity function
# intens <- function(price) {
#   C = 150
#   return(C/price)
# }
Fares <- seq(160,110,-25) # make sure they're sorted descending order
fares_ints<-fare_mean_var[fare_mean_var[,1] %in% Fares,] # pull relevant historically observed intensities per price point
fares_ints<-fares_ints[order(fares_ints[,1],decreasing = TRUE),]

#t = 0 #i = 0
#init_price = min(Fares)

tot_time <- time_left <- 10
cap<-100
p <- EMSRb(Fare = fares_ints[,1], Mean = time_left*fares_ints[,2],
           Var = time_left*fares_ints[,3], cap = cap)
p
init_price <- max(Fares[p==cap])
price<-init_price


event_handler <- function(sold, price) {
    now <- Sys.time()
    elapsed <- difftime(now,initial_timestamp,units="mins")
    time_left <- tot_time - as.numeric(elapsed)
    message("######## NEW EVENT -- Time Left: ",round(time_left,2), " minutes ########")
    if (sold) {
        # log event if you want
        ticket_counter <<- ticket_counter + 1
        message("At ",round(elapsed,2), " minute mark : Sold ticket #",ticket_counter," at price=", price)
        sold_tickets[ticket_counter,] <<- list(time=now, price=price)
        cap <<- cap -1
        message("Cap is now: ", cap)
    }
    else {
        message(now, ": Heartbeat")
    }
    
    change_price <- FALSE
    new_price <- 0
    
    # example how to change prices
#     if (ticket_counter == 10 && sold) {
#         change_price <- TRUE
#         new_price <- 20
#     }
    
    # my code to change prices
    if(time_left>0 & cap>0){
#       p <- EMSRb(Fare = fares_ints[,1], Mean = time_left*predict(mean_mod,newdata=data.frame(X1=c(fares_ints[,1]))),
#                  Var = time_left*predict(var_mod,newdata=data.frame(X1=c(fares_ints[,1]))), cap = cap)
      p <- EMSRb(Fare = fares_ints[,1], Mean = time_left*fares_ints[,2],
                 Var = time_left*fares_ints[,3], cap = cap)
      new_price_est <- max(Fares[p==cap])
      print("New Protection Levels:")
      print(p)
      if(price != new_price_est){
        change_price <- TRUE
        new_price <- price <- new_price_est
      }
    }
    
    if (change_price) {
        # update price_changes dataframe (append last value):
        price_changes_counter <<- price_changes_counter + 1
        price_changes[price_changes_counter,] <<- list(time=now, price=new_price)
    }

    Draw()
    
    return( list(change_price=change_price, price=new_price) )
}


# plot once in a second
Draw <- function()
{
    now <- Sys.time();
    if (difftime(now, plot_timestamp, unit='sec') >= 1) {
        plot_timestamp <<- now;
        if (ticket_counter > 0) {
            title <- paste0("Tickets sold: ", ticket_counter, 
                            ", current price: ", price_changes$price[price_changes_counter]);
            t <- difftime(sold_tickets$time[1:ticket_counter], initial_timestamp, unit='sec');
            plot(x=t, y=1:length(t), 
                 xlim=c(0, difftime(now, initial_timestamp, unit='sec')),
                 type='s', xlab='time (seconds)', ylab='tickets sold',
                 main=title);
            
            if (price_changes_counter > 0) {
                # draw price changes
                for (i in 1:price_changes_counter) {
                    x <- difftime(price_changes$time[i], initial_timestamp, unit='sec')
                    abline(v=x, col='red', lwd=2);
                    text(x, ticket_counter/2, as.character(price_changes$price[i]), pos=4)
                }
            }
        }
    }
}


# server options
host <- "datastream.ilykei.com"
port <- 30008
login <- "durfey@uchicago.edu"
password <- "rnVGOrGr"
stream_name <- "Yield"
catch_handler_errors <- TRUE  # we recommend using TRUE during the test and FALSE during homework
# make connection with your personal handler
result <- Connect(host, port, login, password, stream_name, event_handler, catch_handler_errors, init_price)

# remove empty values from buffers
sold_tickets <- sold_tickets[!is.na(sold_tickets$time),]
price_changes <- price_changes[!is.na(price_changes$time),]

# after all you can dump your data/results and analyze it later
dump(c("sold_tickets", "price_changes", "result"), file = "results.txt")

