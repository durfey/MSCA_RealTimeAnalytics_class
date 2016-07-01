setwd("//Users/rdurfey/R_misc/RealTime/ProjectMaterials_ATMSecurity")
#!/usr/bin/env Rscript
source("atm_connection.R")
load('globalPar.rda')
load('custParams.rda')

# global vars
event_counter <- 0
BUF_SIZE <- 1000                                                  # we create buffer in advance:
event_data <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),        # dataframe for received events
                         visit_id=as.integer(rep(NA, BUF_SIZE)),
                         atm_signal=character(10),
                         alert_signal=as.integer(rep(NA, BUF_SIZE)),
                         stringsAsFactors=FALSE )

initial_timestamp <- Sys.time()
alerts_counter <- 0


cardNumber <- ''   # current card number
errorCode <- 0
t0PIN <- 0         # input PIN started timestamp
t0 <- 0            # amount selected timestamp
PINAccepted <- FALSE
MesID <- ''
alert_signal <- 0

# user defined handler
## event handler is expected to have two arguments:
### 1) visit_id (integer vector of unit length),
### 2) atm_signal (character vector of unit length);
## and to return integer alert signal which is sent back to server
event_handler <- function(visit_id, atm_signal) {
    # log event if you need
    now <- Sys.time()
    message(now, ': visit_id=', visit_id, ', atm_signal=', atm_signal)
    
    # store event in event_data dataframe (alert signal is set at the end of handler)
    event_counter <<- event_counter + 1
    event_data[event_counter,] <<- list(now, visit_id, atm_signal, NA)
    
    
    # parse ATM signal (output: lst$MesID, lst$value - both characters!)
    lst <- parse_line(atm_signal)
    print(lst$MesID)
    MesID <<- lst$MesID
    if (MesID == 'CARD_INSERT') {
        cardNumber <<- lst$value
    }
    else if (MesID == 'ESD_SENSOR') {
      ESD <<- as.numeric(lst$value)
    }
    else if (MesID == 'PIN_INIT_START') {
      t0PIN <<- Sys.time()
    }
    else if (MesID == 'PIN_STOP') {
      tPIN <<- as.numeric(Sys.time() - t0PIN)
      tPIN.stop <<- Sys.time()
    }
    else if (MesID == "AMOUNT_SEL") {
      t0 <<- Sys.time()
    }
    else if (MesID == 'WDR_INIT_START') {
      t0Wdr <<- Sys.time()
    }
    else if (MesID == 'WDR_STOP') {
      tToWdr <<- Sys.time() - t0Wdr
    }
    else if (MesID == 'CARD_REMOVED') {
        # clean global vars
        errorCode <<- 0
        t0PIN <<- 0
        t0 <<- 0
        PINAccepted <<- FALSE
        alert_signal <<- 0
        tPIN.stop<<-0
        t0Wdr<<-0
        tToWdr <<- 0
        tPIN<<-0
    }
    
    # else ...
    
    
    # Scenario 1: cash dispense without preceding card entry & PIN entry
    if (MesID == "WDR_STOP"){
      x<-0
      for(i in 5:1){
        a<-tail(event_data[complete.cases(event_data),],6)[i,]
        b<-parse_line(a$atm_signal)
        if(b$MesID == "PIN_STOP")
          x<-x+1
        if(b$MesID == "PIN_INIT_START")
          x<-x+1
        if(b$MesID == "CARD_INSERT")
          x<-x+1
      }
      if(x<3){
        alert_signal <- 1
        print("SCENARIO 1 TRIGGERED")
      }
    }
    
    # Scenario 2: abnormal level of ESD signal -> skimming attack
    if (MesID == "ESD_SENSOR_CODE"){
      if(ESD>globalPar$ESD){
        alert_signal <- 1
        print("SCENARIO 2 TRIGGERED")
      }
    }
    
    # Scenario 3: PIN input time is abnormal for this customer
    if (MesID == "PIN_STOP"){
      low_lim<-custParams[rownames(custParams)==cardNumber,]$low
      high_lim<-custParams[rownames(custParams)==cardNumber,]$high
      
      if(low_lim != 0 | high_lim !=0){
        if(tPIN < low_lim | tPIN > high_lim){
          alert_signal <- 1
          print("SCENARIO 3 TRIGGERED")
        }
      }
    }
    
    # Scenario 4: time from selecting amount to cash dispense is abnormal
    if (MesID == "WDR_STOP"){
      duration <- as.numeric(tToWdr - tPIN.stop)
      if(duration > globalPar$perf){
        alert_signal <- 1
        print("SCENARIO 4 TRIGGERED")
      }
    }
    
#     # calc alert_signal example
#     alert_signal <- 0
#     if (difftime(now, initial_timestamp, unit="sec") > 30 && alerts_counter == 0)
#     {
#         alert_signal <- 1
#         alerts_counter <<- alerts_counter + 1
#     }
    
    # store alert_signal in event_data dataframe
    event_data$alert_signal[event_counter] <<- alert_signal
    
    return(alert_signal)
}


parse_line <- function (line) {
    str = unlist(strsplit(line,"[ ,]"))  # using <space> and <comma> as delimiters
    MessageID <- str[2]
    Value <- ifelse(length(str) >= 5, str[5], NA)
    return(list(MesID=MessageID, value=Value))
}


# server options
host <- "datastream.ilykei.com"
port <- 30006
login <- "durfey@uchicago.edu"
password <- "rnVGOrGr"
stream_name <- "ATM"
catch_handler_errors <- TRUE  # we recommend using TRUE during the test and FALSE during homework
# make connection with your personal handler
result <- Connect(host, port, login, password, stream_name, event_handler, catch_handler_errors)


# remove empty values from buffers
event_data <- event_data[!is.na(event_data$time),]

# after all you can dump your data/result and analyze it later
dump(c("event_data", "result"), file = "results.txt")
