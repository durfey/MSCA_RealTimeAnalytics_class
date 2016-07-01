library(evir)

# threshold: 0.25 may be a good value
# may encounter issue in for loop, with a singularity happening and exiting out of the loop
## in that case, need to find which iteration causes this, and change the threshold for that specific case

# Need to detect if something is going wrong, declare what is going wrong, and sound the alarm

# Need to check all 4 kinds of problems outlined in 1.0.3 "Project assumptions"

## ######################################


# load dataset
datapath<-"./"
eventLog <- read.table(paste0(datapath,"ATM_train.txt"), header = TRUE, 
                      sep = "|", stringsAsFactors = FALSE)
eventLog[1:4,]

# remove keyword MessageID (makes parsing later faster)
eventLog$event[] <- sub("MessageID: ","",eventLog$event)

# find CARD_INSERT in eventLog (these are the visits)
visits <- grep("CARD_INSERT",eventLog$event) # index of visit start
nVisits <- length(visits)
nVisits

# gather info about customers' visits
histData <- data.frame(ID = character(nVisits),      # card number
                      ESD = integer(nVisits),       # ESD value
                      tPIN = numeric(nVisits),      # time to enter PIN
                      tToWdr = numeric(nVisits),    # time from amount                                  
                      # selection to cash withdraw
                      wrongSequence = logical(nVisits), # TRUE in case of
                      # cash withdraw withoun PIN input
                      stringsAsFactors = FALSE)
head(histData)

# this function fills out the historical record of people
parseVisit <- function(start,  # row number with MessageID=CARD_INSERT
                       lst) {  # eventLog data frame
  PINEntered <- FALSE
  cardNumber <- substring(lst$event[start], nchar(lst$event[start])-15)
  for (i in (start+1):nrow(lst)) {
    str <- unlist(strsplit(eventLog$event[i],"[ ]"))
    if (str[1] == 'ESD_SENSOR,') ESDValue <- as.numeric(str[3])
    if (str[1] == 'PIN_INIT_START') t0PIN <- eventLog$time[i]
    if (str[1] == 'PIN_STOP') {
      PINTime <- eventLog$time[i] - t0PIN
      PINEntered <- TRUE
    }
    if (str[1] == 'AMOUNT_SEL') t0 <- eventLog$time[i]
    if (str[1] == 'WDR_INIT_START') {
      if (PINEntered) t0 <- eventLog$time[i] 
      else return(list(ID = cardNumber, ESD = ESDValue, tPIN = NA,
                       tToWdr = NA,wrongSequence = TRUE))
    }
    if (str[1] == 'WDR_STOP') fromSelToWdr <- eventLog$time[i] - t0
    if (str[1] == 'CARD_REMOVED') {
      return(list(ID = cardNumber, ESD = ESDValue, tPIN = PINTime,
                  tToWdr = fromSelToWdr,wrongSequence = FALSE))
    }
  }
}

# use the function on all visits
for (i in 1:nVisits) histData[i,] <- parseVisit(visits[i],eventLog)
head(histData)

# now we create some profile info (e.g., PIN input times) for all customers
customers <- sort(unique(histData$ID))
(nCustomers <- length(customers))
custParams <- data.frame(low = rep(0,nCustomers), high = rep(0,nCustomers),
                        min = rep(0,nCustomers), max = rep(0,nCustomers),
                        row.names = customers)
# ... these values will be calculated below based on critical values found


# now calculate critical values with evir
# confidence level, P
P = 0.999

# first, look at ESD signal values to find potential extreme values (bad guys)
# plots an estimate of a high quantile in the tail of a dataset baesd on GPD approximation
quant(histData$ESD, P,start = 0.05*length(histData$ESD), 
      end = 0.3*length(histData$ESD))
# NOTE: quant() also returns the table of values, which is very useful, but it's returned silently,
# so you need to assign the quant() output to a variable and output that variable in order for it to show up
# or just put the whole line in parentheses

# choose threshold in which 10% of observations exceed it
gpdESD = gpd(histData$ESD, nextremes = 0.25*length(histData$ESD), method = "ml")
ESDUpperLimit = as.numeric(riskmeasures(gpdESD, P)[,"quantile"])
c(ESDUpperLimit,max(histData$ESD))


# now look at time from cash amount selection to cash withdrawal
tToWdr = histData$tToWdr[is.finite(histData$tToWdr)]
quant(tToWdr, P, start = 0.05*length(tToWdr), 
      end = 0.3*length(tToWdr))

gpdPerf = gpd(tToWdr, nextremes = 0.25*length(tToWdr), method = "ml")
perfUpperLimit = as.numeric(riskmeasures(gpdPerf, P)[,"quantile"])
c(perfUpperLimit,max(tToWdr))

100*sum(tToWdr>perfUpperLimit)/length(tToWdr)

# estimate lower and upper limits of PIN input time for customers
PCust = 0.995
ID = rownames(custParams)[1]
tPIN = as.numeric(histData[histData$ID == ID & is.finite(histData$tPIN),
                           "tPIN"])
quant(tPIN, PCust,start = 0.2*length(tPIN), end = 0.4*length(tPIN) )

# similar to above cases, but take lower confidence level & higher threshold to ensure at least 30% observations are over it (this is due to our sample size being much smaller now)
# this gives upper limit
gpdtPIN = gpd(tPIN, nextremes = 0.35*length(tPIN), method = "ml")
tPINUpperLimit = riskmeasures(gpdtPIN, PCust)[,"quantile"]
c(tPINUpperLimit,max(tPIN))

# apply same procedure to -histData$ID gives lower limit estimate
quant(-tPIN, PCust,start = 0.2*length(tPIN), end = 0.4*length(tPIN) )

gpdtPIN = gpd(-tPIN, nextremes = 0.35*length(tPIN), method = "ml")
tPINLowerLimit = -as.numeric(riskmeasures(gpdtPIN, PCust)[,"quantile"])
c(tPINLowerLimit,min(tPIN))


# now estimate limits for other custParams

# for (ID in rownames(custParams)) {
#   tPIN = as.numeric(histData[histData$ID == ID & is.finite(histData$tPIN),
#                              "tPIN"])
#   print(length(histData$ID[histData$wrongSequence])) 
#   custParams[ID,c("min","max")] = c(min(tPIN),max(tPIN))
#   gpdtPIN = gpd(tPIN, nextremes = 0.35*length(tPIN), method = "ml")
#   custParams[ID,"high"] = riskmeasures(gpdtPIN, P)[,"quantile"]
#   gpdtPIN = gpd(-tPIN, nextremes = 0.35*length(tPIN), method = "ml")
#   custParams[ID,"low"] = -riskmeasures(gpdtPIN, P)[,"quantile"]
# }
# custParams

# need to run this a few times, because the loops exits when it hits a bump, so hit it again to restart it to the point after it fails
while(length(rownames(custParams[rowSums(custParams)==0,]))>0){
  for (ID in rownames(custParams[rowSums(custParams)==0,])){
    tPIN = as.numeric(histData[histData$ID == ID & is.finite(histData$tPIN),
                               "tPIN"])
    print(length(histData$ID[histData$wrongSequence])) 
    custParams[ID,c("min","max")] = c(min(tPIN),max(tPIN))
    gpdtPIN = gpd(tPIN, nextremes = 0.25*length(tPIN), method = "ml")
    custParams[ID,"high"] = riskmeasures(gpdtPIN, P)[,"quantile"]
    gpdtPIN = gpd(-tPIN, nextremes = 0.25*length(tPIN), method = "ml")
    custParams[ID,"low"] = -riskmeasures(gpdtPIN, P)[,"quantile"]
  }
}
# min = min time seen for PIN input by this customer
# max = max time seen for PIN input by this customer

#plot that shit!
plot(custParams$min, type="o", col="blue", ylim=range(custParams$min,custParams$max))
lines(custParams$low, type="l", pch=22, lty=2, col="red")
lines(custParams$max, type="o", pch=22, lty=2, col="blue")
lines(custParams$high, type="l", pch=22, lty=2, col="red")

# save limit values to files
globalPar = list(ESD = ESDUpperLimit, perf = perfUpperLimit)
save(globalPar, file = paste(datapath,'globalPar.rda',sep='/'))
save(custParams, file = paste(datapath,'custParams.rda',sep='/'))