setwd("//Users/rdurfey/R_misc/RealTime/ProjectMaterials_TextAnalysis")
library(lda)
#!/usr/bin/env Rscript
## AirTweet_Client.R
#
# This file should contain student's handler that processes incoming tweets
# from the server in real time. 
####

source('AirTweet_Connection.R')
load("slda.model.Rdata")

# global vars
tweets_counter <- 0                                               # tweet event counter
BUF_SIZE <- 1000                                                  # we create buffers in advance:
received_data <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),     # dataframe for received tweets
                            text=character(BUF_SIZE),
                            negative_prob=as.numeric(rep(NaN, BUF_SIZE)),
                            neutral_prob=as.numeric(rep(NaN, BUF_SIZE)),
                            positive_prob=as.numeric(rep(NaN, BUF_SIZE)),
                            stringsAsFactors=FALSE )

library(lda)

# user defined handler
## arguments:
#### text - character vector of unit length
## returns:
#### sentiment probability vector of length 3
new_tweet_handler <- function(text) {
    now <- Sys.time()
    # log event
    message(now, ' : ', text)
    
    # update dataframe (store probability after classification)
    tweets_counter <<- tweets_counter + 1
    received_data[tweets_counter,] <<- list(now, text, NaN, NaN, NaN)
    
    
    ################### documents prep ###############
    # clean tweets
    texts2 <- gsub("http[^[:space:]]+", " ", received_data[tweets_counter,]$text)  # remove http links
    texts2 <- gsub("[^a-zA-Z']+", " ", texts2)        # change non-letter sequences to whitespaces
    texts2 <- gsub("(^ | $)", "", texts2)             # remove whitespace at beginning and end of documents
    texts2 <<- tolower(texts2)                         # force to lowercase
    head(texts2)
    
    # split documents to words:
    doc.list2 <- strsplit(texts2, " ")
    
#     # put the documents into the format required by the lda package:
#     get.terms <- function(x) { # x is vector of words
#       index <- match(x, vocab)
#       index <- index[!is.na(index)]
#       rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
#     }
    
    documents2 <- lapply(doc.list2, get.terms)
    
    ###################################################
    
    
    # calculate probabilities (some sophisticated model usage is expected to be here)
    #sentiment_probabilities <- c(0, 1, 0)  # (negative, neutral, positive) probabilities

    predictions2 <- slda.predict(documents2,
                                fit$topics, 
                                fit$model,
                                alpha=alpha,
                                eta=eta)
    
    # calculate negative, neutral, and positive probabilities from predicted rating
    neg<-densfun_neg(predictions2)
    neut<-densfun_neut(predictions2)
    pos<-densfun_pos(predictions2)
    
    sentiment_probabilities<-c(neg,neut,pos)/sum(neg,neut,pos)

        
    # log result
    message('Predicted probabilities: ', paste0(sentiment_probabilities, ' '))
    
    # store probabilities in global dataframe
    received_data$negative_prob[tweets_counter] <<- sentiment_probabilities[1]
    received_data$neutral_prob[tweets_counter] <<- sentiment_probabilities[2]
    received_data$positive_prob[tweets_counter] <<- sentiment_probabilities[3]
    
    return(sentiment_probabilities)
}


# server options
host <- "datastream.ilykei.com"
port <- 30009
login <- "durfey@uchicago.edu"
password <- "rnVGOrGr"
stream_name <- "AirTweet"
catch_handler_errors <- TRUE  # we recommend using TRUE during the test and FALSE during homework
# make connection with your personal handler
result <- Connect(host, port, login, password, stream_name, new_tweet_handler, catch_handler_errors)

# remove empty values from buffers
received_data <- received_data[!is.na(received_data$time),]

# after all you can dump your data/result and analyze it later
dump(c("received_data", "result"), file = "results.txt")

