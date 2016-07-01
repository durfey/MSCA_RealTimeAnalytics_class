###########################################
# notes/code for Text Analysis Assignment #
###########################################
setwd("//Users/rdurfey/R_misc/RealTime/ProjectMaterials_TextAnalysis")

library(lda)
library(tm)
library(ggplot2)

# read csv-file
df <- read.csv(file=paste(dataPath,'air_tweet_train.csv',sep="/"), 
               col.names = c('sentiment','text'),
               stringsAsFactors = F)
head(df)


# generate stop words
aircompanies_accounts <- c('VirginAmerica', 'United', 'SouthwestAir', 'JetBlue', 'Delta', 'USAirways', 'AmericanAir')
other_sw <- c('fly', 'flying', 'flight', 'flights', 'plane')
stop_words <- c(tm::stopwords("SMART"), tolower(aircompanies_accounts), other_sw )
stop_words

# all tweets
texts <- df$text
head(texts)

# tweet sentiments
sentiment <- df$sentiment
sentiment.f <- rep('Neutral', length(sentiment))
sentiment.f[sentiment < 0] <- 'Negative'
sentiment.f[sentiment > 0] <- 'Positive'
sentiment.f <- as.factor(sentiment.f)
head(sentiment)

# clean tweets
texts <- gsub("http[^[:space:]]+", " ", texts)  # remove http links
texts <- gsub("[^a-zA-Z']+", " ", texts)        # change non-letter sequences to whitespaces
texts <- gsub("(^ | $)", "", texts)             # remove whitespace at beginning and end of documents
texts <- tolower(texts)                         # force to lowercase
head(texts)

# split documents to words:
doc.list <- strsplit(texts, " ")
head(doc.list)

# compute table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

head(term.table, 10) #  most popular words

tail(term.table, 10) # least popular words

# remove terms that are stop words or occur fewer than 5 times or have length < 3:
del_idx <- names(term.table) %in% stop_words | term.table < 5 | nchar(names(term.table)) < 3
term.table <- term.table[!del_idx]
head(term.table, 10) #  most popular words after cleaning

tail(term.table, 10) # least popular words after cleaning

# generate vocabulary
vocab <- names(term.table)
str(vocab)

# put the documents into the format required by the lda package:
get.terms <- function(x) { # x is vector of words
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

documents <- lapply(doc.list, get.terms)
head(documents)

shortVector<-c("cancelled","like","great","bad")
get.terms(shortVector)

match(shortVector, vocab)

vocab[69]

vocab[22]

vocab[1]

# remove documents that do not contain words from vocab
notempty_documents_idx <- sapply(documents, function(doc) ncol(doc) > 0 )
head(notempty_documents_idx)

(corp<-summary(notempty_documents_idx))


# fit model
set.seed(12345)
num.topics <- 12
alpha <- 1
eta <- 0.1
params <- sample(c(-1,0, 1), num.topics, replace=TRUE)
fit <- slda.em(documents=documents[notempty_documents_idx],
               K=num.topics,
               vocab=vocab,
               num.e.iterations=100,
               num.m.iterations=40,
               alpha=alpha, #Dirichlet distribution parmeter for prior for theta_d
               eta=eta,  #Dirichlet distribution parameter for prior for beta_k
               annotations=sentiment[notempty_documents_idx], 
               params=params,  #numeric vector of initial regression coefficients for EM
               variance=0.25, # initial value for response variance
               method="sLDA")

summary(fit$model)


# print top 5 words for each topic
top.topic.words(fit$topics, 5, by.score=TRUE)

# plot coefs for each topic
Topics <- apply(top.topic.words(fit$topics, 5, by.score=TRUE), 2, paste, collapse=" ")
coefs <- data.frame(coef(summary(fit$model)))
coefs <- cbind(coefs, Topics=factor(Topics, Topics[order(coefs$Estimate)]))
(coefs <- coefs[order(coefs$Estimate),])


# qplot(Topics, Estimate, colour=Estimate, size=abs(t.value), data=coefs) +
#   geom_errorbar(width=0.5, aes(ymin=Estimate-Std..Error, ymax=Estimate+Std..Error)) + 
#   coord_flip()


# PREDICTION
set.seed(12345)
predictions <- slda.predict(documents,
                            fit$topics, 
                            fit$model,
                            alpha=alpha,
                            eta=eta)
head(predictions)
range(predictions[!is.nan(predictions)])

# plot predictions for negative, neutral and positive tweets
set.seed(12345)
qplot(predictions,
      fill=sentiment.f,
      xlab = "predicted rating",
      ylab = "density",
      alpha=I(0.5),
      geom="density") +
  geom_vline(aes(xintercept=0)) +
  geom_vline(aes(xintercept=-.65),col="red",lwd=2) +
  geom_vline(aes(xintercept=.08),col="blue",lwd=2)


# Based on Predictions for entire training set, estimate Density Functions that will then be used in creation of probability vector
negs<-predictions[sentiment.f=="Negative"]
densfun_neg<-approxfun(density(negs[!is.nan(negs)]))
#densfun_neg(0.1)

neuts<-predictions[sentiment.f=="Neutral"]
densfun_neut<-approxfun(density(neuts[!is.nan(neuts)]))
#densfun_neut(0.1)

pos<-predictions[sentiment.f=="Positive"]
densfun_pos<-approxfun(density(pos[!is.nan(pos)]))
#densfun_pos(0.1)


# confusion matrices from training
confusion<-table(data.frame(predict=findInterval(predictions,c(-.65,.8)),actual=sentiment.f))
rownames(confusion)<-c("Negative","Neutral","Positive")
confusion


# something about bin indexes....
binIdx<-findInterval(predictions,c(-.1,.1))==1
probVec<-table(sentiment[binIdx])
(probVec<-probVec/sum(probVec))


# save model in binary to use it in real time handler: INCLUDE DENSITY FUNCTIONS!
save(list=c('stop_words', 'vocab', 'get.terms', 'alpha', 'eta', 'fit', 
            'densfun_neg', 'densfun_neut', 'densfun_pos'), file = "slda.model.Rdata")
