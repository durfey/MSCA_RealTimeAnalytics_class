setwd("//Users/rdurfey/R_misc/RealTime/ProjectMaterials_HFT4")

datapath<-"./"
trades <- read.csv(file=paste(datapath,'hft4_train.csv',sep="/"))
plot(trades$time, trades$price, type='s')
head(trades)

plot(trades$time, trades$price, type='s')


# estimating Cox process

# cut interval [2000, 4000]:
trades <- trades[trades$time > 2000 & trades$time < 4000,]
trades$time <- trades$time - 2000
rownames(trades) <- NULL
head(trades)


#############################################
# RUN CLIENT CODE WITH DIFFERENT PARAMETERS #
#############################################

width <<- 750
confidenceLevel <<- 0.95

# old_penalty <<-13000
# old_width <<- 0
# old_confidenceLevel <<- 0

# width_done <<- FALSE
# confidenceLevel_done <<-FALSE
# 
# while(width_done==FALSE){
#   
#   source('~/R_misc/RealTime/ProjectMaterials_HFT4/hft4_client.R')
#   
#   if(confidenceLevel_done==FALSE){
#     confidenceLevel<<-confidenceLevel-0.5
#     }
#   
#   if(width>=800 & confidenceLevel_done==TRUE){width_done==TRUE}
#   confidenceLevel <<- confidenceLevel - 0.05;
# }

done_j<-seq(700,900,50)
done_i<-0.96

for(i in seq(0.965,0.955,by=-0.005)){
  for(j in seq(900,700,by=-10)){
    width<<-j
    confidenceLevel<<-i
    if(!(i %in% done_i & j %in% done_j))
      source('~/R_misc/RealTime/ProjectMaterials_HFT4/hft4_client.R')
  }
}


tests<-read.table("./outputs.txt",header=T)
tests[order(tests$Penalty),]
