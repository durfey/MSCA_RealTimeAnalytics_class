#############################################
# Notes/Code for Revenue Management
# Most of this is random notes & random code (plus lots of frustration...)


setwd("//Users/rdurfey/R_misc/RealTime/ProjectMaterials_RevenueManagement")
library(RM2)

## Run a simple EMRSb instance
Fare <- seq(50,250,25) #c(150, 100, 50, 250)
Mean <- c(75, 125, 500, 50)
Var <- c(75, 125, 500, 50)
cap <- 400
p <- EMSRb(Fare = Fare, Mean = Mean, Var = Var, cap = cap)
p



# Strategy example from workshop
set.seed(20)
Fare <- sort(c(150, 100, 50, 250),decreasing = TRUE)
cap <- 100

# set time duration
T = 3

# calculate intensity based on price. C is a constant that is set (arbitrarily?)
intens <- function(price) {
  C = 5000
  return(C/price)
}

# since we're assuming Poisson intensity, Mean = Var = Intensity
Mean = Var = intens(Fare)

# prepare the data frame of the trading session results
res = data.frame(timeInMimutes = numeric(cap),
                 price=numeric(cap),residualCapacity=numeric(cap),
                 futureIntensity=numeric(cap)) 

# start process of selling tickets, with initial price at minimum
t = i = 0
price = min(Fare)
while(t<T & cap>0){
  i = i + 1
  t = t + rexp(1,intens(price))
  p <- EMSRb(Fare = Fare, Mean = (T-t)* intens(Fare), 
             Var = (T-t)*intens(Fare), cap = cap)
  price = max(Fare[p==cap])
  cap = cap - 1
  res[i,] = c(round(t,2),price,cap,round(cap/(T-t),2))
}
print(res)

## NOTES
# We can change the our prices offered, as well as how many price points there are.
# (from Yuri) We'll probably need use more than just 4 price points, like what's in the workshop.
# Could probably do up to 50. Although the more price points, the more intensity estimating 
# will have to be done



intens <- function(price) {
  C = 5000
  return(C/price)
}
Fares <- seq(250,50,-25) # make sure they're sorted descending order
cap<-100
tot_time<-10
t<-t+rexp(1,ints[ints[,1]==price,2])
p <- EMSRb(Fare = Fares, Mean = (tot_time-t)* ints[,2],Var = (tot_time-t)* ints[,2], cap = cap)
p


####### Automating script to run source many times with different prices locked in #######
for(j in 11:20){
  for(i in seq(30,200,10)){
    init_price<<-i
    run<<-j
    source('~/R_misc/RealTime/ProjectMaterials_RevenueManagement/Yield_client_IntensityTesting.R')
  }
}

revenues<-c()
for(w in 1:11){
  source('~/R_misc/RealTime/ProjectMaterials_RevenueManagement/Yield_client_durfey.R')
  revenues<-rbind(revenues,this_revenue)
}

# ###########################
# # analyze output from script above
# it<-read.table("./IntensityTesting_outputs1.txt",header = TRUE,sep="\t",colClasses = c("POSIXct","integer"))
# 
# intensities<-c()
# for(j in unique(it[,2])){
#   int<-c(j,(length(it[it[,2]==j,2])-1)/as.numeric(difftime(tail(it[it[,2]==j,1],1),
#                                                              head(it[it[,2]==j,1],1),units="mins")))
#   intensities<-rbind(intensities,int)
# }
# intensities<-cbind(unique(it[,2]),table(it$price))
# save(intensities,file="./calc_intensities.Rdata") # load these into client file
# 
# intensities_2<-intensities
# intensities_1<-intensities
# 
# cbind(rbind(intensities,c(1,1),c(1,1)),intensities_2,intensities_1[intensities_1[,1] %in% intensities_2[,1],])


### read in multiple files to get mean/var of intensity

fare_sum<-c()
for(run in list.files()[grepl(list.files(),pattern="IntensityTesting_outputs_")]){
  file<-read.table((run),header = TRUE,sep="\t",colClasses = c("POSIXct","integer"))
  
  for(j in unique(file[,2])){
    if(length(file[file[,2]==j,2]) < 100){
      this_sum<-c(j,length(file[file[,2]==j,2])/10)}
    else{
      this_sum<-c(j,(length(file[file[,2]==j,2])-1)/
                    as.numeric(difftime(tail(file[file[,2]==j,1],1),
                                      head(file[file[,2]==j,1],1),units = "mins")))}
    fare_sum<-rbind(fare_sum,this_sum)
  }
}

fare_mean_var<-c()
for(k in unique(fare_sum[,1])){
  this_mean<-mean(fare_sum[fare_sum[,1]==k,2])
  this_var<-var(fare_sum[fare_sum[,1]==k,2])
  fare_mean_var<-rbind(fare_mean_var,c(k,this_mean,this_var,sum(fare_sum[,1]==k)))
}
fare_mean_var
save(fare_mean_var,file="./fare_mean_var.Rdata") # load these into client file

fare_mean_var_old<-fare_mean_var
cbind(fare_mean_var_old,fare_mean_var,fare_mean_var_old[,2]-fare_mean_var[,2])
cbind(fare_mean_var[,1],fare_mean_var[,1]*fare_mean_var[,2]*10,fare_mean_var[,4])

c(60,70,80,90,100,110,120)
seq(120,60,-10)

f<-fare_mean_var
f<-f[order(f[,1],decreasing = F),]

# mean
plot(df$X2~df$X2)

fit<- fitdist(f[,2],distr="lnorm")
plotdist(f[,2],distr="lnorm",para=list(fit$estimate[1],fit$estimate[2]))
fit<- fitdist(f[f[,1] %% 10==0 & f[,1]<=200,2],distr="lnorm")
plotdist(f[f[,1] %% 10==0 & f[,1]<=200,2],distr="lnorm",para=list(fit$estimate[1],fit$estimate[2]))

# variance
plot(f[,3]~f[,1])
fit2<- fitdist(f[f[,1] %% 10==0 & f[,1]<=200 & f[,3] >0,3],distr="lnorm")
plotdist(f[f[,1] %% 10==0 & f[,1]<=200 & f[,3] >0,3],distr="lnorm",para=list(fit2$estimate[1],fit2$estimate[2]))


mean_mu<-2.211709 #as.numeric(fit$estimate[1])
mean_sd<-0.6701783 #as.numeric(fit$estimate[2])
ln_mod<-gamlss(X2~X1,data=df,family="LOGNO",mean_mu,mean_sd)


df<-data.frame(f[f[,1] %% 10==0 & f[,1]<=200 & f[,3] >0,])

lmod<-lm(X2~log(X1),data=df)
predict(lmod,newdata=data.frame(X1=c(50)))

lmod2<-lm(X3~log(X1),data=df)
predict(lmod2,newdata=data.frame(X1=c(150)))

ll<-glm(X2~log(X1),data=df,family=gaussian(link="log"))
predict(ll,newdata=data.frame(X1=c(150)))



f<-fare_mean_var
f<-f[order(f[,1],decreasing = F),]
df<-data.frame(f[f[,1] %% 10==0 & f[,1]<=200 & f[,3] >0,])

mean_mod<-glm(X2~dlnorm(X1,fit$estimate[1],fit$estimate[2]),data=df)# ,family=gaussian(link="log"))
var_mod<-glm(X3~dlnorm(X1,fit2$estimate[1],fit2$estimate[2]),data=df)# ,family=gaussian(link="log"))

pri<-150
predict(mean_mod,newdata=data.frame(X1=c(pri)))
predict(var_mod,newdata=data.frame(X1=c(pri)))

save(list="mean_mod",file = "yield_mean_mod.Rdata")
save(list="var_mod",file = "yield_var_mod.Rdata")

################################
# to be plugged into client
Fares <- seq(250,50,-25) # make sure they're sorted descending order
fares_ints<-intensities[intensities[,1] %in% Fares,]
fares_ints<-ints[order(fares_ints[,1],decreasing = TRUE),]
################################

# test
price<-50
cap<-100
tot_time<-10
t<-0
t<-0
p <- EMSRb(Fare = Fares, Mean = fares_ints[,2]*cap,
           Var = fares_ints[,2]*cap, cap = cap)
p
