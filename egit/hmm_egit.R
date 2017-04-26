library (HMM)
library (lattice)
library(ggplot2)
library(mhsmm)
library(MASS)

setwd("C:\\Users\\vhonsel\\Documents\\Mining\\Case Studies\\Egit")
#commit data
committers = read.csv("commits_egit.csv", header=T, sep=",", stringsAsFactors=FALSE)
#mailing list data
ml = read.csv("ml_egit.csv", header=T,  sep=",", stringsAsFactors=FALSE)
#bugzilla data
bz_comments = read.csv("bz_comments_egit.csv", header=T, sep=",", stringsAsFactors=FALSE)

committers <- committers[grep('Script',committers$name,invert = TRUE),]
committers <- committers[grep('script',committers$name,invert = TRUE),]
committers <- committers[grep('No Author',committers$name,invert = TRUE),]

for (s in 1:nrow(committers)){
  
  committers$month[s] <- substr(committers$commit_date[s],1,7) 
  
} 


for (t in 1:nrow(committers)){
  
  committers$noc[t] <- sum(committers$name == committers$name[t]) 
  
} 

# omit developers with less than 50 commits

committers <- committers[committers$noc > 50,]

for (u in 1:nrow(committers)){
  
  committers$dnoc[u] <- sum(committers$month == committers$month[u] & committers$name == committers$name[u] & committers$is_bug_fix != 1) 
  
}

for (v in 1:nrow(committers)){
  
  committers$bugfixes[v] <- sum(committers$month == committers$month[v] & committers$name == committers$name[v] & committers$is_bug_fix == 1) 
  
}

devcom_per_m <- committers[, c(14,17,19,20)]
devcom_per_m <- unique(devcom_per_m)

print(ggplot(devcom_per_m, aes(x=devcom_per_m$month, y=devcom_per_m$noc, colour=devcom_per_m$name, group=devcom_per_m$name)) 
      #   + geom_point() 
      + geom_line(aes(devcom_per_m$month, devcom_per_m$dnoc)) 
      + theme(axis.text.x=element_text(angle=-90)) 
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="commits"))

print(ggplot(devcom_per_m, aes(x=devcom_per_m$month, y=devcom_per_m$bugfixes, colour=devcom_per_m$name, group=devcom_per_m$name)) 
      #   + geom_point() 
      + geom_line(aes(devcom_per_m$month, devcom_per_m$bugfixes)) 
      + theme(axis.text.x=element_text(angle=-90)) 
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="bugfixes"))


#communication activities

for (s in 1:nrow(ml)){
  
  ml$month[s] <- substr(ml$msg_date[s],1,7) 
  
  } 


mlComm <-ml[grep("dev84|dev83|Baumgart, Jens|Kevin|Kinzler|Sohn|Suen|Rosenberg|Stocker|Lay|Zarna",ml$msg_sender_name),]


for (s in 1:nrow(mlComm)){
  
  if (mlComm$msg_sender_name[s]=="portal on behalf of dev84")
  {
      mlComm$msg_sender_name[s]<-"dev84"
  }
} 

for (s in 1:nrow(mlComm)){
  
  if (mlComm$msg_sender_name[s]=="portal on behalf of dev83")
  {
    mlComm$msg_sender_name[s]<-"dev83"
  }
} 

for (s in 1:nrow(mlComm)){
  
  if (mlComm$msg_sender_name[s]=="portal on behalf of dev14")
  {
    mlComm$msg_sender_name[s]<-"dev14"
  }
} 

for (s in 1:nrow(mlComm)){
  
  if (mlComm$msg_sender_name[s]=="portal on behalf of dev35")
  {
    mlComm$msg_sender_name[s]<-"dev35"
  }
} 

for (s in 1:nrow(mlComm)){
  
  if (mlComm$msg_sender_name[s]=="Kinzler, Mathias")
  {
    mlComm$msg_sender_name[s]<-"dev35"
  }
} 


for (s in 1:nrow(mlComm)){
  
  if (mlComm$msg_sender_name[s]=="portal on behalf of dev12")
  {
    mlComm$msg_sender_name[s]<-"dev12"
  }
} 

for (s in 1:nrow(mlComm)){
  
  if (mlComm$msg_sender_name[s]=="Sohn, Matthias")
  {
    mlComm$msg_sender_name[s]<-"dev12"
  }
} 

for (s in 1:nrow(mlComm)){
  
  if (mlComm$msg_sender_name[s]=="portal on behalf of dev82")
  {
    mlComm$msg_sender_name[s]<-"dev82"
  }
} 

for (s in 1:nrow(mlComm)){
  
  if (mlComm$msg_sender_name[s]=="Remy Chi Jian Suen")
  {
    mlComm$msg_sender_name[s]<-"dev82"
  }
} 

for (s in 1:nrow(mlComm)){
  
  if (mlComm$msg_sender_name[s]=="portal on behalf of dev51")
  {
    mlComm$msg_sender_name[s]<-"dev51"
  }
} 

for (s in 1:nrow(mlComm)){
  
  if (mlComm$msg_sender_name[s]=="portal on behalf of dev81")
  {
    mlComm$msg_sender_name[s]<-"dev81"
  }
} 

for (s in 1:nrow(mlComm)){
  
  if (mlComm$msg_sender_name[s]=="Lay, Stefan")
  {
    mlComm$msg_sender_name[s]<-"dev81"
  }
} 


for (t in 1:nrow(mlComm)){
  
  mlComm$activities[t] <- sum(mlComm$msg_sender_name[t] == mlComm$msg_sender_name)
  mlComm$opened[t] <- sum(mlComm$month[t] == mlComm$month & mlComm$msg_sender_name[t] == mlComm$msg_sender_name & mlComm$parent_id == 'NULL')
  mlComm$responses[t] <- sum(mlComm$month[t] == mlComm$month & mlComm$msg_sender_name[t] == mlComm$msg_sender_name & mlComm$parent_id != 'NULL')
  
} 

print(ggplot(mlComm, aes(x=mlComm$month, y=mlComm$responses, colour=mlComm$msg_sender_name, group=mlComm$msg_sender_name)) 
      #   + geom_point() 
      + geom_line(aes(mlComm$month, mlComm$responses)) 
      + theme(axis.text.x=element_text(angle=-90)) 
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="responses"))

print(ggplot(mlComm, aes(x=mlComm$month, y=mlComm$opened, colour=mlComm$msg_sender_name, group=mlComm$msg_sender_name)) 
      #   + geom_point() 
      + geom_line(aes(mlComm$month, mlComm$opened)) 
      + theme(axis.text.x=element_text(angle=-90)) 
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="threads opened"))


# bug activities

for (u in 1:nrow(bz_comments)){
  
  bz_comments$month[u] <- substr(bz_comments$commentTime[u],1,7) 
  
} 


for (x in 1:nrow(bz_comments)){
  
  bz_comments$nocomm[x] <- sum(bz_comments$commentAuthor[x] == bz_comments$commentAuthor & bz_comments$month[x] == bz_comments$month)
  
}


bz_comments_dev <-bz_comments[grep("dev84|dev83|dev52|dev14|Kinzler|Sohn|Suen|Rosenberg|Stocker|Lay|Zarna",bz_comments$commentAuthor),]


print(ggplot(bz_comments_dev, aes(bz_comments_dev$month, y=bz_comments_dev$nocomm, colour=bz_comments_dev$commentAuthor, group=bz_comments_dev$commentAuthor))
      + geom_line(aes(bz_comments_dev$month, bz_comments_dev$nocomm))
      + theme(axis.text.x=element_text(angle=-90))
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="number of bug comments"))

# prepare for threshold learner

contributions <- committers[, c(14,17,19,20)]
contributions <- unique(contributions)

communication <- mlComm[,c(2,8,10,11)]
communication <- unique(communication)

comments <- bz_comments_dev[,c(4,9,10)]
comments <- unique(comments)

write.csv(contributions, "contributions.csv", row.names=FALSE)
write.csv(communication, "communication.csv", row.names=FALSE)
write.csv(comments, "bug_comments.csv", row.names=FALSE)

for (l in 1:nrow(communication)) {
  communication$activities[l] <- communication$opened[l] + communication$responses[l]
}

write.csv(communication, "communication_summarized.csv", row.names=FALSE)

#thresholds: commits  3/11   , bugfixes 1/1 , ml activity  6/11 , bug comments  9/26 

# classify by thresholds

for (k in 1:nrow(contributions)) {
  if (contributions$dnoc[k] < 3) 
  {
    contributions$state[k] <- "low"
  }
  if (contributions$dnoc[k] < 12 && contributions$dnoc[k] > 2) 
  {
    contributions$state[k] <- "medium"
  }
  if (contributions$dnoc[k] > 11 && contributions$bugfixes[k] > 0) 
  {
    contributions$state[k] <- "high"
  }
}

for (k in 1:nrow(communication)) {
  if (communication$activities[k] < 6) 
  {
    communication$state2[k] <- "low"
  }
  if (communication$activities[k] < 11 && communication$activities[k] > 5) 
  {
    communication$state2[k] <- "medium"
  }
  if (communication$activities[k] > 10) 
  {
    communication$state2[k] <- "high"
  }
}

for (k in 1:nrow(comments)) {
  if (comments$nocomm[k] < 9) 
  {
    comments$state[k] <- "low"
  }
  if (comments$nocomm[k] < 26 && comments$nocomm[k] > 8) 
  {
    comments$state[k] <- "medium"
  }
  if (comments$nocomm[k] > 25) 
  {
    comments$state[k] <- "high"
  }
}

#determine timespan

write.csv(contributions, "contributions.csv", row.names=FALSE)
write.csv(communication, "communication.csv", row.names=FALSE)
write.csv(comments, "bug_comments.csv", row.names=FALSE)

timespan <- sort(unique(contributions$month))


#---------------------------------- train model -----------------------------------------

# --------------------------------- functions ------------------------------------------

calculateState <- function (state1,state2,state3) {
  if(state1 == "low") {
    state1 = 1
  }
  if(state2 == "low") {
    state2 = 1
  }
  if(state3 == "low") {
    state3 = 1
  }
  if(state1 == "medium") {
    state1 = 2
  }
  if(state2 == "medium") {
    state2 = 2
  }
  if(state3 == "medium") {
    state3 = 2
  }
  if(state1 == "high") {
    state1 = 3
  }
  if(state2 == "high") {
    state2 = 3
  }
  if(state3 == "high") {
    state3 = 3
  }
  
  tmp <- state1+state2+state3
  
  if(tmp<5){
    stateLabel <- "low"
  }
  if(tmp<7 & tmp>4){
    stateLabel <- "medium"
  }
  if(tmp>6){
    stateLabel <- "high"
  }
  return(stateLabel)
}

estimatePar <- function (obs1,obs2,obs3,obs4) {
  
  #indexSet1 <- sample(1:length(obs1), length(obs1)/3, replace=F)
  #indexSet2 <- sample(setdiff(1:length(obs1), indexSet1), length(obs1)/3, replace=F)
  #indexSet3 <- setdiff(setdiff(1:length(obs1),indexSet1), indexSet2)
  #indexSet3 <- setdiff(1:length(obs1), union(indexSet1, indexSet2))
  
  obs1mu1 <- mean(obs1[indexSet1])
  obs1mu2 <- mean(obs1[indexSet2])
  obs1mu3 <- mean(obs1[indexSet3])
  obs1sigma1 <- var(obs1[indexSet1])
  obs1sigma2 <- var(obs1[indexSet2])
  obs1sigma3 <- var(obs1[indexSet3])
  
  obs2mu1 <- mean(obs2[indexSet1])
  obs2mu2 <- mean(obs2[indexSet2])
  obs2mu3 <- mean(obs2[indexSet3])
  obs2sigma1 <- var(obs2[indexSet1])
  obs2sigma2 <- var(obs2[indexSet2])
  obs2sigma3 <- var(obs2[indexSet3])
  
  obs3mu1 <- mean(obs3[indexSet1])
  obs3mu2 <- mean(obs3[indexSet2])
  obs3mu3 <- mean(obs3[indexSet3])
  obs3sigma1 <- var(obs3[indexSet1])
  obs3sigma2 <- var(obs3[indexSet2])
  obs3sigma3 <- var(obs3[indexSet3])
  
  obs4mu1 <- mean(obs4[indexSet1])
  obs4mu2 <- mean(obs4[indexSet2])
  obs4mu3 <- mean(obs4[indexSet3])
  obs4sigma1 <- var(obs4[indexSet1])
  obs4sigma2 <- var(obs4[indexSet2])
  obs4sigma3 <- var(obs4[indexSet3])
  
  cov1 <- cov(matrix(c(obs1[indexSet1], obs2[indexSet1], obs3[indexSet1], obs4[indexSet1]), ncol=4))
  cov2 <- cov(matrix(c(obs1[indexSet2], obs2[indexSet2], obs3[indexSet2], obs4[indexSet2]), ncol=4))
  cov3 <- cov(matrix(c(obs1[indexSet3], obs2[indexSet3], obs3[indexSet3], obs4[indexSet3]), ncol=4))
  
  obslist <<- list(c(obs1mu1, obs2mu1, obs3mu1, obs4mu1), c(obs1mu2, obs2mu2, obs3mu2, obs4mu2), c(obs1mu3, obs2mu3, obs3mu3, obs4mu3))
  tmp <<- obslist
  sigmalist <<- list(cov1,cov2,cov3)
  if (length(indexSet3)==0) {
    obslist <- obslist[-3]
    sigmalist <- sigmalist[-3]
  }
  if (length(indexSet2)==0) {
    obslist <- obslist[-2]
    sigmalist <- sigmalist[-2]
  }
  if (length(indexSet1)==0) {
    obslist <- obslist[-1]
    sigmalist <- sigmalist[-1]
  }
  
  emis <- list(mu = obslist, sigma = sigmalist)
  
  
  #   cov1 <- matrix(c(obs1sigma1, cov(obs1[indexSet1],obs2[indexSet1]), cov(obs1[indexSet1],obs3[indexSet1]), cov(obs1[indexSet1],obs4[indexSet1]),
  #                    cov(obs2[indexSet1],obs1[indexSet1]), obs2sigma1, cov(obs2[indexSet1],obs3[indexSet1]), cov(obs2[indexSet1],obs4[indexSet1]),
  #                    cov(obs3[indexSet1],obs1[indexSet1]), cov(obs3[indexSet1],obs2[indexSet1]), obs3sigma1, cov(obs3[indexSet1],obs4[indexSet1]),
  #                    cov(obs1[indexSet1],obs1[indexSet1]), cov(obs1[indexSet1],obs1[indexSet1]), cov(obs1[indexSet1],obs1[indexSet1]), obs4sigma1), ncol =4)
  #   cov2 <- matrix(c(obs1sigma2, cov(obs1[indexSet2],obs2[indexSet2]), cov(obs1[indexSet2],obs3[indexSet2]), cov(obs1[indexSet2],obs4[indexSet2]),
  #                    cov(obs2[indexSet2],obs1[indexSet2]), obs2sigma2, cov(obs2[indexSet2],obs3[indexSet2]), cov(obs2[indexSet2],obs4[indexSet2]),
  #                    cov(obs3[indexSet2],obs1[indexSet2]), cov(obs3[indexSet2],obs2[indexSet2]), obs3sigma2, cov(obs3[indexSet2],obs4[indexSet2]),
  #                    cov(obs4[indexSet2],obs1[indexSet2]), cov(obs4[indexSet2],obs2[indexSet2]), cov(obs4[indexSet2],obs3[indexSet2]), obs4sigma2), ncol =4)
  #   cov3 <- matrix(c(obs1sigma3, cov(obs1[indexSet3],obs2[indexSet3]), cov(obs1[indexSet3],obs3[indexSet3]), cov(obs1[indexSet3],obs4[indexSet3]),
  #                    cov(obs2[indexSet3],obs1[indexSet3]), obs2sigma3, cov(obs2[indexSet3],obs3[indexSet3]), cov(obs2[indexSet3],obs4[indexSet3]),
  #                    cov(obs3[indexSet3],obs1[indexSet3]), cov(obs3[indexSet3],obs2[indexSet3]), obs3sigma3, cov(obs3[indexSet3],obs4[indexSet3]),
  #                    cov(obs4[indexSet3],obs1[indexSet3]), cov(obs4[indexSet3],obs2[indexSet3]), cov(obs4[indexSet3],obs3[indexSet3]), obs4sigma3), ncol =4)
  
  #  emis <- list(mu = list(c(obs1mu1, obs2mu1, obs3mu1, obs4mu1), c(obs1mu2, obs2mu2, obs3mu2, obs4mu2), c(obs1mu3, obs2mu3, obs3mu3, obs4mu3)),
  #sigma = list(matrix(c(obs1sigma1, 0, 0, 0, obs1sigma2, 0, 0, 0, obs1sigma3), ncol = 3), 
  #              matrix(c(obs2sigma1, 0, 0, 0, obs2sigma2, 0, 0, 0, obs2sigma3), ncol = 3),
  #              matrix(c(obs3sigma1, 0, 0, 0, obs3sigma2, 0, 0, 0, obs3sigma3), ncol = 3),
  #              matrix(c(obs4sigma1, 0, 0, 0, obs4sigma2, 0, 0, 0, obs4sigma3), ncol = 3)))
  #sigma = list(t(matrix(c(obs1sigma1, obs2sigma1, obs3sigma1, obs4sigma1, obs1sigma2, obs2sigma2, obs3sigma2, obs4sigma2, obs1sigma3, obs2sigma3, obs3sigma3, obs4sigma3), ncol=3)),
  #             t(matrix(c(obs1sigma1, obs2sigma1, obs3sigma1, obs4sigma1, obs1sigma2, obs2sigma2, obs3sigma2, obs4sigma2, obs1sigma3, obs2sigma3, obs3sigma3, obs4sigma3), ncol=3)),
  #             t(matrix(c(obs1sigma1, obs2sigma1, obs3sigma1, obs4sigma1, obs1sigma2, obs2sigma2, obs3sigma2, obs4sigma2, obs1sigma3, obs2sigma3, obs3sigma3, obs4sigma3), ncol=3))))
  #              sigma = list(cov1,cov2,cov3))
  
  return(emis)
}

trans <- t(matrix(c(0.6,0.3,0.1,0.3,0.5,0.2,0.1,0.4,0.5),3))

setInit <- function() {
  init <- c(0.8,0.15,0.05)
  if (length(indexSet3)==0) {
    init <- init[-3]
  }
  if (length(indexSet2)==0) {
    init <- init[-2]
  }
  if (length(indexSet1)==0) {
    init <- init[-1]
  }
  if( sum(init)!=1 ) {
    init <- init/sum(init)
  }
  return(init)
}

setTrans <- function() {
  trans <- t(matrix(c(0.6,0.3,0.1,
                      0.3,0.5,0.2,
                      0.1,0.4,0.5),3))
  if (length(indexSet3)==0) {
    trans <- trans[-3,]
    trans <- trans[,-3]
  }
  if (length(indexSet2)==0) {
    trans <- trans[-2,]
    trans <- trans[,-2]
  }
  if (length(indexSet1)==0) {
    trans <- trans[-1,]
    trans <- trans[,-1]
  }
  for( i in 1:ncol(trans) ) {
    if( sum(trans[i,])!=1 ) {
      trans[i,] <- trans[i,]/sum(trans[i,])
    }
  }
  return(trans)
}


mstep.mvnorm <- function(x, wt) {
  emission <- list(mu = list(), sigma = list())
  print(x)
  print(wt)
  for(i in 1:ncol(wt)) {
    tmp <- cov.wt(x, wt[,i])
    emission$mu[[i]] <- tmp$center
    emission$sigma[[i]] <- tmp$cov
  }
  emission
}

rmvnorm.hsmm <- function(j, model) rmvnorm(1, model$parms.emission$mu[[j]], model$parms.emission$sigma[[j]])

dmvnorm.hsmm <- function(x, j, model) dmvnorm(x, model$parms.emission$mu[[j]], model$parms.emission$sigma[[j]])

# -------------------------------------------------------------------------------------------------------------------------------------------

# ------------------------------------------ dev 1 -----------------------------------------------------

contributionStatesDev1 <- contributions[contributions$name == "dev12",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev1$month==timespan[i])) {
  }
  else {
    contributionStatesDev1 <- rbind(contributionStatesDev1,c(contributionStatesDev1$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev1 <- contributionStatesDev1[order(contributionStatesDev1$month),]
#contributionStatesDev1 <- contributionStatesDev1[contributionStatesDev1$month<"2014-08",]
colnames(contributionStatesDev1)[5] <- "state1"

bugStatesDev1 <- comments[comments$commentAuthor == "dev12",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev1$month==timespan[i])) {
  }
  else {
    bugStatesDev1 <- rbind(bugStatesDev1,c(bugStatesDev1$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev1 <- bugStatesDev1[order(bugStatesDev1$month),]
bugStatesDev1 <- bugStatesDev1[bugStatesDev1$month>"2009-09",]
colnames(bugStatesDev1)[4] <- "state2"

commStatesDev1 <- communication[communication$msg_sender_name == "dev12",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev1$month==timespan[i])) {
  }
  else {
    commStatesDev1 <- rbind(commStatesDev1,c(commStatesDev1$name[1],timespan[i],0,0,0,"low"))
  }
}

commStatesDev1 <- commStatesDev1[order(commStatesDev1$month),]
commStatesDev1 <- commStatesDev1[commStatesDev1$month>"2009-09",]
commStatesDev1 <- commStatesDev1[commStatesDev1$month<"2014-09",]
colnames(commStatesDev1)[6] <- "state3"

classObsDev1 <- cbind(contributionStatesDev1,bugStatesDev1,commStatesDev1)

for (i in 1:nrow(classObsDev1)) {
  
  classObsDev1$stateLabel[i] <- calculateState(classObsDev1$state1[i],classObsDev1$state2[i],classObsDev1$state3[i])
  
}

indexSet1 <- sample(which(classObsDev1$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev1$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev1$stateLabel=="high"))

obs1 <- as.numeric(contributionStatesDev1$dnoc)
obs2 <- as.numeric(contributionStatesDev1$bugfixes)
obs3 <- as.numeric(commStatesDev1$activities)
obs4 <- as.numeric(bugStatesDev1$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train1 <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm1 <- hmmfit(train1, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev1$dnoc))
obs <- cbind(as.numeric(contributionStatesDev1$dnoc),as.numeric(contributionStatesDev1$bugfixes),as.numeric(commStatesDev1$activities),as.numeric(bugStatesDev1$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"

plot(trainDev)
pred1 <- predict(hmm1, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred1$s)

plot(pred1$s,type="l")
par(mar=c(5.1,4.1,4.1,4.1)) 
plot(pred1$s,type="l", yaxt = "n", xlab = "time", ylab="state")
axis(4, seq(1, 3, by = 1), labels=c("low","medium","high"), las=1)



#--------------------------------- dev2 -------------------------------------------

contributionStatesDev2 <- contributions[contributions$name == "dev13",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev2$month==timespan[i])) {
  }
  else {
    contributionStatesDev2 <- rbind(contributionStatesDev2,c(contributionStatesDev2$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev2 <- contributionStatesDev2[order(contributionStatesDev2$month),]
colnames(contributionStatesDev2)[5] <- "state1"

bugStatesDev2 <- comments[comments$commentAuthor == "dev13",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev2$month==timespan[i])) {
  }
  else {
    bugStatesDev2 <- rbind(bugStatesDev2,c(bugStatesDev2$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev2 <- bugStatesDev2[order(bugStatesDev2$month),]
colnames(bugStatesDev2)[4] <- "state2"

commStatesDev2 <- communication[communication$msg_sender_name == "dev13",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev2$month==timespan[i])) {
  }
  else {
    commStatesDev2 <- rbind(commStatesDev2,c(commStatesDev2$msg_sender_name[1],timespan[i],0,0,0,"low"))
  }
}

commStatesDev2 <- commStatesDev2[commStatesDev2$month %in% timespan,]
commStatesDev2 <- commStatesDev2[order(commStatesDev2$month),]
colnames(commStatesDev2)[6] <- "state3"

classObsDev2 <- cbind(contributionStatesDev2,bugStatesDev2,commStatesDev2)

for (i in 1:nrow(classObsDev2)) {
  
  classObsDev2$stateLabel[i] <- calculateState(classObsDev2$state1[i],classObsDev2$state2[i],classObsDev2$state3[i])
  
}

indexSet1 <- sample(which(classObsDev2$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev2$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev2$stateLabel=="high"))

obs1 <- as.numeric(contributionStatesDev2$dnoc)
obs2 <- as.numeric(contributionStatesDev2$bugfixes)
obs3 <- as.numeric(commStatesDev2$activities)
obs4 <- as.numeric(bugStatesDev2$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train2 <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm2 <- hmmfit(train2, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev2$dnoc))
obs <- cbind(as.numeric(contributionStatesDev2$dnoc),as.numeric(contributionStatesDev2$bugfixes),as.numeric(commStatesDev2$activities),as.numeric(bugStatesDev2$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"

plot(trainDev)
pred2 <- predict(hmm2, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred2$s)

plot(pred2$s,type="l")
plot(pred2$s,type="l", yaxt = "n", xlab = "time", ylab = "state")
axis(2, seq(1, 3, by = 1)) 


#--------------------------------- dev 3 -------------------------------------------


contributionStatesDev3 <- contributions[contributions$name == "dev35",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev3$month==timespan[i])) {
  }
  else {
    contributionStatesDev3 <- rbind(contributionStatesDev3,c(contributionStatesDev3$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev3 <- contributionStatesDev3[order(contributionStatesDev3$month),]
colnames(contributionStatesDev3)[5] <- "state1"

bugStatesDev3 <- comments[comments$commentAuthor == "dev35",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev3$month==timespan[i])) {
  }
  else {
    bugStatesDev3 <- rbind(bugStatesDev3,c(bugStatesDev3$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev3 <- bugStatesDev3[order(bugStatesDev3$month),]
colnames(bugStatesDev3)[4] <- "state2"

commStatesDev3 <- communication[communication$msg_sender_name == "dev35",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev3$month==timespan[i])) {
  }
  else {
    commStatesDev3 <- rbind(commStatesDev3,c(commStatesDev3$msg_sender_name[1],timespan[i],0,0,0,"low"))
  }
}

commStatesDev3 <- commStatesDev3[commStatesDev3$month %in% timespan,]
commStatesDev3 <- commStatesDev3[order(commStatesDev3$month),]
colnames(commStatesDev3)[6] <- "state3"

classObsDev3 <- cbind(contributionStatesDev3,bugStatesDev3,commStatesDev3)

for (i in 1:nrow(classObsDev3)) {
  
  classObsDev3$stateLabel[i] <- calculateState(classObsDev3$state1[i],classObsDev3$state2[i],classObsDev3$state3[i])
  
}

indexSet1 <- sample(which(classObsDev3$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev3$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev3$stateLabel=="high"))

obs1 <- as.numeric(contributionStatesDev3$dnoc)
obs2 <- as.numeric(contributionStatesDev3$bugfixes)
obs3 <- as.numeric(commStatesDev3$activities)
obs4 <- as.numeric(bugStatesDev3$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train3 <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm3 <- hmmfit(train3, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev3$dnoc))
obs <- cbind(as.numeric(contributionStatesDev3$dnoc),as.numeric(contributionStatesDev3$bugfixes),as.numeric(commStatesDev3$activities),as.numeric(bugStatesDev3$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"

plot(trainDev)
pred3 <- predict(hmm3, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred3$s)

plot(pred3$s,type="l")

#--------------------------------- dev 4 -------------------------------------------


contributionStatesDev4 <- contributions[contributions$name == "dev14",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev4$month==timespan[i])) {
  }
  else {
    contributionStatesDev4 <- rbind(contributionStatesDev4,c(contributionStatesDev4$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev4 <- contributionStatesDev4[order(contributionStatesDev4$month),]
colnames(contributionStatesDev4)[5] <- "state1"

bugStatesDev4 <- comments[comments$commentAuthor == "dev14",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev4$month==timespan[i])) {
  }
  else {
    bugStatesDev4 <- rbind(bugStatesDev4,c(bugStatesDev4$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev4 <- bugStatesDev4[bugStatesDev4$month %in% timespan,]
bugStatesDev4 <- bugStatesDev4[order(bugStatesDev4$month),]
colnames(bugStatesDev4)[4] <- "state2"

commStatesDev4 <- communication[communication$msg_sender_name == "dev14",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev4$month==timespan[i])) {
  }
  else {
    commStatesDev4 <- rbind(commStatesDev4,c(commStatesDev4$msg_sender_name[1],timespan[i],0,0,0,"low"))
  }
}

commStatesDev4 <- commStatesDev4[commStatesDev4$month %in% timespan,]
commStatesDev4 <- commStatesDev4[order(commStatesDev4$month),]
colnames(commStatesDev4)[6] <- "state3"

classObsDev4 <- cbind(contributionStatesDev4,bugStatesDev4,commStatesDev4)

for (i in 1:nrow(classObsDev4)) {
  
  classObsDev4$stateLabel[i] <- calculateState(classObsDev4$state1[i],classObsDev4$state2[i],classObsDev4$state3[i])
  
}

indexSet1 <- sample(which(classObsDev4$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev4$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev4$stateLabel=="high"))

obs1 <- as.numeric(contributionStatesDev4$dnoc)
obs2 <- as.numeric(contributionStatesDev4$bugfixes)
obs3 <- as.numeric(commStatesDev4$activities)
obs4 <- as.numeric(bugStatesDev4$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train4 <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm4 <- hmmfit(train4, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev4$dnoc))
obs <- cbind(as.numeric(contributionStatesDev4$dnoc),as.numeric(contributionStatesDev4$bugfixes),as.numeric(commStatesDev4$activities),as.numeric(bugStatesDev4$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"

plot(trainDev)
pred4 <- predict(hmm4, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred4$s)

plot(pred4$s,type="l")
plot(pred4$s,type="l", yaxt = "n", xlab = "time", ylab = "state")
axis(2, seq(1, 3, by = 1)) 


#--------------------------------- dev 5 -------------------------------------------


contributionStatesDev5 <- contributions[contributions$name == "dev83",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev5$month==timespan[i])) {
  }
  else {
    contributionStatesDev5 <- rbind(contributionStatesDev5,c(contributionStatesDev5$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev5 <- contributionStatesDev5[order(contributionStatesDev5$month),]
colnames(contributionStatesDev5)[5] <- "state1"

bugStatesDev5 <- comments[comments$commentAuthor == "dev83",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev5$month==timespan[i])) {
  }
  else {
    bugStatesDev5 <- rbind(bugStatesDev5,c(bugStatesDev5$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev5 <- bugStatesDev5[bugStatesDev5$month %in% timespan,]
bugStatesDev5 <- bugStatesDev5[order(bugStatesDev5$month),]
colnames(bugStatesDev5)[4] <- "state2"

commStatesDev5 <- communication[communication$msg_sender_name == "dev83",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev5$month==timespan[i])) {
  }
  else {
    commStatesDev5 <- rbind(commStatesDev5,c(commStatesDev5$msg_sender_name[1],timespan[i],0,0,0,"low"))
  }
}

commStatesDev5 <- commStatesDev5[commStatesDev5$month %in% timespan,]
commStatesDev5 <- commStatesDev5[order(commStatesDev5$month),]
colnames(commStatesDev5)[6] <- "state3"

classObsDev5 <- cbind(contributionStatesDev5,bugStatesDev5,commStatesDev5)

for (i in 1:nrow(classObsDev5)) {
  
  classObsDev5$stateLabel[i] <- calculateState(classObsDev5$state1[i],classObsDev5$state2[i],classObsDev5$state3[i])
  
}

indexSet1 <- sample(which(classObsDev5$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev5$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev5$stateLabel=="high"))

obs1 <- as.numeric(contributionStatesDev5$dnoc)
obs2 <- as.numeric(contributionStatesDev5$bugfixes)
obs3 <- as.numeric(commStatesDev5$activities)
obs4 <- as.numeric(bugStatesDev5$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train5 <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm5 <- hmmfit(train5, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev5$dnoc))
obs <- cbind(as.numeric(contributionStatesDev5$dnoc),as.numeric(contributionStatesDev5$bugfixes),as.numeric(commStatesDev5$activities),as.numeric(bugStatesDev5$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"

plot(trainDev)
pred5 <- predict(hmm5, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred5$s)

plot(pred5$s,type="l")

#--------------------------------- dev 6 -------------------------------------------


contributionStatesDev6 <- contributions[contributions$name == "dev52",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev6$month==timespan[i])) {
  }
  else {
    contributionStatesDev6 <- rbind(contributionStatesDev6,c(contributionStatesDev6$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev6 <- contributionStatesDev6[order(contributionStatesDev6$month),]
colnames(contributionStatesDev6)[5] <- "state1"

bugStatesDev6 <- comments[comments$commentAuthor == "dev52",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev6$month==timespan[i])) {
  }
  else {
    bugStatesDev6 <- rbind(bugStatesDev6,c(bugStatesDev6$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev6 <- bugStatesDev6[bugStatesDev6$month %in% timespan,]
bugStatesDev6 <- bugStatesDev6[order(bugStatesDev6$month),]
colnames(bugStatesDev6)[4] <- "state2"

commStatesDev6 <- communication[communication$msg_sender_name == "Baumgart, Jens",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev6$month==timespan[i])) {
  }
  else {
    commStatesDev6 <- rbind(commStatesDev6,c(commStatesDev6$msg_sender_name[1],timespan[i],0,0,0,"low"))
  }
}

commStatesDev6 <- commStatesDev6[commStatesDev6$month %in% timespan,]
commStatesDev6 <- commStatesDev6[order(commStatesDev6$month),]
colnames(commStatesDev6)[6] <- "state3"

classObsDev6 <- cbind(contributionStatesDev6,bugStatesDev6,commStatesDev6)

for (i in 1:nrow(classObsDev6)) {
  
  classObsDev6$stateLabel[i] <- calculateState(classObsDev6$state1[i],classObsDev6$state2[i],classObsDev6$state3[i])
  
}

indexSet1 <- sample(which(classObsDev6$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev6$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev6$stateLabel=="high"))

obs1 <- as.numeric(contributionStatesDev6$dnoc)
obs2 <- as.numeric(contributionStatesDev6$bugfixes)
obs3 <- as.numeric(commStatesDev6$activities)
obs4 <- as.numeric(bugStatesDev6$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train6 <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm6 <- hmmfit(train6, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev6$dnoc))
obs <- cbind(as.numeric(contributionStatesDev6$dnoc),as.numeric(contributionStatesDev6$bugfixes),as.numeric(commStatesDev6$activities),as.numeric(bugStatesDev6$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"

plot(trainDev)
pred6 <- predict(hmm6, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred6$s)

plot(pred6$s,type="l")

#--------------------------------- dev 7 -------------------------------------------


contributionStatesDev7 <- contributions[contributions$name == "dev51",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev7$month==timespan[i])) {
  }
  else {
    contributionStatesDev7 <- rbind(contributionStatesDev7,c(contributionStatesDev7$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev7 <- contributionStatesDev7[order(contributionStatesDev7$month),]
colnames(contributionStatesDev7)[5] <- "state1"

bugStatesDev7 <- comments[comments$commentAuthor == "dev51",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev7$month==timespan[i])) {
  }
  else {
    bugStatesDev7 <- rbind(bugStatesDev7,c(bugStatesDev7$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev7 <- bugStatesDev7[bugStatesDev7$month %in% timespan,]
bugStatesDev7 <- bugStatesDev7[order(bugStatesDev7$month),]
colnames(bugStatesDev7)[4] <- "state2"

commStatesDev7 <- communication[communication$msg_sender_name == "dev51",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev7$month==timespan[i])) {
  }
  else {
    commStatesDev7 <- rbind(commStatesDev7,c(commStatesDev7$msg_sender_name[1],timespan[i],0,0,0,"low"))
  }
}

commStatesDev7 <- commStatesDev7[commStatesDev7$month %in% timespan,]
commStatesDev7 <- commStatesDev7[order(commStatesDev7$month),]
colnames(commStatesDev7)[6] <- "state3"

classObsDev7 <- cbind(contributionStatesDev7,bugStatesDev7,commStatesDev7)

for (i in 1:nrow(classObsDev7)) {
  
  classObsDev7$stateLabel[i] <- calculateState(classObsDev7$state1[i],classObsDev7$state2[i],classObsDev7$state3[i])
  
}

indexSet1 <- sample(which(classObsDev7$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev7$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev7$stateLabel=="high"))

obs1 <- as.numeric(contributionStatesDev7$dnoc)
obs2 <- as.numeric(contributionStatesDev7$bugfixes)
obs3 <- as.numeric(commStatesDev7$activities)
obs4 <- as.numeric(bugStatesDev7$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train7 <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm7 <- hmmfit(train7, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev7$dnoc))
obs <- cbind(as.numeric(contributionStatesDev7$dnoc),as.numeric(contributionStatesDev7$bugfixes),as.numeric(commStatesDev7$activities),as.numeric(bugStatesDev7$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"

plot(trainDev)
pred7 <- predict(hmm7, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred7$s)

plot(pred7$s,type="l",ylim=c(1,3))
plot(pred7$s,type="l", yaxt = "n", ylim=c(1,3), xlab = "time", ylab = "state")
axis(2, seq(1, 3, by = 1)) 


#--------------------------------- dev 8 -------------------------------------------

contributionStatesDev8 <- contributions[contributions$name == "dev81",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev8$month==timespan[i])) {
  }
  else {
    contributionStatesDev8 <- rbind(contributionStatesDev8,c(contributionStatesDev8$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev8 <- contributionStatesDev8[order(contributionStatesDev8$month),]
colnames(contributionStatesDev8)[5] <- "state1"

bugStatesDev8 <- comments[comments$commentAuthor == "dev81",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev8$month==timespan[i])) {
  }
  else {
    bugStatesDev8 <- rbind(bugStatesDev8,c(bugStatesDev8$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev8 <- bugStatesDev8[bugStatesDev8$month %in% timespan,]
bugStatesDev8 <- bugStatesDev8[order(bugStatesDev8$month),]
colnames(bugStatesDev8)[4] <- "state2"

commStatesDev8 <- communication[communication$msg_sender_name == "dev81",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev8$month==timespan[i])) {
  }
  else {
    commStatesDev8 <- rbind(commStatesDev8,c(commStatesDev8$msg_sender_name[1],timespan[i],0,0,0,"low"))
  }
}

commStatesDev8 <- commStatesDev8[commStatesDev8$month %in% timespan,]
commStatesDev8 <- commStatesDev8[order(commStatesDev8$month),]
colnames(commStatesDev8)[6] <- "state3"

classObsDev8 <- cbind(contributionStatesDev8,bugStatesDev8,commStatesDev8)

for (i in 1:nrow(classObsDev8)) {
  
  classObsDev8$stateLabel[i] <- calculateState(classObsDev8$state1[i],classObsDev8$state2[i],classObsDev8$state3[i])
  
}

indexSet1 <- sample(which(classObsDev8$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev8$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev8$stateLabel=="high"))

obs1 <- as.numeric(contributionStatesDev8$dnoc)
obs2 <- as.numeric(contributionStatesDev8$bugfixes)
obs3 <- as.numeric(commStatesDev8$activities)
obs4 <- as.numeric(bugStatesDev8$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train8 <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm8 <- hmmfit(train8, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev8$dnoc))
obs <- cbind(as.numeric(contributionStatesDev8$dnoc),as.numeric(contributionStatesDev8$bugfixes),as.numeric(commStatesDev8$activities),as.numeric(bugStatesDev8$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"


plot(trainDev)
pred8 <- predict(hmm8, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred8$s)

plot(pred8$s,type="l",ylim=c(1,3))


#--------------------------------- dev 9 -------------------------------------------

contributionStatesDev9 <- contributions[contributions$name == "dev53",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev9$month==timespan[i])) {
  }
  else {
    contributionStatesDev9 <- rbind(contributionStatesDev9,c(contributionStatesDev9$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev9 <- contributionStatesDev9[order(contributionStatesDev9$month),]
colnames(contributionStatesDev9)[5] <- "state1"

bugStatesDev9 <- comments[comments$commentAuthor == "dev53",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev9$month==timespan[i])) {
  }
  else {
    bugStatesDev9 <- rbind(bugStatesDev9,c(bugStatesDev9$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev9 <- bugStatesDev9[bugStatesDev9$month %in% timespan,]
bugStatesDev9 <- bugStatesDev9[order(bugStatesDev9$month),]
colnames(bugStatesDev9)[4] <- "state2"

commStatesDev9 <- communication[communication$msg_sender_name == "dev53",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev9$month==timespan[i])) {
  }
  else {
    commStatesDev9 <- rbind(commStatesDev9,c(commStatesDev9$msg_sender_name[1],timespan[i],0,0,0,"low"))
  }
}

commStatesDev9 <- commStatesDev9[commStatesDev9$month %in% timespan,]
commStatesDev9 <- commStatesDev9[order(commStatesDev9$month),]
colnames(commStatesDev9)[6] <- "state3"

classObsDev9 <- cbind(contributionStatesDev9,bugStatesDev9,commStatesDev9)

for (i in 1:nrow(classObsDev9)) {
  
  classObsDev9$stateLabel[i] <- calculateState(classObsDev9$state1[i],classObsDev9$state2[i],classObsDev9$state3[i])
  
}

indexSet1 <- sample(which(classObsDev9$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev9$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev9$stateLabel=="high"))

obs1 <- as.numeric(contributionStatesDev9$dnoc)
obs2 <- as.numeric(contributionStatesDev9$bugfixes)
obs3 <- as.numeric(commStatesDev9$activities)
obs4 <- as.numeric(bugStatesDev9$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train9 <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm9 <- hmmfit(train9, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev9$dnoc))
obs <- cbind(as.numeric(contributionStatesDev9$dnoc),as.numeric(contributionStatesDev9$bugfixes),as.numeric(commStatesDev9$activities),as.numeric(bugStatesDev9$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"


plot(trainDev)
pred9 <- predict(hmm9, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred9$s)

plot(pred9$s,type="l",ylim=c(1,3))

#--------------------------------- dev 10 -------------------------------------------

contributionStatesDev10 <- contributions[contributions$name == "dev84",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev10$month==timespan[i])) {
  }
  else {
    contributionStatesDev10 <- rbind(contributionStatesDev10,c(contributionStatesDev10$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev10 <- contributionStatesDev10[order(contributionStatesDev10$month),]
colnames(contributionStatesDev10)[5] <- "state1"

bugStatesDev10 <- comments[comments$commentAuthor == "dev84",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev10$month==timespan[i])) {
  }
  else {
    bugStatesDev10 <- rbind(bugStatesDev10,c(bugStatesDev10$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev10 <- bugStatesDev10[bugStatesDev10$month %in% timespan,]
bugStatesDev10 <- bugStatesDev10[order(bugStatesDev10$month),]
colnames(bugStatesDev10)[4] <- "state2"

commStatesDev10 <- communication[communication$msg_sender_name == "dev84",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev10$month==timespan[i])) {
  }
  else {
    commStatesDev10 <- rbind(commStatesDev10,c(commStatesDev10$msg_sender_name[1],timespan[i],0,0,0,"low"))
  }
}

commStatesDev10 <- commStatesDev10[commStatesDev10$month %in% timespan,]
commStatesDev10 <- commStatesDev10[order(commStatesDev10$month),]
colnames(commStatesDev10)[6] <- "state3"

classObsDev10 <- cbind(contributionStatesDev10,bugStatesDev10,commStatesDev10)

for (i in 1:nrow(classObsDev10)) {
  
  classObsDev10$stateLabel[i] <- calculateState(classObsDev10$state1[i],classObsDev10$state2[i],classObsDev10$state3[i])
  
}

indexSet1 <- sample(which(classObsDev10$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev10$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev10$stateLabel=="high"))

obs1 <- as.numeric(contributionStatesDev10$dnoc)
obs2 <- as.numeric(contributionStatesDev10$bugfixes)
obs3 <- as.numeric(commStatesDev10$activities)
obs4 <- as.numeric(bugStatesDev10$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train10 <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm10 <- hmmfit(train10, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev10$dnoc))
obs <- cbind(as.numeric(contributionStatesDev10$dnoc),as.numeric(contributionStatesDev10$bugfixes),as.numeric(commStatesDev10$activities),as.numeric(bugStatesDev10$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"


plot(trainDev)
pred10 <- predict(hmm10, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred10$s)

plot(pred10$s,type="l",ylim=c(1,3))

#--------------------------------- dev 11 -------------------------------------------


contributionStatesDev11 <- contributions[contributions$name == "dev82",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev11$month==timespan[i])) {
  }
  else {
    contributionStatesDev11 <- rbind(contributionStatesDev11,c(contributionStatesDev11$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev11 <- contributionStatesDev11[order(contributionStatesDev11$month),]
colnames(contributionStatesDev11)[5] <- "state1"

bugStatesDev11 <- comments[comments$commentAuthor == "dev82",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev11$month==timespan[i])) {
  }
  else {
    bugStatesDev11 <- rbind(bugStatesDev11,c(bugStatesDev11$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev11 <- bugStatesDev11[bugStatesDev11$month %in% timespan,]
bugStatesDev11 <- bugStatesDev11[order(bugStatesDev11$month),]
colnames(bugStatesDev11)[4] <- "state2"

commStatesDev11 <- communication[communication$msg_sender_name == "dev82",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev11$month==timespan[i])) {
  }
  else {
    commStatesDev11 <- rbind(commStatesDev11,c(commStatesDev11$msg_sender_name[1],timespan[i],0,0,0,"low"))
  }
}

commStatesDev11 <- commStatesDev11[commStatesDev11$month %in% timespan,]
commStatesDev11 <- commStatesDev11[order(commStatesDev11$month),]
colnames(commStatesDev11)[6] <- "state3"

classObsDev11 <- cbind(contributionStatesDev11,bugStatesDev11,commStatesDev11)

for (i in 1:nrow(classObsDev11)) {
  
  classObsDev11$stateLabel[i] <- calculateState(classObsDev11$state1[i],classObsDev11$state2[i],classObsDev11$state3[i])
  
}

indexSet1 <- sample(which(classObsDev11$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev11$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev11$stateLabel=="high"))

obs1 <- as.numeric(contributionStatesDev11$dnoc)
obs2 <- as.numeric(contributionStatesDev11$bugfixes)
obs3 <- as.numeric(commStatesDev11$activities)
obs4 <- as.numeric(bugStatesDev11$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train11 <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm11 <- hmmfit(train11, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev11$dnoc))
obs <- cbind(as.numeric(contributionStatesDev11$dnoc),as.numeric(contributionStatesDev11$bugfixes),as.numeric(commStatesDev11$activities),as.numeric(bugStatesDev11$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"


plot(trainDev)
pred11 <- predict(hmm11, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred11$s)

plot(pred11$s,type="l",ylim=c(1,3))

#estimate initial probability

init_est <- c(0.91,0.09,0)

# ------------------------------------------------ q-q-plot ----------------------------------

# for (i in 1:nrow(classObsDev1)) {
#   if(classObsDev1$stateLabel[i] == "low")
#   {
#     classObsDev1$stateLabel[i] = 1
#   }
#   if(classObsDev1$stateLabel[i] == "medium")
#   {
#     classObsDev1$stateLabel[i] = 2
#   }
#   if(classObsDev1$stateLabel[i] == "high")
#   {
#     classObsDev1$stateLabel[i] = 3
#   }
# }
# 
# plot(classObsDev1$stateLabel~pred$s)

# --------------------------------------------------------------------------------------------

# create plot for all logliks?

loglik <- c(-722.8469,-598.3831,-365.3775,-128.9717,-842.0167,-540.1508,-830.4783,-491.1799,-470.1307,-542.0286,-487.2652)
label <- c("Egit","Egit","Egit","Egit","Egit","Egit","Egit","Egit","Egit","Egit","Egit")
loglikRekonq <- c(-1104.142,-618.5615,-1222.992,-784.181,-608.8629,-967.6032,-686.9542,-853.4479)
label2 <- c("Rekonq","Rekonq","Rekonq","Rekonq","Rekonq","Rekonq","Rekonq","Rekonq")
loglikKonsole <- c(-1512.034,-1839.236,-1068.331,-1896.037,-3735.508,-1142.073,-1441.15,-1259.696,-1143.255,-1401.025)
label3 <- c("Konsole","Konsole","Konsole","Konsole","Konsole","Konsole","Konsole","Konsole","Konsole","Konsole")
loglikAnt <-c(-1466.413,-1313.41,-889.2396,-773.3005,-947.0933,-4213.85,-843.8844,-299.5819,-2587.732,-1655.241,-1587.899,
              -1451.45,-529.1718,-818.8025)
label4 <- c("Ant","Ant","Ant","Ant","Ant","Ant","Ant","Ant","Ant","Ant","Ant","Ant","Ant","Ant")
loglikPoi <-c(-1873.973, -1686.423,-1614.976,-1834.715,-1039.518,-1124.892,-1473.104,-1079.133,-936.9312,-623.3883,
              -1605.274,-617.2076,-1871.359)
label5 <- c("Poi","Poi","Poi","Poi","Poi","Poi","Poi","Poi","Poi","Poi","Poi","Poi","Poi")
loglikLog4j <- c(-1063.657,-1560.842,-362.5309,-558.462,-871.577,-315.9504,-909.655,-1886.798,-1237.015,-1065.55)
label6 <- c("Log4j","Log4j","Log4j","Log4j","Log4j","Log4j","Log4j","Log4j","Log4j","Log4j")
boxplot(loglik, horizontal = TRUE)



# misclassification rate
#table(train$s,pred$s) ##classification matrix
#mean(pred$s!=train$s) ##misclassification rate

for (i in 1:nrow(classObsDev1)) {
   if(classObsDev1$stateLabel[i] == "low")
   {
     classObsDev1$stateLabel[i] = 1
   }
   if(classObsDev1$stateLabel[i] == "medium")
   {
     classObsDev1$stateLabel[i] = 2
   }
   if(classObsDev1$stateLabel[i] == "high")
   {
     classObsDev1$stateLabel[i] = 3
   }
}

for (i in 1:nrow(classObsDev2)) {
  if(classObsDev2$stateLabel[i] == "low")
  {
    classObsDev2$stateLabel[i] = 1
  }
  if(classObsDev2$stateLabel[i] == "medium")
  {
    classObsDev2$stateLabel[i] = 2
  }
  if(classObsDev2$stateLabel[i] == "high")
  {
    classObsDev2$stateLabel[i] = 3
  }
}

for (i in 1:nrow(classObsDev3)) {
  if(classObsDev3$stateLabel[i] == "low")
  {
    classObsDev3$stateLabel[i] = 1
  }
  if(classObsDev3$stateLabel[i] == "medium")
  {
    classObsDev3$stateLabel[i] = 2
  }
  if(classObsDev3$stateLabel[i] == "high")
  {
    classObsDev3$stateLabel[i] = 3
  }
}

for (i in 1:nrow(classObsDev4)) {
  if(classObsDev4$stateLabel[i] == "low")
  {
    classObsDev4$stateLabel[i] = 1
  }
  if(classObsDev4$stateLabel[i] == "medium")
  {
    classObsDev4$stateLabel[i] = 2
  }
  if(classObsDev4$stateLabel[i] == "high")
  {
    classObsDev4$stateLabel[i] = 3
  }
}

for (i in 1:nrow(classObsDev5)) {
  if(classObsDev5$stateLabel[i] == "low")
  {
    classObsDev5$stateLabel[i] = 1
  }
  if(classObsDev5$stateLabel[i] == "medium")
  {
    classObsDev5$stateLabel[i] = 2
  }
  if(classObsDev5$stateLabel[i] == "high")
  {
    classObsDev5$stateLabel[i] = 3
  }
}

for (i in 1:nrow(classObsDev6)) {
  if(classObsDev6$stateLabel[i] == "low")
  {
    classObsDev6$stateLabel[i] = 1
  }
  if(classObsDev6$stateLabel[i] == "medium")
  {
    classObsDev6$stateLabel[i] = 2
  }
  if(classObsDev6$stateLabel[i] == "high")
  {
    classObsDev6$stateLabel[i] = 3
  }
}

for (i in 1:nrow(classObsDev7)) {
  if(classObsDev7$stateLabel[i] == "low")
  {
    classObsDev7$stateLabel[i] = 1
  }
  if(classObsDev7$stateLabel[i] == "medium")
  {
    classObsDev7$stateLabel[i] = 2
  }
  if(classObsDev7$stateLabel[i] == "high")
  {
    classObsDev7$stateLabel[i] = 3
  }
}

for (i in 1:nrow(classObsDev8)) {
  if(classObsDev8$stateLabel[i] == "low")
  {
    classObsDev8$stateLabel[i] = 1
  }
  if(classObsDev8$stateLabel[i] == "medium")
  {
    classObsDev8$stateLabel[i] = 2
  }
  if(classObsDev8$stateLabel[i] == "high")
  {
    classObsDev8$stateLabel[i] = 3
  }
}

for (i in 1:nrow(classObsDev9)) {
  if(classObsDev9$stateLabel[i] == "low")
  {
    classObsDev9$stateLabel[i] = 1
  }
  if(classObsDev9$stateLabel[i] == "medium")
  {
    classObsDev9$stateLabel[i] = 2
  }
  if(classObsDev9$stateLabel[i] == "high")
  {
    classObsDev9$stateLabel[i] = 3
  }
}

for (i in 1:nrow(classObsDev10)) {
  if(classObsDev10$stateLabel[i] == "low")
  {
    classObsDev10$stateLabel[i] = 1
  }
  if(classObsDev10$stateLabel[i] == "medium")
  {
    classObsDev10$stateLabel[i] = 2
  }
  if(classObsDev10$stateLabel[i] == "high")
  {
    classObsDev10$stateLabel[i] = 3
  }
}

for (i in 1:nrow(classObsDev11)) {
  if(classObsDev11$stateLabel[i] == "low")
  {
    classObsDev11$stateLabel[i] = 1
  }
  if(classObsDev11$stateLabel[i] == "medium")
  {
    classObsDev11$stateLabel[i] = 2
  }
  if(classObsDev11$stateLabel[i] == "high")
  {
    classObsDev11$stateLabel[i] = 3
  }
}

table(classObsDev1$stateLabel,pred$s)
mr1 <- mean(pred1$s!=classObsDev1$stateLabel)
mr2 <- mean(pred2$s!=classObsDev2$stateLabel)
mr3 <- mean(pred3$s!=classObsDev3$stateLabel)
mr4 <- mean(pred4$s!=classObsDev4$stateLabel)
mr5 <- mean(pred5$s!=classObsDev5$stateLabel)
mr6 <- mean(pred6$s!=classObsDev6$stateLabel)
mr7 <- mean(pred7$s!=classObsDev7$stateLabel)
mr8 <- mean(pred8$s!=classObsDev8$stateLabel)
mr9 <- mean(pred9$s!=classObsDev9$stateLabel)
mr10 <- mean(pred10$s!=classObsDev10$stateLabel)
mr11 <- mean(pred11$s!=classObsDev11$stateLabel)

mrEgit <- c(mr1,mr2,mr3,mr4,mr5,mr6,mr7,mr8,mr9,mr10,mr11)
mrRekonq <- c(0.125,0.08333333,0.2222222,0.05555556,0.05555556,0.09722222,0.08333333,0.04166667,0.08333333,0.1111111)
mrKonsole <- c(0.03157895,0.1421053,0.07368421,0.08421053,0.02105263,0.1578947,0.05263158,0.005263158,0.01052632,0.02105263)
# two outliers for dev7, dev8
mrLog4j <- c(0.09375,0.0875,0.06875,0.03125,0.00625,0.0375,0.00625,0.04375)
# outliers for dev10-dev14
mrPoi <- c(0.09868421,0.1513158,0.09210526,0.01973684,0.03289474,0.1381579,0.07894737,0.1315789,0.01315789)
mrAnt <- c(0.1257143,0.06285714,0.02285714,0.05714286,0.08,0.2171429,0.06857143,0.03428571,0.1371429,0.08571429,0.09142857,0.06285714,0.1714286,0.09714286)

# boxplot 

mr <- c(mrEgit,mrRekonq,mrKonsole,mrLog4j,mrPoi,mrAnt)
#mean: 0.0859

project <- c("Egit","Egit","Egit","Egit","Egit","Egit","Egit","Egit","Egit","Egit","Egit",
            "Rekonq","Rekonq","Rekonq","Rekonq","Rekonq","Rekonq","Rekonq","Rekonq","Rekonq","Rekonq",
            "Konsole","Konsole","Konsole","Konsole","Konsole","Konsole","Konsole","Konsole","Konsole","Konsole",
            "Log4j","Log4j","Log4j","Log4j","Log4j","Log4j","Log4j","Log4j",
            "Poi","Poi","Poi","Poi","Poi","Poi","Poi","Poi","Poi",
            "Ant","Ant","Ant","Ant","Ant","Ant","Ant","Ant","Ant","Ant","Ant","Ant","Ant","Ant")

mrProjects <- cbind.data.frame(mr,project)

p <- ggplot(data=mrProjects, aes(x=factor(mrProjects$project),y=mrProjects$mr))
print(p + geom_boxplot() + xlab("project") + ylab("misclassification rate"))


# compare emission distributions 
# train 1-4 major developers, others minor

plot(density(train4$x), main="", xlab="contribution")
lines(density(train2$x), col=2)
lines(density(train3$x), col=3)
lines(density(train1$x), col=4)
legend("topright", legend=c("dev1","dev2","dev3","dev4"), col=c("blue","red","green","black"), lty=c(1,1,1,1))

plot(density(train9$x), main="", xlab="contribution")
lines(density(train5$x), col=2)
lines(density(train6$x), col=3)
#lines(density(train7$x), col=4)
lines(density(train8$x), col=4)
#lines(density(train10$x), col=6)
#lines(density(train11$x), col=7)
legend("topright", legend=c("dev5","dev6","dev7","dev8"), col=c("blue","red","green","black"), lty=c(1,1,1,1))

boxplot(train1$x, horizontal=TRUE)
boxplot(train2$x, horizontal=TRUE)
boxplot(train3$x, horizontal=TRUE)
boxplot(train4$x, horizontal=TRUE)

#compare means and standard deviation
m1 <- mean(train1$x)
m2 <- mean(train2$x)
m3 <- mean(train3$x)
m4 <- mean(train4$x)

m5 <- mean(train5$x)
m6 <- mean(train6$x)
m8 <- mean(train8$x)
m9 <- mean(train9$x)

sd1 <- sd(train1$x)
sd2 <- sd(train2$x)
sd3 <- sd(train3$x)
sd4 <- sd(train4$x)

sd5 <- sd(train5$x)
sd6 <- sd(train6$x)
sd8 <- sd(train8$x)
sd9 <- sd(train9$x)

# average emission distribution 

# major: 1,2,3,4

A <- matrix(c(.25,0,0,0,
              0,.25,0,0,
              0,0,.25,0,
              0,0,0,.25),nrow=4)

mlow <- A%*%hmm1$model$parms.emission$mu[[1]]+A%*%hmm2$model$parms.emission$mu[[1]]+A%*%hmm3$model$parms.emission$mu[[1]]+
        A%*%hmm3$model$parms.emission$mu[[1]]

mmed <- A%*%hmm1$model$parms.emission$mu[[2]]+A%*%hmm2$model$parms.emission$mu[[2]]+A%*%hmm3$model$parms.emission$mu[[2]]+
        A%*%hmm4$model$parms.emission$mu[[2]]

mhigh <- A%*%hmm1$model$parms.emission$mu[[3]]+A%*%hmm2$model$parms.emission$mu[[3]]+A%*%hmm3$model$parms.emission$mu[[3]]+
         A%*%hmm4$model$parms.emission$mu[[3]]

mmajor <- list(as.vector(mlow),as.vector(mmed),as.vector(mhigh))

#covariances

sigmalow <- (A%*%hmm1$model$parms.emission$sigma[[1]]%*%t(A))+(A%*%hmm2$model$parms.emission$sigma[[1]]%*%t(A))+(A%*%hmm3$model$parms.emission$sigma[[1]]%*%t(A))+
            (A%*%hmm4$model$parms.emission$sigma[[1]]%*%t(A))

sigmamed <- (A%*%hmm1$model$parms.emission$sigma[[2]]%*%t(A))+(A%*%hmm2$model$parms.emission$sigma[[2]]%*%t(A))+(A%*%hmm8$model$parms.emission$sigma[[2]]%*%t(A))+
            (A%*%hmm4$model$parms.emission$sigma[[2]]%*%t(A))

sigmahigh <- (A%*%hmm1$model$parms.emission$sigma[[3]]%*%t(A))+(A%*%hmm2$model$parms.emission$sigma[[3]]%*%t(A))+(A%*%hmm8$model$parms.emission$sigma[[3]]%*%t(A))+
             (A%*%hmm4$model$parms.emission$sigma[[3]]%*%t(A))

sigmamajor <- list(sigmalow,sigmamed,sigmahigh)

majorEgit <- list(mu=mmajor,sigma=sigmamajor)

save(majorEgit, file="majorEgit.rda")


# minor: 8

minor3Egit <- hmm8$model$parms.emission

save(minor3Egit, file="minor3Egit.rda")

# two state: 6

minor2Egit <- hmm6$model$parms.emission

save(minor2Egit, file="minor2Egit.rda")

# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# learning with universal model 

#major

#dev1

major <- t(matrix(c(0.6475,0.2825,0.07,0.3750,0.4450,0.18,0.11,0.25,0.64),ncol=3))

modelMajor <- hmmspec(init = c(1,0,0), trans = major, parms.emis = emissionMajor, dens.emis = dmvnorm.hsmm) 
train <- simulate(modelMajor, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm <- hmmfit(train, modelMajor, mstep = mstep.mvnorm)

plot(trainDev)
pred <- predict(hmm, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred$s)

plot(pred$s,type="l")
par(mar=c(5.1,4.1,4.1,4.1)) 
plot(pred$s,type="l", yaxt = "n", xlab = "time", ylab="state")
axis(4, seq(1, 3, by = 1), labels=c("low","medium","high"), las=1)

# mr 

mr1u <- mean(pred$s!=classObsDev1$stateLabel)

#0.41

#dev2

mr2u <- mean(pred$s!=classObsDev2$stateLabel)

#0.39

#dev3

mr3u <- mean(pred$s!=classObsDev3$stateLabel)

#0.20

#dev4

mr4u <- mean(pred$s!=classObsDev4$stateLabel)

#0.12

#minor

minor_high <- t(matrix(c(0.67,0.29,0.04,0.32,0.51,0.17,0.09,0.18,0.73),ncol=3))

modelMinor3 <- hmmspec(init = c(1,0,0), trans = minor_high, parms.emis = emissionMinor3, dens.emis = dmvnorm.hsmm)

train <- simulate(modelMinor3, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm <- hmmfit(train, modelMinor3, mstep = mstep.mvnorm)

plot(trainDev)
pred <- predict(hmm, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred$s)

plot(pred$s,type="l")
par(mar=c(5.1,4.1,4.1,4.1)) 
plot(pred$s,type="l", yaxt = "n", xlab = "time", ylab="state")
axis(4, seq(1, 3, by = 1), labels=c("low","medium","high"), las=1)

#dev8

mr8u <- mean(pred$s!=classObsDev8$stateLabel)

#0.54

modelMinor2 <- hmmspec(init = c(1,0), trans = minor_low, parms.emis = emissionMinor2, dens.emis = dmvnorm.hsmm)

train <- simulate(modelMinor2, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm <- hmmfit(train, modelMinor2, mstep = mstep.mvnorm)

plot(trainDev)
pred <- predict(hmm, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred$s)

plot(pred$s,type="l")
par(mar=c(5.1,4.1,4.1,4.1)) 
plot(pred$s,type="l", yaxt = "n", xlab = "time", ylab="state")
axis(4, seq(1, 3, by = 1), labels=c("low","medium","high"), las=1)

#dev6

mr6u <- mean(pred$s!=classObsDev6$stateLabel)

# 0.19
