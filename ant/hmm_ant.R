library (HMM)
library (lattice)
library(ggplot2)
library(mhsmm)
library(MASS)
library(fitdistrplus)

setwd("C:\\Users\\vhonsel\\Documents\\Mining\\Case Studies\\Ant")
#commit data
committers = read.csv("committer_ant.csv", header=T, sep=",", stringsAsFactors=FALSE)
#mailing list data
ml_dev = read.csv("ml_ant_dev_short.csv", header=T,  sep=",", stringsAsFactors=FALSE)
ml_u = read.csv("ml_ant_users_short.csv", header=T,  sep=",", stringsAsFactors=FALSE)
#bugzilla data
bz_comments = read.csv("bz_comments.csv", header=T, sep=",", stringsAsFactors=FALSE)
bz_issues =  read.csv("bz_issues.csv", header=T, sep=",", stringsAsFactors=FALSE)
bz_actions =  read.csv("bz_actions.csv", header=T, sep=",", stringsAsFactors=FALSE) 

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

for (s in 1:nrow(ml_dev)){
  
  ml_dev$month[s] <- substr(ml_dev$first_date[s],1,7) 
  #committers$monthly_commits[s] <-sum(committers$commit_date <'2008-11')
  
} 

for (s in 1:nrow(ml_u)){
  
  ml_u$month[s] <- substr(ml_u$first_date[s],1,7) 
  #committers$monthly_commits[s] <-sum(committers$commit_date <'2008-11')
  
} 

#dev71 = Simeon Fitch

for (a in 1:nrow(ml_dev)) {
  if (grepl("manolache",ml_dev$username[a])==TRUE) {
        ml_dev$name[a] <- "dev70"
  }
  if (grepl("ehatcher",ml_dev$username[a])==TRUE) {
    ml_dev$name[a] <- "dev48"
  }
  if (grepl("rubys",ml_dev$username[a])==TRUE) {
    ml_dev$name[a] <- "dev69"
  }
}

mlComm_dev <-ml_dev[grep("Antoine Levy|Conor Mac|dev72|dev48|dev49|Jan Mat|dev80|dev9|
                     simeon|dev34|dev77|Bodewig|dev74|Steve Loughra|Lalev|dev70|dev69",ml_dev$name),]

mlComm_u <-ml_u[grep("Antoine Levy|Conor Mac|dev72|dev48|dev49|Jan Mat|dev80|dev9|
                         simeon|dev34|dev77|Bodewig|dev74|dev11|Lalev|dev70|dev69",ml_u$name),]

mlComm <- rbind(mlComm_dev,mlComm_u)

for (a in 1:nrow(mlComm)) {
  if (grepl("Jan M",mlComm$name[a])==TRUE) {
    mlComm$name[a] <- "dev78"
  }
}


for (t in 1:nrow(mlComm)){
  
  mlComm$activities[t] <- sum(mlComm$name[t] == mlComm$name)
  mlComm$opened[t] <- sum(mlComm$month[t] == mlComm$month & mlComm$name[t] == mlComm$name & mlComm$is_response_of == 'NULL')
  mlComm$responses[t] <- sum(mlComm$month[t] == mlComm$month & mlComm$name[t] == mlComm$name & mlComm$is_response_of != 'NULL')
  
} 

write.csv(mlComm_dev, "C:/Users/vhonsel/Documents/Mining/Case Studies/Ant/ml_dev_act.csv", row.names=FALSE)
 
# mlComm = read.csv("ml_dev_act.csv", header=T,  sep=",", stringsAsFactors=FALSE)


print(ggplot(mlComm, aes(x=mlComm$month, y=mlComm$responses, colour=mlComm$name, group=mlComm$name)) 
      #   + geom_point() 
      + geom_line(aes(mlComm$month, mlComm$responses)) 
      + theme(axis.text.x=element_text(angle=-90)) 
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="responses"))

print(ggplot(mlComm, aes(x=mlComm$month, y=mlComm$opened, colour=mlComm$name, group=mlComm$name)) 
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

for (u in 1:nrow(bz_comments)){
  
  if (bz_comments$commentAuthor[u]=="peter reilly") {
    bz_comments$commentAuthor[u] <- "dev77"
  }
  if (bz_comments$commentAuthor[u]=="Jesse") {
    bz_comments$commentAuthor[u] <- "dev79"
  }
  
} 


bz_comments_dev <-bz_comments[grep("dev1|Kruithof|dev75|dev10|dev11|dev79|Jan Mat|
                      dev77|dev8|dev9|dev74|dev34|dev72|dev69|
                                   dev70|dev48|Kev Jackson|Simeon|dev73|Nicolas Lal",bz_comments$commentAuthor),]


print(ggplot(bz_comments_dev, aes(bz_comments_dev$month, y=bz_comments_dev$nocomm, colour=bz_comments_dev$commentAuthor, group=bz_comments_dev$commentAuthor))
      + geom_line(aes(bz_comments_dev$month, bz_comments_dev$nocomm))
      + theme(axis.text.x=element_text(angle=-90))
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="number of bug comments"))

# prepare for threshold learner

contributions <- committers[, c(14,17,19,20)]
contributions <- unique(contributions)

communication <- mlComm[,c(5,7,9,10)]
communication <- unique(communication)

comments <- bz_comments_dev[,c(4,9,10)]
comments <- unique(comments)

write.csv(contributions, "contributions.csv", row.names=FALSE)
write.csv(communication, "communication.csv", row.names=FALSE)
write.csv(comments, "bug_comments.csv", row.names=FALSE)

# put questions and answers together or omit questions

for (l in 1:nrow(communication)) {
  communication$activities[l] <- communication$opened[l] + communication$responses[l]
}

write.csv(communication, "communication_summarized.csv", row.names=FALSE)

#thresholds: commits  7.8/15.6  , bugfixes  0/1.6 , ml activity  13/25.6 , bug comments  12.6/24 

# classify by thresholds

for (k in 1:nrow(contributions)) {
  if (contributions$dnoc[k] < 8) 
  {
    contributions$state[k] <- "low"
  }
  if (contributions$dnoc[k] < 16 && contributions$dnoc[k] > 7) 
  {
    contributions$state[k] <- "medium"
  }
  if (contributions$dnoc[k] > 15 && contributions$bugfixes[k] > 1) 
  {
    contributions$state[k] <- "high"
  }
}

for (k in 1:nrow(communication)) {
  if (communication$activities[k] < 13) 
  {
    communication$state2[k] <- "low"
  }
  if (communication$activities[k] < 26 && communication$activities[k] > 12) 
  {
    communication$state2[k] <- "medium"
  }
  if (communication$activities[k] > 25) 
  {
    communication$state2[k] <- "high"
  }
}

for (k in 1:nrow(comments)) {
  if (comments$nocomm[k] < 13) 
  {
    comments$state[k] <- "low"
  }
  if (comments$nocomm[k] < 24 && comments$nocomm[k] > 1) 
  {
    comments$state[k] <- "medium"
  }
  if (comments$nocomm[k] > 23) 
  {
    comments$state[k] <- "high"
  }
}

#determine timespan

timespan <- sort(c(unique(contributions$month),"2012-10"))

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

contributionStatesDev1 <- contributions[contributions$name == "dev10",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev1$month==timespan[i])) {
  }
  else {
    contributionStatesDev1 <- rbind(contributionStatesDev1,c(contributionStatesDev1$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev1 <- contributionStatesDev1[order(contributionStatesDev1$month),]
colnames(contributionStatesDev1)[5] <- "state1"

bugStatesDev1 <- comments[comments$commentAuthor == "dev10",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev1$month==timespan[i])) {
  }
  else {
    bugStatesDev1 <- rbind(bugStatesDev1,c(bugStatesDev1$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev1 <- bugStatesDev1[order(bugStatesDev1$month),]
colnames(bugStatesDev1)[4] <- "state2"

commStatesDev1 <- communication[communication$name == "dev10",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev1$month==timespan[i])) {
  }
  else {
    commStatesDev1 <- rbind(commStatesDev1,c(commStatesDev1$name[1],timespan[i],0,0,0,"low"))
  }
}

commStatesDev1 <- commStatesDev1[order(commStatesDev1$month),]
commStatesDev1 <- commStatesDev1[commStatesDev1$month<"2014-08",]
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
train <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm1 <- hmmfit(train, model, mstep = mstep.mvnorm)

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
plot(pred1$s,type="l", ylim=c(1,3), yaxt = "n", xlab = "time", ylab="state")
axis(4, seq(1, 3, by = 1), labels=c("low","medium","high"), las=1)

#--------------------------------- dev2 -------------------------------------------

contributionStatesDev2 <- contributions[contributions$name == "dev8",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev2$month==timespan[i])) {
  }
  else {
    contributionStatesDev2 <- rbind(contributionStatesDev2,c(contributionStatesDev2$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev2 <- contributionStatesDev2[order(contributionStatesDev2$month),]
colnames(contributionStatesDev2)[5] <- "state1"

bugStatesDev2 <- comments[comments$commentAuthor == "dev8",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev2$month==timespan[i])) {
  }
  else {
    bugStatesDev2 <- rbind(bugStatesDev2,c(bugStatesDev2$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev2 <- bugStatesDev2[order(bugStatesDev2$month),]
colnames(bugStatesDev2)[4] <- "state2"

commStatesDev2 <- communication[communication$name == "dev8",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev2$month==timespan[i])) {
  }
  else {
    commStatesDev2 <- rbind(commStatesDev2,c(commStatesDev2$name[1],timespan[i],0,0,0,"low"))
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
train <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm2 <- hmmfit(train, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev2$dnoc))
obs <- cbind(as.numeric(contributionStatesDev2$dnoc),as.numeric(contributionStatesDev2$bugfixes),as.numeric(commStatesDev2$activities),as.numeric(bugStatesDev2$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"

plot(trainDev)
pred2 <- predict(hmm2, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred2$s)

plot(pred2$s,type="l")

#--------------------------------- dev 3 -------------------------------------------

contributionStatesDev3 <- contributions[contributions$name == "dev72",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev3$month==timespan[i])) {
  }
  else {
    contributionStatesDev3 <- rbind(contributionStatesDev3,c(contributionStatesDev3$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev3 <- contributionStatesDev3[order(contributionStatesDev3$month),]
colnames(contributionStatesDev3)[5] <- "state1"

bugStatesDev3 <- comments[comments$commentAuthor == "dev72",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev3$month==timespan[i])) {
  }
  else {
    bugStatesDev3 <- rbind(bugStatesDev3,c(bugStatesDev3$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev3 <- bugStatesDev3[order(bugStatesDev3$month),]
colnames(bugStatesDev3)[4] <- "state2"

commStatesDev3 <- communication[communication$name == "dev72",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev3$month==timespan[i])) {
  }
  else {
    commStatesDev3 <- rbind(commStatesDev3,c(commStatesDev3$name[1],timespan[i],0,0,0,"low"))
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
train <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm3 <- hmmfit(train, model, mstep = mstep.mvnorm)

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

contributionStatesDev4 <- contributions[contributions$name == "dev48",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev4$month==timespan[i])) {
  }
  else {
    contributionStatesDev4 <- rbind(contributionStatesDev4,c(contributionStatesDev4$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev4 <- contributionStatesDev4[order(contributionStatesDev4$month),]
colnames(contributionStatesDev4)[5] <- "state1"

bugStatesDev4 <- comments[comments$commentAuthor == "dev48",]

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

commStatesDev4 <- communication[communication$name == "dev48",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev4$month==timespan[i])) {
  }
  else {
    commStatesDev4 <- rbind(commStatesDev4,c(commStatesDev4$name[1],timespan[i],0,0,0,"low"))
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
train <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm4 <- hmmfit(train, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev4$dnoc))
obs <- cbind(as.numeric(contributionStatesDev4$dnoc),as.numeric(contributionStatesDev4$bugfixes),as.numeric(commStatesDev4$activities),as.numeric(bugStatesDev4$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"

plot(trainDev)
pred4 <- predict(hmm4, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred4$s)

plot(pred4$s,type="l")

#--------------------------------- dev 5 -------------------------------------------

contributionStatesDev5 <- contributions[contributions$name == "Jacobus Martinus Kruithof",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev5$month==timespan[i])) {
  }
  else {
    contributionStatesDev5 <- rbind(contributionStatesDev5,c(contributionStatesDev5$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev5 <- contributionStatesDev5[order(contributionStatesDev5$month),]
colnames(contributionStatesDev5)[5] <- "state1"

bugStatesDev5 <- comments[comments$commentAuthor == "J.M. (Martijn) Kruithof",]

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

commStatesDev5 <- communication[communication$name == "dev49",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev5$month==timespan[i])) {
  }
  else {
    commStatesDev5 <- rbind(commStatesDev5,c(commStatesDev5$name[1],timespan[i],0,0,0,"low"))
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
train <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm5 <- hmmfit(train, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev5$dnoc))
obs <- cbind(as.numeric(contributionStatesDev5$dnoc),as.numeric(contributionStatesDev5$bugfixes),as.numeric(commStatesDev5$activities),as.numeric(bugStatesDev5$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"

plot(trainDev)
pred5 <- predict(hmm5, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred5$s)

plot(pred5$s,type="l")

plot(pred5$s,type="l", yaxt = "n", xlab = "time", ylab = "state", ylim=c(1,3))
axis(2, seq(1, 3, by = 1)) 
par(mar=c(5.1,4.1,4.1,4.1)) 
plot(pred5$s,type="l", ylim=c(1,3), yaxt = "n", xlab = "time", ylab="state")
axis(4, seq(1, 3, by = 1), labels=c("low","medium","high"), las=1)

#ggplot for contributions

library("reshape2")
reshaped <- classObsDev5[,c(2,3,4,8,14)]
colnames(reshaped)[2] <- "commits"
colnames(reshaped)[3] <- "bugfixes"
colnames(reshaped)[4] <- "bug comments"
colnames(reshaped)[5] <- "ML posts"
reshaped <- melt(reshaped, id="month")
reshaped$value <- sapply(reshaped$value, as.numeric)

print(ggplot(data=reshaped, aes(x=month, y=value, group=variable, colour=variable))
      + geom_line(size=1)
      + scale_x_discrete(breaks = c("2000-01","2000-07","2001-01","2001-07","2002-01","2002-07","2003-01","2003-07","2004-01","2004-07","2005-01","2005-07",
                                    "2006-01","2006-07","2007-01","2007-07","2008-01","2008-07","2009-01","2009-07","2010-01","2010-07","2011-01","2011-07",
                                    "2012-01","2012-07","2013-01","2013-07","2014-01"))
      + scale_y_continuous("number")
#      + geom_hline(yintercept=0, color = pred5$s)
      + theme(legend.title=element_blank())
      + theme(axis.text.x=element_text(angle=-90))) 


#--------------------------------- dev 6 -------------------------------------------


contributionStatesDev6 <- contributions[contributions$name == "dev78",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev6$month==timespan[i])) {
  }
  else {
    contributionStatesDev6 <- rbind(contributionStatesDev6,c(contributionStatesDev6$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev6 <- contributionStatesDev6[order(contributionStatesDev6$month),]
colnames(contributionStatesDev6)[5] <- "state1"

bugStatesDev6 <- comments[comments$commentAuthor == "Jan Mat",]

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

commStatesDev6 <- communication[communication$name == "dev78",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev6$month==timespan[i])) {
  }
  else {
    commStatesDev6 <- rbind(commStatesDev6,c(commStatesDev6$name[1],timespan[i],0,0,0,"low"))
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
train <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm6 <- hmmfit(train, model, mstep = mstep.mvnorm)

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

contributionStatesDev7 <- contributions[contributions$name == "dev80",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev7$month==timespan[i])) {
  }
  else {
    contributionStatesDev7 <- rbind(contributionStatesDev7,c(contributionStatesDev7$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev7 <- contributionStatesDev7[order(contributionStatesDev7$month),]
colnames(contributionStatesDev7)[5] <- "state1"

bugStatesDev7 <- comments[comments$commentAuthor == "Kev Jackson",]

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

commStatesDev7 <- communication[communication$name == "dev80",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev7$month==timespan[i])) {
  }
  else {
    commStatesDev7 <- rbind(commStatesDev7,c(commStatesDev7$name[1],timespan[i],0,0,0,"low"))
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
train <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm7 <- hmmfit(train, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev7$dnoc))
obs <- cbind(as.numeric(contributionStatesDev7$dnoc),as.numeric(contributionStatesDev7$bugfixes),as.numeric(commStatesDev7$activities),as.numeric(bugStatesDev7$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"

plot(trainDev)
pred7 <- predict(hmm7, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred7$s)

plot(pred7$s,type="l",ylim=c(1,3))


#--------------------------------- dev 8 -------------------------------------------

contributionStatesDev8 <- contributions[contributions$name == "dev34",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev8$month==timespan[i])) {
  }
  else {
    contributionStatesDev8 <- rbind(contributionStatesDev8,c(contributionStatesDev8$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev8 <- contributionStatesDev8[order(contributionStatesDev8$month),]
colnames(contributionStatesDev8)[5] <- "state1"

bugStatesDev8 <- comments[comments$commentAuthor == "dev34",]

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

commStatesDev8 <- communication[communication$name == "dev34",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev8$month==timespan[i])) {
  }
  else {
    commStatesDev8 <- rbind(commStatesDev8,c(commStatesDev8$name[1],timespan[i],0,0,0,"low"))
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
train <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm8 <- hmmfit(train, model, mstep = mstep.mvnorm)

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

contributionStatesDev9 <- contributions[contributions$name == "dev1",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev9$month==timespan[i])) {
  }
  else {
    contributionStatesDev9 <- rbind(contributionStatesDev9,c(contributionStatesDev9$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev9 <- contributionStatesDev9[order(contributionStatesDev9$month),]
colnames(contributionStatesDev9)[5] <- "state1"

bugStatesDev9 <- comments[comments$commentAuthor == "dev1",]

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

commStatesDev9 <- communication[communication$name == "dev1",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev9$month==timespan[i])) {
  }
  else {
    commStatesDev9 <- rbind(commStatesDev9,c(commStatesDev9$name[1],timespan[i],0,0,0,"low"))
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
train <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm9 <- hmmfit(train, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev9$dnoc))
obs <- cbind(as.numeric(contributionStatesDev9$dnoc),as.numeric(contributionStatesDev9$bugfixes),as.numeric(commStatesDev9$activities),as.numeric(bugStatesDev9$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"


plot(trainDev)
pred9 <- predict(hmm9, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred9$s)

plot(pred9$s,type="l", yaxt = "n", xlab = "time", ylab = "state")
axis(2, seq(1, 3, by = 1)) 
par(mar=c(5.1,4.1,4.1,4.1)) 
plot(pred9$s,type="l", ylim=c(1,3), yaxt = "n", xlab = "time", ylab="state")
axis(4, seq(1, 3, by = 1), labels=c("low","medium","high"), las=1)

#ggplot for contributions

library("reshape2")
reshaped <- classObsDev9[,c(2,3,4,8,14)]
colnames(reshaped)[2] <- "commits"
colnames(reshaped)[3] <- "bugfixes"
colnames(reshaped)[4] <- "bug comments"
colnames(reshaped)[5] <- "ML posts"
reshaped <- melt(reshaped, id="month")
reshaped$value <- sapply(reshaped$value, as.numeric)

print(ggplot(data=reshaped, aes(x=month, y=value, group=variable, colour=variable))
  + geom_line(size=1)
  + scale_x_discrete(breaks = c("2000-01","2000-07","2001-01","2001-07","2002-01","2002-07","2003-01","2003-07","2004-01","2004-07","2005-01","2005-07",
                                "2006-01","2006-07","2007-01","2007-07","2008-01","2008-07","2009-01","2009-07","2010-01","2010-07","2011-01","2011-07",
                                "2012-01","2012-07","2013-01","2013-07","2014-01"))
  + scale_y_continuous("number")
  + theme(legend.title=element_blank())
  + theme(axis.text.x=element_text(angle=-90))) 


#--------------------------------- dev 10 -------------------------------------------


contributionStatesDev10 <- contributions[contributions$name == "dev11",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev10$month==timespan[i])) {
  }
  else {
    contributionStatesDev10 <- rbind(contributionStatesDev10,c(contributionStatesDev10$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev10 <- contributionStatesDev10[order(contributionStatesDev10$month),]
colnames(contributionStatesDev10)[5] <- "state1"

bugStatesDev10 <- comments[comments$commentAuthor == "dev11",]

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

commStatesDev10 <- communication[communication$name == "dev11",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev10$month==timespan[i])) {
  }
  else {
    commStatesDev10 <- rbind(commStatesDev10,c(commStatesDev10$name[1],timespan[i],0,0,0,"low"))
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
train <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm10 <- hmmfit(train, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev10$dnoc))
obs <- cbind(as.numeric(contributionStatesDev10$dnoc),as.numeric(contributionStatesDev10$bugfixes),as.numeric(commStatesDev10$activities),as.numeric(bugStatesDev10$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"


plot(trainDev)
pred10 <- predict(hmm10, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred10$s)

plot(pred10$s,type="l",ylim=c(1,3))

plot(pred10$s,type="l", yaxt = "n", xlab = "time", ylab = "state")
axis(2, seq(1, 3, by = 1)) 
par(mar=c(5.1,4.1,4.1,4.1)) 
plot(pred10$s,type="l", ylim=c(1,3), yaxt = "n", xlab = "time", ylab="state")
axis(4, seq(1, 3, by = 1), labels=c("low","medium","high"), las=1)

#ggplot for contributions

library("reshape2")
reshaped <- classObsDev10[,c(2,3,4,8,14)]
colnames(reshaped)[2] <- "commits"
colnames(reshaped)[3] <- "bugfixes"
colnames(reshaped)[4] <- "bug comments"
colnames(reshaped)[5] <- "ML posts"
reshaped <- melt(reshaped, id="month")
reshaped$value <- sapply(reshaped$value, as.numeric)

print(ggplot(data=reshaped, aes(x=month, y=value, group=variable, colour=variable))
      + geom_line(size=1)
      + scale_x_discrete(breaks = c("2000-01","2000-07","2001-01","2001-07","2002-01","2002-07","2003-01","2003-07","2004-01","2004-07","2005-01","2005-07",
                                    "2006-01","2006-07","2007-01","2007-07","2008-01","2008-07","2009-01","2009-07","2010-01","2010-07","2011-01","2011-07",
                                    "2012-01","2012-07","2013-01","2013-07","2014-01"))
      + scale_y_continuous("number")
      + theme(legend.title=element_blank())
      + theme(axis.text.x=element_text(angle=-90))) 



#--------------------------------- dev 11 -------------------------------------------

contributionStatesDev11 <- contributions[contributions$name == "Matthew Jason Benson",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev11$month==timespan[i])) {
  }
  else {
    contributionStatesDev11 <- rbind(contributionStatesDev11,c(contributionStatesDev11$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev11 <- contributionStatesDev11[order(contributionStatesDev11$month),]
colnames(contributionStatesDev11)[5] <- "state1"

bugStatesDev11 <- comments[comments$commentAuthor == "dev9",]

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

commStatesDev11 <- communication[communication$name == "dev9",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev11$month==timespan[i])) {
  }
  else {
    commStatesDev11 <- rbind(commStatesDev11,c(commStatesDev11$name[1],timespan[i],0,0,0,"low"))
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
train <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm11 <- hmmfit(train, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev11$dnoc))
obs <- cbind(as.numeric(contributionStatesDev11$dnoc),as.numeric(contributionStatesDev11$bugfixes),as.numeric(commStatesDev11$activities),as.numeric(bugStatesDev11$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"


plot(trainDev)
pred11 <- predict(hmm11, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred11$s)

plot(pred11$s,type="l",ylim=c(1,3))


#--------------------------------- dev 12 -------------------------------------------

contributionStatesDev12 <- contributions[contributions$name == "dev74",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev12$month==timespan[i])) {
  }
  else {
    contributionStatesDev12 <- rbind(contributionStatesDev12,c(contributionStatesDev12$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev12 <- contributionStatesDev12[order(contributionStatesDev12$month),]
colnames(contributionStatesDev12)[5] <- "state1"

bugStatesDev12 <- comments[comments$commentAuthor == "dev74",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev12$month==timespan[i])) {
  }
  else {
    bugStatesDev12 <- rbind(bugStatesDev12,c(bugStatesDev12$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev12 <- bugStatesDev12[bugStatesDev12$month %in% timespan,]
bugStatesDev12 <- bugStatesDev12[order(bugStatesDev12$month),]
colnames(bugStatesDev12)[4] <- "state2"

commStatesDev12 <- communication[communication$name == "dev74",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev12$month==timespan[i])) {
  }
  else {
    commStatesDev12 <- rbind(commStatesDev12,c(commStatesDev12$name[1],timespan[i],0,0,0,"low"))
  }
}

commStatesDev12 <- commStatesDev12[commStatesDev12$month %in% timespan,]
commStatesDev12 <- commStatesDev12[order(commStatesDev12$month),]
colnames(commStatesDev12)[6] <- "state3"

classObsDev12 <- cbind(contributionStatesDev12,bugStatesDev12,commStatesDev12)

for (i in 1:nrow(classObsDev12)) {
  
  classObsDev12$stateLabel[i] <- calculateState(classObsDev12$state1[i],classObsDev12$state2[i],classObsDev12$state3[i])
  
}

indexSet1 <- sample(which(classObsDev12$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev12$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev12$stateLabel=="high"))

obs1 <- as.numeric(contributionStatesDev12$dnoc)
obs2 <- as.numeric(contributionStatesDev12$bugfixes)
obs3 <- as.numeric(commStatesDev12$activities)
obs4 <- as.numeric(bugStatesDev12$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm12 <- hmmfit(train, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev12$dnoc))
obs <- cbind(as.numeric(contributionStatesDev12$dnoc),as.numeric(contributionStatesDev12$bugfixes),as.numeric(commStatesDev12$activities),as.numeric(bugStatesDev12$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"


plot(trainDev)
pred12 <- predict(hmm12, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred12$s)

plot(pred12$s,type="l",ylim=c(1,3))

#--------------------------------- dev 13 -------------------------------------------

contributionStatesDev13 <- contributions[contributions$name == "dev50",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev13$month==timespan[i])) {
  }
  else {
    contributionStatesDev13 <- rbind(contributionStatesDev13,c(contributionStatesDev13$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev13 <- contributionStatesDev13[order(contributionStatesDev13$month),]
colnames(contributionStatesDev13)[5] <- "state1"

bugStatesDev13 <- comments[comments$commentAuthor == "Nicolas Lalevée",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev13$month==timespan[i])) {
  }
  else {
    bugStatesDev13 <- rbind(bugStatesDev13,c(bugStatesDev13$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev13 <- bugStatesDev13[bugStatesDev13$month %in% timespan,]
bugStatesDev13 <- bugStatesDev13[order(bugStatesDev13$month),]
colnames(bugStatesDev13)[4] <- "state2"

commStatesDev13 <- communication[grep("Nicolas Lal",communication$name),]

for (i in 1:length(timespan)) {
  if (any(commStatesDev13$month==timespan[i])) {
  }
  else {
    commStatesDev13 <- rbind(commStatesDev13,c(commStatesDev13$name[1],timespan[i],0,0,0,"low"))
  }
}

commStatesDev13 <- commStatesDev13[commStatesDev13$month %in% timespan,]
commStatesDev13 <- commStatesDev13[order(commStatesDev13$month),]
colnames(commStatesDev13)[6] <- "state3"

classObsDev13 <- cbind(contributionStatesDev13,bugStatesDev13,commStatesDev13)

for (i in 1:nrow(classObsDev13)) {
  
  classObsDev13$stateLabel[i] <- calculateState(classObsDev13$state1[i],classObsDev13$state2[i],classObsDev13$state3[i])
  
}

indexSet1 <- sample(which(classObsDev13$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev13$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev13$stateLabel=="high"))

obs1 <- as.numeric(contributionStatesDev13$dnoc)
obs2 <- as.numeric(contributionStatesDev13$bugfixes)
obs3 <- as.numeric(commStatesDev13$activities)
obs4 <- as.numeric(bugStatesDev13$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm13 <- hmmfit(train, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev13$dnoc))
obs <- cbind(as.numeric(contributionStatesDev13$dnoc),as.numeric(contributionStatesDev13$bugfixes),as.numeric(commStatesDev13$activities),as.numeric(bugStatesDev13$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"


plot(trainDev)
pred13 <- predict(hmm13, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred13$s)

plot(pred13$s,type="l",ylim=c(1,3))

#--------------------------------- dev 14 -------------------------------------------

contributionStatesDev14 <- contributions[contributions$name == "dev69",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev14$month==timespan[i])) {
  }
  else {
    contributionStatesDev14 <- rbind(contributionStatesDev14,c(contributionStatesDev14$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev14 <- contributionStatesDev14[order(contributionStatesDev14$month),]
colnames(contributionStatesDev14)[5] <- "state1"

bugStatesDev14 <- comments[comments$commentAuthor == "dev69",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev14$month==timespan[i])) {
  }
  else {
    bugStatesDev14 <- rbind(bugStatesDev14,c(bugStatesDev14$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev14 <- bugStatesDev14[bugStatesDev14$month %in% timespan,]
bugStatesDev14 <- bugStatesDev14[order(bugStatesDev14$month),]
colnames(bugStatesDev14)[4] <- "state2"

commStatesDev14 <- communication[communication$name == "dev69",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev14$month==timespan[i])) {
  }
  else {
    commStatesDev14 <- rbind(commStatesDev14,c(commStatesDev14$name[1],timespan[i],0,0,0,"low"))
  }
}

commStatesDev14 <- commStatesDev14[commStatesDev14$month %in% timespan,]
commStatesDev14 <- commStatesDev14[order(commStatesDev14$month),]
colnames(commStatesDev14)[6] <- "state3"

classObsDev14 <- cbind(contributionStatesDev14,bugStatesDev14,commStatesDev14)

for (i in 1:nrow(classObsDev14)) {
  
  classObsDev14$stateLabel[i] <- calculateState(classObsDev14$state1[i],classObsDev14$state2[i],classObsDev14$state3[i])
  
}

indexSet1 <- sample(which(classObsDev14$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev14$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev14$stateLabel=="high"))

obs1 <- as.numeric(contributionStatesDev14$dnoc)
obs2 <- as.numeric(contributionStatesDev14$bugfixes)
obs3 <- as.numeric(commStatesDev14$activities)
obs4 <- as.numeric(bugStatesDev14$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm14 <- hmmfit(train, model, mstep = mstep.mvnorm)

N <- as.numeric(length(contributionStatesDev14$dnoc))
obs <- cbind(as.numeric(contributionStatesDev14$dnoc),as.numeric(contributionStatesDev14$bugfixes),as.numeric(commStatesDev14$activities),as.numeric(bugStatesDev14$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"


plot(trainDev)
pred14 <- predict(hmm14, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred14$s)

plot(pred14$s,type="l",ylim=c(1,3))


#more developer? 

#estimate initial probability

init_est <- c(0.9285714,0.07142857,0)

# ----------------------------------------------------------------------------------

# compare emission distributions 

#f1 <- fitdist(train$x[,1][train$s==1],"norm")
#plotdist(train$x[,1][train$s==1],"norm",para=list(mean=f1$estimate[1],sd=f1$estimate[2]))
#f2 <- fitdist(train$x[,1][train$s==2],"norm")
#plotdist(train$x[,1][train$s==2],"norm",para=list(mean=f2$estimate[1],sd=f2$estimate[2]))
#f3 <- fitdist(train$x[,1][train$s==3],"norm")
#plotdist(train$x[,1][train$s==3],"norm",para=list(mean=f3$estimate[1],sd=f3$estimate[2]))

hmm$model$parms.emission
cov2cor(hmm$model$parms.emission$sigma[[1]])

# variance of emission distribution 

sqrt(diag(hmm$model$parms.emission$sigma[[1]]))
sqrt(diag(hmm$model$parms.emission$sigma[[2]]))
sqrt(diag(hmm$model$parms.emission$sigma[[3]]))

# combine distribtuions 

# core developer 

coreAnt <- hmm9$model$parms.emission

save(coreAnt, file="coreAnt.rda")


# major developer: 1,2,8,10,11

A <- matrix(c(.2,0,0,0,
              0,.2,0,0,
              0,0,.2,0,
              0,0,0,.2),nrow=4)

#mus

mlow <- A%*%hmm1$model$parms.emission$mu[[1]]+A%*%hmm2$model$parms.emission$mu[[1]]+A%*%hmm8$model$parms.emission$mu[[1]]+
              A%*%hmm10$model$parms.emission$mu[[1]]+A%*%hmm11$model$parms.emission$mu[[1]]
         
mmed <- A%*%hmm1$model$parms.emission$mu[[2]]+A%*%hmm2$model$parms.emission$mu[[2]]+A%*%hmm8$model$parms.emission$mu[[2]]+
            A%*%hmm10$model$parms.emission$mu[[2]]+A%*%hmm11$model$parms.emission$mu[[2]]
  
mhigh <- A%*%hmm1$model$parms.emission$mu[[3]]+A%*%hmm2$model$parms.emission$mu[[3]]+A%*%hmm8$model$parms.emission$mu[[3]]+
             A%*%hmm10$model$parms.emission$mu[[3]]+A%*%hmm11$model$parms.emission$mu[[3]]

mmajor <- list(as.vector(mlow),as.vector(mmed),as.vector(mhigh))

#covariances

sigmalow <- (A%*%hmm1$model$parms.emission$sigma[[1]]%*%t(A))+(A%*%hmm2$model$parms.emission$sigma[[1]]%*%t(A))+(A%*%hmm8$model$parms.emission$sigma[[1]]%*%t(A))+
            (A%*%hmm10$model$parms.emission$sigma[[1]]%*%t(A))+(A%*%hmm11$model$parms.emission$sigma[[1]]%*%t(A))

sigmamed <- (A%*%hmm1$model$parms.emission$sigma[[2]]%*%t(A))+(A%*%hmm2$model$parms.emission$sigma[[2]]%*%t(A))+(A%*%hmm8$model$parms.emission$sigma[[2]]%*%t(A))+
  (A%*%hmm10$model$parms.emission$sigma[[2]]%*%t(A))+(A%*%hmm11$model$parms.emission$sigma[[2]]%*%t(A))

sigmahigh <- (A%*%hmm1$model$parms.emission$sigma[[3]]%*%t(A))+(A%*%hmm2$model$parms.emission$sigma[[3]]%*%t(A))+(A%*%hmm8$model$parms.emission$sigma[[3]]%*%t(A))+
  (A%*%hmm10$model$parms.emission$sigma[[3]]%*%t(A))+(A%*%hmm11$model$parms.emission$sigma[[3]]%*%t(A))

sigmamajor <- list(sigmalow,sigmamed,sigmahigh)

majorAnt <- list(mu=mmajor,sigma=sigmamajor)

save(majorAnt, file="majorAnt.rda")


#minor developer: 4

minor3Ant <- hmm4$model$parms.emission

save(minor3Ant, file="minor3Ant.rda")

# 2 state: 5,13

A <- matrix(c(.5,0,0,0,
              0,.5,0,0,
              0,0,.5,0,
              0,0,0,.5),nrow=4)


mlow <- A%*%hmm5$model$parms.emission$mu[[1]]+A%*%hmm13$model$parms.emission$mu[[1]]

mmed <- A%*%hmm5$model$parms.emission$mu[[2]]+A%*%hmm13$model$parms.emission$mu[[2]]

mminor <- list(as.vector(mlow),as.vector(mmed))

#covariances

sigmalow <- (A%*%hmm5$model$parms.emission$sigma[[1]]%*%t(A))+(A%*%hmm13$model$parms.emission$sigma[[1]]%*%t(A))

sigmamed <- (A%*%hmm5$model$parms.emission$sigma[[2]]%*%t(A))+(A%*%hmm13$model$parms.emission$sigma[[2]]%*%t(A))

sigmaminor <- list(sigmalow,sigmamed)

minor2Ant <- list(mu=mmajor,sigma=sigmamajor)

save(minor2Ant, file="minor2Ant.rda")


#learn with universal model

#core 

core <- t(matrix(c(.658,0.266,0.076,0.37,0.496,0.134,0.11,0.21,0.68),ncol=3))

modelCore <- hmmspec(init = c(1,0,0), trans = core, parms.emis = emissionCore, dens.emis = dmvnorm.hsmm) 
train <- simulate(modelCore, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm <- hmmfit(train, modelCore, mstep = mstep.mvnorm)

plot(trainDev)
pred <- predict(hmm, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l")
addStates(pred$s)

plot(pred$s,type="l")
par(mar=c(5.1,4.1,4.1,4.1)) 
plot(pred$s,type="l", yaxt = "n", xlab = "time", ylab="state")
axis(4, seq(1, 3, by = 1), labels=c("low","medium","high"), las=1)

mr9u <- mean(pred$s!=classObsDev9$stateLabel)

# major 

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
mr2u <- mean(pred$s!=classObsDev2$stateLabel)
mr8u <- mean(pred$s!=classObsDev8$stateLabel)
mr10u <- mean(pred$s!=classObsDev10$stateLabel)
mr11u <- mean(pred$s!=classObsDev11$stateLabel)

# minor

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

#mr

mr4u <- mean(pred$s!=classObsDev4$stateLabel)

# 2 state

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

mr5u <- mean(pred$s!=classObsDev5$stateLabel)
mr13u <- mean(pred$s!=classObsDev13$stateLabel)
mr7u <- mean(pred$s!=classObsDev7$stateLabel)



