library (HMM)
library (lattice)
library(ggplot2)
library(mhsmm)
library(MASS)

setwd("C:\\Users\\vhonsel\\Documents\\Mining\\Case Studies\\Konsole\\input")
#commit data
committers = read.csv("committer_konsole.csv", header=T, sep=",", stringsAsFactors=FALSE)
commits = read.csv("commits_konsole_short.csv", header=T, sep=",", stringsAsFactors=FALSE)
#mailinglist data
ml = read.csv("ml_konsole_short.csv", header=T,  sep=",", stringsAsFactors=FALSE)
#bugzilla data
bz_comments = read.csv("bz_comments.csv", header=T, sep=",", stringsAsFactors=FALSE)
bz_issues =  read.csv("bz_issues.csv", header=T, sep=",", stringsAsFactors=FALSE)
bz_actions =  read.csv("bz_actions.csv", header=T, sep=",", stringsAsFactors=FALSE) 

# commit stats 

for (i in 1:nrow(commits)){
  
  commits$noc[i] <-sum(commits$name == commits$name[i])
  
  commits$nobf[i] <- sum(commits$is_bug_fix[commits$name == commits$name[i]]==1)
  
} 

# filter skript kiddy
commits <- commits[grep('Script',commits$name,invert = TRUE),]
commits <- commits[grep('script',commits$name,invert = TRUE),]

committers <- committers[grep('Script',committers$name,invert = TRUE),]
committers <- committers[grep('script',committers$name,invert = TRUE),]


# whole commits

for (s in 1:nrow(committers)){
  
  committers$month[s] <- substr(committers$commit_date[s],1,7) 
  
} 


for (t in 1:nrow(committers)){
  
  committers$noc[t] <- sum(committers$name == committers$name[t]) 
  
} 

# omit developers with less than 20 commits

committers <- committers[committers$noc > 29,]



for (u in 1:nrow(committers)){
  
  committers$dnoc[u] <- sum(committers$month == committers$month[u] & committers$name == committers$name[u] & committers$is_bug_fix != 1) 
  
}

for (v in 1:nrow(committers)){
  
  committers$bugfixes[v] <- sum(committers$month == committers$month[v] & committers$name == committers$name[v] & committers$is_bug_fix == 1) 
  
}


# visualize developer activity


#test <- committers[,c(1,9)]
#test <- unique(test)

devcom_per_m <- committers[, c(1,8,9,10,11)]
devcom_per_m <- unique(devcom_per_m)
xx <- ggplot(devcom_per_m, aes(devcom_per_m$month, devcom_per_m$dnoc))
print(xx + geom_bar(stat = "identity", aes(devcom_per_m$month, devcom_per_m$dnoc, fill = devcom_per_m$name, position = "dodge"), width=1.0)
      + theme(axis.text.x=element_text(angle=-90))
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="commits")) 


yy <- ggplot(devcom_per_m, aes(devcom_per_m$month, y=devcom_per_m$dnoc, colour=devcom_per_m$name)) 
print(yy  + geom_point()
      + theme(axis.text.x=element_text(angle=-90))
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="commits"))

# other charts?

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

for (u in 1:nrow(ml)) {
  if (grepl("Hindenburg",ml$name[u])== TRUE) {
    ml$name[u] <- "dev2"
  }
}

for (s in 1:nrow(ml)){
  
  ml$month[s] <- substr(ml$first_date[s],1,7) 
  #committers$monthly_commits[s] <-sum(committers$commit_date <'2008-11')
  
} 

for (t in 1:nrow(ml)){
  
  ml$activities[t] <- sum(ml$name[t] == ml$name)
  ml$opened[t] <- sum(ml$month[t] == ml$month & ml$name[t] == ml$name & ml$is_response_of == 'NULL')
  ml$responses[t] <- sum(ml$month[t] == ml$month & ml$name[t] == ml$name & ml$is_response_of != 'NULL')
  
} 

mlComm <-ml[grep("dev91|dev89|dev54|dev86|dev39|Hindenburg|dev37|dev67|dev87|
  Lubos Lunak|dev38|dev88|dev15|dev85|dev40",ml$name),]


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

#bug activities

for (u in 1:nrow(bz_comments)){
  
  bz_comments$month[u] <- substr(bz_comments$commentTime[u],1,7) 
  
} 

for (u in 1:nrow(bz_issues)){
  
  bz_issues$reported_month[u] <- substr(bz_issues$reportedOn[u],1,7) 
  
} 

for (u in 1:nrow(bz_actions)){
  
  bz_actions$month[u] <- substr(bz_actions$date[u],1,7) 
  
} 

for (x in 1:nrow(bz_comments)){
  
  bz_comments$nocomm[x] <- sum(bz_comments$commentAuthor[x] == bz_comments$commentAuthor & bz_comments$month[x] == bz_comments$month)
  
}

for (y in 1:nrow(bz_issues)){
  
  bz_issues$assignments[y] <- sum(bz_issues$assignedTo[y] == bz_issues$assignedTo & bz_issues$reported_month[y] == bz_issues$reported_month)
  bz_issues$reported[y] <- sum(bz_issues$reportedBy[y] == bz_issues$reportedBy & bz_issues$reported_month[y] == bz_issues$reported_month) 
  
} 

for (z in 1:nrow(bz_actions)){
  
  bz_actions$actions[z] <- sum(bz_actions$author[z] == bz_actions$author & bz_actions$month[z] == bz_actions$month)
  
}

bz_comments_dev <-bz_comments[grep("dev91|dev89|dev54|dev86|dev39|Hindenburg|dev37|dev67|dev87|
  Lubos Lunak|dev38|dev88|dev15|dev85|dev40",bz_comments$commentAuthor),]
bz_issues_rep <-bz_issues[grep("dev91|dev89|dev54|dev86|dev39|Hindenburg|dev37|dev67|dev87|
  Lubos Lunak|dev38|dev88|dev15|dev85|dev40",bz_issues$reportedBy),] 
#bz_issues_act <-  bz_actions[grep("dev91|dev89|dev54|dev86|dev39|Hindenburg|dev37|dev67|dev87|
#  Lubos Lunak|dev38|dev88|dev15|dev85|dev40",bz_actions$author),]

print(ggplot(bz_comments_dev, aes(bz_comments_dev$month, y=bz_comments_dev$nocomm, colour=bz_comments_dev$commentAuthor, group=bz_comments_dev$commentAuthor))
      + geom_line(aes(bz_comments_dev$month, bz_comments_dev$nocomm))
      + theme(axis.text.x=element_text(angle=-90))
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="number of bug comments"))

print(ggplot(bz_issues_rep, aes(bz_issues_rep$reported_month, y=bz_issues_rep$reported, colour=bz_issues_rep$reportedBy, group=bz_issues_rep$reportedBy))
      + geom_line(aes(bz_issues_rep$reported_month, bz_issues_rep$reported))
      + theme(axis.text.x=element_text(angle=-90))
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="number of bug reports"))

#print(ggplot(bz_issues_act, aes(bz_issues_act$month, y=bz_issues_act$actions, colour=bz_issues_act$author, group=bz_issues_act$author))
#      + geom_line(aes(bz_issues_act$month, bz_issues_act$actions))
#      + theme(axis.text.x=element_text(angle=-90))
#      + scale_x_discrete(name="month") 
#      + scale_y_continuous(name="number of bug activities"))

summary(committers)
summary(ml)
sd(committers$dnoc)
sd(committers$bugfixes)
sd(ml$opened)
sd(ml$responses)
summary(bz_comments)
summary(bz_issues_rep)
sd(bz_comments$nocomm)
sd(bz_issues_rep$reported)

# prepare for threshold learner

contributions <- committers[, c(1,8,10,11)]
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


#thresholds: commits  5/14  , bugfixes  0/3 , ml activity  16/29  , bug comments  1/ 19 

# classify by thresholds

for (k in 1:nrow(contributions)) {
  if (contributions$dnoc[k] < 5) 
  {
    contributions$state[k] <- "low"
  }
  if (contributions$dnoc[k] < 14 && contributions$dnoc[k] > 4) 
  {
    contributions$state[k] <- "medium"
  }
  if (contributions$dnoc[k] > 13 && contributions$bugfixes[k] > 2) 
  {
    contributions$state[k] <- "high"
  }
}

for (k in 1:nrow(communication)) {
  if (communication$activities[k] < 16) 
  {
    communication$state2[k] <- "low"
  }
  if (communication$activities[k] < 29 && communication$activities[k] > 15) 
  {
    communication$state2[k] <- "medium"
  }
  if (communication$activities[k] > 28) 
  {
    communication$state2[k] <- "high"
  }
}

for (k in 1:nrow(comments)) {
  if (comments$nocomm[k] < 2) 
  {
    comments$state[k] <- "low"
  }
  if (comments$nocomm[k] < 19 && comments$nocomm[k] > 1) 
  {
    comments$state[k] <- "medium"
  }
  if (comments$nocomm[k] > 18) 
  {
    comments$state[k] <- "high"
  }
}

#determine timespan

timespan <- c(unique(contributions$month),"2010-08","2010-12","2014-06")
timespan <- sort(timespan)

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

# dev37
# too sparse data, hmm for sparse data?

contributionStatesDev1 <- contributions[contributions$name == "dev37",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev1$month==timespan[i])) {
  }
  else {
    contributionStatesDev1 <- rbind(contributionStatesDev1,c(contributionStatesDev1$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev1 <- contributionStatesDev1[order(contributionStatesDev1$month),]
colnames(contributionStatesDev1)[5] <- "state1"

bugStatesDev1 <- comments[comments$commentAuthor == "dev37",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev1$month==timespan[i])) {
  }
  else {
    bugStatesDev1 <- rbind(bugStatesDev1,c(bugStatesDev1$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev1 <- bugStatesDev1[order(bugStatesDev1$month),]
colnames(bugStatesDev1)[4] <- "state2"

commStatesDev1 <- communication[communication$name == "dev37",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev1$month==timespan[i])) {
  }
  else {
    commStatesDev1 <- rbind(commStatesDev1,c(commStatesDev1$name[1],timespan[i],0,0,0,"low"))
  }
}

commStatesDev1 <- commStatesDev1[order(commStatesDev1$month),]
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

#--------------------------------- dev2 -------------------------------------------

contributionStatesDev2 <- contributions[contributions$name == "dev2",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev2$month==timespan[i])) {
  }
  else {
    contributionStatesDev2 <- rbind(contributionStatesDev2,c(contributionStatesDev2$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev2 <- contributionStatesDev2[order(contributionStatesDev2$month),]
colnames(contributionStatesDev2)[5] <- "state1"

bugStatesDev2 <- comments[comments$commentAuthor == "dev2",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev2$month==timespan[i])) {
  }
  else {
    bugStatesDev2 <- rbind(bugStatesDev2,c(bugStatesDev2$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev2 <- bugStatesDev2[order(bugStatesDev2$month),]
colnames(bugStatesDev2)[4] <- "state2"

commStatesDev2 <- communication[communication$name == "dev2",]

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
plot(pred2$s,type="l", ylim=c(1,3), yaxt = "n", xlab = "time", ylab = "state")
axis(2, seq(1, 3, by = 1)) 


#--------------------------------- dev 3 -------------------------------------------

contributionStatesDev3 <- contributions[contributions$name == "dev15",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev3$month==timespan[i])) {
  }
  else {
    contributionStatesDev3 <- rbind(contributionStatesDev3,c(contributionStatesDev3$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev3 <- contributionStatesDev3[order(contributionStatesDev3$month),]
colnames(contributionStatesDev3)[5] <- "state1"

bugStatesDev3 <- comments[comments$commentAuthor == "dev15",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev3$month==timespan[i])) {
  }
  else {
    bugStatesDev3 <- rbind(bugStatesDev3,c(bugStatesDev3$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev3 <- bugStatesDev3[order(bugStatesDev3$month),]
colnames(bugStatesDev3)[4] <- "state2"

commStatesDev3 <- communication[communication$name == "dev15",]

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
plot(pred3$s,type="l", ylim=c(1,3), yaxt = "n", xlab = "time", ylab = "state")
axis(2, seq(1, 3, by = 1)) 


#--------------------------------- dev 4 -------------------------------------------

contributionStatesDev4 <- contributions[contributions$name == "dev38",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev4$month==timespan[i])) {
  }
  else {
    contributionStatesDev4 <- rbind(contributionStatesDev4,c(contributionStatesDev4$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev4 <- contributionStatesDev4[order(contributionStatesDev4$month),]
colnames(contributionStatesDev4)[5] <- "state1"

bugStatesDev4 <- comments[comments$commentAuthor == "dev38",]

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

commStatesDev4 <- communication[communication$name == "dev38",]

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

contributionStatesDev5 <- contributions[contributions$name == "dev39",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev5$month==timespan[i])) {
  }
  else {
    contributionStatesDev5 <- rbind(contributionStatesDev5,c(contributionStatesDev5$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev5 <- contributionStatesDev5[order(contributionStatesDev5$month),]
colnames(contributionStatesDev5)[5] <- "state1"

bugStatesDev5 <- comments[comments$commentAuthor == "dev39",]

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

commStatesDev5 <- communication[communication$name == "dev39",]

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

#--------------------------------- dev 6 -------------------------------------------

contributionStatesDev6 <- contributions[contributions$name == "dev40",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev6$month==timespan[i])) {
  }
  else {
    contributionStatesDev6 <- rbind(contributionStatesDev6,c(contributionStatesDev6$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev6 <- contributionStatesDev6[order(contributionStatesDev6$month),]
colnames(contributionStatesDev6)[5] <- "state1"

bugStatesDev6 <- comments[comments$commentAuthor == "dev40",]

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

commStatesDev6 <- communication[communication$name == "dev40",]

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

contributionStatesDev7 <- contributions[contributions$name == "dev85",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev7$month==timespan[i])) {
  }
  else {
    contributionStatesDev7 <- rbind(contributionStatesDev7,c(contributionStatesDev7$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev7 <- contributionStatesDev7[order(contributionStatesDev7$month),]
colnames(contributionStatesDev7)[5] <- "state1"

bugStatesDev7 <- comments[comments$commentAuthor == "dev85",]

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

commStatesDev7 <- communication[communication$name == "dev85",]

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

contributionStatesDev8 <- contributions[contributions$name == "dev67",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev8$month==timespan[i])) {
  }
  else {
    contributionStatesDev8 <- rbind(contributionStatesDev8,c(contributionStatesDev8$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev8 <- contributionStatesDev8[order(contributionStatesDev8$month),]
colnames(contributionStatesDev8)[5] <- "state1"

bugStatesDev8 <- comments[comments$commentAuthor == "dev67",]

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

commStatesDev8 <- communication[communication$name == "dev67",]

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

contributionStatesDev9 <- contributions[contributions$name == "dev91",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev9$month==timespan[i])) {
  }
  else {
    contributionStatesDev9 <- rbind(contributionStatesDev9,c(contributionStatesDev9$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev9 <- contributionStatesDev9[order(contributionStatesDev9$month),]
colnames(contributionStatesDev9)[5] <- "state1"

bugStatesDev9 <- comments[comments$commentAuthor == "dev91",]

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

commStatesDev9 <- communication[communication$name == "dev91",]

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

plot(pred9$s,type="l",ylim=c(1,3))

#--------------------------------- dev 10 -------------------------------------------


contributionStatesDev10 <- contributions[contributions$name == "dev54",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev10$month==timespan[i])) {
  }
  else {
    contributionStatesDev10 <- rbind(contributionStatesDev10,c(contributionStatesDev10$name[1],timespan[i],0,0,"low"))
  }
}

contributionStatesDev10 <- contributionStatesDev10[order(contributionStatesDev10$month),]
colnames(contributionStatesDev10)[5] <- "state1"

bugStatesDev10 <- comments[comments$commentAuthor == "dev54",]

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

commStatesDev10 <- communication[communication$name == "dev54",]

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


# estimate initial probabilities

init_est <- c(0.9,0.1,0)

# average emission distribution 

# core: 2

coreKonsole <- hmm2$model$parms.emission

save(coreKonsole, file="coreKonsole.rda")

# major 

majorKonsole <- hmm3$model$parms.emission

save(majorKonsole, file="majorKonsole.rda")

# minor:

minorKonsole <- hmm7$model$parms.emission

save(minorKonsole, file="minorKonsole.rda")


# learn with universal model 

# core

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

mr2u <- mean(pred$s!=classObsDev2$stateLabel)

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

mr3u <- mean(pred$s!=classObsDev3$stateLabel)
mr1u <- mean(pred$s!=classObsDev1$stateLabel)
mr4u <- mean(pred$s!=classObsDev4$stateLabel)
mr5u <- mean(pred$s!=classObsDev5$stateLabel)
mr6u <- mean(pred$s!=classObsDev6$stateLabel)

# minor 

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

mr7u <- mean(pred$s!=classObsDev7$stateLabel)







