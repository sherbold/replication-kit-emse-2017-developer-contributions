library (HMM)
library (lattice)
library(ggplot2)
library(mhsmm)
library(MASS)

#commit data
setwd("C:\\Users\\vhonsel\\Documents\\Mining\\Case Studies\\Rekonq\\input")
committers = read.csv("committer_rekonq.csv", header=T, sep=",", stringsAsFactors=FALSE)
commits = read.csv("commits_rekonq.csv", header=T, sep=",", stringsAsFactors=FALSE)
#mailinglist data
setwd("C:\\Users\\vhonsel\\Documents\\Mining\\Case Studies\\Rekonq\\MailingLists")
mailingList = read.csv("ml_rekonq.csv", header=T,  sep=",", stringsAsFactors=FALSE)
ml = read.csv("ml_rekonq_wm.csv", header=T,  sep=",", stringsAsFactors=FALSE)
#bugzilla data
setwd("C:\\Users\\vhonsel\\Documents\\Mining\\Case Studies\\Rekonq\\HMM")
bz = read.csv("bugzilla.csv", header=T,  sep=",", stringsAsFactors=FALSE)
setwd("C:\\Users\\vhonsel\\Documents\\Mining\\Case Studies\\Rekonq\\input")
bz_comments = read.csv("bz_comments.csv", header=T, sep=",", stringsAsFactors=FALSE)
bz_issues =  read.csv("bz_issues.csv", header=T, sep=",", stringsAsFactors=FALSE)
bz_actions =  read.csv("bz_events.csv", header=T, sep=",", stringsAsFactors=FALSE) 


#filter Script Kiddy

committers <- committers[grep('Script',committers$name,invert = TRUE),]
committers <- committers[grep('script',committers$name,invert = TRUE),]

#fix naming problems

for  (w in 1:nrow(committers)){
  if(committers$name[w] == 'adjam')
  {
    committers$name[w] <- 'dev6'
  }
  if(committers$name[w] == 'lionelc')
  {
    committers$name[w] <- 'dev122'
  }
  if(committers$name[w] == 'megabigbug')
  {
    committers$name[w] <- 'dev122'
  }
  if(committers$name[w] == 'matgic78')
  {
    committers$name[w] <- 'dev123'
  }
} 



#files_touched

for (p in 1:nrow(committers)){
  
  committers$touched[p] <-sum(committers$name == committers$name[p])
  
} 


#omit developers with less than 20 touches

committers <- committers[committers$touched >= 20,]


write.csv(commiters, "commiters.csv", row.names=FALSE)

#observations

#general observation, contributions from all developers over time

for (s in 1:nrow(committers)){
  
  committers$month[s] <- substr(committers$commit_date[s],1,7) 
  #committers$monthly_commits[s] <-sum(committers$commit_date <'2008-11')
  
} 

for (t in 1:nrow(committers)){
  
  committers$noc[t] <- sum(committers$month == committers$month[t]) 
  
} 

summary(committers)

#plot commits per month

com_per_m <- committers[, c(9,10)]
com_per_m <- unique(com_per_m)

zz <- ggplot(com_per_m, aes(com_per_m$month, com_per_m$noc))
print(zz + geom_bar(stat = "identity", aes(com_per_m$month, com_per_m$noc), width=1.0, colour = "black")
      + theme(axis.text.x=element_text(angle=-90))
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="commits")) 

#code activities



for (u in 1:nrow(committers)){
  
  committers$dnoc[u] <- sum(committers$month == committers$month[u] & committers$name == committers$name[u] & committers$is_bug_fix != 1) 
  
}

for (v in 1:nrow(committers)){
  
  committers$bugfixes[v] <- sum(committers$month == committers$month[v] & committers$name == committers$name[v] & committers$is_bug_fix == 1) 
  
}

devcom_per_m <- committers[, c(1,9,10,11,12)]
devcom_per_m <- unique(devcom_per_m)
xx <- ggplot(devcom_per_m, aes(devcom_per_m$month, devcom_per_m$noc))
print(xx + geom_bar(stat = "identity", aes(devcom_per_m$month, devcom_per_m$dnoc, fill = devcom_per_m$name, position = "dodge"), width=1.0)
      + theme(axis.text.x=element_text(angle=-90))
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="commits")) 


yy <- ggplot(devcom_per_m, aes(devcom_per_m$month, y=devcom_per_m$noc, colour=devcom_per_m$name)) 
print(yy  + geom_point()
  + theme(axis.text.x=element_text(angle=-90))
  + scale_x_discrete(name="month") 
  + scale_y_continuous(name="commits"))


print(ggplot(devcom_per_m, aes(x=devcom_per_m$month, y=devcom_per_m$noc, colour=devcom_per_m$name, group=devcom_per_m$name)) 
#   + geom_point() 
   + geom_line(aes(devcom_per_m$month, devcom_per_m$dnoc)) 
   + theme(axis.text.x=element_text(angle=-90)) 
   + scale_x_discrete(name="month") 
   + scale_y_continuous(name="commits")
   + scale_colour_discrete(name  ="Developer", 
                          breaks = c("dev6", "dev47", "dev122", "dev123", "dev110", 
                                     "dev19", "dev124", "dev125eñalba", "dev126", "dev127röscher"),
                          labels=c("dev 1", "dev 2", "dev 3", "dev 4", "dev 5", "dev 6", "dev 7", "dev 8", "dev 9", "dev 10")))

print(ggplot(devcom_per_m, aes(x=devcom_per_m$month, y=devcom_per_m$bugfixes, colour=devcom_per_m$name, group=devcom_per_m$name)) 
      #   + geom_point() 
      + geom_line(aes(devcom_per_m$month, devcom_per_m$bugfixes)) 
      + theme(axis.text.x=element_text(angle=-90)) 
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="bugfixes")
      + scale_colour_discrete(name  ="Developer", 
                              breaks = c("dev6", "dev47", "dev122", "dev123", "dev110", 
                                         "dev19", "dev124", "dev125eñalba", "dev126", "dev127röscher"),
                              labels=c("dev 1", "dev 2", "dev 3", "dev 4", "dev 5", "dev 6", "dev 7", "dev 8", "dev 9", "dev 10")))



#communication activities

for (s in 1:nrow(ml)){
  
  ml$month[s] <- substr(ml$first_date[s],1,7) 
  #committers$monthly_commits[s] <-sum(committers$commit_date <'2008-11')
  
} 

for (t in 1:nrow(ml)){
  
  ml$activities[t] <- sum(ml$name[t] == ml$name)
  ml$opened[t] <- sum(ml$month[t] == ml$month & ml$name[t] == ml$name & ml$is_response_of == 'NULL')
  ml$responses[t] <- sum(ml$month[t] == ml$month & ml$name[t] == ml$name & ml$is_response_of != 'NULL')
  
} 


# filter devs with less than 10 actions

ml <- ml[ml$activities >= 20,]

# choose devs under consideration

ml <-ml[grep("dev6|dev47|dev122|MatthieuG|dev110|dev19|dev124|dev125|dev126|dev127r",ml$name),] 

# dev123 = MatthieuG

vx <- ggplot(ml, aes(ml$month, y=ml$opened, colour=ml$name)) 
print(vx  + geom_point()
      + theme(axis.text.x=element_text(angle=-90))
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="threads opened"))

vy <- ggplot(ml, aes(ml$month, y=ml$responses, colour=ml$name)) 
print(vy  + geom_point()
      + theme(axis.text.x=element_text(angle=-90))
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="responses"))

for  (j in 1:nrow(ml)){
  if(grepl('dev125',ml$name[j])==TRUE)
  {
    ml$name[j] <- 'dev125'
  }
  if(grepl('dev127r',ml$name[j]) == TRUE)
  {
    ml$name[j] <- 'dev127'
  }
}

print(ggplot(ml, aes(x=ml$month, y=ml$responses, colour=ml$name, group=ml$name)) 
      #   + geom_point() 
      + geom_line(aes(ml$month, ml$responses)) 
      + theme(axis.text.x=element_text(angle=-90)) 
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="responses")
      + scale_colour_discrete(name  ="Developer", 
                              breaks = c("dev6", "dev47", "dev122", "MatthieuG", "dev110", 
                                         "dev19", "dev124", "dev125", "dev126", "dev127"),
                              labels=c("dev 1", "dev 2", "dev 3", "dev 4", "dev 5", "dev 6", "dev 7", "dev 8", "dev 9", "dev 10")))

print(ggplot(ml, aes(x=ml$month, y=ml$opened, colour=ml$name, group=ml$name)) 
      #   + geom_point() 
      + geom_line(aes(ml$month, ml$opened)) 
      + theme(axis.text.x=element_text(angle=-90)) 
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="threads opened")
      + scale_colour_discrete(name  ="Developer", 
                              breaks = c("dev6", "dev47", "dev122", "MatthieuG", "dev110", 
                                         "dev19", "dev124", "dev125", "dev126", "dev127"),
                              labels=c("dev 1", "dev 2", "dev 3", "dev 4", "dev 5", "dev 6", "dev 7", "dev 8", "dev 9", "dev 10")))





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

bz_comments_dev <-bz_comments[grep("dev6|dev47|dev122|matgic|dev110|dev19|dev124|dev125|dev126|dev127r",bz_comments$commentAuthor),]
bz_issues_rep <-bz_issues[grep("dev6|dev47|dev122|matgic|dev110|dev19|dev124|dev125|dev126|dev127r",bz_issues$reportedBy),] 
bz_issues_ass <- bz_issues[grep("dev6|dev47|dev122|matgic|dev110|dev19|dev124|dev125|dev126|dev127r",bz_issues$assignedTo),] 
#need to link the authornames to real names
bz_issues_act <-  bz_actions[grep("dev6|dev47|dev122|matgic|dev110|dev19|dev124|dev125|dev126|dev127r",bz_actions$author),]


print(ggplot(bz_comments_dev, aes(bz_comments_dev$month, y=bz_comments_dev$nocomm, colour=bz_comments_dev$commentAuthor, group=bz_comments_dev$commentAuthor))
      + geom_line(aes(bz_comments_dev$month, bz_comments_dev$nocomm))
      + theme(axis.text.x=element_text(angle=-90))
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="number of bug comments")
      + scale_colour_discrete(name  ="Developer", 
                              breaks = c("dev6", "dev47", "dev122", "matgic78", "dev110", 
                                         "dev19", "dev124", "dev125", "dev126", "dev127"),
                              labels=c("dev 1", "dev 2", "dev 3", "dev 4", "dev 5", "dev 6", "dev 7", "dev 8", "dev 9", "dev 10")))

print(ggplot(bz_issues_rep, aes(bz_issues_rep$reported_month, y=bz_issues_rep$reported, colour=bz_issues_rep$reportedBy, group=bz_issues_rep$reportedBy))
      + geom_line(aes(bz_issues_rep$reported_month, bz_issues_rep$reported))
      + theme(axis.text.x=element_text(angle=-90))
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="number of bug reports")
      + scale_colour_discrete(name  ="Developer", 
                              breaks = c("dev6", "dev47", "dev122", "matgic78", "dev110", 
                                         "dev19", "dev124", "dev125", "dev126", "dev127"),
                              labels=c("dev 1", "dev 2", "dev 3", "dev 4", "dev 5", "dev 6", "dev 7", "dev 8", "dev 9", "dev 10")))

#theme(legend.position="none") 

#to be improved

print(ggplot(bz_actions, aes(bz_actions$month, y=bz_actions$actions, colour=bz_actions$author, group=bz_actions$author))
        + geom_line(aes(bz_actions$month, bz_actions$actions))
      + theme(axis.text.x=element_text(angle=-90))
      + scale_x_discrete(name="month") 
      + scale_y_continuous(name="number of bug activities"))

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

# 1: contribution actions

contributions <- committers[, c(1,9,11,12)]
contributions <- unique(contributions)
communication <- ml[,c(9,13,15,16)]
communication <- unique(communication)
#communication <- communication[,c(3,4)]

#for (n in 1:nrow(bz_comments_dev)){
#  if (bz_comments_dev$commentAuthor == bz_issues_rep$reportedBy & bz_comments_dev$month == bz_issues_rep$reported_month)
#  {
#    bz_comments_dev$reported[] <- bz_issues_rep$reported
#  }
#}
#bugactivity <- 

reports <- bz_issues_rep[,c(18,29,30,31)]
comments <- bz_comments_dev[,c(4,9,10)]
comments <- unique(comments)

write.csv(contributions, "contributions.csv", row.names=FALSE)
write.csv(communication, "communication.csv", row.names=FALSE)
write.csv(comments, "bug_comments.csv", row.names=FALSE)

#prepare data according to retrieved thresholds

#thresholds: commits 13.8/ 34.5 , bugfixes 0/5.5, opened 0/0 , responses 11/27.5 , bug comments 20.7/62.1 , reports 0/0

#learn model

states =  c("lowLearn", "mediumLearn", "highLearn")

#try out with contribution data, one-dimensional

contributionStates =  read.csv("contr_thresholds.csv", header=T, sep=";", stringsAsFactors=FALSE)



#calculate probabilities 

symbols = c("low", "medium", "high")

# choose developer dev6

dev <- contributionStates[contributionStates$name == "dev6",]
dev <- dev[order(dev$month),]
devComm <- communication[communication$name == "dev6",]
devComm <- devComm[order(devComm$month),]
devBug <- bz_comments_dev[bz_comments_dev$commentAuthor == "dev6",]
devBug <- devBug[,c(4,9,10)]
devBug <- unique(devBug)
devBug <- devBug[order(devBug$month),]

timespan <- c(dev$month,"2012-05","2012-06","2012-07","2012-08","2012-09","2012-10","2012-11","2012-12",
              "2013-01","2013-02","2013-03","2013-04","2013-05","2013-06","2013-07","2013-08","2013-09","2013-10","2013-11","2013-12",
              "2014-01","2014-02","2014-03","2014-04","2014-05","2014-06","2014-07","2014-08","2014-09")

# fill N/As

for (i in 1:length(timespan)) {
  if (any(dev$month==timespan[i])) {
 }
 else {
    dev <- rbind(dev,c(dev$name[1],timespan[i],0,0,0,0,"low"))
  }
}


for (i in 1:length(timespan)) {
  if (any(devComm$month==timespan[i])) {
  }
  else {
    devComm <- rbind(devComm,c(devComm$name[1],timespan[i],0,0))
  }
}


for (i in 1:length(timespan)) {
  if (any(devBug$month==timespan[i])) {
  }
  else {
    devBug <- rbind(devBug,c(devBug$commentAuthor[1],timespan[i],0))
  }
}

dev <- dev[order(dev$month),]
devComm <- devComm[order(devComm$month),]
devBug <- devBug[order(devBug$month),]

observations <- dev$state

hmm = initHMM(states,symbols,
              startProbs=c(0.8,0.15,0.05),
              transProbs=matrix(c(0.6,0.3,0.1,0.3,0.5,0.2,0.1,0.4,0.5),3),
              emissionProbs=matrix(c(0.8,0.15,0.05,0.2,0.5,0.3,0.05,0.45,0.5),3))
print(hmm)

# Baum-Welch

bw = baumWelch(hmm,observations,30)
print(bw$hmm)

# Viterbi to get most probable state sequence

viterbi <- viterbi(bw$hmm,observations)
print(viterbi)

# choose developer dev47

dev2 <- contributionStates[contributionStates$name == "dev47",]

# fill N/As

for (i in 1:length(timespan)) {
  if (any(dev2$month==timespan[i])) {
  }
  else {
    dev2 <- rbind(dev2,c(dev2$name[1],timespan[i],0,0,0,0,"low"))
  }
}

dev2 <- dev2[order(dev2$month),]


observations2 <- dev2$state

hmm = initHMM(states,symbols,
              startProbs=c(0.8,0.15,0.05),
              transProbs=matrix(c(0.6,0.3,0.1,0.3,0.5,0.2,0.1,0.4,0.5),3),
              emissionProbs=matrix(c(0.8,0.15,0.05,0.2,0.5,0.3,0.05,0.45,0.5),3))
print(hmm)

# Baum-Welch

bw = baumWelch(hmm,observations2,30)
print(bw$hmm)

# Viterbi to get most probable state sequence

viterbi <- viterbi(bw$hmm,observations2)
print(viterbi)


# choose developer dev122

dev3 <- contributionStates[contributionStates$name == "dev122",]

# fill N/As

for (i in 1:length(timespan)) {
  if (any(dev3$month==timespan[i])) {
  }
  else {
    dev3 <- rbind(dev3,c(dev3$name[1],timespan[i],0,0,0,0,"low"))
  }
}

dev3 <- dev3[order(dev3$month),]


observations3 <- dev3$state

hmm = initHMM(states,symbols,
              startProbs=c(0.8,0.15,0.05),
              transProbs=matrix(c(0.6,0.3,0.1,0.3,0.5,0.2,0.1,0.4,0.5),3),
              emissionProbs=matrix(c(0.8,0.15,0.05,0.2,0.5,0.3,0.05,0.45,0.5),3))
print(hmm)

# Baum-Welch

bw = baumWelch(hmm,observations3,30)
print(bw$hmm)

# Viterbi to get most probable state sequence

viterbi <- viterbi(bw$hmm,observations3)
print(viterbi)

# choose developer dev123

dev4 <- contributionStates[contributionStates$name == "dev123",]

# fill N/As

for (i in 1:length(timespan)) {
  if (any(dev4$month==timespan[i])) {
  }
  else {
    dev4 <- rbind(dev4,c(dev4$name[1],timespan[i],0,0,0,0,"low"))
  }
}

dev4 <- dev4[order(dev4$month),]


observations4 <- dev4$state

hmm = initHMM(states,symbols,
              startProbs=c(0.8,0.15,0.05),
              transProbs=matrix(c(0.6,0.3,0.1,0.3,0.5,0.2,0.1,0.4,0.5),3),
              emissionProbs=matrix(c(0.8,0.15,0.05,0.2,0.5,0.3,0.05,0.45,0.5),3))
print(hmm)

# Baum-Welch

bw = baumWelch(hmm,observations4,30)
print(bw$hmm)

# Viterbi to get most probable state sequence

viterbi <- viterbi(bw$hmm,observations3)
print(viterbi)
      
# ---------------------- multi observation training ---------------------------------------------------     

# needed: vector including all observations, vector with length of observations

N <- as.numeric(c(length(dev$dnoc),length(dev$bugfixes),length(devComm$responses),length(devBug$nocomm)))
obs <- as.numeric(c(dev$dnoc,x=dev$bugfixes,x=devComm$responses,x=devBug$nocomm))
train <- list(x = obs, N = N)
class(train) <- "hsmm.data"

plot(train)

# estimate mu and sigma

# 1: split data into three parts and calculate mean and variance

tmp <- as.data.frame(obs)
trainSets <- split(tmp, sample(1:3, nrow(tmp), replace=T))
trainSets
mu1 <- mean(unlist(trainSets$`1`))
mu2 <- mean(unlist(trainSets$`2`))
mu3 <- mean(unlist(trainSets$`3`))
sigma1 <- var(unlist(trainSets$`1`))
sigma2 <- var(unlist(trainSets$`2`))
sigma3 <- var(unlist(trainSets$`3`))


# 2: k-means 


# compare likelihoods





init <- c(0.8,0.15,0.05)
trans <- matrix(c(0.6,0.3,0.1,0.3,0.5,0.2,0.1,0.4,0.5),3)
emis <- list(mu = c(mu1, mu2, mu3), sigma = c(sigma1, sigma2, sigma3))

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dnorm.hsmm)
model

hmm <- hmmfit(train, model, mstep = mstep.norm)

statesPred <- predict(hmm, train, method = "viterbi")
plot(train)
addStates(statesPred$s)
plot(hmm$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iteration")

#try out model with different series 

#estimate emission distribution new

#----------------------- train model -----------------------------------------

#training set according to threshold labeling

contributionStates =  read.csv("contr_thresholds.csv", header=T, sep=";", stringsAsFactors=FALSE)
bugStates = read.csv("bug_thresholds.csv", header=T, sep=";", stringsAsFactors=FALSE)
commStates = read.csv("comm_thresholds.csv", header=T, sep=";", stringsAsFactors=FALSE)

contributionStatesDev1 <- contributionStates[contributionStates$name == "dev6",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev1$month==timespan[i])) {
  }
  else {
    contributionStatesDev1 <- rbind(contributionStatesDev1,c(contributionStatesDev1$name[1],timespan[i],0,0,0,0,"low"))
  }
}

contributionStatesDev1 <- contributionStatesDev1[order(contributionStatesDev1$month),]
colnames(contributionStatesDev1)[7] <- "state1"

bugStatesDev1 <- bugStates[bugStates$commentAuthor == "dev6",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev1$month==timespan[i])) {
  }
  else {
    bugStatesDev1 <- rbind(bugStatesDev1,c(bugStatesDev1$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev1 <- bugStatesDev1[order(bugStatesDev1$month),]
colnames(bugStatesDev1)[4] <- "state2"

commStatesDev1 <- commStates[commStates$name == "dev6",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev1$month==timespan[i])) {
  }
  else {
    commStatesDev1 <- rbind(commStatesDev1,c(commStatesDev1$name[1],timespan[i],0,0,"low"))
  }
}

commStatesDev1 <- commStatesDev1[order(commStatesDev1$month),]
colnames(commStatesDev1)[5] <- "state3"

classObsDev1 <- cbind(contributionStatesDev1,bugStatesDev1,commStatesDev1)

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

for (i in 1:nrow(classObsDev1)) {
  
  classObsDev1$stateLabel[i] <- calculateState(classObsDev1$state1[i],classObsDev1$state2[i],classObsDev1$state3[i])
  
}

# split indexes according to stateLabel

indexSet1 <- sample(which(classObsDev1$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev1$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev1$stateLabel=="high"))


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


obs1 <- as.numeric(dev$dnoc)
obs2 <- as.numeric(dev$bugfixes)
obs3 <- as.numeric(devComm$responses)
obs4 <- as.numeric(devBug$nocomm)

emis <- estimatePar(obs1,obs2,obs3,obs4)
init <- setInit()
trans <- setTrans()
print(emis)
print(init)
print(trans)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train <- simulate(model, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm1 <- hmmfit(train, model, mstep = mstep.mvnorm)
#hmm <- hmmfit(trainDev, model, mstep = mstep.mvnorm)

N <- as.numeric(length(dev$dnoc))
obs <- cbind(as.numeric(dev$dnoc),as.numeric(dev$bugfixes),as.numeric(devComm$responses),as.numeric(devBug$nocomm))
trainDev <- list(x = obs, N = N)
class(trainDev) <- "hsmm.data"

plot(trainDev, main = "Contribution Activities")
pred1 <- predict(hmm1, trainDev, method = "viterbi")
plot(trainDev$x[,1],type="l", xlab="days", ylab="commits")
addStates(pred1$s)

# TODO: presentation of results

plot(pred1$s,type="l", xlab="days", ylab="state")

# state prediction for other developers

#--------------------- dev2 -----------------------

#dev2 <- contributionStates[contributionStates$name == "dev47",]
#dev2Comm <- communication[communication$name == "dev47",]
#dev2Bug <- bz_comments_dev[bz_comments_dev$commentAuthor == "dev47",]
#dev2Bug <- dev2Bug[,c(4,9,10)]
#dev2Bug <- unique(dev2Bug)

contributionStatesDev2 <- contributionStates[contributionStates$name == "dev47",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev2$month==timespan[i])) {
  }
  else {
    contributionStatesDev2 <- rbind(contributionStatesDev2,c(contributionStatesDev2$name[1],timespan[i],0,0,0,0,"low"))
  }
}

contributionStatesDev2 <- contributionStatesDev2[order(contributionStatesDev2$month),]
colnames(contributionStatesDev2)[7] <- "state1"

bugStatesDev2 <- bugStates[bugStates$commentAuthor == "dev47",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev2$month==timespan[i])) {
  }
  else {
    bugStatesDev2 <- rbind(bugStatesDev2,c(bugStatesDev2$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev2 <- bugStatesDev2[order(bugStatesDev2$month),]
colnames(bugStatesDev2)[4] <- "state2"

commStatesDev2 <- commStates[commStates$name == "dev47",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev2$month==timespan[i])) {
  }
  else {
    commStatesDev2 <- rbind(commStatesDev2,c(commStatesDev2$name[1],timespan[i],0,0,"low"))
  }
}

commStatesDev2 <- commStatesDev2[order(commStatesDev2$month),]
colnames(commStatesDev2)[5] <- "state3"

classObsDev2 <- cbind(contributionStatesDev2,bugStatesDev2,commStatesDev2)

for (i in 1:nrow(classObsDev2)) {
  
  classObsDev2$stateLabel[i] <- calculateState(classObsDev2$state1[i],classObsDev2$state2[i],classObsDev2$state3[i])
  
}

# split indexes according to stateLabel

indexSet1 <- sample(which(classObsDev2$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev2$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev2$stateLabel=="high"))

obs1 <- as.numeric(classObsDev2$dnoc)
obs2 <- as.numeric(classObsDev2$bugfixes)
obs3 <- as.numeric(classObsDev2$responses)
obs4 <- as.numeric(classObsDev2$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train <- simulate(model, nsim=50, seed = 123, rand.emis = rmvnorm.hsmm)

hmm2 <- hmmfit(train, model, mstep = mstep.mvnorm)
#hmmDev <- hmmfit(trainDev, model, mstep = mstep.mvnorm)


N <- as.numeric(length(classObsDev2$dnoc))
obs <- cbind(as.numeric(classObsDev2$dnoc),as.numeric(classObsDev2$bugfixes),as.numeric(classObsDev2$responses),as.numeric(classObsDev2$nocomm))
trainDev2 <- list(x = obs, N = N)
class(trainDev2) <- "hsmm.data"

plot(trainDev2)
pred2 <- predict(hmm2, trainDev2, method = "viterbi")
plot(trainDev2$x[,1],type="l")
addStates(pred2$s)

plot(pred2$s,type="l")

# TODO: adapt for other developers

# --------------------- dev3 ------------------------------

#dev3 <- contributionStates[contributionStates$name == "dev122",]
#dev3Comm <- communication[communication$name == "dev122",]
#dev3Bug <- bz_comments_dev[bz_comments_dev$commentAuthor == "dev122",]
#dev3Bug <- dev3Bug[,c(4,9,10)]
#dev3Bug <- unique(dev3Bug)

contributionStatesDev3 <- contributionStates[contributionStates$name == "dev122",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev3$month==timespan[i])) {
  }
  else {
    contributionStatesDev3 <- rbind(contributionStatesDev3,c(contributionStatesDev3$name[1],timespan[i],0,0,0,0,"low"))
  }
}

contributionStatesDev3 <- contributionStatesDev3[order(contributionStatesDev3$month),]
colnames(contributionStatesDev3)[7] <- "state1"

bugStatesDev3 <- bugStates[bugStates$commentAuthor == "dev122",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev3$month==timespan[i])) {
  }
  else {
    bugStatesDev3 <- rbind(bugStatesDev3,c(bugStatesDev3$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev3 <- bugStatesDev3[order(bugStatesDev3$month),]
colnames(bugStatesDev3)[4] <- "state2"

commStatesDev3 <- commStates[commStates$name == "dev122",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev3$month==timespan[i])) {
  }
  else {
    commStatesDev3 <- rbind(commStatesDev3,c(commStatesDev3$name[1],timespan[i],0,0,"low"))
  }
}

commStatesDev3 <- commStatesDev3[order(commStatesDev2$month),]
colnames(commStatesDev3)[5] <- "state3"

classObsDev3 <- cbind(contributionStatesDev3,bugStatesDev3,commStatesDev3)

for (i in 1:nrow(classObsDev3)) {
  
  classObsDev3$stateLabel[i] <- calculateState(classObsDev3$state1[i],classObsDev3$state2[i],classObsDev3$state3[i])
  
}

# split indexes according to stateLabel

indexSet1 <- sample(which(classObsDev3$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev3$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev3$stateLabel=="high"))

obs1 <- as.numeric(classObsDev3$dnoc)
obs2 <- as.numeric(classObsDev3$bugfixes)
obs3 <- as.numeric(classObsDev3$responses)
obs4 <- as.numeric(classObsDev3$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train <- simulate(model, nsim=50, seed = 123, rand.emis = rmvnorm.hsmm)

hmm3 <- hmmfit(train, model, mstep = mstep.mvnorm)
#hmmDev <- hmmfit(trainDev, model, mstep = mstep.mvnorm)


N <- as.numeric(length(classObsDev3$dnoc))
obs <- cbind(as.numeric(classObsDev3$dnoc),as.numeric(classObsDev3$bugfixes),as.numeric(classObsDev3$responses),as.numeric(classObsDev3$nocomm))
trainDev3 <- list(x = obs, N = N)
class(trainDev3) <- "hsmm.data"

plot(trainDev3)
pred3 <- predict(hmm3, trainDev3, method = "viterbi")
plot(trainDev3$x[,1],type="l")
addStates(pred3$s)

plot(pred3$s,type="l")

# --------------------- dev4 ------------------------------
 

#dev4 <- contributionStates[contributionStates$name == "dev123",]
#dev4Comm <- communication[communication$name == "MatthieuG",]
#dev4Bug <- bz_comments_dev[bz_comments_dev$commentAuthor == "matgic78",]
#dev4Bug <- dev4Bug[,c(4,9,10)]
#dev4Bug <- unique(dev4Bug)

contributionStatesDev4 <- contributionStates[contributionStates$name == "dev123",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev4$month==timespan[i])) {
  }
  else {
    contributionStatesDev4 <- rbind(contributionStatesDev4,c(contributionStatesDev4$name[1],timespan[i],0,0,0,0,"low"))
  }
}

contributionStatesDev4 <- contributionStatesDev4[order(contributionStatesDev4$month),]
colnames(contributionStatesDev4)[7] <- "state1"

bugStatesDev4 <- bugStates[bugStates$commentAuthor == "matgic78",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev4$month==timespan[i])) {
  }
  else {
    bugStatesDev4 <- rbind(bugStatesDev4,c(bugStatesDev4$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev4 <- bugStatesDev4[order(bugStatesDev4$month),]
colnames(bugStatesDev4)[4] <- "state2"

commStatesDev4 <- commStates[commStates$name == "MatthieuG",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev4$month==timespan[i])) {
  }
  else {
    commStatesDev4 <- rbind(commStatesDev4,c(commStatesDev4$name[1],timespan[i],0,0,"low"))
  }
}

commStatesDev4 <- commStatesDev4[order(commStatesDev2$month),]
colnames(commStatesDev4)[5] <- "state3"

classObsDev4 <- cbind(contributionStatesDev4,bugStatesDev4,commStatesDev4)

for (i in 1:nrow(classObsDev4)) {
  
  classObsDev4$stateLabel[i] <- calculateState(classObsDev4$state1[i],classObsDev4$state2[i],classObsDev4$state3[i])
  
}

# split indexes according to stateLabel

indexSet1 <- sample(which(classObsDev4$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev4$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev4$stateLabel=="high"))

obs1 <- as.numeric(classObsDev4$dnoc)
obs2 <- as.numeric(classObsDev4$bugfixes)
obs3 <- as.numeric(classObsDev4$responses)
obs4 <- as.numeric(classObsDev4$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train <- simulate(model, nsim=50, seed = 123, rand.emis = rmvnorm.hsmm)

hmm4 <- hmmfit(train, model, mstep = mstep.mvnorm)
#hmmDev <- hmmfit(trainDev, model, mstep = mstep.mvnorm)


N <- as.numeric(length(classObsDev4$dnoc))
obs <- cbind(as.numeric(classObsDev4$dnoc),as.numeric(classObsDev4$bugfixes),as.numeric(classObsDev4$responses),as.numeric(classObsDev4$nocomm))
trainDev4 <- list(x = obs, N = N)
class(trainDev4) <- "hsmm.data"

plot(trainDev4)
pred4 <- predict(hmm4, trainDev4, method = "viterbi")
plot(trainDev4$x[,1],type="l")
addStates(pred4$s)

plot(pred4$s,type="l")

# --------------------- dev5 ------------------------------

#dev5 <- contributionStates[contributionStates$name == "dev19",]
#dev5Comm <- communication[communication$name == "dev19",]
#dev5Bug <- c(rep(0,72))


contributionStatesDev5 <- contributionStates[contributionStates$name == "dev19",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev5$month==timespan[i])) {
  }
  else {
    contributionStatesDev5 <- rbind(contributionStatesDev5,c(contributionStatesDev5$name[1],timespan[i],0,0,0,0,"low"))
  }
}

contributionStatesDev5 <- contributionStatesDev5[order(contributionStatesDev5$month),]
colnames(contributionStatesDev5)[7] <- "state1"

bugStatesDev5 <- bugStates[bugStates$commentAuthor == "dev19",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev5$month==timespan[i])) {
  }
  else {
    bugStatesDev5 <- rbind(bugStatesDev5,c(bugStatesDev5$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev5 <- bugStatesDev5[order(bugStatesDev5$month),]
colnames(bugStatesDev5)[4] <- "state2"

commStatesDev5 <- commStates[commStates$name == "dev19",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev5$month==timespan[i])) {
  }
  else {
    commStatesDev5 <- rbind(commStatesDev5,c(commStatesDev5$name[1],timespan[i],0,0,"low"))
  }
}

commStatesDev5 <- commStatesDev5[order(commStatesDev2$month),]
colnames(commStatesDev5)[5] <- "state3"

classObsDev5 <- cbind(contributionStatesDev5,bugStatesDev5,commStatesDev5)

for (i in 1:nrow(classObsDev5)) {
  
  classObsDev5$stateLabel[i] <- calculateState(classObsDev5$state1[i],classObsDev5$state2[i],classObsDev5$state3[i])
  
}

# split indexes according to stateLabel

indexSet1 <- sample(which(classObsDev5$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev5$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev5$stateLabel=="high"))

obs1 <- as.numeric(classObsDev5$dnoc)
obs2 <- as.numeric(classObsDev5$bugfixes)
obs3 <- as.numeric(classObsDev5$responses)
obs4 <- as.numeric(classObsDev5$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train <- simulate(model, nsim=50, seed = 123, rand.emis = rmvnorm.hsmm)

hmm5 <- hmmfit(train, model, mstep = mstep.mvnorm)
#hmmDev <- hmmfit(trainDev, model, mstep = mstep.mvnorm)


N <- as.numeric(length(classObsDev5$dnoc))
obs <- cbind(as.numeric(classObsDev5$dnoc),as.numeric(classObsDev5$bugfixes),as.numeric(classObsDev5$responses),as.numeric(classObsDev5$nocomm))
trainDev5 <- list(x = obs, N = N)
class(trainDev5) <- "hsmm.data"

plot(trainDev5)
pred5 <- predict(hmm5, trainDev5, method = "viterbi")
plot(trainDev5$x[,1],type="l")
addStates(pred5$s)

plot(pred5$s,type="l")



#------------------- dev 6 ------------------------------

#dev6 <- contributionStates[contributionStates$name == "dev110",]
#dev6Comm <- communication[communication$name == "dev110",]
#dev6Bug <- bz_comments_dev[bz_comments_dev$commentAuthor == "dev110",]
#dev6Bug <- dev6Bug[,c(4,9,10)]
#dev6Bug <- unique(dev6Bug)

contributionStatesDev6 <- contributionStates[contributionStates$name == "dev110",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev6$month==timespan[i])) {
  }
  else {
    contributionStatesDev6 <- rbind(contributionStatesDev6,c(contributionStatesDev6$name[1],timespan[i],0,0,0,0,"low"))
  }
}

contributionStatesDev6 <- contributionStatesDev6[order(contributionStatesDev6$month),]
colnames(contributionStatesDev6)[7] <- "state1"

bugStatesDev6 <- bugStates[bugStates$commentAuthor == "dev110",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev6$month==timespan[i])) {
  }
  else {
    bugStatesDev6 <- rbind(bugStatesDev6,c(bugStatesDev6$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev6 <- bugStatesDev6[order(bugStatesDev6$month),]
colnames(bugStatesDev6)[4] <- "state2"

commStatesDev6 <- commStates[commStates$name == "dev110",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev6$month==timespan[i])) {
  }
  else {
    commStatesDev6 <- rbind(commStatesDev6,c(commStatesDev6$name[1],timespan[i],0,0,"low"))
  }
}

commStatesDev6 <- commStatesDev6[order(commStatesDev2$month),]
colnames(commStatesDev6)[5] <- "state3"

classObsDev6 <- cbind(contributionStatesDev6,bugStatesDev6,commStatesDev6)

for (i in 1:nrow(classObsDev6)) {
  
  classObsDev6$stateLabel[i] <- calculateState(classObsDev6$state1[i],classObsDev6$state2[i],classObsDev6$state3[i])
  
}

# split indexes according to stateLabel

indexSet1 <- sample(which(classObsDev6$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev6$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev6$stateLabel=="high"))

obs1 <- as.numeric(classObsDev6$dnoc)
obs2 <- as.numeric(classObsDev6$bugfixes)
obs3 <- as.numeric(classObsDev6$responses)
obs4 <- as.numeric(classObsDev6$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train <- simulate(model, nsim=50, seed = 123, rand.emis = rmvnorm.hsmm)

hmm6 <- hmmfit(train, model, mstep = mstep.mvnorm)
#hmmDev <- hmmfit(trainDev, model, mstep = mstep.mvnorm)


N <- as.numeric(length(classObsDev6$dnoc))
obs <- cbind(as.numeric(classObsDev6$dnoc),as.numeric(classObsDev6$bugfixes),as.numeric(classObsDev6$responses),as.numeric(classObsDev6$nocomm))
trainDev6 <- list(x = obs, N = N)
class(trainDev6) <- "hsmm.data"

plot(trainDev6)
pred6 <- predict(hmm6, trainDev6, method = "viterbi")
plot(trainDev6$x[,1],type="l")
addStates(pred6$s)

plot(pred6$s,type="l")


#------------------- dev 7 ------------------------------

#dev7 <- contributionStates[contributionStates$name == "dev124",]
#dev7Comm <- communication[communication$name == "dev124",]
#dev7Bug <- bz_comments_dev[bz_comments_dev$commentAuthor == "dev124",]
#dev7Bug <- dev7Bug[,c(4,9,10)]
#dev7Bug <- unique(dev7Bug)

contributionStatesDev7 <- contributionStates[contributionStates$name == "dev124",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev7$month==timespan[i])) {
  }
  else {
    contributionStatesDev7 <- rbind(contributionStatesDev7,c(contributionStatesDev7$name[1],timespan[i],0,0,0,0,"low"))
  }
}

contributionStatesDev7 <- contributionStatesDev7[order(contributionStatesDev7$month),]
colnames(contributionStatesDev7)[7] <- "state1"

bugStatesDev7 <- bugStates[bugStates$commentAuthor == "dev124",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev7$month==timespan[i])) {
  }
  else {
    bugStatesDev7 <- rbind(bugStatesDev7,c(bugStatesDev7$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev7 <- bugStatesDev7[order(bugStatesDev7$month),]
colnames(bugStatesDev7)[4] <- "state2"

commStatesDev7 <- commStates[commStates$name == "dev124",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev7$month==timespan[i])) {
  }
  else {
    commStatesDev7 <- rbind(commStatesDev7,c(commStatesDev7$name[1],timespan[i],0,0,"low"))
  }
}

commStatesDev7 <- commStatesDev7[order(commStatesDev2$month),]
colnames(commStatesDev7)[5] <- "state3"

classObsDev7 <- cbind(contributionStatesDev7,bugStatesDev7,commStatesDev7)

for (i in 1:nrow(classObsDev7)) {
  
  classObsDev7$stateLabel[i] <- calculateState(classObsDev7$state1[i],classObsDev7$state2[i],classObsDev7$state3[i])
  
}

# split indexes according to stateLabel

indexSet1 <- sample(which(classObsDev7$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev7$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev7$stateLabel=="high"))

obs1 <- as.numeric(classObsDev7$dnoc)
obs2 <- as.numeric(classObsDev7$bugfixes)
obs3 <- as.numeric(classObsDev7$responses)
obs4 <- as.numeric(classObsDev7$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train <- simulate(model, nsim=50, seed = 123, rand.emis = rmvnorm.hsmm)

hmm7 <- hmmfit(train, model, mstep = mstep.mvnorm)
#hmmDev <- hmmfit(trainDev, model, mstep = mstep.mvnorm)


N <- as.numeric(length(classObsDev7$dnoc))
obs <- cbind(as.numeric(classObsDev7$dnoc),as.numeric(classObsDev7$bugfixes),as.numeric(classObsDev7$responses),as.numeric(classObsDev7$nocomm))
trainDev7 <- list(x = obs, N = N)
class(trainDev7) <- "hsmm.data"

plot(trainDev7)
pred7 <- predict(hmm7, trainDev7, method = "viterbi")
plot(trainDev7$x[,1],type="l")
addStates(pred7$s)

plot(pred7$s,type="l")

#--------------------- dev 8 ------------------------------------------

#dev8 <- contributionStates[contributionStates$name == "dev126",]
#dev8Comm <- communication[communication$name == "dev126",]
#dev8Bug <- bz_comments_dev[bz_comments_dev$commentAuthor == "dev126",]
#dev8Bug <- dev8Bug[,c(4,9,10)]
#dev8Bug <- unique(dev8Bug)

contributionStatesDev8 <- contributionStates[contributionStates$name == "dev126",]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev8$month==timespan[i])) {
  }
  else {
    contributionStatesDev8 <- rbind(contributionStatesDev8,c(contributionStatesDev8$name[1],timespan[i],0,0,0,0,"low"))
  }
}

contributionStatesDev8 <- contributionStatesDev8[order(contributionStatesDev8$month),]
colnames(contributionStatesDev8)[7] <- "state1"

bugStatesDev8 <- bugStates[bugStates$commentAuthor == "dev126",]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev8$month==timespan[i])) {
  }
  else {
    bugStatesDev8 <- rbind(bugStatesDev8,c(bugStatesDev8$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev8 <- bugStatesDev8[order(bugStatesDev8$month),]
colnames(bugStatesDev8)[4] <- "state2"

commStatesDev8 <- commStates[commStates$name == "dev126",]

for (i in 1:length(timespan)) {
  if (any(commStatesDev8$month==timespan[i])) {
  }
  else {
    commStatesDev8 <- rbind(commStatesDev8,c(commStatesDev8$name[1],timespan[i],0,0,"low"))
  }
}

commStatesDev8 <- commStatesDev8[order(commStatesDev2$month),]
colnames(commStatesDev8)[5] <- "state3"

classObsDev8 <- cbind(contributionStatesDev8,bugStatesDev8,commStatesDev8)

for (i in 1:nrow(classObsDev8)) {
  
  classObsDev8$stateLabel[i] <- calculateState(classObsDev8$state1[i],classObsDev8$state2[i],classObsDev8$state3[i])
  
}

# split indexes according to stateLabel

indexSet1 <- sample(which(classObsDev8$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev8$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev8$stateLabel=="high"))

obs1 <- as.numeric(classObsDev8$dnoc)
obs2 <- as.numeric(classObsDev8$bugfixes)
obs3 <- as.numeric(classObsDev8$responses)
obs4 <- as.numeric(classObsDev8$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train <- simulate(model, nsim=50, seed = 123, rand.emis = rmvnorm.hsmm)

hmm8 <- hmmfit(train, model, mstep = mstep.mvnorm)
#hmmDev <- hmmfit(trainDev, model, mstep = mstep.mvnorm)


N <- as.numeric(length(classObsDev8$dnoc))
obs <- cbind(as.numeric(classObsDev8$dnoc),as.numeric(classObsDev8$bugfixes),as.numeric(classObsDev8$responses),as.numeric(classObsDev8$nocomm))
trainDev8 <- list(x = obs, N = N)
class(trainDev8) <- "hsmm.data"

plot(trainDev8)
pred8 <- predict(hmm8, trainDev8, method = "viterbi")
plot(trainDev8$x[,1],type="l")
addStates(pred8$s)

plot(pred8$s,type="l")


#--------------------- dev 9 ------------------------------------------

#dev9 <- contributionStates[contributionStates$name == "dev127r",]
#dev9Comm <- communication[communication$name == "dev127r",]
#dev9Bug <- bz_comments_dev[bz_comments_dev$commentAuthor == "dev127r",]
#dev9Bug <- dev9Bug[,c(4,9,10)]
#dev9Bug <- unique(dev9Bug)

contributionStatesDev9 <- contributionStates[grep("Johannes", contributionStates$name),]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev9$month==timespan[i])) {
  }
  else {
    contributionStatesDev9 <- rbind(contributionStatesDev9,c(contributionStatesDev9$name[1],timespan[i],0,0,0,0,"low"))
  }
}

contributionStatesDev9 <- contributionStatesDev9[order(contributionStatesDev9$month),]
colnames(contributionStatesDev9)[7] <- "state1"

bugStatesDev9 <- bugStates[grep("Johannes", bugStates$name),]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev9$month==timespan[i])) {
  }
  else {
    bugStatesDev9 <- rbind(bugStatesDev9,c(bugStatesDev9$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev9 <- bugStatesDev9[order(bugStatesDev9$month),]
colnames(bugStatesDev9)[4] <- "state2"

commStatesDev9 <- commStates[grep("Johannes", commStates$name),]

for (i in 1:length(timespan)) {
  if (any(commStatesDev9$month==timespan[i])) {
  }
  else {
    commStatesDev9 <- rbind(commStatesDev9,c(commStatesDev9$name[1],timespan[i],0,0,"low"))
  }
}

commStatesDev9 <- commStatesDev9[order(commStatesDev2$month),]
colnames(commStatesDev9)[5] <- "state3"

classObsDev9 <- cbind(contributionStatesDev9,bugStatesDev9,commStatesDev9)

for (i in 1:nrow(classObsDev9)) {
  
  classObsDev9$stateLabel[i] <- calculateState(classObsDev9$state1[i],classObsDev9$state2[i],classObsDev9$state3[i])
  
}

# split indexes according to stateLabel

indexSet1 <- sample(which(classObsDev9$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev9$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev9$stateLabel=="high"))

obs1 <- as.numeric(classObsDev9$dnoc)
obs2 <- as.numeric(classObsDev9$bugfixes)
obs3 <- as.numeric(classObsDev9$responses)
obs4 <- as.numeric(classObsDev9$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train <- simulate(model, nsim=50, seed = 123, rand.emis = rmvnorm.hsmm)

hmm9 <- hmmfit(train, model, mstep = mstep.mvnorm)
#hmmDev <- hmmfit(trainDev, model, mstep = mstep.mvnorm)


N <- as.numeric(length(classObsDev9$dnoc))
obs <- cbind(as.numeric(classObsDev9$dnoc),as.numeric(classObsDev9$bugfixes),as.numeric(classObsDev9$responses),as.numeric(classObsDev9$nocomm))
trainDev9 <- list(x = obs, N = N)
class(trainDev9) <- "hsmm.data"

plot(trainDev9)
pred9 <- predict(hmm9, trainDev9, method = "viterbi")
plot(trainDev9$x[,1],type="l")
addStates(pred9$s)

plot(pred9$s,type="l")

#---------------------------- dev 10 ----------------------------------------------

#dev10 <- contributionStates[contributionStates$name == "dev125",]
#dev10Comm <- communication[communication$name == "dev125",]
#dev10Bug <- bz_comments_dev[bz_comments_dev$commentAuthor == "dev125",]
#dev10Bug <- dev10Bug[,c(4,9,10)]
#dev10Bug <- unique(dev10Bug)

contributionStatesDev10 <- contributionStates[grep("Jon", contributionStates$name),]

for (i in 1:length(timespan)) {
  if (any(contributionStatesDev10$month==timespan[i])) {
  }
  else {
    contributionStatesDev10 <- rbind(contributionStatesDev10,c(contributionStatesDev10$name[1],timespan[i],0,0,0,0,"low"))
  }
}

contributionStatesDev10 <- contributionStatesDev10[order(contributionStatesDev10$month),]
colnames(contributionStatesDev10)[7] <- "state1"

bugStatesDev10 <- bugStates[grep("Jon", bugStates$name),]

for (i in 1:length(timespan)) {
  if (any(bugStatesDev10$month==timespan[i])) {
  }
  else {
    bugStatesDev10 <- rbind(bugStatesDev10,c(bugStatesDev10$commentAuthor[1],timespan[i],0,"low"))
  }
}

bugStatesDev10 <- bugStatesDev10[order(bugStatesDev10$month),]
colnames(bugStatesDev10)[4] <- "state2"

commStatesDev10 <- commStates[grep("Jon", commStates$name),]

for (i in 1:length(timespan)) {
  if (any(commStatesDev10$month==timespan[i])) {
  }
  else {
    commStatesDev10 <- rbind(commStatesDev10,c(commStatesDev10$name[1],timespan[i],0,0,"low"))
  }
}

commStatesDev10 <- commStatesDev10[order(commStatesDev2$month),]
colnames(commStatesDev10)[5] <- "state3"

classObsDev10 <- cbind(contributionStatesDev10,bugStatesDev10,commStatesDev10)

for (i in 1:nrow(classObsDev10)) {
  
  classObsDev10$stateLabel[i] <- calculateState(classObsDev10$state1[i],classObsDev10$state2[i],classObsDev10$state3[i])
  
}

# split indexes according to stateLabel

indexSet1 <- sample(which(classObsDev10$stateLabel=="low"))
indexSet2 <- sample(which(classObsDev10$stateLabel=="medium"))
indexSet3 <- sample(which(classObsDev10$stateLabel=="high"))

obs1 <- as.numeric(classObsDev10$dnoc)
obs2 <- as.numeric(classObsDev10$bugfixes)
obs3 <- as.numeric(classObsDev10$responses)
obs4 <- as.numeric(classObsDev10$nocomm)

init <- setInit()
trans <- setTrans()
emis <- estimatePar(obs1,obs2,obs3,obs4)

model <- hmmspec(init = init, trans = trans, parms.emis = emis, dens.emis = dmvnorm.hsmm)
train <- simulate(model, nsim=50, seed = 123, rand.emis = rmvnorm.hsmm)

hmm10 <- hmmfit(train, model, mstep = mstep.mvnorm)
#hmmDev <- hmmfit(trainDev, model, mstep = mstep.mvnorm)


N <- as.numeric(length(classObsDev10$dnoc))
obs <- cbind(as.numeric(classObsDev10$dnoc),as.numeric(classObsDev10$bugfixes),as.numeric(classObsDev10$responses),as.numeric(classObsDev10$nocomm))
trainDev10 <- list(x = obs, N = N)
class(trainDev10) <- "hsmm.data"

plot(trainDev10)
pred10 <- predict(hmm10, trainDev10, method = "viterbi")
plot(trainDev10$x[,1],type="l")
addStates(pred10$s)

plot(pred10$s,type="l")

# TODO: estimate initial probabilities
# TODO: build learning model for developer types (incorporate with simulation)


# ---------------------------------------------------------------------------------


# average emission distributions 

# core: 1

coreRekonq <- hmm1$model$parms.emission

save(coreRekonq, file="coreRekonq.rda")

# minor: 2

minorRekonq <- hmm2$model$parms.emission

save(minorRekonq, file="minorRekonq.rda")

# learn with universal model

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

#mr 0.33

#minor 

modelMinor2 <- hmmspec(init = c(1,0), trans = minor_low, parms.emis = emissionMinor2, dens.emis = dmvnorm.hsmm)

train <- simulate(modelMinor2, nsim=100, seed = 123, rand.emis = rmvnorm.hsmm)

hmm <- hmmfit(train, modelMinor2, mstep = mstep.mvnorm)

plot(trainDev2)
pred <- predict(hmm, trainDev2, method = "viterbi")
plot(trainDev2$x[,1],type="l")
addStates(pred$s)

plot(pred$s,type="l")
par(mar=c(5.1,4.1,4.1,4.1)) 
plot(pred$s,type="l", yaxt = "n", xlab = "time", ylab="state")
axis(4, seq(1, 3, by = 1), labels=c("low","medium","high"), las=1)

#mr 0.64 

#dev2


#---------------------------------------------------------------------------------------------------------------------------------------

# example

J <- 3
initial <- rep(1/J, J)
P <- matrix(c(0.8, 0.5, 0.1, 0.05, 0.2, 0.5, 0.15, 0.3, 0.4), nrow = J)
b <- list(mu = c(-3, 0, 2), sigma = c(2, 1, 0.5))
model <- hmmspec(init = initial, trans = P, parms.emis = b, dens.emis = dnorm.hsmm)
model

train <- simulate(model, nsim = 300, seed = 1234, rand.emis = rnorm.hsmm)
str(train)
plot(train, xlim = c(0, 100))

init0 <- rep(1/J, J)
P0 <- matrix(1/J, nrow = J, ncol = J)
b0 <- list(mu = c(-3, 1, 3), sigma = c(1, 1, 1))
startval <- hmmspec(init = init0, trans = P0, parms.emis = b0, dens.emis = dnorm.hsmm)

h1 = hmmfit(train, startval, mstep = mstep.norm)
plot(h1$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iteration")
summary(h1)

# example

rmvnorm.hsmm <- function(j, model) rmvnorm(1, mean = model$parms.emission$mu[[j]], sigma = model$parms.emission$sigma[[j]])
dmvnorm.hsmm <- function(x, j, model) dmvnorm(x, mean = model$parms.emission$mu[[j]], sigma = model$parms.emission$sigma[[j]])
J <- 2
init <- c(1, 0)
P <- matrix(c(0, 1, 1, 0), nrow = J)
B <- list(mu = list(c(2, 3), c(3, 4)), sigma = list(matrix(c(4, 2, 2, 3), ncol = J), diag(J)))
d <- list(shape = c(10, 25), scale = c(2, 2), type = "gamma")
model <- hsmmspec(init, P, parms.emis = B, sojourn = d, dens.emis = dmvnorm.hsmm)
train <- simulate(model, c(10, 12, 13, 14), seed = 123, rand.emis = rmvnorm.hsmm)
plot(train)

init0 <- rep(1/J, J)
B0 <- list(mu = list(c(1, 2), c(2, 3)), sigma = list(matrix(c(3, 1.5, 1.5, 2.5), ncol = J), diag(J) * 1.5))
M <- 200
d0 <- cbind(dunif(1:M, 1, 50), dunif(1:M, 20, 100))
startval <- hsmmspec(init0, P, parms.emis = B0, sojourn=list(d = d0, type = "gamma"), dens.emis = dmvnorm.hsmm)
hmv <- hsmmfit(train, startval, mstep = mstep.mvnorm, M = 200)
summary(hmv)

pred <- predict(hmv, train, method = "viterbi")
plot(train$x[,1],type="l")
plot(train$x[,2],type="l")
addStates(pred$s)


