library (HMM)
library (lattice)
library(ggplot2)
library(mhsmm)
library(MASS)

setwd("C:\\Users\\vhonsel\\Documents\\Mining\\Case Studies\\K3b\\input")
#commit data
committers = read.csv("committer_k3b.csv", header=T, sep=",", stringsAsFactors=FALSE)
#mailing list data
ml = read.csv("k3b_ml.csv", header=T,  sep=";", stringsAsFactors=FALSE)
#bugzilla data
bz_comments = read.csv("bz_comments.csv", header=T, sep=",", stringsAsFactors=FALSE)

# commits: 6217

committers <- committers[grep('Script',committers$name,invert = TRUE),]
committers <- committers[grep('ft0001',committers$name,invert = TRUE),]
committers <- committers[grep('No Author',committers$name,invert = TRUE),]
committers <- committers[grep('CVS deamon',committers$name,invert = TRUE),]

for (s in 1:nrow(committers)){
  
  committers$month[s] <- substr(committers$commit_date[s],1,7) 
  
} 

for (t in 1:nrow(committers)){
  
  committers$noc[t] <- sum(committers$name == committers$name[t])
  committers$nobf[t] <- sum(committers$name == committers$name[t] & committers$is_bug_fix == 1)
  
} 

committers <- committers[committers$noc > 45,]

for (u in 1:nrow(committers)){
  
  committers$dnoc[u] <- sum(committers$month == committers$month[u] & committers$name == committers$name[u] & committers$is_bug_fix != 1) 
  
}

for (v in 1:nrow(committers)){
  
  committers$bugfixes[v] <- sum(committers$month == committers$month[v] & committers$name == committers$name[v] & committers$is_bug_fix == 1) 
  
}

stats <- committers[,c(1,9,10)]
stats <- unique(stats)

#core: dev112
#maintainer: dev115, dev67
#major: dev116, dev117
#minor: dev118, dev119, dev86, dev120

#communication activities

for (s in 1:nrow(ml)){
  
  ml$month[s] <- substr(ml$arrival_data[s],5,7)
  if (ml$month[s]=="Jan") {ml$month[s]<-"01"}
  if (ml$month[s]=="Feb") {ml$month[s]<-"02"}
  if (ml$month[s]=="Mar") {ml$month[s]<-"03"}
  if (ml$month[s]=="Apr") {ml$month[s]<-"04"}
  if (ml$month[s]=="May") {ml$month[s]<-"05"}
  if (ml$month[s]=="Jun") {ml$month[s]<-"06"}
  if (ml$month[s]=="Jul") {ml$month[s]<-"07"}
  if (ml$month[s]=="Aug") {ml$month[s]<-"08"}
  if (ml$month[s]=="Sep") {ml$month[s]<-"09"}
  if (ml$month[s]=="Oct") {ml$month[s]<-"10"}
  if (ml$month[s]=="Nov") {ml$month[s]<-"11"}
  if (ml$month[s]=="Dec") {ml$month[s]<-"12"}
  ml$year[s] <- substr(ml$arrival_data[s],nchar(ml$arrival_data[s])-4,nchar(ml$arrival_data[s]))
  ml$month[s] <- paste(ml$year[s],ml$month[s],sep = "-")
  
} 

#change name of MichaÅ, MaÅ,ek

for (t in 1:nrow(ml)){
  
  if (grepl("MichaÅ",ml$name[t])==TRUE)
  {
    ml$name[t] <- "dev115"
  }
  else if (grepl("Sebastian",ml$name[t])==TRUE)
  {
    ml$name[t] <- "dev112"
  }
}

mlComm <-ml[grep("Sebastian|dev115|Kvasny|Froescher|Habacker|Mueller|Boiko",ml$name),]

for (t in 1:nrow(mlComm)){
  
  mlComm$activities[t] <- sum(mlComm$name[t] == mlComm$name & mlComm$month[t] == mlComm$month)
} 

# bug activities

for (t in 1:nrow(bz_comments)){
  
  if (grepl("Micha?",bz_comments$commentAuthor[t])==TRUE)
  {
    bz_comments$commentAuthor[t] <- "dev115"
  }
}


for (u in 1:nrow(bz_comments)){
  
  bz_comments$month[u] <- substr(bz_comments$commentTime[u],1,7) 
  
} 

for (x in 1:nrow(bz_comments)){
  
  bz_comments$nocomm[x] <- sum(bz_comments$commentAuthor[x] == bz_comments$commentAuthor & bz_comments$month[x] == bz_comments$month)
  
}


bz_comments_dev <-bz_comments[grep("dev112|dev115|Kvasny|Froescher|Habacker|dev86|Boiko",bz_comments$commentAuthor),]


# prepare for threshold learner

contributions <- committers[, c(1,8,11,12)]
contributions <- unique(contributions)

communication <- mlComm[,c(3,6,8)]
communication <- unique(communication)

comments <- bz_comments_dev[,c(4,9,10)]
comments <- unique(comments)

write.csv(contributions, "contributions.csv", row.names=FALSE)
write.csv(communication, "communication.csv", row.names=FALSE)
write.csv(comments, "bug_comments.csv", row.names=FALSE)


