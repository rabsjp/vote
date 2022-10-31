rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(haven)
library(xtable)

# add paths to files
file<-"voting_2022-09-22.csv"
your_path<-here("data/220922/")

d<-read.csv(paste(your_path, '/',file,sep=""),header=T, stringsAsFactors = FALSE,sep=",")
keep.variables<-c("participant.id_in_session","player.id_in_group","player.t","player.vote"
                  ,"player.bid","player.lama","player.payoff","group.price","group.policy",
                  "group.uniforme","group.costo",
                  "subsession.round_number","session.code")
d<-d[,c(keep.variables)]
d<-na.omit(d)
data_survey<-read.csv(paste(your_path, '/',"survey2209.csv",sep=""),header=T, stringsAsFactors = FALSE,sep=";")

keep.variables_survey<-c("participant.id_in_session","player.why_accept",
                         "player.why_reject","player.other_accept","player.other_reject","player.age",
                         "player.gender","player.major","player.political","player.gpa")
data_survey<-data_survey[,c(keep.variables_survey)]

d<-merge(d, data_survey, by = "participant.id_in_session")

d$player.finalassets<-d$player.t+1
d$player.vote[d$player.finalassets==0]<-NA
d$player.lama_star<- d$player.finalassets*d$group.costo/100 
d$player.lama_star[d$player.lama_star==0]<-NA
d$player.vote_cond<-NA
d$player.vote_cond[d$player.lama>=d$player.lama_star]<-1
d$player.vote_cond[d$player.lama<d$player.lama_star]<-0
d$marca<-ceiling(d$subsession.round_number/4)

save(d,file = "dsep22.Rda")
#####
#### Work only with LATE periods
d<-d[d$marca %% 2 ==0,]
#check_vote<-cbind(check_vote,apply(check_vote,1,sum))

parsito<-d$group.uniforme==0
check_vote<-table(d$player.vote_cond[parsito],d$player.vote,d$group.costo[parsito])
approval_low<-tapply(d$group.policy[parsito],d$group.costo[parsito],mean,na.rm=T)
price_low<-tapply(d$group.price[parsito],d$group.costo[parsito],median,na.rm=T)
bid_low<-tapply(d$player.bid[parsito],d$player.lama[parsito],median,na.rm=T)
vote_low<-tapply(d$player.vote[parsito],d$player.lama[parsito],mean,na.rm=T)
vote_low_all<-tapply(d$player.vote[parsito],list(d$player.lama[parsito],d$group.costo[parsito]),mean,na.rm=T)
holdings_low<-tapply(d$player.finalassets[parsito],list(d$player.lama[parsito],d$group.costo[parsito]),median,na.rm=T)
bid_low_all<-tapply(d$player.bid[parsito],list(d$player.lama[parsito],d$group.costo[parsito]),mean,na.rm=T)

approval_low
xtable(check_vote)

pdf(paste("price_plot",d$group.uniforme[1],d$session.code[1],".pdf",sep=""))
plot(price_low,type="l",ylim=c(20,120),col="black", xaxt = "n",ylab = "price",xlab = "cost")
axis(1, at = c(1, 2, 3),labels = c(20,35,60))
#legend(3, 60, legend=c("Low", "High"), lty=1:2, cex=0.8)
dev.off()

pdf(paste("bid_low",d$group.uniforme[1],d$session.code[1],".pdf",sep=""))
names(bid_low)<-unique(d$player.lama[d$group.uniforme==1])
barplot(bid_low,type="l",ylim=c(0,180),col="black",ylab = "bid",xlab = "lambda")
dev.off()

pdf("bid_high.pdf")
barplot(bid_high,ylim=c(0,180),ylab = "bid",xlab = "lambda")
dev.off()

pdf(paste("vote_low",d$group.uniforme[1],d$session.code[1],".pdf",sep=""))
par(mfrow=c(1,3))
for(i in seq(1:dim(vote_low_all)[2])){
  barplot(vote_low_all[,i],ylim=c(0,1),ylab = "approval vote",xlab = paste("cost=",colnames(vote_low_all)[i]))
}
dev.off()

pdf(paste("bid_low_all",d$group.uniforme[1],d$session.code[1],".pdf",sep=""))
par(mfrow=c(1,3))
for(i in seq(1:dim(bid_low_all)[2])){
  barplot(bid_low_all[,i],ylim=c(0,200),ylab = "bid",xlab = paste("cost=",colnames(bid_low_all)[i]))
}
dev.off()




pdf("vote_high.pdf")
par(mfrow=c(1,3))
for(i in seq(1:dim(vote_low_all)[2])){
  barplot(vote_high_all[,i],ylim=c(0,1),ylab = "approval vote",xlab = paste("cost=",colnames(vote_low_all)[i]))
}
dev.off()

pdf("holdings_low.pdf")
barplot(as.matrix(t(holdings_low)),beside=TRUE)
legend(20, 2, legend=seq(20,80,20),fill=gray.colors(4),cex=0.8)
dev.off()

pdf("holdings_high.pdf")
barplot(as.matrix(t(holdings_high)),beside=TRUE)
legend(2, 2, legend=seq(20,80,20),fill=gray.colors(4),cex=0.8)
dev.off()
