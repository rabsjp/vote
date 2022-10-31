rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(haven)
library(xtable)

# add paths to files
your_path<-here("data/090922/")
d<-read.csv(paste(your_path, '/',"voting_2022-09-10.csv",sep=""),header=T, stringsAsFactors = FALSE,sep=",")
keep.variables<-c("participant.id_in_session","player.id_in_group","player.t","player.vote"
                  ,"player.bid","player.lama","player.payoff","group.price","group.policy",
                  "group.uniforme","group.costo",
                  "subsession.round_number","session.code")
d<-d[,c(keep.variables)]

dp<-read.csv(paste(your_path, '/',"pilot1708.csv",sep=""),header=T, stringsAsFactors = FALSE,sep=",")
dp<-dp[,c("round","uniforme","tau")]
d<- left_join(d, dp, by = c("subsession.round_number" = "round"))
d<-na.omit(d)
d$player.finalassets<-d$player.t+1
d$player.vote[d$player.finalassets==0]<-NA
d$player.lama_star<- d$player.finalassets*d$tau/100 
d$player.lama_star[d$player.lama_star==0]<-NA
d$player.vote_cond<-NA
d$player.vote_cond[d$player.lama>=d$player.lama_star]<-1
d$player.vote_cond[d$player.lama<d$player.lama_star]<-0
d$marca<-ceiling(d$subsession.round_number/3)
save(d,file = "daug.Rda")

#### Work only with LATE periods
#d<-d[d$marca %% 2 ==0,]
check_vote<-table(d$player.vote_cond,d$player.vote,d$uniforme)
#check_vote<-cbind(check_vote,apply(check_vote,1,sum))


parsito<-d$uniforme=="TRUE"
approval_low<-tapply(d$group.policy[parsito],d$tau[parsito],mean,na.rm=T)
price_low<-tapply(d$group.price[parsito],d$tau[parsito],median,na.rm=T)
bid_low<-tapply(d$player.bid[parsito],d$player.lama[parsito],median,na.rm=T)
vote_low<-tapply(d$player.vote[parsito],d$player.lama[parsito],mean,na.rm=T)
vote_low_all<-tapply(d$player.vote[parsito],list(d$player.lama[parsito],d$tau[parsito]),mean,na.rm=T)
holdings_low<-tapply(d$player.finalassets[parsito],list(d$player.lama[parsito],d$tau[parsito]),median,na.rm=T)

parsito<-d$uniforme=="FALSE"
approval_high<-tapply(d$group.policy[parsito],d$tau[parsito],mean,na.rm=T)
price_high<-tapply(d$group.price[parsito],d$tau[parsito],median,na.rm=T)
bid_high<-tapply(d$player.bid[parsito],d$player.lama[parsito],median,na.rm=T)
vote_high<-tapply(d$player.vote[parsito],d$player.lama[parsito],mean,na.rm=T)
vote_high_all<-tapply(d$player.vote[parsito],list(d$player.lama[parsito],d$tau[parsito]),mean,na.rm=T)
holdings_high<-tapply(d$player.finalassets[parsito],list(d$player.lama[parsito],d$tau[parsito]),median,na.rm=T)

xtable(rbind(approval_low,approval_high))
xtable(check_vote)

pdf("price_plot.pdf")
plot(price_low,type="l",ylim=c(20,120),col="black", xaxt = "n",ylab = "price",xlab = "cost")
lines(price_high,col="black",lty=2)
axis(1, at = c(1, 2, 3,4),labels = c(20,40,60,80))
legend(3, 60, legend=c("Low", "High"), lty=1:2, cex=0.8)
dev.off()

pdf("bid_low.pdf")
names(bid_low)<-unique(d$player.lama[d$uniforme=="TRUE"])
barplot(bid_low,type="l",ylim=c(0,180),col="black",ylab = "bid",xlab = "lambda")
dev.off()

pdf("bid_high.pdf")
barplot(bid_high,ylim=c(0,180),ylab = "bid",xlab = "lambda")
dev.off()

pdf("vote_low.pdf")
par(mfrow=c(1,4))
for(i in seq(1:dim(vote_low_all)[2])){
  barplot(vote_low_all[,i],ylim=c(0,1),ylab = "approval vote",xlab = paste("cost=",colnames(vote_low_all)[i]))
}
dev.off()

pdf("vote_high.pdf")
par(mfrow=c(1,4))
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
