rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(haven)
library(xtable)

# add paths to files
load("dsept.Rda")

d<-arrange(d,d$session.code,d$player.lama,d$subsession.round_number)

d$player.better.yes<-0
d$player.better.yes[d$player.lama>=d$group.costo/100*2]<-1

d <- d  %>% group_by(player.lama) %>% mutate(player.change_vote = player.vote - lag(player.vote), player.better.yes.lag = lag(player.better.yes), player.vote.lag = lag(player.vote), group.policy.lag = lag(group.policy),player.bid.delta = player.bid - lag(player.bid))

dplot <- d  %>% filter(group.uniforme==1, group.costo==35, group.policy.lag==0, player.better.yes.lag==1, marca %% 2 ==0)


dmean <- dplot %>% 
  group_by(player.lama) %>%
  summarise(player.mean_delta=median(player.bid.delta))

dplot <- left_join(dplot,dmean,by="player.lama")

png(paste("delta_bids",dplot$group.uniforme[1],dplot$group.policy.lag[1],dplot$group.costo[1],dplot$session.code,".png",sep=""), width = 400, height = 400)

p<-ggplot(dplot, aes(x=player.lama)) + geom_point(aes(y=player.bid.delta)) + 
  xlab("players") + ylab("change of bid") + ylim(-200,200) + geom_line(aes(y=player.mean_delta),color="red")
  
print(p)
dev.off()  
