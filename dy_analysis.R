rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(haven)
library(xtable)
library(foreign)
# add paths to files
load("full_data.Rda")

d<-arrange(full_data,full_data$session.code,full_data$player.id_in_group,full_data$subsession.round_number)

d$player.better.yes<-0
d$player.better.yes[d$player.lama>=d$group.costo/100*2]<-1


d <- d  %>% group_by(player.id_in_group) %>% mutate(player.change_vote = player.vote - lag(player.vote), player.better.yes.lag = lag(player.better.yes), player.t.lag = lag(player.t) , player.vote.lag = lag(player.vote), group.policy.lag = lag(group.policy), total_yes.lag = lag(total_yes),player.bid.delta = player.bid - lag(player.bid))


write.csv(d,"data_to_stata.csv")


dplot <- d  %>% filter(group.uniforme==1, group.costo==20, group.policy.lag==0, player.better.yes.lag==1, marca %% 2 ==0, player.t.lag>0)
dplot_high <- d  %>% filter(group.uniforme==0, group.costo==20, group.policy.lag==0, player.better.yes.lag==1, marca %% 2 ==0, player.t.lag>0)
plot(dplot$player.bid.delta)
plot(dplot_high$player.bid.delta)

mean(dplot$player.bid.delta>0)
mean(dplot_high$player.bid.delta>0)

median(dplot$player.bid.delta)
median(dplot_high$player.bid.delta)


dmean <- dplot %>% 
  group_by(player.lama) %>%
  summarise(player.mean_delta=median(player.bid.delta))

dplot <- left_join(dplot,dmean,by="player.lama")


png(paste("delta_bids",dplot$group.uniforme[1],dplot$group.policy.lag[1],dplot$group.costo[1],dplot$session.code,".png",sep=""), width = 400, height = 400)

p<-ggplot(dplot, aes(x=player.lama)) + geom_point(aes(y=player.bid.delta)) + 
  xlab("players") + ylab("change of bid") + ylim(-200,200) + geom_line(aes(y=player.mean_delta),color="red")
  
print(p)
dev.off()  


plot(dplot_high$player.bid.delta)


