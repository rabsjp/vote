rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(haven)
library(xtable)

# add paths to files
load("doct1.Rda")
d$player.vote_1<-d$player.vote
d$player.vote_1[d$player.vote==1]<-"Y"
d$player.vote_1[d$player.vote==0]<-"N"
d$player.vote_2<-NA
d$dibujo.cost<-paste("cost ",d$group.costo)
#d$dibujo.cost[!(d$subsession.round_number %in% c(1,7,13,19,25,31,37,43))]<-NA
#d$dibujo.cost[d$player.id_in_group!=5]<-NA
d$lama.dibujo<-2*d$group.costo/100*11
d$lama.dibujo[d$group.costo>50]<-NA

d$player.vote_2[d$player.finalassets==2]<-d$player.vote_1[d$player.finalassets==2]
d$colores<-"#ff000" 
d$colores[d$group.policy==0]<-"#353436"
d$ponte<-"rejected"
d$ponte[d$group.policy==1]<-"approved"
dp<- d %>% filter(group.uniforme==1, group.costo==20)

png(paste("sum_periods_",dp$group.uniforme[1],dp$group.costo[1],dp$session.code, ".png",sep=""), width = 600, height = 600)

p<-ggplot(data=dp, aes(x=player.lama, y=player.bid)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=player.vote_1,color=colores),size=4,position=position_stack(vjust=0.3)) +
  geom_text(aes(label=player.vote_2,color=colores),size=4,position=position_stack(vjust=0.5)) +
  geom_line(aes(x = player.lama,
                y = group.price),color="black") +
  geom_text(aes(label=ponte),size=3,x=2.18,y=195) +
  xlab("") + ylab("") + ylim(0,200)+ theme(axis.ticks.x=element_blank(),axis.text.x=element_blank(),panel.background = element_rect(fill='transparent')) +
  #geom_line(aes(x = lama.dibujo),color="green") +
  #geom_vline(xintercept = lama.dibujo) +
  facet_wrap( ~ subsession.round_number, nrow = 4, ncol = 4) + theme(legend.position = "none")
print(p)
dev.off()