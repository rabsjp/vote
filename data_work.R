rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(haven)
library(xtable)
library(reshape2)
your_path<-here("data/clean/")
path = list.files(path=your_path, pattern="*.Rda")

# add paths to files
full_data<-NULL
for(i in (along=path)){
  load(paste(your_path,i,sep=""))
  print(i)
  full_data<-rbind(full_data,d)
  remove(d)
}

#pr< filter(full_data, session != "49mqxl65" & subsession.round_number!=9)
full_data<-as.data.frame(full_data)

#Eliminate typo in round 9 of session 49mqxl65
full_data<-full_data[!(full_data$session.code=="49mqxl65" & full_data$subsession.round_number==9),]

df_co <- full_data %>%
  group_by(session.code, subsession.round_number)  %>%
  summarize(total_yes = sum(player.vote*(player.t+1),na.rm=T), total_shares_one = sum((player.lama>group.costo*2/100)*(player.t+1),na.rm=T))


full_data<-merge(full_data, df_co, by = c("session.code","subsession.round_number"))

full_data$best_respond<-0

full_data<- full_data %>% 
  mutate(best_respond = ifelse(player.vote_cond== player.vote,1,best_respond)) 

full_data<- full_data %>% 
  mutate(unique_subject = as.numeric(as.factor(session.code))*100+player.id_in_group) 

save(full_data,file = "full_data.Rda")

full_data_late<-full_data[full_data$marca %% 2 ==0,]

parsito<-full_data_late$group.uniforme==1
parsi<-full_data_late$group.uniforme==0

approval_low<-tapply(full_data_late$group.policy[parsito],full_data_late$group.costo[parsito],mean,na.rm=T)
approval_high<-tapply(full_data_late$group.policy[parsi],full_data_late$group.costo[parsi],mean,na.rm=T)

check_vote<-table(full_data_late$player.vote_cond[parsito],full_data_late$player.vote[parsito],full_data_late$group.costo[parsito])
check_vote_high<-table(full_data_late$player.vote_cond[parsi],full_data_late$player.vote[parsi],full_data_late$group.costo[parsi])

fail_to_reject<-full_data_late %>% filter(player.vote_cond==0 & player.vote==1)

fail_to_reject<- fail_to_reject %>% group_by(session.code) %>% distinct(player.id_in_group,.keep_all = TRUE)
table(fail_to_reject$player.why_accept,fail_to_reject$group.uniforme)

ok_to_reject<-full_data_late %>% filter(player.vote_cond==0 & player.vote==0)
fail_to_accept<-full_data_late %>% filter(player.vote_cond==1 & player.vote==0)
fail_to_accept<- fail_to_accept %>% group_by(session.code) %>% distinct(player.id_in_group,.keep_all = TRUE)
table(fail_to_accept$player.why_reject,fail_to_accept$group.uniforme)

check_tab_low<-cbind(check_vote[,2,1]/apply(check_vote[,,1],1,sum),apply(check_vote[,,1],1,sum),check_vote[,2,2]/apply(check_vote[,,2],1,sum),apply(check_vote[,,2],1,sum),check_vote[,2,3]/apply(check_vote[,,3],1,sum),apply(check_vote[,,3],1,sum))
check_tab_high<-cbind(check_vote_high[,2,1]/apply(check_vote_high[,,1],1,sum),apply(check_vote_high[,,1],1,sum),check_vote_high[,2,2]/apply(check_vote_high[,,2],1,sum),apply(check_vote_high[,,2],1,sum),check_vote_high[,2,3]/apply(check_vote_high[,,3],1,sum),apply(check_vote_high[,,3],1,sum))
check_export<-rbind(check_tab_low,check_tab_high)
xtable(check_export)



dp<- full_data_late%>%group_by(session.code, group.costo)%>%summarise_all(funs(mean)) %>% select(session.code, group.costo,group.policy,group.price, group.uniforme)

pol20<-wilcox.test(dp$group.policy[dp$group.uniforme==0 & dp$group.costo==20],dp$group.policy[dp$group.uniforme==1 & dp$group.costo==20])
pol20$p.value
pol35<-wilcox.test(dp$group.policy[dp$group.uniforme==0 & dp$group.costo==35],dp$group.policy[dp$group.uniforme==1 & dp$group.costo==35])
pol35$p.value
pol60<-wilcox.test(dp$group.policy[dp$group.uniforme==0 & dp$group.costo==60],dp$group.policy[dp$group.uniforme==1 & dp$group.costo==60])
pol60$p.value
wilcox.test(dp$group.price[dp$group.uniforme==1 & dp$group.costo==35],dp$group.price[dp$group.uniforme==1 & dp$group.costo==60],paired=T)

dp$group.policy<-round(dp$group.policy*100,2)
dp$group.costo[dp$group.costo==35]<-dp$group.costo[dp$group.costo==35]+5

dp<-as.data.frame(dp)
pdf("approval_all.pdf")
plot <- ggplot(dp, aes(x = group.costo)) +
  geom_bar(data = subset(dp, group.uniforme == 0),
           aes(y = group.policy, fill = 0),stat="summary",width=10) +
  geom_bar(data = subset(dp, group.uniforme == 1), 
           aes(y = -group.policy, fill = 1),stat="summary",width=10) + 
  coord_flip() +
  scale_y_continuous(breaks = seq(-100,100, by = 10),
                    labels = (c(seq(100, 0, by = -10), seq(10,100,by=10)))) +
  scale_x_continuous(breaks = seq(0,60, by = 20),
                     labels = c(0,20,35,60)) + 
  theme(panel.grid.major = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),  axis.text=element_text(size=14), axis.title = element_text(size = 14)) +  xlab("Policy cost") + ylab("Policy approval rate (%)") + geom_hline(yintercept = 0) + guides(fill="none") +
  annotate("text", x=65, y=-50, label= "Low",size = 6)  +
  annotate("text", x=65, y=50, label= "High",size = 6) +
  geom_segment(aes(y = -5, x = 27, yend = 5, xend = 27),color="gray") +
  geom_segment(aes(y = -5, x = 26, yend = -5, xend = 27),color="gray") +
  geom_segment(aes(y = 5, x = 26, yend = 5, xend = 27),color="gray") +
  annotate("text", x=28, y=5, label=paste("p-value=",round(pol20$p.value,3)),size = 4) +
  geom_segment(aes(y = -5, x = 47, yend = 5, xend = 47),color="gray") +
  geom_segment(aes(y = -5, x = 46, yend = -5, xend = 47),color="gray") +
  geom_segment(aes(y = 5, x = 46, yend = 5, xend = 47),color="gray") +
  annotate("text", x=48, y=7, label=paste("p-value=",round(pol35$p.value,3)),size = 4) 
print(plot)
dev.off()

table(full_data_late$group.uniforme,full_data_late$group.costo)
df1 <- full_data_late%>%group_by(group.uniforme, group.costo)%>%summarise_all(funs(mean)) %>% select(group.costo,group.policy,group.price, group.uniforme)
df1$group.costo[df1$group.costo==35]<-df1$group.costo[df1$group.costo==35]+5
df1$group.policy[df1$group.uniforme==0 & df1$group.costo==60]<-0.001
pdf("approval_all_hori.pdf")
ggplot(df1, aes(x=group.costo, y=group.policy, fill=factor(group.uniforme))) +
  geom_bar(stat='identity', position='dodge',width=10) + scale_fill_manual(labels = c("Polarized","Dispersed" ), values=c("black","gray"),name="")  + 
  scale_x_continuous(breaks = seq(0,60, by = 20),
                     labels = c(0,20,35,60)) +
  theme(legend.position = c(.8, .7), legend.title = element_blank(), axis.text=element_text(size=14), axis.title = element_text(size = 14), 
      legend.text = element_text(size = 14) ,legend.key = element_rect(fill = "transparent"), panel.grid.major = element_blank(), 
      panel.background = element_blank(), axis.line = element_line(colour = "black")) +   xlab("Policy cost") + ylab("Policy approval rate")+
  geom_segment(aes(y = 0.82, x = 16, yend = 0.82, xend = 24),color="gray") + 
  geom_segment(aes(y = 0.82, x = 16, yend = 0.80, xend = 16),color="gray") +
  geom_segment(aes(y = 0.82, x = 24, yend = 0.80, xend = 24),color="gray") +
  annotate("text", x=19, y=.85, label=paste("p-value=",round(pol20$p.value,3)),size = 4) +
  geom_segment(aes(y = 0.72, x = 36, yend = 0.72, xend = 44),color="gray") + 
  geom_segment(aes(y = 0.72, x = 36, yend = 0.70, xend = 36),color="gray") +
  geom_segment(aes(y = 0.72, x = 44, yend = 0.70, xend = 44),color="gray") +
  annotate("text", x=39, y=.75, label=paste("p-value=",round(pol35$p.value,3)),size = 4) +
  geom_segment(aes(y = 0.05, x = 56, yend = 0.05, xend = 64),color="gray") + 
  geom_segment(aes(y = 0.05, x = 56, yend = 0.04, xend = 56),color="gray") +
  geom_segment(aes(y = 0.05, x = 64, yend = 0.04, xend = 64),color="gray") +
  annotate("text", x=59, y=.07, label=paste("p-value=",round(pol60$p.value,3)),size = 4)



dev.off()


theme(panel.grid.major = element_blank(), legend.position = c(.6, .8),  legend.text = element_text(size = 14), legend.key = element_rect(fill = "transparent"), panel.background = element_blank(), 
      axis.line = element_line(colour = "gray"),  axis.text=element_text(size=14), axis.title = element_text(size = 14)) +  xlab("Policy cost") + ylab("Policy approval rate") + guides(fill="none") 




dbest<- full_data_late%>%group_by(group.uniforme, unique_subject) %>% summarize(best_responde = sum(best_respond, na.rm=T), numero = sum(best_respond>-1,na.rm=T))
dbest<- dbest%>% mutate(time_bb = best_responde/numero)
dbest<- dbest%>% mutate(suerte = .5)

t.test(dbest$time_bb[dbest$group.uniforme==0],dbest$suerte[dbest$group.uniforme==0])
t.test(dbest$time_bb[dbest$group.uniforme==1],dbest$suerte[dbest$group.uniforme==1])

binom.test(c(dbest$best_responde[dbest$group.uniforme==0]),c(dbest$numero[dbest$group.uniforme==0]), p = 1/2)
binom.test(c(dbest$best_respond[dbest$group.uniforme==1]), p = 1/2)

dprice_all<- full_data_late%>%group_by(session.code, subsession.round_number) %>% select(group.costo,group.policy,group.price, group.uniforme) %>%summarise_all(funs(mean))

dprice_all_high<-filter(dprice_all, group.uniforme==0)
dprice_all_low<-filter(dprice_all, group.uniforme==1)

median(dprice_all_high$group.price[dprice_all_high$group.costo==35])
median(dprice_all_high$group.price[dprice_all_high$group.costo==20])
median(dprice_all_high$group.price[dprice_all_high$group.costo==60])

median(dprice_all_high$group.price[dprice_all_high$group.costo==35 & dprice_all_high$group.policy==0])
median(dprice_all_high$group.price[dprice_all_high$group.costo==35 & dprice_all_high$group.policy==1])

median(dprice_all_high$group.price[dprice_all_high$group.costo==20 & dprice_all_high$group.policy==1])
median(dprice_all_low$group.price[dprice_all_low$group.costo==20 & dprice_all_low$group.policy==1])



median(dprice_all_low$group.price[dprice_all_low$group.costo==35])
median(dprice_all_low$group.price[dprice_all_low$group.costo==20])
median(dprice_all_low$group.price[dprice_all_low$group.costo==60])


dprice_all_high$group.costo <- as.factor(dprice_all_high$group.costo)
dprice_all_low$group.costo <- as.factor(dprice_all_low$group.costo)

pdf("precio_high.pdf")
p <- ggplot(dprice_all_high, aes(x=group.costo, y=group.price)) + 
  geom_boxplot() +  ylim(0, 120) + theme(legend.position = c(.8, .4), axis.text=element_text(size=14), axis.title = element_text(size = 14), legend.text = element_text(size = 14) ,legend.key = element_rect(fill = "transparent"), panel.grid.major = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Policy cost") + ylab("Price") + annotate("text", x=1, y=80, label= "--",size = 6,color="red") +
  annotate("text", x=2, y=65, label= "--",size = 6,color="red") + annotate("text", x=3, y=100, label= "--",size = 6,color="red") 
print(p)
dev.off()


pdf("precio_low.pdf")
p <- ggplot(dprice_all_low, aes(x=group.costo, y=group.price)) + 
  geom_boxplot() +  ylim(0, 120) + theme(legend.position = c(.8, .4), axis.text=element_text(size=14), axis.title = element_text(size = 14), legend.text = element_text(size = 14) ,legend.key = element_rect(fill = "transparent"), panel.grid.major = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Policy cost") + ylab("Price") + annotate("text", x=1, y=80, label= "--",size = 6,color="red") +
  annotate("text", x=2, y=65, label= "--",size = 6,color="red") + annotate("text", x=3, y=100, label= "--",size = 6,color="red") +
  annotate("text", x=2, y=100, label= "--",size = 6,color="red") + annotate("text", x=1, y=100, label= "--",size = 6,color="red")
print(p)
dev.off()


dprice_all_high_accept<-filter(dprice_all, group.uniforme==0 & group.policy==1)
dprice_all_high_reject<-filter(dprice_all, group.uniforme==0 & group.policy==0)

dprice_all_low_accept<-filter(dprice_all, group.uniforme==1 & group.policy==1)
dprice_all_low_reject<-filter(dprice_all, group.uniforme==1 & group.policy==0)

dprice_all_high_accept$group.costo <- as.factor(dprice_all_high_accept$group.costo)
dprice_all_high_reject$group.costo <- as.factor(dprice_all_high_reject$group.costo)

dprice_all_low_accept$group.costo <- as.factor(dprice_all_low_accept$group.costo)
dprice_all_low_reject$group.costo <- as.factor(dprice_all_low_reject$group.costo)


pdf("precio_high_accept.pdf")
p <- ggplot(dprice_all_high_accept, aes(x=group.costo, y=group.price)) + 
  geom_boxplot() +  ylim(30, 120) + theme(legend.position = c(.8, .4), axis.text=element_text(size=14), axis.title = element_text(size = 14), legend.text = element_text(size = 14) ,legend.key = element_rect(fill = "transparent"), panel.grid.major = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Policy cost") + ylab("Price") + annotate("text", x=1, y=80, label= "--",size = 6,color="red") +
  annotate("text", x=2, y=65, label= "--",size = 6,color="red") 
print(p)
dev.off()

pdf("precio_high_reject.pdf")
p <- ggplot(dprice_all_high_reject, aes(x=group.costo, y=group.price)) + 
  geom_boxplot() +  ylim(30, 120) + theme(legend.position = c(.8, .4), axis.text=element_text(size=14), axis.title = element_text(size = 14), legend.text = element_text(size = 14) ,legend.key = element_rect(fill = "transparent"), panel.grid.major = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Policy cost") + ylab("Price") + annotate("text", x=1, y=100, label= "--",size = 6,color="red") +
  annotate("text", x=3, y=100, label= "--",size = 6,color="red") + annotate("text", x=2, y=100, label= "--",size = 6,color="red") 
print(p)
dev.off()

pdf("precio_low_accept.pdf")
p <- ggplot(dprice_all_low_accept, aes(x=group.costo, y=group.price)) + 
  geom_boxplot() +  ylim(30, 120) + theme(legend.position = c(.8, .4), axis.text=element_text(size=14), axis.title = element_text(size = 14), legend.text = element_text(size = 14) ,legend.key = element_rect(fill = "transparent"), panel.grid.major = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Policy cost") + ylab("Price") + annotate("text", x=1, y=80, label= "--",size = 6,color="red") +
  annotate("text", x=2, y=65, label= "--",size = 6,color="red") 
print(p)
dev.off()

pdf("precio_low_reject.pdf")
p <- ggplot(dprice_all_low_reject, aes(x=group.costo, y=group.price)) + 
  geom_boxplot() +  ylim(30, 120) + theme(legend.position = c(.8, .4), axis.text=element_text(size=14), axis.title = element_text(size = 14), legend.text = element_text(size = 14) ,legend.key = element_rect(fill = "transparent"), panel.grid.major = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Policy cost") + ylab("Price") + annotate("text", x=1, y=100, label= "--",size = 6,color="red") +
  annotate("text", x=2, y=100, label= "--",size = 6,color="red") + annotate("text", x=3, y=100, label= "--",size = 6,color="red") 
print(p)
dev.off()


dprice_all_session<- full_data_late%>%group_by(session.code, group.policy, group.costo, group.uniforme) %>% select(session.code,group.costo,group.policy,group.price, group.uniforme) %>%summarise_all(funs(median))

wilcox.test(dprice_all_session$group.price[dprice_all_session$group.uniforme==0 &dprice_all_session$group.costo==20 & dprice_all_session$group.policy==1],dprice_all_session$group.price[dprice_all_session$group.uniforme==0 &dprice_all_session$group.costo==20 & dprice_all_session$group.policy==0],paired = T)
wilcox.test(dprice_all_session$group.price[dprice_all_session$group.uniforme==1 &dprice_all_session$group.costo==20 & dprice_all_session$group.policy==1],dprice_all_session$group.price[dprice_all_session$group.uniforme==1 &dprice_all_session$group.costo==35 & dprice_all_session$group.policy==0],paired = T)

wilcox.test(dprice_all_session$group.price[dprice_all_session$group.uniforme==0 &dprice_all_session$group.costo==20 & dprice_all_session$group.policy==1],dprice_all_session$group.price[dprice_all_session$group.uniforme==1 &dprice_all_session$group.costo==20 & dprice_all_session$group.policy==1])



lm(dprice_all_low$group.price[dprice_all_low$group.costo==20] ~dprice_all_low$group.policy[dprice_all_low$group.costo==20])

median(dprice_all_high_accept$group.price[dprice_all_high_accept$group.costo==35])
median(dprice_all_high_reject$group.price[dprice_all_high_reject$group.costo==35])

dplayer_median<- full_data_late%>%group_by(session.code, group.costo, player.lama, player.id_in_group) %>% select(group.costo,group.uniforme,player.bid) %>%summarise_all(funs(median))
player_high_20_0<-dplayer_median%>%filter(group.uniforme==0,group.costo==20,player.lama==0)
player_high_35_0<-dplayer_median%>%filter(group.uniforme==0,group.costo==35,player.lama==0)
wilcox.test(player_high_20_0$player.bid-player_high_35_0$player.bid)

player_high_20<-dplayer_median%>%filter(group.uniforme==0,group.costo==20,player.lama==1)
player_high_35<-dplayer_median%>%filter(group.uniforme==0,group.costo==35,player.lama==1)
wilcox.test(player_high_20$player.bid,player_high_35$player.bid,paired=T)

wilcox.test(player_high_20$player.bid,player_high_20_0$player.bid[player_high_20_0$player.id_in_group!=6],paired=T)

wilcox.test(player_high_35$player.bid,player_high_35_0$player.bid[player_high_35_0$player.id_in_group!=6],paired=T)

pdf("precio_all.pdf")

plot <- ggplot(dt_median, aes(x = group.costo,y=group.price)) +
  geom_line(aes(color=factor(group.uniforme)),size=1.4) +
  scale_x_continuous(breaks = seq(0,60, by = 20),
                     labels = c(0,20,35,60)) + 
  ylim(70, 100) +
  #scale_y_continuous(breaks = seq(50,100, by = 5),
  #                   labels = seq(50,100, by = 5)) + 
  scale_color_manual(labels = c("High","Low" ),values=c("black","gray"),name="") +  
  theme(legend.position = c(.8, .4), axis.text=element_text(size=14), axis.title = element_text(size = 14), legend.text = element_text(size = 14) ,legend.key = element_rect(fill = "transparent"), panel.grid.major = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Policy cost") + ylab("Price") +  geom_hline(yintercept = 100, linetype='dotted') 

print(plot)
dev.off()

zero_high<-full_data_late %>% filter(player.lama==0, group.uniforme==0, group.costo<60)
one_high<-full_data_late %>% filter(player.lama==1, group.uniforme==0, group.costo<60)


shares_high<-full_data_late %>% filter(group.uniforme==0, group.costo<60, player.lama==1)
shares_low<-full_data_late %>% filter(group.uniforme==1, group.costo<60, player.lama>=6/11)


zero_high_3<-full_data_late %>% filter(player.lama==0, group.uniforme==0, group.costo==20)
zero_cost35<-full_data_late %>% filter(group.uniforme==0, group.costo==35)
zero_cost20<-full_data_late %>% filter(group.uniforme==0, group.costo==20)

zero_20<-full_data_late %>% filter(group.uniforme==0, group.costo==20)
zero_35<-full_data_late %>% filter(group.uniforme==0, group.costo==35)

pdf("shares_top_high.pdf")
ggplot(data=shares_high) +
  stat_ecdf(geom='step', aes(x=total_shares_one, colour=as.factor(group.costo)), size=1) +
  scale_colour_manual(labels = c("cost = 20","cost = 35" ), values=c("black","gray"),name="") +
  scale_x_continuous(breaks = seq(0,10, by = 1),
                     labels = seq(0,10, by = 1),name="Shares") + scale_y_continuous(name='CDF') +
  theme(legend.position = c(.2, .7), axis.text=element_text(size=14), axis.title = element_text(size = 14), 
        legend.text = element_text(size = 14) ,legend.key = element_rect(fill = "transparent"), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 
dev.off()

pdf("shares_top_low.pdf")
ggplot(data=shares_low) +
  stat_ecdf(geom='step', aes(x=total_shares_one, colour=as.factor(group.costo)), size=1) +
  scale_colour_manual(labels = c("cost = 20","cost = 35" ), values=c("black","gray"),name="") +
  scale_x_continuous(breaks = seq(0,10, by = 1),
                     labels = seq(0,10, by = 1),name="Shares") + scale_y_continuous(name='CDF') +
  theme(legend.position = c(.2, .7), axis.text=element_text(size=14), axis.title = element_text(size = 14), 
        legend.text = element_text(size = 14) ,legend.key = element_rect(fill = "transparent"), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 
dev.off()




barplot(zero_high$player.bid[zero_high$group.costo==35 & zero_high$group.policy>0])
abline(h=65)
abline(h=median(zero_high$player.bid[zero_high$group.costo==35 & zero_high$group.policy>0 ]),col="red")
#lineplot(zero_high$total_yes[zero_high$group.costo==35])
hist(zero_high$player.t[zero_high$group.costo==20 & zero_high$group.policy>0])
punto<-median(zero_high$player.bid[zero_high$group.costo==20])
punto_one<-median(one_high$player.bid[one_high$group.costo==20])

n<-unique(zero_high$session.code)
dibuja<-zero_high %>%filter(session.code==n[4])
pdf(paste("bid_high_zero",dibuja$session.code[1],".pdf",sep=""))
ggplot(data=dibuja) +
  stat_ecdf(geom='step', aes(x=player.bid, colour=as.factor(group.costo)), size=1) +
  scale_x_continuous(name='Bid') +  scale_y_continuous(name='CDF') +
  scale_colour_manual(labels = c("cost = 20","cost = 35" ), values=c("black","gray"),name="") +
  theme(legend.position = c(.2, .7), axis.text=element_text(size=14), axis.title = element_text(size = 14), 
        legend.text = element_text(size = 14) ,legend.key = element_rect(fill = "transparent"), panel.grid.major = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = .5, linetype='dotted') 
  #geom_vline(xintercept = punto, linetype='dotted')
dev.off()



n<-unique(one_high$session.code)
dibuja<-one_high %>%filter(session.code==n[1])
pdf(paste("bid_high_one",dibuja$session.code[1],".pdf",sep=""))
ggplot(data=dibuja) +
  stat_ecdf(geom='step', aes(x=player.bid, colour=as.factor(group.costo)), size=1) +
  scale_x_continuous(name='Bid') +  scale_y_continuous(name='CDF') +
  scale_colour_manual(labels = c("cost = 20","cost = 35" ), values=c("black","gray"),name="") +
  theme(legend.position = c(.2, .7), axis.text=element_text(size=14), axis.title = element_text(size = 14), 
        legend.text = element_text(size = 14) ,legend.key = element_rect(fill = "transparent"), panel.grid.major = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = .5, linetype='dotted') 
#geom_vline(xintercept = punto, linetype='dotted')
dev.off()


pdf("bid_high_cost20.pdf")
ggplot(data=zero_high) +
  stat_ecdf(geom='step', aes(x=player.bid, colour=as.factor(group.costo)), size=1) +
  scale_x_continuous(name='Bid') +  scale_y_continuous(name='CDF') +
  scale_colour_manual(labels = c("lambda=1","lambda=0" ), values=c("black","gray"),name="") +
  theme(legend.position = c(.2, .7), axis.text=element_text(size=14), axis.title = element_text(size = 14), 
        legend.text = element_text(size = 14) ,legend.key = element_rect(fill = "transparent"), panel.grid.major = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = .5, linetype='dotted') +
  geom_vline(xintercept = punto, linetype='dotted')
dev.off()

pdf("bid_high_zero.pdf")
ggplot(data=zero_high) +
  stat_ecdf(geom='step', aes(x=player.bid, colour=as.factor(group.costo)), size=1) +
  scale_x_continuous(name='Bid') +  scale_y_continuous(name='CDF') +
  scale_colour_manual(labels = c("cost = 20","cost = 35" ), values=c("black","gray"),name="") +
  theme(legend.position = c(.2, .7), axis.text=element_text(size=14), axis.title = element_text(size = 14), 
        legend.text = element_text(size = 14) ,legend.key = element_rect(fill = "transparent"), panel.grid.major = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = .5, linetype='dotted') +
  geom_vline(xintercept = punto, linetype='dotted')
dev.off()



pdf("bid_high_one.pdf")
ggplot(data=one_high) +
  stat_ecdf(geom='step', aes(x=player.bid, colour=as.factor(group.costo)), size=1) +
  scale_x_continuous(name='Bid') +  scale_y_continuous(name='CDF') +
  scale_colour_manual(labels = c("cost = 20","cost = 35" ), values=c("black","gray"),name="") +
  theme(legend.position = c(.2, .7), axis.text=element_text(size=14), axis.title = element_text(size = 14), 
        legend.text = element_text(size = 14) ,legend.key = element_rect(fill = "transparent"), panel.grid.major = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = .5, linetype='dotted') +
  geom_vline(xintercept = punto_one, linetype='dotted')
dev.off()

pdf("bid_high_35.pdf")
ggplot(data=zero_cost35) +
  stat_ecdf(geom='step', aes(x=player.bid, colour=as.factor(player.lama)), size=1) +
  scale_x_continuous(name='Bid') +  scale_y_continuous(name='CDF') +
  scale_colour_manual(labels = c("lambda = 0","lambda = 1" ), values=c("gray","black"),name="") +
  theme(legend.position = c(.2, .7), axis.text=element_text(size=14), axis.title = element_text(size = 14), 
        legend.text = element_text(size = 14) ,legend.key = element_rect(fill = "transparent"), panel.grid.major = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = .5, linetype='dotted')  +
  geom_segment(aes(y = -0.05, x = 65, yend = 0, xend = 65),color="red",arrow = arrow(length = unit(0.5, "cm")))

dev.off()

pdf("bid_high_20.pdf")
ggplot(data=zero_cost20) +
  stat_ecdf(geom='step', aes(x=player.bid, colour=as.factor(player.lama)), size=1) +
  scale_x_continuous(name='Bid') +  scale_y_continuous(name='CDF') +
  scale_colour_manual(labels = c("lambda = 0","lambda = 1" ),  values=c("gray","black"),name="") +
  theme(legend.position = c(.2, .7), axis.text=element_text(size=14), axis.title = element_text(size = 14), 
        legend.text = element_text(size = 14) ,legend.key = element_rect(fill = "transparent"), panel.grid.major = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = .5, linetype='dotted') +
  geom_segment(aes(y = -0.05, x = 80, yend = 0, xend = 80),color="red",arrow = arrow(length = unit(0.5, "cm")))
dev.off()

  #geom_vline(xintercept = punto, linetype='dotted')

#labels = c("cost = 20","cost = 35" )