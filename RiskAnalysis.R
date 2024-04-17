## This is the data analyses and data visualization for Bumblebee risk sensitivity
attach(Risk)
summary(Risk)


### testing the data for normality 
bartlett.test(p.fluct~q,data=Risk)
bartlett.test(p.fluct~interaction(q,Alternating.Color),data=Risk)
bartlett.test(p.fluct~interaction(Subject,Colony),data=Risk)
bartlett.test(p.fluct~Alternating.Color,data=Risk)

###initial simple anovas to determine potential differences
qanova<-aov(p.fluct~q, data=Risk)
summary(qanova)
plot(qanova)
Colonyanova<-aov(p.fluct~Colony, data=Risk)
plot(p.fluct~Colony)
summary(Colonyanova)
plot(Colonyanova)
Alt.Coloranova<-aov(p.fluct~Alternating.Color, data=Risk)
summary(Alt.Coloranova)
plot((Alt.Coloranova))
BlockAov<-aov(p.fluct~Block.Number,data=Risk)
summary(BlockAov)
AgeAov<-aov(p.fluct~Bee.Age,data=Risk)
summary(AgeAov)
SizeAov<-aov(p.fluct~Thorax.Width, data=Risk)
summary(SizeAov)
#age and size not significant


#### package installs for other downstream analyses and visualization 
install.packages("lme4")
library(lme4)
library(afex)
install.packages("pbkrtest")
require(pbkrtest)
install.packages("metafor")
library(metafor)
install.packages("glmulti")
library(glmulti)
install.packages("mvtnorm",dependencies = TRUE)
library(multcomp)
install.packages("rJava")


######AIC #######################
youtput<-glmulti(p.fluct~Alternating.Color-1+q-1+First.Choice+Block.Number+Bee.Age+Thorax.Width*Colony+Starting.State, data=Risk, name = "glmulti.analysis", confsetsize =70,
        level = 1,crit="aicc")
bestmodel<-youtput@objects[[1]]
bestmodel
print(youtput)
tmp <- weightable(youtput)
tmp <- tmp[tmp$aicc <= min(tmp$aicc) + 2,]
tmp
summary(youtput@objects[[1]])
predict(youtput)
summary(bestmodel)
coef(youtput)
library(ggplot2)

#########################################################################
########################Repeated Measures Anova############################
if(!require(psych)){install.packages("psych")}
if(!require(nlme)){install.packages("nlme")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(rcompanion)){install.packages("rcompanion")}
library(nlme)

repeated.aov<-with(Risk, aov(p.fluct ~  Colony*Starting.State*Alternating.Color*q*Block.Number+
                         Error(Subject / (Colony)))) 
                        
library(car)
summary(repeated.aov)

library(multcomp)

################Interaction plots###########################
interaction.plot(x.factor=Risk$Colony,trace.factor=Risk$Alternating.Color,response=Risk$p.fluct,type="b",col=c(1:2),lwd=1,pch=c(18,24), xlab="Colony",ylab="Probability of Fluctuating",main="Prob of Fluctuating interaction Colony and Alternating Color")
interaction.plot(x.factor=Risk$Block.Number,trace.factor=Risk$q,response=Risk$p.fluct,type="b",col=c(1:3),leg.bty="o ",lwd=2,pch=c(18,24) ,xlab="Block Number",ylab="Probability of Fluctuating",main="Prob of Fluctuating interaction block and q" )
interaction.plot(x.factor=Risk$Colony,trace.factor=Risk$Starting.State,response=Risk$p.fluct,type="b",col=c(1:3),leg.bty="o ",lwd=2,pch=c(18,24) ,xlab="Colony",ylab="Probability of Fluctuating",main="Prob fluctuating interaction colony and state")
interaction.plot(x.factor=Risk_edited$Starting.State,trace.factor=Risk_edited$Alternating.Color,response=Risk_edited$p.fluct,type="b",col=c(1:3),leg.bty="o ",lwd=2,pch=c(18,24) ,xlab="Starting State",ylab="Probability of Fluctuating")
interaction.plot(x.factor=Risk$Colony,trace.factor=Risk$Block.Number,response=Risk$p.fluct,type="l",col=c(1:8),leg.bty="o ",leg.bg="beige",lwd=2,pch=c(18,24) ,xlab="Alternating Color",ylab="Probability of Fluctuating",main="Starting State and Persistence Interaction plot")

InteractionBNC<-ggplot(Colony3, aes(x=Colony3$Block.Number,y=Colony3$p.fluct, color=Colony3$q)) + 
  geom_smooth(method=lm) +theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())
InteractionBNC
library(dplyr)
library(ggplot2)

 
####attempting to add error bars in GGplot. So far unsuccessful## 
Col3_summary <- Colony3 %>% # the names of the new data frame and the data frame to be summarised
summarise(mean_pf = mean(Colony3$p.fluct),  # calculates the mean of each group
          sd_PL = sd(Colony3$p.fluct), # calculates the standard deviation of each group
          n_PL = n(),  # calculates the sample size per group
          SE_PL = sd(p.fluct)/sqrt(n()))
mean(Colony3$p.fluct)
mean_pf
sd_PL
SE_PL

Colony3
SE_pf<-sd(Colony3$p.fluct, na.rm=FALSE) /  
  sqrt(length(Colony3$p.fluct[!is.na(Colony3$ p.fluct)]))
SE_pf


ppls<- ggplot(Colony3, aes(x=Colony3$Block.Number, y=Colony3$p.fluct, group=as.factor(Colony3$q), color=as.factor(Colony3$q)))+
  geom_point()+
  geom_line(aes(x=as.numeric(as.factor(Block.Number)))) +
  facetwidth(~q)
  geom_errorbar((aes(ymin=p.fluct-SE_pf,ymax=p.fluct+SE_pf,width=1)),
                position=position_dodge(0.05))
PP2<-ppls+labs(x="Block Number",y = "Probabiiity")
  ppl1<-PP2+ (scale_color_manual(values=c("#B2182B","#C2A5CF", "#1B7837", "#4393C3"),name="Persistence"))
ppl1+ theme_bw()


#####Three way interaction plots #####

State1 <- subset(Risk, Risk$Starting.State == "good")
State2 <- subset(Risk, Risk$Starting.State == "bad")
State1
#Now we do separate two dimensional plots for Day and then for Night.
###Starting State, alternating color and q##### 
install.packages("RColorBrewer")
library("RColorBrewer")
brewer.pal(n = 8, name = "PRGn")
library(ggplot2)
interaction.plot(x.factor = State1$Alternating.Color, trace.factor = State1$q, response = State1$p.fluct,
                   type = "l", col=c("#B2182B","#C2A5CF", "#1B7837", "#4393C3"),lty=1:4,lwd=3,ylim=c(0.2,0.8), legend=F,
                 trace.label="Persistence", xlab = "Alternating Color", ylab = "Mean Choice ofFluctutating Resource",cex.axis = 1.4,cex.lab=1.2)
legend("topleft",legend=c("0.66", "0.73","0.86","0.99"),
       col=c("#B2182B","#C2A5CF", "#1B7837", "#4393C3"), lty=1:4, lwd=3, cex=1.1,
       box.lty=0)
interaction.plot(x.factor = State2$Alternating.Color, trace.factor = State2$q, response = State2$p.fluct,
                 type = "l", lty=1:4, col=c("#B2182B","#C2A5CF", "#1B7837", "#4393C3"), ylim=(0:1),lwd=3, legend=F,
                 trace.label="Persistence", xlab = "Alternating Color", ylab = "Mean Choice of Fluctuating Reward",cex.axis = 1.4,cex.lab=1.2)
legend("topleft",legend=c("0.6", "0.73","0.86","0.99"),
       col=c("#B2182B","#C2A5CF", "#1B7837", "#4393C3"), lty=1:4, lwd=3, cex=1.0,
       box.lty=0)
###Starting State, q and colony #####
interaction.plot(x.factor = State1$Colony, trace.factor = State1$q, response = State1$p.fluct,
                 type = "l", col=c(1:4), leg.bty="o ",leg.bg="beige",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Persistence", xlab = "Colony", ylab = "Probability of Fluctutating", main= "When Starting State is Good")

interaction.plot(x.factor = State2$Colony, trace.factor = State2$q, response = State2$p.fluct,
                 type = "b", col=c(1:4), leg.bty="o ",pch=c(18,22),  axes=T,
                 trace.label="Persistence", xlab = "Colony", ylab = "Probability of Fluctutating", main= "When Starting State is Bad")
#####Starting State Alternating color and colony######
interaction.plot(x.factor = State1$Colony, trace.factor = State1$Alternating.Color, response = State1$p.fluct,
                 type = "l", col=c(1:4), leg.bty="o ",leg.bg="beige",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Alternating Color", xlab = "Colony", ylab = "Probability of Fluctutating", main= "When Starting State is Good")
interaction.plot(x.factor = State2$Colony, trace.factor = State2$Alternating.Color, response = State2$p.fluct,
                 type = "l", col=c(1:4), leg.bty="o ",leg.bg="beige",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Alternating Color", xlab = "Colony", ylab = "Probability of Fluctutating", main= "When Starting State is Bad")

####Starting State Block Number Colony two different ways#########
Block1<-subset(Risk, Risk$Block.Number == "1")
Block2<-subset(Risk, Risk$Block.Number == "2")
Block3<-subset(Risk, Risk$Block.Number == "3")
Block4<-subset(Risk, Risk$Block.Number == "4")
Block5<-subset(Risk, Risk$Block.Number == "5")
Block6<-subset(Risk, Risk$Block.Number == "6")
Block7<-subset(Risk, Risk$Block.Number == "7")
Block8<-subset(Risk, Risk$Block.Number == "8")

interaction.plot(x.factor = Block1$Colony, trace.factor = Block1$Starting.State, response = Block1$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting State", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 1")
interaction.plot(x.factor = Block2$Colony, trace.factor = Block2$Starting.State, response = Block2$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting State", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 2")
interaction.plot(x.factor = Block3$Colony, trace.factor = Block3$Starting.State, response = Block3$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting State", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 3")
interaction.plot(x.factor = Block4$Colony, trace.factor = Block4$Starting.State, response = Block4$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting State", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 4")
interaction.plot(x.factor = Block5$Colony, trace.factor = Block5$Starting.State, response = Block5$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting State", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 5")
interaction.plot(x.factor = Block6$Colony, trace.factor = Block6$Starting.State, response = Block6$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting.State", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 6")
interaction.plot(x.factor = Block7$Colony, trace.factor = Block7$Starting.State, response = Block7$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting.State", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 7")
interaction.plot(x.factor = Block8$Colony, trace.factor = Block8$Starting.State, response = Block8$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting.State", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 8")
Colony1<-subset(Risk, Risk$Colony == "AD1")
Colony2<-subset(Risk, Risk$Colony == "AD3") #
Colony3<-subset(Risk, Risk$Colony == "AD4")
Colony4<-subset(Risk, Risk$Colony == "AD5")
Colony5<-subset(Risk, Risk$Colony == "AD6") #
Colony6<-subset(Risk, Risk$Colony == "AD11")
Colony7<-subset(Risk, Risk$Colony == "AD16") #
Colony8<-subset(Risk, Risk$Colony == "AD15") #
Colony9<-subset(Risk, Risk$Colony == "AD18")
summary(Risk)

interaction.plot(x.factor = Colony1$Block.Number, trace.factor = Colony1$Starting.State, response = Colony1$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting.State", xlab = "Block Number", ylab = "Probability of Fluctutating", main= "Interaction Plot: Colony AD1")



interaction.plot(x.factor = Colony2$Block.Number, trace.factor = Colony2$Starting.State, response = Colony2$p.fluct,
                 type = "l", col=c(1:2), leg.bty="o ",ylim=(0:1), lwd=2, legend=F, axes=T,
                 trace.label="Starting.State", xlab = "Block Number", ylab = "Probability of Fluctutating")
legend(2,0.2,legend=c("Bad", "Good"),
       col=c(1:2), lty=2:1, cex=0.7,lwd=2,
       box.lty=0)



interaction.plot(x.factor = Colony3$Block.Number, trace.factor = Colony3$Starting.State, response = Colony3$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting.State", xlab = "Block Number", ylab = "Probability of Fluctutating", main= "Interaction Plot: Colony AD4")
interaction.plot(x.factor = Colony4$Block.Number, trace.factor = Colony4$Starting.State, response = Colony4$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting.State", xlab = "Block Number", ylab = "Probability of Fluctutating", main= "Interaction Plot: Colony AD5")


interaction.plot(x.factor = Colony5$Block.Number, trace.factor = Colony5$Starting.State, response = Colony5$p.fluct,
                 type = "l", col=c(1:2), ylim=(0:1), lwd=2, legend=F, axes=T,
                 trace.label="Starting.State", xlab = "Block Number", ylab = "Probability of Fluctutating")
legend(1.5,0.2,legend=c("Bad", "Good"),
       col=c(1:2), lty=2:1, cex=0.7, lwd=2,
       box.lty=0)


interaction.plot(x.factor = Colony6$Block.Number, trace.factor = Colony6$Starting.State, response = Colony6$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting.State", xlab = "Block Number", ylab = "Probability of Fluctutating", main= "Interaction Plot: Colony AD11")



interaction.plot(x.factor = Colony7$Block.Number, trace.factor = Colony7$Starting.State, response = Colony7$p.fluct,
                 type = "l", col=c(1:2), leg.bty="o ",ylim=(0:1), lwd=2,  legend=F, axes=T,
                 trace.label="Starting.State", xlab = "Block Number", ylab = "Probability of Fluctutating")
legend("topleft",legend=c("Bad", "Good"),
       col=c(1:2), lty=2:1, cex=0.7,lwd=2,
       box.lty=0)


interaction.plot(x.factor = Colony8$Block.Number, trace.factor = Colony8$Starting.State, response = Colony8$p.fluct,
                 type = "l", col=c(1:2), leg.bty="o ",ylim=(0:1), lwd=2, legend=F, axes=T,
                 trace.label="Starting.State", xlab = "Block Number", ylab = "Probability of Fluctutating")
legend(6.5,0.2,legend=c("Bad", "Good"),
       col=c(1:2), lty=2:1, cex=0.7, lwd=2,
       box.lty=0)



interaction.plot(x.factor = Colony9$Block.Number, trace.factor = Colony9$Starting.State, response = Colony9$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting.State", xlab = "Block Number", ylab = "Probability of Fluctutating", main= "Interaction Plot: Colony AD18")


#####Colony q block number############

interaction.plot(x.factor = Block1$Colony, trace.factor = Block1$q, response = Block1$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting State", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 1")
interaction.plot(x.factor = Block2$Colony, trace.factor = Block2$q, response = Block2$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting State", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 2")
interaction.plot(x.factor = Block3$Colony, trace.factor = Block3$q, response = Block3$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting State", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 3")
interaction.plot(x.factor = Block4$Colony, trace.factor = Block4$q, response = Block4$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting State", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 4")
interaction.plot(x.factor = Block5$Colony, trace.factor = Block5$q, response = Block5$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting State", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 5")
interaction.plot(x.factor = Block6$Colony, trace.factor = Block6$q, response = Block6$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting.State", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 6")
interaction.plot(x.factor = Block7$Colony, trace.factor = Block7$q, response = Block7$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting.State", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 7")
interaction.plot(x.factor = Block8$Colony, trace.factor = Block8$q, response = Block8$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting.State", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 8")

##another way##
interaction.plot(x.factor = Colony1$Block.Number, trace.factor = Colony1$q, response = Colony1$p.fluct,
                 type = "l", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Persistence", xlab = "Block Number", ylab = "Probability of Fluctutating", main= "Interaction Plot: Colony AD1")

means=by(Risk$p.fluct,list(Risk$Colony,Risk$Block.Number,Risk$q),mean)
means
se=function(x) sqrt(var(x)/length(x))
ses=by(Colony2$p.fluct,list(Colony2$Block.Number,Colony2$q,se)) 
ses 

interaction.plot(x.factor = Colony2$Block.Number, trace.factor = Colony2$q, response = Colony2$p.fluct,
                 type = "l", col=c("#B2182B","#C2A5CF", "#1B7837", "#4393C3"),las=1, lty=1:4,lwd=2,ylim=(0:1),legend=F,  axes=T,
                 trace.label="Persistence", xlab = "Block Number", ylab = "Probability of Fluctutating")
legend("bottomleft",legend=c("0.6","0.73","0.86","0.99"),
       col=c("#B2182B","#C2A5CF", "#1B7837", "#4393C3"), lty=1:4, cex=0.7,lwd=2,
       box.lty=0)
arrows(Colony2$Block.Number,means+ses, Colony2$Block.Number,means-ses, length=0.05, angle=90)
library(sciplot)

lineplot.CI(Colony3$Block.Number, Colony3$p.fluct, group = Colony3$q, data = Risk, 
trace.label="Persistence",fixed=FALSE, xlab="Block Number", lty=1:4, lwd=2, legend=FALSE,
ylab="Probability", cex.leg=1, col=c("#B2182B","#C2A5CF", "#1B7837", "#4393C3"),
ci.fun= function(x) c(fun(x)-se(x), fun(x)+se(x)),
err.width = 0.05)
legend("bottomright",legend=c("0.6","0.73","0.86","0.99"),
       col=c("#B2182B","#C2A5CF", "#1B7837", "#4393C3"), lty=1:4, cex=0.7,lwd=2,
       box.lty=0)



interaction.plot(x.factor = Colony3$Block.Number, trace.factor = Colony3$q, response = Colony3$p.fluct,
                 type = "b", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Persistence", xlab = "Block Number", ylab = "Probability of Fluctutating", main= "Interaction Plot: Colony AD4")
interaction.plot(x.factor = Colony4$Block.Number, trace.factor = Colony4$q, response = Colony4$p.fluct,
                 type = "b", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Persistence", xlab = "Block Number", ylab = "Probability of Fluctutating", main= "Interaction Plot: Colony AD5")


interaction.plot(x.factor = Colony5$Block.Number, trace.factor = Colony5$q, response = Colony5$p.fluct,
                 type = "l", col=c("#B2182B","#C2A5CF", "#1B7837", "#4393C3"),lty=1:4, ylim=(0:1), lwd=2, legend=F,  axes=T,
                 trace.label="Persistence", xlab = "Block Number", ylab = "Probability of Fluctutating")
legend("bottomleft",legend=c("0.6","0.73","0.86","0.99"),
       col=c("#B2182B","#C2A5CF", "#1B7837", "#4393C3"), lty=1:4, cex=0.7,lwd=2,
       box.lty=0)



interaction.plot(x.factor = Colony6$Block.Number, trace.factor = Colony6$q, response = Colony6$p.fluct,
                 type = "b", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Persistence", xlab = "Block Number", ylab = "Probability of Fluctutating", main= "Interaction Plot: Colony AD11")

interaction.plot(x.factor = Colony7$Block.Number, trace.factor = Colony7$q, response = Colony7$p.fluct,
                 type = "l", col=c("#B2182B","#C2A5CF", "#1B7837", "#4393C3"), ylim=(0:1), lty=1:4, lwd=2, legend=F, axes=T,
                 trace.label="Persistence", xlab = "Block Number", ylab = "Probability of Fluctutating")
legend("topleft",legend=c("0.6","0.73","0.86","0.99"),
       col=c("#B2182B","#C2A5CF", "#1B7837", "#4393C3"), lty=1:4, cex=0.7,lwd=2,
       box.lty=0)


interaction.plot(x.factor = Colony8$Block.Number, trace.factor = Colony8$q, response = Colony8$p.fluct,
                 type = "l", col=c("#B2182B","#C2A5CF", "#1B7837", "#4393C3"), ylim = (0:1), lty=1:4, lwd=2,  legend=F, axes=T,
                 trace.label="Persistence", xlab = "Block Number", ylab = "Probability of Fluctutating")
legend("topleft",legend=c("0.6","0.73","0.86","0.99"),
       col=c("#B2182B","#C2A5CF", "#1B7837", "#4393C3"), lty=1:4, cex=0.7,lwd=2,
       box.lty=0)
interaction.plot(x.factor = Colony9$Block.Number, trace.factor = Colony9$q, response = Colony9$p.fluct,
                 type = "b", col=c(2:4), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Persistence", xlab = "Block Number", ylab = "Probability of Fluctutating", main= "Interaction Plot: Colony AD18")


############Colony alternating color q ###########

Color1<- subset(Risk, Risk$Alternating.Color == "Yellow")
Color2 <- subset(Risk, Risk$Alternating.Color == "Blue")
Color1
interaction.plot(x.factor = Color1$Colony, trace.factor = Color1$q, response = Color1$p.fluct,
                type = "b", col=c(1:4), leg.bty="o ",leg.bg="beige",ylim=c(0,1),lwd=2,pch=c(1,4),  axes=T,
                trace.label="Persistence", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Alternating Color Yellow, Q, Colony")

interaction.plot(x.factor = State2$Colony, trace.factor = State2$q, response = State2$p.fluct,
                 type = "b", col=c(1:4), leg.bty="o ",pch=c(18,22),  axes=T,
                 trace.label="Persistence", xlab = "Colony", ylab = "Probability of Fluctutating", main= "Interaction Plot: Alternating Color Blue, Q, Colony")


######Starting State q Blocknumber
interaction.plot(x.factor = Block1$q, trace.factor = Block1$Starting.State, response = Block1$p.fluct,
                 type = "l", col=c(1:2), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting State", xlab = "Persistence", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 1")
interaction.plot(x.factor = Block2$q, trace.factor = Block2$Starting.State, response = Block2$p.fluct,
                 type = "l", col=c(1:2), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting State", xlab = "Persistence", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 2")
interaction.plot(x.factor = Block3$q, trace.factor = Block3$Starting.State, response = Block3$p.fluct,
                 type = "l", col=c(1:2), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting State", xlab = "Persistence", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 3")
interaction.plot(x.factor = Block4$q, trace.factor = Block4$Starting.State, response = Block4$p.fluct,
                 type = "l", col=c(1:2), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting State", xlab = "Persistence", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 4")
interaction.plot(x.factor = Block5$q, trace.factor = Block5$Starting.State, response = Block5$p.fluct,
                 type = "l", col=c(1:2), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting State", xlab = "Persistence", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 5")
interaction.plot(x.factor = Block6$q, trace.factor = Block6$Starting.State, response = Block6$p.fluct,
                 type = "l", col=c(1:2), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting.State", xlab = "Persistence", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 6")
interaction.plot(x.factor = Block7$q, trace.factor = Block7$Starting.State, response = Block7$p.fluct,
                 type = "l", col=c(1:2), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting.State", xlab = "Persistence", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 7")
interaction.plot(x.factor = Block8$q, trace.factor = Block8$Starting.State, response = Block8$p.fluct,
                 type = "l", col=c(1:2), leg.bty="o ",lwd=2,pch=c(18,22),  axes=T,
                 trace.label="Starting.State", xlab = "Persistence", ylab = "Probability of Fluctutating", main= "Interaction Plot: Block 8")

install.packages("esquisse")
install.packages("ggpmisc")
library(ggpmisc)
model <- aov(p.fluct~Alternating.Color+q+Starting.State, data=Risk)
summary(model)


q1<- subset(Risk, Risk$q == "0.99")
q2<- subset(Risk, Risk$q == "0.86")
q3<- subset(Risk, Risk$q == "0.73")
q4<- subset(Risk, Risk$q == "0.6")
interaction.plot(x.factor = q2$Block.Number, trace.factor = q2$Starting.State, response = q2$p.fluct,
                 type = "l", col=c(1:2), leg.bty="o ",lwd=2,pch=c(18,22), ,ylim=c(0.2,0.8), axes=T,cex.lab=1.2,cex=1.2,
                 trace.label="Starting.State",,legend=F, xlab = "Block", ylab = "Mean Choice of Fluctuating Resource")
legend("topleft",legend=c("good","bad"),
       col=c(1:2), lty=1:4, cex=0.7,lwd=2,
       box.lty=0)

plot44<-interaction.plot(x.factor = q2$Block.Number, trace.factor = q2$Starting.State, response = q2$p.fluct,
                 type = "l", col=c(1:2),lty=1:2, leg.bty="o ",lwd=2,pch=c(18,22), ,ylim=c(0.2,0.8), axes=T,cex.lab=1.2,cex=1.3,
                 trace.label="Starting.State",legend=T, xlab = "Block", ylab = "Mean Choice of Fluctuating Resource")
legend("topleft",legend=c("bad","good"),
       col=c(1:2), lty=1:2, cex=1,lwd=2,
       box.lty=0)


q4<-subset(q4, Block.Number %in% c('1','2',"3","4"))
q4
class(q4$Block.Number)
library(tidyverse)
q4$Block.Number<-as.factor(q4$Block.Number)
q2 %>% 
 group_by(Block.Number,Starting.State) %>% 
 summarise(mean_p.fluct = mean(p.fluct, na.rm = TRUE)) 
  ggplot(aes(x = Block.Number, y = mean_p.fluct, color = Starting.State)) +
  geom_boxplot()
plot

attach(my_file)
class(Block.Number)
plot2<-ggplot(my_file,aes(x = Block.Number, y = p.fluct, color = Starting.State)) +
  geom_line()+geom_point()
plot3<-plot2+geom_errorbar(aes(x=Block.Number,ymax = p.fluct+se,ymin = p.fluct-se))
plot2+theme_bw()+ylim(0.1,1)


std.error <- function(x) sd(x)/sqrt(length(x))
ste<-std.error(q1$p.fluct)
q1<-subset(q1, Block.Number %in% c('1','2',"3","4"))
q1B1<-subset(q1, Block.Number %in% c('1'))
q1B2<-subset(q1, Block.Number %in% c('2'))
q1B3<-subset(q1, Block.Number %in% c("3"))
q1B4<-subset(q1,Block.Number %in% c("4"))


data_col_NA<-q1[!is.na(q1$p.fluct),]
data_col_NA
df.summary3<-data_col_NA %>%
  group_by(data_col_NA$Block.Number,data_col_NA$Starting.State) %>%
  summarise(
    sd=sd(data_col_NA$p.fluct),
    len=mean(data_col_NA$p.fluct)
  )
df.summary3

tgc<-summarySE(q1,measurevar="p.fluct",groupvars = c("Block.Number","Starting.State"))
tgc
write.csv(tgc,"/Users/Rachel/Desktop/my_file.csv")


