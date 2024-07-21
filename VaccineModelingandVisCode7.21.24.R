#install.packages(car)
library(car)
mds3 = read.csv("2024JuneData23.csv")
##########################################################################################################
#creating table 1 descriptive statistics

mean(mds3$sixmoup)
mean(mds3$ninemoup)
mean(mds3$twelvemoup)
mean(mds3$libdem)
mean(mds3$vscoren)
mean(mds3$ruralperc)
mean(mds3$percChinese)
mean(mds3$dvaxpc)
mean(mds3$loggdp)
mean(mds3$loggdppc)
mean(mds3$logaid)
mean(mds3$vaxprio12)
mean(mds3$vaxfin12)
mean(mds3$vaxavail12)
mean(mds3$vaxman12)

median(mds3$sixmoup)
median(mds3$ninemoup)
median(mds3$twelvemoup)
median(mds3$libdem)
median(mds3$vscoren)
median(mds3$ruralperc)
median(mds3$percChinese)
median(mds3$dvaxpc)
median(mds3$loggdp)
median(mds3$loggdppc)
median(mds3$logaid)
median(mds3$vaxprio12)
median(mds3$vaxfin12)
median(mds3$vaxavail12)
median(mds3$vaxman12)

sd(mds3$sixmoup)
sd(mds3$ninemoup)
sd(mds3$twelvemoup)
sd(mds3$libdem)
sd(mds3$vscoren)
sd(mds3$ruralperc)
sd(mds3$percChinese)
sd(mds3$dvaxpc)
sd(mds3$loggdp)
sd(mds3$loggdppc)
sd(mds3$logaid)
sd(mds3$vaxprio12)
sd(mds3$vaxfin12)
sd(mds3$vaxavail12)
sd(mds3$vaxman12)

min(mds3$sixmoup)
min(mds3$ninemoup)
min(mds3$twelvemoup)
min(mds3$libdem)
min(mds3$vscoren)
min(mds3$ruralperc)
min(mds3$percChinese)
min(mds3$dvaxpc)
min(mds3$loggdp)
min(mds3$loggdppc)
min(mds3$logaid)
min(mds3$vaxprio12)
min(mds3$vaxfin12)
min(mds3$vaxavail12)
min(mds3$vaxman12)

max(mds3$sixmoup)
max(mds3$ninemoup)
max(mds3$twelvemoup)
max(mds3$libdem)
max(mds3$vscoren)
max(mds3$ruralperc)
max(mds3$percChinese)
max(mds3$dvaxpc)
max(mds3$loggdp)
max(mds3$loggdppc)
max(mds3$logaid)
max(mds3$vaxprio12)
max(mds3$vaxfin12)
max(mds3$vaxavail12)
max(mds3$vaxman12)

###################### Visualizing the vaxscore and 12 month uptake variables
library(ggplot2)
ggplot(mds3, aes(x=twelvemoup)) +
  geom_histogram(color="black",fill="lightgray", binwidth=5)+
  labs(title="Percentage Vaccinated against COVID-19 at 12 Months after Country's First Vaccination", x="Percentage Vaccinated", y="Density")

ggplot(mds3, aes(x=vscoren))+
  geom_histogram(color="black",fill="lightgray", binwidth=.1)+
  labs(title="Vax-score of Analyzed Countries", x="Vaxscore", y="Density")+
  theme(plot.title = element_text(hjust=.5))


###################### Initial model for including GDP, GDP per capita, and Vax-score

initialmodel = lm(sixmoup ~ loggdp+loggdppc+vscoren+ruralperc, data=mds3)
summary(initialmodel)
vif(initialmodel)

initialmodel2 = lm(ninemoup ~ loggdp+loggdppc+vscoren+ruralperc, data=mds3)
summary(initialmodel2)
vif(initialmodel2)

initialmodel3 = lm(twelvemoup ~ loggdp+loggdppc+vscoren+ruralperc, data=mds3)
summary(initialmodel3)
vif(initialmodel3)

##################### Backward model selection creation. Prelim models are for 12 months, then replicated for 9 and 6 months.
prelimmodel = lm(twelvemoup ~ vscoren + libdem + dvaxpc + percChinese + ruralperc + loggdp + loggdppc + vaxprio12 + vaxavail12 +vaxfin12+vaxman12+logaid, data=mds3)
summary(prelimmodel)
vif(prelimmodel)
nobs(prelimmodel)
plot(prelimmodel)

prelimmodel2 = lm(twelvemoup ~ vscoren + libdem + dvaxpc + percChinese + ruralperc + loggdp + loggdppc + vaxprio12 + vaxavail12 +vaxfin12+logaid, data=mds3)
summary(prelimmodel2)
vif(prelimmodel)

prelimmodel3 = lm(twelvemoup ~ vscoren + libdem + dvaxpc + percChinese + loggdp + loggdppc + vaxprio12 + vaxavail12 +vaxfin12+logaid, data=mds3)
summary(prelimmodel3)

prelimmodel4 = lm(twelvemoup ~ vscoren + dvaxpc + percChinese + loggdp + loggdppc + vaxprio12 + vaxavail12 +vaxfin12+logaid, data=mds3)
summary(prelimmodel4)

prelimmodel5 = lm(twelvemoup ~ vscoren + dvaxpc + loggdp + loggdppc + vaxprio12 + vaxavail12 +vaxfin12+logaid, data=mds3)
summary(prelimmodel5)

prelimmodel6 = lm(twelvemoup ~ vscoren + dvaxpc + loggdp + loggdppc + vaxavail12 +vaxfin12+logaid, data=mds3)
summary(prelimmodel6)

prelimmodel7 = lm(twelvemoup ~ vscoren + dvaxpc + loggdp + loggdppc + vaxavail12 +vaxfin12, data=mds3)
summary(prelimmodel7)
nobs(prelimmodel7)
vif(prelimmodel7)
plot(prelimmodel7)

hist(mds3$vscoren)
#############################################################################################################
prelimmodel9.1 = lm(ninemoup ~ vscoren + libdem + dvaxpc + percChinese + ruralperc + loggdp + loggdppc + vaxprio9 + vaxavail9 +vaxfin9 + vaxman9 + logaid, data=mds3)
summary(prelimmodel9.1)
vif(prelimmodel9.1)
nobs(prelimmodel9.1)
plot(prelimmodel9.1)

prelimmodel9.2 = lm(ninemoup ~ vscoren + dvaxpc + ruralperc + percChinese + loggdp + loggdppc + vaxprio9 + vaxavail9 +vaxfin9 + vaxman9 + logaid, data=mds3)
summary(prelimmodel9.2)

prelimmodel9.3 = lm(ninemoup ~ vscoren + dvaxpc + ruralperc + percChinese + loggdp + loggdppc + vaxavail9 +vaxfin9 + vaxman9 + logaid, data=mds3)
summary(prelimmodel9.3)

prelimmodel9.4 = lm(ninemoup ~ vscoren + dvaxpc + ruralperc + loggdp + loggdppc + vaxavail9 +vaxfin9 + vaxman9 + logaid, data=mds3)
summary(prelimmodel9.4)

prelimmodel9.5 = lm(ninemoup ~ vscoren + dvaxpc + ruralperc + loggdp + loggdppc + vaxavail9 +vaxfin9 + logaid, data=mds3)
summary(prelimmodel9.5)

prelimmodel9.6 = lm(ninemoup ~ vscoren + dvaxpc + loggdp + loggdppc + vaxavail9 +vaxfin9 + logaid, data=mds3)
summary(prelimmodel9.6)
nobs(prelimmodel9.6)

prelimmodel9.7 = lm(ninemoup ~ vscoren + dvaxpc + loggdp + loggdppc + vaxavail9 + logaid, data=mds3)
summary(prelimmodel9.7)

prelimmodel9.8 = lm(ninemoup ~ vscoren + dvaxpc + loggdp + loggdppc + logaid, data=mds3)
summary(prelimmodel9.8)

prelimmodel9.9 = lm(ninemoup ~ vscoren + dvaxpc + loggdp + loggdppc, data=mds3)
summary(prelimmodel9.9)

prelimmodel9.10 = lm(ninemoup ~ vscoren + dvaxpc + loggdppc, data=mds3)
summary(prelimmodel9.10)
vif(prelimmodel9.10)
nobs(prelimmodel9.10)
plot(prelimmodel9.10)

###############################################################################################################
prelimmodel6.1 = lm(sixmoup ~ vscoren + libdem + dvaxpc + percChinese +ruralperc + loggdp + loggdppc + vaxprio6 + vaxavail6 +vaxfin6 + vaxman6 + logaid, data=mds3)
summary(prelimmodel6.1)
nobs(prelimmodel6.1)
plot(prelimmodel6.1)

prelimmodel6.2 = lm(sixmoup ~ vscoren + libdem + dvaxpc + percChinese +ruralperc + loggdp + loggdppc + vaxavail6 +vaxfin6 + vaxman6 + logaid, data=mds3)
summary(prelimmodel6.2)

prelimmodel6.3 = lm(sixmoup ~ vscoren + libdem + dvaxpc + ruralperc + loggdp + loggdppc + vaxavail6 +vaxfin6 + vaxman6 + logaid, data=mds3)
summary(prelimmodel6.3)

prelimmodel6.4 = lm(sixmoup ~ vscoren + libdem + dvaxpc + ruralperc + loggdp + loggdppc + vaxavail6 +vaxfin6 + logaid, data=mds3)
summary(prelimmodel6.4)

prelimmodel6.5 = lm(sixmoup ~ vscoren + libdem + dvaxpc + loggdp + loggdppc + vaxavail6 +vaxfin6 + logaid, data=mds3)
summary(prelimmodel6.5)

prelimmodel6.6 = lm(sixmoup ~ vscoren + libdem + dvaxpc + loggdppc + vaxavail6 +vaxfin6 + logaid, data=mds3)
summary(prelimmodel6.6)

prelimmodel6.7 = lm(sixmoup ~ vscoren + libdem + dvaxpc + loggdppc + vaxavail6 + logaid, data=mds3)
summary(prelimmodel6.7)

prelimmodel6.8 = lm(sixmoup ~ vscoren + dvaxpc + loggdppc + vaxavail6 + logaid, data=mds3)
summary(prelimmodel6.8)

prelimmodel6.9 = lm(sixmoup ~ vscoren + dvaxpc + loggdppc + logaid, data=mds3)
summary(prelimmodel6.9)

prelimmodel6.10 = lm(sixmoup ~ vscoren + dvaxpc + loggdppc, data=mds3)
summary(prelimmodel6.10)
nobs(prelimmodel6.10)
plot(prelimmodel6.10)