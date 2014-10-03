library(lattice)
library(ggplot2)

gdays <- read.csv('/Volumes/group_dv/personal/DValenzano/Sep2014/qtl-direction-analysis/ReAutoReqtlresults/gdays2_tab.csv', sep=',', header=TRUE)
gdaysM <- read.csv('/Volumes/group_dv/personal/DValenzano/Sep2014/qtl-direction-analysis/ReAutoReqtlresults/gdaysM2_tab.csv', sep=',', header=TRUE)
gdaysF <- read.csv('/Volumes/group_dv/personal/DValenzano/Sep2014/qtl-direction-analysis/ReAutoReqtlresults/gdaysF2_tab.csv', sep=',', header=TRUE)


# First, I start with gdays

gdays_LG1 <- subset(gdays, gdays$LG == '1')
gdays_LG2 <- subset(gdays, gdays$LG == '2')
gdays_LG3 <- subset(gdays, gdays$LG == '3')
gdays_LG4 <- subset(gdays, gdays$LG == '4')
gdays_LG5 <- subset(gdays, gdays$LG == '5')
gdays_LG6 <- subset(gdays, gdays$LG == '6')
gdays_LG7 <- subset(gdays, gdays$LG == '7')
gdays_LG8 <- subset(gdays, gdays$LG == '8')
gdays_LG9 <- subset(gdays, gdays$LG == '9')
gdays_LG10 <- subset(gdays, gdays$LG == '10')
gdays_LG11 <- subset(gdays, gdays$LG == '11')
gdays_LG12 <- subset(gdays, gdays$LG == '12')
gdays_LG13 <- subset(gdays, gdays$LG == '13')
gdays_LG14 <- subset(gdays, gdays$LG == '14')
gdays_LG15 <- subset(gdays, gdays$LG == '15')
gdays_LG16 <- subset(gdays, gdays$LG == '16')
gdays_LG17 <- subset(gdays, gdays$LG == '17')
gdays_LG18 <- subset(gdays, gdays$LG == '18')
gdays_LG19 <- subset(gdays, gdays$LG == '19')

################################################
########FIRST WITH THE LATTICE PACKAGE##########
################################################

layout(matrix(c(1,2),2,1,byrow=TRUE), heights=c(1,4))
plot(gdays_LG1$cM, gdays_LG1$neg_log.qval., col="orange", lwd=3, ylab="-log(qval)", xlab="cM", ylim=c(0,3.0),  main="G cross, LG1", xlim=c(0,max(gdays_LG1$cM)))
plot(gdays_LG1$cM, gdays_LG1$med0, col="red", ylim=c(180,380), xlim=c(0,max(gdays_LG1$cM)), ylab="days", xlab="cM", main="G cross, LG1: Survival by Genotype")
par(new=TRUE)
plot(gdays_LG1$cM, gdays_LG1$med1, col="black", ylim=c(180,380), xlim=c(0,max(gdays_LG1$cM)), ylab="", xlab="")
par(new=TRUE)
plot(gdays_LG1$cM, gdays_LG1$med2, col="blue", ylim=c(180,380), xlim=c(0,max(gdays_LG1$cM)), ylab="", xlab="")
legend(10,380,  c("SL/SL", "SL/LL", "LL/LL"), lty=c(1,1,1), lwd=c(2.5,2.5,2.5), col=c("red","black", "blue"))

xyplot(gdays_LG1$med0 ~ gdays_LG1$cM, col="red", ylim=c(180,380), xlim=c(0,max(gdays_LG1$cM)), ylab="days", xlab="cM", main="G cross, LG1: Survival by Genotype", type=c("smooth", "p"), lwd=3)

################################################
########THEN WITH THE GGPLOT PACKAGE############
################################################

layout(matrix(c(1,2),2,1,byrow=TRUE), heights=c(1,4))
plot(gdays_LG1$cM, gdays_LG1$neg_log.qval., col="orange", lwd=3, ylab="-log(qval)", xlab="cM", ylim=c(0,3.0),  main="G cross, LG1", xlim=c(0,max(gdays_LG1$cM)))
ggplot(gdays_LG1, aes(cM,med0)) + geom_point(colour="#CC0000") + geom_smooth(colour="#CC0000") + stat_smooth(fill="red", colour="darkred", size=2, alpha = 0.2) + ylim(180,380) + xlim(0,max(gdays_LG1$cM)) + xlab("cM")+ylab("Days")+ 
  ggtitle("G cross, LG1: Survival by Genotype")
par(new=TRUE)
ggplot(gdays_LG1, aes(cM,med1)) + geom_point(colour="#000000") + geom_smooth(colour="#000000") + stat_smooth(fill="grey", colour="black", size=2, alpha = 0.2) + ylim(180,380) + xlim(0,max(gdays_LG1$cM)) 
par(new=TRUE)
ggplot(gdays_LG1, aes(cM,med2)) + geom_point(colour="#3366CC") + geom_smooth(colour="#3366CC") + stat_smooth(fill="blue", colour="darkblue", size=2, alpha = 0.2) + ylim(180,380) + xlim(0,max(gdays_LG1$cM)) 


layout(matrix(c(1,2),2,1,byrow=TRUE), heights=c(1,4))
plot(gdays_LG1$cM, gdays_LG1$neg_log.qval., col="orange", lwd=3, ylab="-log(qval)", xlab="cM", ylim=c(0,3.0),  main="G cross, LG1", xlim=c(0,max(gdays_LG1$cM)))
ggplot(gdays_LG1, aes(cM,med0)) + geom_point(colour="#CC0000") + geom_smooth(colour="#CC0000") + stat_smooth(fill="red", colour="darkred", size=2, alpha = 0.2) + ylim(180,380) + xlim(0,max(gdays_LG1$cM)) + xlab("cM")+ylab("Days")+ 
  ggtitle("G cross, LG1: Survival by Genotype") +
geom_line(gdays_LG1, aes(cM,med1)) + geom_point(colour="#000000") + geom_smooth(colour="#000000") + stat_smooth(fill="grey", colour="black", size=2, alpha = 0.2) + ylim(180,380) + xlim(0,max(gdays_LG1$cM)) 
geom_line(gdays_LG1, aes(cM,med2)) + geom_point(colour="#3366CC") + geom_smooth(colour="#3366CC") + stat_smooth(fill="blue", colour="darkblue", size=2, alpha = 0.2) + ylim(180,380) + xlim(0,max(gdays_LG1$cM)) 

df <- data.frame(r=gdays_LG1$med0, bk=gdays_LG1$med1, be=gdays_LG1$med2)


#FIRST, I NEED A DIFFERENT DATA FRAME http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
###LG1 for gdays

gdays_LG1_2 <- gdays_LG1[,1:3]
gdays_LG1_2
gdays_LG1_3 <- rbind(gdays_LG1_2, gdays_LG1_2, gdays_LG1_2)
med1 <- c(gdays_LG1$med0,gdays_LG1$med1, gdays_LG1$med2) 
mgr1 <- c(rep('SL/SL', length(gdays_LG1$med0)), rep('SL/LL', length(gdays_LG1$med1)), rep('LL/LL', length(gdays_LG1$med2)))
gdays_LG1_3$med <- med1
gdays_LG1_3$genotype <- mgr1
gdays_LG1_3

pg1 <- ggplot(gdays_LG1_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG1") + ylim(180,380) + xlim(0,max(gdays_LG1$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")
  
q1<- qplot(cM,neg_log.qval., data=gdays_LG1, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG1$cM)))
#multiplot(pg1, q1, cols=1)

###LG2 for gdays
gdays_LG2_2 <- gdays_LG2[,1:3]
gdays_LG2_2
gdays_LG2_3 <- rbind(gdays_LG2_2, gdays_LG2_2, gdays_LG2_2)
med2 <- c(gdays_LG2$med0,gdays_LG2$med1, gdays_LG2$med2) 
mgr2 <- c(rep('SL/SL', length(gdays_LG2$med0)), rep('SL/LL', length(gdays_LG2$med1)), rep('LL/LL', length(gdays_LG2$med2)))
gdays_LG2_3$med <- med2
gdays_LG2_3$genotype <- mgr2
gdays_LG2_3

pg2<- ggplot(gdays_LG2_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG2") + ylim(180,380) + xlim(0,max(gdays_LG2$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q2<- qplot(cM,neg_log.qval., data=gdays_LG2, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG2$cM)))
#multiplot(pg2, q2, cols=1)

###LG3 for gdays
gdays_LG3_2 <- gdays_LG3[,1:3]
gdays_LG3_2
gdays_LG3_3 <- rbind(gdays_LG3_2, gdays_LG3_2, gdays_LG3_2)
med3 <- c(gdays_LG3$med0,gdays_LG3$med1, gdays_LG3$med2) 
mgr3 <- c(rep('SL/SL', length(gdays_LG3$med0)), rep('SL/LL', length(gdays_LG3$med1)), rep('LL/LL', length(gdays_LG3$med2)))
gdays_LG3_3$med <- med3
gdays_LG3_3$genotype <- mgr3
gdays_LG3_3

pg3<- ggplot(gdays_LG3_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG3") + ylim(180,380) + xlim(0,max(gdays_LG3$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q3<- qplot(cM,neg_log.qval., data=gdays_LG3, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG3$cM)))
#multiplot(pg3, q3, cols=1)

###LG4 for gdays
gdays_LG4_2 <- gdays_LG4[,1:3]
gdays_LG4_2
gdays_LG4_3 <- rbind(gdays_LG4_2, gdays_LG4_2, gdays_LG4_2)
med4 <- c(gdays_LG4$med0,gdays_LG4$med1, gdays_LG4$med2) 
mgr4 <- c(rep('SL/SL', length(gdays_LG4$med0)), rep('SL/LL', length(gdays_LG4$med1)), rep('LL/LL', length(gdays_LG4$med2)))
gdays_LG4_3$med <- med4
gdays_LG4_3$genotype <- mgr4
gdays_LG4_3

pg4<- ggplot(gdays_LG4_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG4") + ylim(180,380) + xlim(0,max(gdays_LG4$cM)) + xlab("cM")+ylab("Days") +theme(legend.position="bottom")

q4<- qplot(cM,neg_log.qval., data=gdays_LG4, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG4$cM)))
#multiplot(pg4, q4, cols=1)

#multiplot(pg1, pg2, pg3, pg4, cols=2)

#TO ARRANGE MULTIPLE PANELS SEE HERE:
# http://stackoverflow.com/questions/9490482/combined-plot-of-ggplot2-not-in-a-single-plot-using-par-or-layout-functio

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q1, pg1, q2, pg2, q3, pg3, q4, pg4, layout=layout)

###LG5 for gdays

gdays_LG5_2 <- gdays_LG5[,1:3]
gdays_LG5_2
gdays_LG5_3 <- rbind(gdays_LG5_2, gdays_LG5_2, gdays_LG5_2)
med5 <- c(gdays_LG5$med0,gdays_LG5$med1, gdays_LG5$med2) 
mgr5 <- c(rep('SL/SL', length(gdays_LG5$med0)), rep('SL/LL', length(gdays_LG5$med1)), rep('LL/LL', length(gdays_LG5$med2)))
gdays_LG5_3$med <- med5
gdays_LG5_3$genotype <- mgr5
gdays_LG5_3

pg5 <- ggplot(gdays_LG5_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG5") + ylim(180,380) + xlim(0,max(gdays_LG5$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q5<- qplot(cM,neg_log.qval., data=gdays_LG5, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG5$cM)))
#multiplot(pg1, q1, cols=1)

###LG6 for gdays

gdays_LG6_2 <- gdays_LG6[,1:3]
gdays_LG6_2
gdays_LG6_3 <- rbind(gdays_LG6_2, gdays_LG6_2, gdays_LG6_2)
med6 <- c(gdays_LG6$med0,gdays_LG6$med1, gdays_LG6$med2) 
mgr6 <- c(rep('SL/SL', length(gdays_LG6$med0)), rep('SL/LL', length(gdays_LG6$med1)), rep('LL/LL', length(gdays_LG6$med2)))
gdays_LG6_3$med <- med6
gdays_LG6_3$genotype <- mgr6
gdays_LG6_3

pg6 <- ggplot(gdays_LG6_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG6") + ylim(180,380) + xlim(0,max(gdays_LG6$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q6<- qplot(cM,neg_log.qval., data=gdays_LG6, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG6$cM)))
#multiplot(pg1, q1, cols=1)

###LG7 for gdays
gdays_LG7_2 <- gdays_LG7[,1:3]
gdays_LG7_2
gdays_LG7_3 <- rbind(gdays_LG7_2, gdays_LG7_2, gdays_LG7_2)
med7 <- c(gdays_LG7$med0,gdays_LG7$med1, gdays_LG7$med2) 
mgr7 <- c(rep('SL/SL', length(gdays_LG7$med0)), rep('SL/LL', length(gdays_LG7$med1)), rep('LL/LL', length(gdays_LG7$med2)))
gdays_LG7_3$med <- med7
gdays_LG7_3$genotype <- mgr7
gdays_LG7_3

pg7 <- ggplot(gdays_LG7_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG7") + ylim(180,380) + xlim(0,max(gdays_LG7$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q7<- qplot(cM,neg_log.qval., data=gdays_LG7, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG7$cM)))
#multiplot(pg1, q1, cols=1)

###LG8 for gdays
gdays_LG8_2 <- gdays_LG8[,1:3]
gdays_LG8_2
gdays_LG8_3 <- rbind(gdays_LG8_2, gdays_LG8_2, gdays_LG8_2)
med8 <- c(gdays_LG8$med0,gdays_LG8$med1, gdays_LG8$med2) 
mgr8 <- c(rep('SL/SL', length(gdays_LG8$med0)), rep('SL/LL', length(gdays_LG8$med1)), rep('LL/LL', length(gdays_LG8$med2)))
gdays_LG8_3$med <- med8
gdays_LG8_3$genotype <- mgr8
gdays_LG8_3

pg8 <- ggplot(gdays_LG8_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG8") + ylim(180,380) + xlim(0,max(gdays_LG8$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q8<- qplot(cM,neg_log.qval., data=gdays_LG8, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG8$cM)))
#multiplot(pg1, q1, cols=1)

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q5, pg5, q6, pg6, q7, pg7, q8, pg8, layout=layout)

###LG9 for gdays
gdays_LG9_2 <- gdays_LG9[,1:3]
gdays_LG9_2
gdays_LG9_3 <- rbind(gdays_LG9_2, gdays_LG9_2, gdays_LG9_2)
med9 <- c(gdays_LG9$med0,gdays_LG9$med1, gdays_LG9$med2) 
mgr9 <- c(rep('SL/SL', length(gdays_LG9$med0)), rep('SL/LL', length(gdays_LG9$med1)), rep('LL/LL', length(gdays_LG9$med2)))
gdays_LG9_3$med <- med9
gdays_LG9_3$genotype <- mgr9
gdays_LG9_3

pg9 <- ggplot(gdays_LG9_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG9") + ylim(180,380) + xlim(0,max(gdays_LG9$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q9<- qplot(cM,neg_log.qval., data=gdays_LG9, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG9$cM)))
#multiplot(pg1, q1, cols=1)

###LG10 for gdays
gdays_LG10_2 <- gdays_LG10[,1:3]
gdays_LG10_2
gdays_LG10_3 <- rbind(gdays_LG10_2, gdays_LG10_2, gdays_LG10_2)
med10 <- c(gdays_LG10$med0,gdays_LG10$med1, gdays_LG10$med2) 
mgr10 <- c(rep('SL/SL', length(gdays_LG10$med0)), rep('SL/LL', length(gdays_LG10$med1)), rep('LL/LL', length(gdays_LG10$med2)))
gdays_LG10_3$med <- med10
gdays_LG10_3$genotype <- mgr10
gdays_LG10_3

pg10 <- ggplot(gdays_LG10_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG10") + ylim(180,380) + xlim(0,max(gdays_LG10$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q10<- qplot(cM,neg_log.qval., data=gdays_LG10, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG10$cM)))
#multiplot(pg1, q1, cols=1)

###LG11 for gdays
gdays_LG11_2 <- gdays_LG11[,1:3]
gdays_LG11_2
gdays_LG11_3 <- rbind(gdays_LG11_2, gdays_LG11_2, gdays_LG11_2)
med11 <- c(gdays_LG11$med0,gdays_LG11$med1, gdays_LG11$med2) 
mgr11 <- c(rep('SL/SL', length(gdays_LG11$med0)), rep('SL/LL', length(gdays_LG11$med1)), rep('LL/LL', length(gdays_LG11$med2)))
gdays_LG11_3$med <- med11
gdays_LG11_3$genotype <- mgr11
gdays_LG11_3

pg11 <- ggplot(gdays_LG11_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG11") + ylim(180,380) + xlim(0,max(gdays_LG11$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q11<- qplot(cM,neg_log.qval., data=gdays_LG11, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG11$cM)))
#multiplot(pg1, q1, cols=1)

###LG12 for gdays
gdays_LG12_2 <- gdays_LG12[,1:3]
gdays_LG12_2
gdays_LG12_3 <- rbind(gdays_LG12_2, gdays_LG12_2, gdays_LG12_2)
med12 <- c(gdays_LG12$med0,gdays_LG12$med1, gdays_LG12$med2) 
mgr12 <- c(rep('SL/SL', length(gdays_LG12$med0)), rep('SL/LL', length(gdays_LG12$med1)), rep('LL/LL', length(gdays_LG12$med2)))
gdays_LG12_3$med <- med12
gdays_LG12_3$genotype <- mgr12
gdays_LG12_3

pg12 <- ggplot(gdays_LG12_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG12") + ylim(180,380) + xlim(0,max(gdays_LG12$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q12<- qplot(cM,neg_log.qval., data=gdays_LG12, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG12$cM)))
#multiplot(pg1, q1, cols=1)

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q9, pg9, q10, pg10, q11, pg11, q12, pg12, layout=layout)

###LG13 for gdays
gdays_LG13_2 <- gdays_LG13[,1:3]
gdays_LG13_2
gdays_LG13_3 <- rbind(gdays_LG13_2, gdays_LG13_2, gdays_LG13_2)
med13 <- c(gdays_LG13$med0,gdays_LG13$med1, gdays_LG13$med2) 
mgr13 <- c(rep('SL/SL', length(gdays_LG13$med0)), rep('SL/LL', length(gdays_LG13$med1)), rep('LL/LL', length(gdays_LG13$med2)))
gdays_LG13_3$med <- med13
gdays_LG13_3$genotype <- mgr13
gdays_LG13_3

pg13 <- ggplot(gdays_LG13_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG13") + ylim(180,380) + xlim(0,max(gdays_LG13$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q13<- qplot(cM,neg_log.qval., data=gdays_LG13, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG13$cM)))
#multiplot(pg1, q1, cols=1)

###LG14 for gdays
gdays_LG14_2 <- gdays_LG14[,1:3]
gdays_LG14_2
gdays_LG14_3 <- rbind(gdays_LG14_2, gdays_LG14_2, gdays_LG14_2)
med14 <- c(gdays_LG14$med0,gdays_LG14$med1, gdays_LG14$med2) 
mgr14 <- c(rep('SL/SL', length(gdays_LG14$med0)), rep('SL/LL', length(gdays_LG14$med1)), rep('LL/LL', length(gdays_LG14$med2)))
gdays_LG14_3$med <- med14
gdays_LG14_3$genotype <- mgr14
gdays_LG14_3

pg14 <- ggplot(gdays_LG14_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG14") + ylim(180,380) + xlim(0,max(gdays_LG14$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q14<- qplot(cM,neg_log.qval., data=gdays_LG14, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG14$cM)))
#multiplot(pg1, q1, cols=1)

###LG15 for gdays
gdays_LG15_2 <- gdays_LG15[,1:3]
gdays_LG15_2
gdays_LG15_3 <- rbind(gdays_LG15_2, gdays_LG15_2, gdays_LG15_2)
med15 <- c(gdays_LG15$med0,gdays_LG15$med1, gdays_LG15$med2) 
mgr15 <- c(rep('SL/SL', length(gdays_LG15$med0)), rep('SL/LL', length(gdays_LG15$med1)), rep('LL/LL', length(gdays_LG15$med2)))
gdays_LG15_3$med <- med15
gdays_LG15_3$genotype <- mgr15
gdays_LG15_3

pg15 <- ggplot(gdays_LG15_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG15") + ylim(180,380) + xlim(0,max(gdays_LG15$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q15<- qplot(cM,neg_log.qval., data=gdays_LG15, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG15$cM)))
#multiplot(pg1, q1, cols=1)

###LG16 for gdays
gdays_LG16_2 <- gdays_LG16[,1:3]
gdays_LG16_2
gdays_LG16_3 <- rbind(gdays_LG16_2, gdays_LG16_2, gdays_LG16_2)
med16 <- c(gdays_LG16$med0,gdays_LG16$med1, gdays_LG16$med2) 
mgr16 <- c(rep('SL/SL', length(gdays_LG16$med0)), rep('SL/LL', length(gdays_LG16$med1)), rep('LL/LL', length(gdays_LG16$med2)))
gdays_LG16_3$med <- med16
gdays_LG16_3$genotype <- mgr16
gdays_LG16_3

pg16 <- ggplot(gdays_LG16_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG16") + ylim(180,380) + xlim(0,max(gdays_LG16$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q16<- qplot(cM,neg_log.qval., data=gdays_LG16, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG16$cM)))
#multiplot(pg1, q1, cols=1)

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q13, pg13, q14, pg14, q15, pg15, q16, pg16, layout=layout)

###LG17 for gdays
gdays_LG17_2 <- gdays_LG17[,1:3]
gdays_LG17_2
gdays_LG17_3 <- rbind(gdays_LG17_2, gdays_LG17_2, gdays_LG17_2)
med17 <- c(gdays_LG17$med0,gdays_LG17$med1, gdays_LG17$med2) 
mgr17 <- c(rep('SL/SL', length(gdays_LG17$med0)), rep('SL/LL', length(gdays_LG17$med1)), rep('LL/LL', length(gdays_LG17$med2)))
gdays_LG17_3$med <- med17
gdays_LG17_3$genotype <- mgr17
gdays_LG17_3

pg17 <- ggplot(gdays_LG17_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG17") + ylim(180,380) + xlim(0,max(gdays_LG17$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q17<- qplot(cM,neg_log.qval., data=gdays_LG17, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG17$cM)))
#multiplot(pg1, q1, cols=1)

###LG18 for gdays
gdays_LG18_2 <- gdays_LG18[,1:3]
gdays_LG18_2
gdays_LG18_3 <- rbind(gdays_LG18_2, gdays_LG18_2, gdays_LG18_2)
med18 <- c(gdays_LG18$med0,gdays_LG18$med1, gdays_LG18$med2) 
mgr18 <- c(rep('SL/SL', length(gdays_LG18$med0)), rep('SL/LL', length(gdays_LG18$med1)), rep('LL/LL', length(gdays_LG18$med2)))
gdays_LG18_3$med <- med18
gdays_LG18_3$genotype <- mgr18
gdays_LG18_3

pg18 <- ggplot(gdays_LG18_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG18") + ylim(180,380) + xlim(0,max(gdays_LG18$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q18<- qplot(cM,neg_log.qval., data=gdays_LG18, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG18$cM)))
#multiplot(pg1, q1, cols=1)

###LG19 for gdays
gdays_LG19_2 <- gdays_LG19[,1:3]
gdays_LG19_2
gdays_LG19_3 <- rbind(gdays_LG19_2, gdays_LG19_2, gdays_LG19_2)
med19 <- c(gdays_LG19$med0,gdays_LG19$med1, gdays_LG19$med2) 
mgr19 <- c(rep('SL/SL', length(gdays_LG19$med0)), rep('SL/LL', length(gdays_LG19$med1)), rep('LL/LL', length(gdays_LG19$med2)))
gdays_LG19_3$med <- med19
gdays_LG19_3$genotype <- mgr19
gdays_LG19_3

pg19 <- ggplot(gdays_LG19_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG19") + ylim(180,380) + xlim(0,max(gdays_LG19$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q19<- qplot(cM,neg_log.qval., data=gdays_LG19, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_LG19$cM)))
#multiplot(pg1, q1, cols=1)

layout <- matrix(c(1,2,2,3,4,4,5,6,6), nrow = 3)
multiplot(q17, pg17, q18, pg18, q19, pg19, layout=layout)

#########################SAME FOR MALES############################
gdaysM_LG1 <- subset(gdaysM, gdaysM$LG == '1')
gdaysM_LG2 <- subset(gdaysM, gdaysM$LG == '2')
gdaysM_LG3 <- subset(gdaysM, gdaysM$LG == '3')
gdaysM_LG4 <- subset(gdaysM, gdaysM$LG == '4')
gdaysM_LG5 <- subset(gdaysM, gdaysM$LG == '5')
gdaysM_LG6 <- subset(gdaysM, gdaysM$LG == '6')
gdaysM_LG7 <- subset(gdaysM, gdaysM$LG == '7')
gdaysM_LG8 <- subset(gdaysM, gdaysM$LG == '8')
gdaysM_LG9 <- subset(gdaysM, gdaysM$LG == '9')
gdaysM_LG10 <- subset(gdaysM, gdaysM$LG == '10')
gdaysM_LG11 <- subset(gdaysM, gdaysM$LG == '11')
gdaysM_LG12 <- subset(gdaysM, gdaysM$LG == '12')
gdaysM_LG13 <- subset(gdaysM, gdaysM$LG == '13')
gdaysM_LG14 <- subset(gdaysM, gdaysM$LG == '14')
gdaysM_LG15 <- subset(gdaysM, gdaysM$LG == '15')
gdaysM_LG16 <- subset(gdaysM, gdaysM$LG == '16')
gdaysM_LG17 <- subset(gdaysM, gdaysM$LG == '17')
gdaysM_LG18 <- subset(gdaysM, gdaysM$LG == '18')
gdaysM_LG19 <- subset(gdaysM, gdaysM$LG == '19')

gdaysM_LG1_2 <- gdaysM_LG1[,1:3]
gdaysM_LG1_2

gdaysM_LG1_3 <- rbind(gdaysM_LG1_2, gdaysM_LG1_2, gdaysM_LG1_2)
med1 <- c(gdaysM_LG1$med0,gdaysM_LG1$med1, gdaysM_LG1$med2) 
mgr1 <- c(rep('SL/SL', length(gdaysM_LG1$med0)), rep('SL/LL', length(gdaysM_LG1$med1)), rep('LL/LL', length(gdaysM_LG1$med2)))
gdaysM_LG1_3$med <- med1
gdaysM_LG1_3$genotype <- mgr1
gdaysM_LG1_3

pg1 <- ggplot(gdaysM_LG1_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG1") + ylim(180,380) + xlim(0,max(gdaysM_LG1$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q1<- qplot(cM,neg_log.qval., data=gdaysM_LG1, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG1$cM)))
#multiplot(pg1, q1, cols=1)

###LG2 for gdaysM
gdaysM_LG2_2 <- gdaysM_LG2[,1:3]
gdaysM_LG2_2
gdaysM_LG2_3 <- rbind(gdaysM_LG2_2, gdaysM_LG2_2, gdaysM_LG2_2)
med2 <- c(gdaysM_LG2$med0,gdaysM_LG2$med1, gdaysM_LG2$med2) 
mgr2 <- c(rep('SL/SL', length(gdaysM_LG2$med0)), rep('SL/LL', length(gdaysM_LG2$med1)), rep('LL/LL', length(gdaysM_LG2$med2)))
gdaysM_LG2_3$med <- med2
gdaysM_LG2_3$genotype <- mgr2
gdaysM_LG2_3

pg2<- ggplot(gdaysM_LG2_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG2") + ylim(180,380) + xlim(0,max(gdaysM_LG2$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q2<- qplot(cM,neg_log.qval., data=gdaysM_LG2, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG2$cM)))
#multiplot(pg2, q2, cols=1)

###LG3 for gdaysM
gdaysM_LG3_2 <- gdaysM_LG3[,1:3]
gdaysM_LG3_2
gdaysM_LG3_3 <- rbind(gdaysM_LG3_2, gdaysM_LG3_2, gdaysM_LG3_2)
med3 <- c(gdaysM_LG3$med0,gdaysM_LG3$med1, gdaysM_LG3$med2) 
mgr3 <- c(rep('SL/SL', length(gdaysM_LG3$med0)), rep('SL/LL', length(gdaysM_LG3$med1)), rep('LL/LL', length(gdaysM_LG3$med2)))
gdaysM_LG3_3$med <- med3
gdaysM_LG3_3$genotype <- mgr3
gdaysM_LG3_3

pg3<- ggplot(gdaysM_LG3_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG3") + ylim(180,380) + xlim(0,max(gdaysM_LG3$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q3<- qplot(cM,neg_log.qval., data=gdaysM_LG3, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG3$cM)))
#multiplot(pg3, q3, cols=1)

###LG4 for gdaysM
gdaysM_LG4_2 <- gdaysM_LG4[,1:3]
gdaysM_LG4_2
gdaysM_LG4_3 <- rbind(gdaysM_LG4_2, gdaysM_LG4_2, gdaysM_LG4_2)
med4 <- c(gdaysM_LG4$med0,gdaysM_LG4$med1, gdaysM_LG4$med2) 
mgr4 <- c(rep('SL/SL', length(gdaysM_LG4$med0)), rep('SL/LL', length(gdaysM_LG4$med1)), rep('LL/LL', length(gdaysM_LG4$med2)))
gdaysM_LG4_3$med <- med4
gdaysM_LG4_3$genotype <- mgr4
gdaysM_LG4_3

pg4<- ggplot(gdaysM_LG4_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG4") + ylim(180,380) + xlim(0,max(gdaysM_LG4$cM)) + xlab("cM")+ylab("Days") +theme(legend.position="bottom")

q4<- qplot(cM,neg_log.qval., data=gdaysM_LG4, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG4$cM)))
#multiplot(pg4, q4, cols=1)

#multiplot(pg1, pg2, pg3, pg4, cols=2)

#TO ARRANGE MULTIPLE PANELS SEE HERE:
# http://stackoverflow.com/questions/9490482/combined-plot-of-ggplot2-not-in-a-single-plot-using-par-or-layout-functio

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q1, pg1, q2, pg2, q3, pg3, q4, pg4, layout=layout)

###LG5 for gdaysM

gdaysM_LG5_2 <- gdaysM_LG5[,1:3]
gdaysM_LG5_2
gdaysM_LG5_3 <- rbind(gdaysM_LG5_2, gdaysM_LG5_2, gdaysM_LG5_2)
med5 <- c(gdaysM_LG5$med0,gdaysM_LG5$med1, gdaysM_LG5$med2) 
mgr5 <- c(rep('SL/SL', length(gdaysM_LG5$med0)), rep('SL/LL', length(gdaysM_LG5$med1)), rep('LL/LL', length(gdaysM_LG5$med2)))
gdaysM_LG5_3$med <- med5
gdaysM_LG5_3$genotype <- mgr5
gdaysM_LG5_3

pg5 <- ggplot(gdaysM_LG5_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG5") + ylim(180,380) + xlim(0,max(gdaysM_LG5$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q5<- qplot(cM,neg_log.qval., data=gdaysM_LG5, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG5$cM)))
#multiplot(pg1, q1, cols=1)

###LG6 for gdaysM

gdaysM_LG6_2 <- gdaysM_LG6[,1:3]
gdaysM_LG6_2
gdaysM_LG6_3 <- rbind(gdaysM_LG6_2, gdaysM_LG6_2, gdaysM_LG6_2)
med6 <- c(gdaysM_LG6$med0,gdaysM_LG6$med1, gdaysM_LG6$med2) 
mgr6 <- c(rep('SL/SL', length(gdaysM_LG6$med0)), rep('SL/LL', length(gdaysM_LG6$med1)), rep('LL/LL', length(gdaysM_LG6$med2)))
gdaysM_LG6_3$med <- med6
gdaysM_LG6_3$genotype <- mgr6
gdaysM_LG6_3

pg6 <- ggplot(gdaysM_LG6_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG6") + ylim(180,380) + xlim(0,max(gdaysM_LG6$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q6<- qplot(cM,neg_log.qval., data=gdaysM_LG6, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG6$cM)))
#multiplot(pg1, q1, cols=1)

###LG7 for gdaysM
gdaysM_LG7_2 <- gdaysM_LG7[,1:3]
gdaysM_LG7_2
gdaysM_LG7_3 <- rbind(gdaysM_LG7_2, gdaysM_LG7_2, gdaysM_LG7_2)
med7 <- c(gdaysM_LG7$med0,gdaysM_LG7$med1, gdaysM_LG7$med2) 
mgr7 <- c(rep('SL/SL', length(gdaysM_LG7$med0)), rep('SL/LL', length(gdaysM_LG7$med1)), rep('LL/LL', length(gdaysM_LG7$med2)))
gdaysM_LG7_3$med <- med7
gdaysM_LG7_3$genotype <- mgr7
gdaysM_LG7_3

pg7 <- ggplot(gdaysM_LG7_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG7") + ylim(180,380) + xlim(0,max(gdaysM_LG7$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q7<- qplot(cM,neg_log.qval., data=gdaysM_LG7, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG7$cM)))
#multiplot(pg1, q1, cols=1)

###LG8 for gdaysM
gdaysM_LG8_2 <- gdaysM_LG8[,1:3]
gdaysM_LG8_2
gdaysM_LG8_3 <- rbind(gdaysM_LG8_2, gdaysM_LG8_2, gdaysM_LG8_2)
med8 <- c(gdaysM_LG8$med0,gdaysM_LG8$med1, gdaysM_LG8$med2) 
mgr8 <- c(rep('SL/SL', length(gdaysM_LG8$med0)), rep('SL/LL', length(gdaysM_LG8$med1)), rep('LL/LL', length(gdaysM_LG8$med2)))
gdaysM_LG8_3$med <- med8
gdaysM_LG8_3$genotype <- mgr8
gdaysM_LG8_3

pg8 <- ggplot(gdaysM_LG8_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG8") + ylim(180,380) + xlim(0,max(gdaysM_LG8$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q8<- qplot(cM,neg_log.qval., data=gdaysM_LG8, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG8$cM)))
#multiplot(pg1, q1, cols=1)

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q5, pg5, q6, pg6, q7, pg7, q8, pg8, layout=layout)

###LG9 for gdaysM
gdaysM_LG9_2 <- gdaysM_LG9[,1:3]
gdaysM_LG9_2
gdaysM_LG9_3 <- rbind(gdaysM_LG9_2, gdaysM_LG9_2, gdaysM_LG9_2)
med9 <- c(gdaysM_LG9$med0,gdaysM_LG9$med1, gdaysM_LG9$med2) 
mgr9 <- c(rep('SL/SL', length(gdaysM_LG9$med0)), rep('SL/LL', length(gdaysM_LG9$med1)), rep('LL/LL', length(gdaysM_LG9$med2)))
gdaysM_LG9_3$med <- med9
gdaysM_LG9_3$genotype <- mgr9
gdaysM_LG9_3

pg9 <- ggplot(gdaysM_LG9_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG9") + ylim(180,380) + xlim(0,max(gdaysM_LG9$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q9<- qplot(cM,neg_log.qval., data=gdaysM_LG9, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG9$cM)))
#multiplot(pg1, q1, cols=1)

###LG10 for gdaysM
gdaysM_LG10_2 <- gdaysM_LG10[,1:3]
gdaysM_LG10_2
gdaysM_LG10_3 <- rbind(gdaysM_LG10_2, gdaysM_LG10_2, gdaysM_LG10_2)
med10 <- c(gdaysM_LG10$med0,gdaysM_LG10$med1, gdaysM_LG10$med2) 
mgr10 <- c(rep('SL/SL', length(gdaysM_LG10$med0)), rep('SL/LL', length(gdaysM_LG10$med1)), rep('LL/LL', length(gdaysM_LG10$med2)))
gdaysM_LG10_3$med <- med10
gdaysM_LG10_3$genotype <- mgr10
gdaysM_LG10_3

pg10 <- ggplot(gdaysM_LG10_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG10") + ylim(180,380) + xlim(0,max(gdaysM_LG10$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q10<- qplot(cM,neg_log.qval., data=gdaysM_LG10, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG10$cM)))
#multiplot(pg1, q1, cols=1)

###LG11 for gdaysM
gdaysM_LG11_2 <- gdaysM_LG11[,1:3]
gdaysM_LG11_2
gdaysM_LG11_3 <- rbind(gdaysM_LG11_2, gdaysM_LG11_2, gdaysM_LG11_2)
med11 <- c(gdaysM_LG11$med0,gdaysM_LG11$med1, gdaysM_LG11$med2) 
mgr11 <- c(rep('SL/SL', length(gdaysM_LG11$med0)), rep('SL/LL', length(gdaysM_LG11$med1)), rep('LL/LL', length(gdaysM_LG11$med2)))
gdaysM_LG11_3$med <- med11
gdaysM_LG11_3$genotype <- mgr11
gdaysM_LG11_3

pg11 <- ggplot(gdaysM_LG11_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG11") + ylim(180,380) + xlim(0,max(gdaysM_LG11$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q11<- qplot(cM,neg_log.qval., data=gdaysM_LG11, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG11$cM)))
#multiplot(pg1, q1, cols=1)

###LG12 for gdaysM
gdaysM_LG12_2 <- gdaysM_LG12[,1:3]
gdaysM_LG12_2
gdaysM_LG12_3 <- rbind(gdaysM_LG12_2, gdaysM_LG12_2, gdaysM_LG12_2)
med12 <- c(gdaysM_LG12$med0,gdaysM_LG12$med1, gdaysM_LG12$med2) 
mgr12 <- c(rep('SL/SL', length(gdaysM_LG12$med0)), rep('SL/LL', length(gdaysM_LG12$med1)), rep('LL/LL', length(gdaysM_LG12$med2)))
gdaysM_LG12_3$med <- med12
gdaysM_LG12_3$genotype <- mgr12
gdaysM_LG12_3

pg12 <- ggplot(gdaysM_LG12_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG12") + ylim(180,380) + xlim(0,max(gdaysM_LG12$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q12<- qplot(cM,neg_log.qval., data=gdaysM_LG12, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG12$cM)))
#multiplot(pg1, q1, cols=1)

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q9, pg9, q10, pg10, q11, pg11, q12, pg12, layout=layout)

###LG13 for gdaysM
gdaysM_LG13_2 <- gdaysM_LG13[,1:3]
gdaysM_LG13_2
gdaysM_LG13_3 <- rbind(gdaysM_LG13_2, gdaysM_LG13_2, gdaysM_LG13_2)
med13 <- c(gdaysM_LG13$med0,gdaysM_LG13$med1, gdaysM_LG13$med2) 
mgr13 <- c(rep('SL/SL', length(gdaysM_LG13$med0)), rep('SL/LL', length(gdaysM_LG13$med1)), rep('LL/LL', length(gdaysM_LG13$med2)))
gdaysM_LG13_3$med <- med13
gdaysM_LG13_3$genotype <- mgr13
gdaysM_LG13_3

pg13 <- ggplot(gdaysM_LG13_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG13") + ylim(180,380) + xlim(0,max(gdaysM_LG13$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q13<- qplot(cM,neg_log.qval., data=gdaysM_LG13, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG13$cM)))
#multiplot(pg1, q1, cols=1)

###LG14 for gdaysM
gdaysM_LG14_2 <- gdaysM_LG14[,1:3]
gdaysM_LG14_2
gdaysM_LG14_3 <- rbind(gdaysM_LG14_2, gdaysM_LG14_2, gdaysM_LG14_2)
med14 <- c(gdaysM_LG14$med0,gdaysM_LG14$med1, gdaysM_LG14$med2) 
mgr14 <- c(rep('SL/SL', length(gdaysM_LG14$med0)), rep('SL/LL', length(gdaysM_LG14$med1)), rep('LL/LL', length(gdaysM_LG14$med2)))
gdaysM_LG14_3$med <- med14
gdaysM_LG14_3$genotype <- mgr14
gdaysM_LG14_3

pg14 <- ggplot(gdaysM_LG14_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG14") + ylim(180,380) + xlim(0,max(gdaysM_LG14$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q14<- qplot(cM,neg_log.qval., data=gdaysM_LG14, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG14$cM)))
#multiplot(pg1, q1, cols=1)

###LG15 for gdaysM
gdaysM_LG15_2 <- gdaysM_LG15[,1:3]
gdaysM_LG15_2
gdaysM_LG15_3 <- rbind(gdaysM_LG15_2, gdaysM_LG15_2, gdaysM_LG15_2)
med15 <- c(gdaysM_LG15$med0,gdaysM_LG15$med1, gdaysM_LG15$med2) 
mgr15 <- c(rep('SL/SL', length(gdaysM_LG15$med0)), rep('SL/LL', length(gdaysM_LG15$med1)), rep('LL/LL', length(gdaysM_LG15$med2)))
gdaysM_LG15_3$med <- med15
gdaysM_LG15_3$genotype <- mgr15
gdaysM_LG15_3

pg15 <- ggplot(gdaysM_LG15_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG15") + ylim(180,380) + xlim(0,max(gdaysM_LG15$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q15<- qplot(cM,neg_log.qval., data=gdaysM_LG15, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG15$cM)))
#multiplot(pg1, q1, cols=1)

###LG16 for gdaysM
gdaysM_LG16_2 <- gdaysM_LG16[,1:3]
gdaysM_LG16_2
gdaysM_LG16_3 <- rbind(gdaysM_LG16_2, gdaysM_LG16_2, gdaysM_LG16_2)
med16 <- c(gdaysM_LG16$med0,gdaysM_LG16$med1, gdaysM_LG16$med2) 
mgr16 <- c(rep('SL/SL', length(gdaysM_LG16$med0)), rep('SL/LL', length(gdaysM_LG16$med1)), rep('LL/LL', length(gdaysM_LG16$med2)))
gdaysM_LG16_3$med <- med16
gdaysM_LG16_3$genotype <- mgr16
gdaysM_LG16_3

pg16 <- ggplot(gdaysM_LG16_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG16") + ylim(180,380) + xlim(0,max(gdaysM_LG16$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q16<- qplot(cM,neg_log.qval., data=gdaysM_LG16, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG16$cM)))
#multiplot(pg1, q1, cols=1)

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q13, pg13, q14, pg14, q15, pg15, q16, pg16, layout=layout)

###LG17 for gdaysM
gdaysM_LG17_2 <- gdaysM_LG17[,1:3]
gdaysM_LG17_2
gdaysM_LG17_3 <- rbind(gdaysM_LG17_2, gdaysM_LG17_2, gdaysM_LG17_2)
med17 <- c(gdaysM_LG17$med0,gdaysM_LG17$med1, gdaysM_LG17$med2) 
mgr17 <- c(rep('SL/SL', length(gdaysM_LG17$med0)), rep('SL/LL', length(gdaysM_LG17$med1)), rep('LL/LL', length(gdaysM_LG17$med2)))
gdaysM_LG17_3$med <- med17
gdaysM_LG17_3$genotype <- mgr17
gdaysM_LG17_3

pg17 <- ggplot(gdaysM_LG17_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG17") + ylim(180,380) + xlim(0,max(gdaysM_LG17$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q17<- qplot(cM,neg_log.qval., data=gdaysM_LG17, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG17$cM)))
#multiplot(pg1, q1, cols=1)

###LG18 for gdaysM
gdaysM_LG18_2 <- gdaysM_LG18[,1:3]
gdaysM_LG18_2
gdaysM_LG18_3 <- rbind(gdaysM_LG18_2, gdaysM_LG18_2, gdaysM_LG18_2)
med18 <- c(gdaysM_LG18$med0,gdaysM_LG18$med1, gdaysM_LG18$med2) 
mgr18 <- c(rep('SL/SL', length(gdaysM_LG18$med0)), rep('SL/LL', length(gdaysM_LG18$med1)), rep('LL/LL', length(gdaysM_LG18$med2)))
gdaysM_LG18_3$med <- med18
gdaysM_LG18_3$genotype <- mgr18
gdaysM_LG18_3

pg18 <- ggplot(gdaysM_LG18_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG18") + ylim(180,380) + xlim(0,max(gdaysM_LG18$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q18<- qplot(cM,neg_log.qval., data=gdaysM_LG18, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG18$cM)))
#multiplot(pg1, q1, cols=1)

###LG19 for gdaysM
gdaysM_LG19_2 <- gdaysM_LG19[,1:3]
gdaysM_LG19_2
gdaysM_LG19_3 <- rbind(gdaysM_LG19_2, gdaysM_LG19_2, gdaysM_LG19_2)
med19 <- c(gdaysM_LG19$med0,gdaysM_LG19$med1, gdaysM_LG19$med2) 
mgr19 <- c(rep('SL/SL', length(gdaysM_LG19$med0)), rep('SL/LL', length(gdaysM_LG19$med1)), rep('LL/LL', length(gdaysM_LG19$med2)))
gdaysM_LG19_3$med <- med19
gdaysM_LG19_3$genotype <- mgr19
gdaysM_LG19_3

pg19 <- ggplot(gdaysM_LG19_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysM, LG19") + ylim(180,380) + xlim(0,max(gdaysM_LG19$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q19<- qplot(cM,neg_log.qval., data=gdaysM_LG19, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysM_LG19$cM)))
#multiplot(pg1, q1, cols=1)

layout <- matrix(c(1,2,2,3,4,4,5,6,6), nrow = 3)
multiplot(q17, pg17, q18, pg18, q19, pg19, layout=layout)

###########BELOW SAME ANALYSIS FOR FEMALES##############
gdaysF_LG1 <- subset(gdaysF, gdaysF$LG == '1')
gdaysF_LG2 <- subset(gdaysF, gdaysF$LG == '2')
gdaysF_LG3 <- subset(gdaysF, gdaysF$LG == '3')
gdaysF_LG4 <- subset(gdaysF, gdaysF$LG == '4')
gdaysF_LG5 <- subset(gdaysF, gdaysF$LG == '5')
gdaysF_LG6 <- subset(gdaysF, gdaysF$LG == '6')
gdaysF_LG7 <- subset(gdaysF, gdaysF$LG == '7')
gdaysF_LG8 <- subset(gdaysF, gdaysF$LG == '8')
gdaysF_LG9 <- subset(gdaysF, gdaysF$LG == '9')
gdaysF_LG10 <- subset(gdaysF, gdaysF$LG == '10')
gdaysF_LG11 <- subset(gdaysF, gdaysF$LG == '11')
gdaysF_LG12 <- subset(gdaysF, gdaysF$LG == '12')
gdaysF_LG13 <- subset(gdaysF, gdaysF$LG == '13')
gdaysF_LG14 <- subset(gdaysF, gdaysF$LG == '14')
gdaysF_LG15 <- subset(gdaysF, gdaysF$LG == '15')
gdaysF_LG16 <- subset(gdaysF, gdaysF$LG == '16')
gdaysF_LG17 <- subset(gdaysF, gdaysF$LG == '17')
gdaysF_LG18 <- subset(gdaysF, gdaysF$LG == '18')
gdaysF_LG19 <- subset(gdaysF, gdaysF$LG == '19')

gdaysF_LG1_2 <- gdaysF_LG1[,1:3]
gdaysF_LG1_2
gdaysF_LG1_3 <- rbind(gdaysF_LG1_2, gdaysF_LG1_2, gdaysF_LG1_2)
med1 <- c(gdaysF_LG1$med0,gdaysF_LG1$med1, gdaysF_LG1$med2) 
mgr1 <- c(rep('SL/SL', length(gdaysF_LG1$med0)), rep('SL/LL', length(gdaysF_LG1$med1)), rep('LL/LL', length(gdaysF_LG1$med2)))
gdaysF_LG1_3$med <- med1
gdaysF_LG1_3$genotype <- mgr1
gdaysF_LG1_3

pg1 <- ggplot(gdaysF_LG1_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG1") + ylim(180,380) + xlim(0,max(gdaysF_LG1$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q1<- qplot(cM,neg_log.qval., data=gdaysF_LG1, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG1$cM)))
#multiplot(pg1, q1, cols=1)

###LG2 for gdaysF
gdaysF_LG2_2 <- gdaysF_LG2[,1:3]
gdaysF_LG2_2
gdaysF_LG2_3 <- rbind(gdaysF_LG2_2, gdaysF_LG2_2, gdaysF_LG2_2)
med2 <- c(gdaysF_LG2$med0,gdaysF_LG2$med1, gdaysF_LG2$med2) 
mgr2 <- c(rep('SL/SL', length(gdaysF_LG2$med0)), rep('SL/LL', length(gdaysF_LG2$med1)), rep('LL/LL', length(gdaysF_LG2$med2)))
gdaysF_LG2_3$med <- med2
gdaysF_LG2_3$genotype <- mgr2
gdaysF_LG2_3

pg2<- ggplot(gdaysF_LG2_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG2") + ylim(180,380) + xlim(0,max(gdaysF_LG2$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q2<- qplot(cM,neg_log.qval., data=gdaysF_LG2, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG2$cM)))
#multiplot(pg2, q2, cols=1)

###LG3 for gdaysF
gdaysF_LG3_2 <- gdaysF_LG3[,1:3]
gdaysF_LG3_2
gdaysF_LG3_3 <- rbind(gdaysF_LG3_2, gdaysF_LG3_2, gdaysF_LG3_2)
med3 <- c(gdaysF_LG3$med0,gdaysF_LG3$med1, gdaysF_LG3$med2) 
mgr3 <- c(rep('SL/SL', length(gdaysF_LG3$med0)), rep('SL/LL', length(gdaysF_LG3$med1)), rep('LL/LL', length(gdaysF_LG3$med2)))
gdaysF_LG3_3$med <- med3
gdaysF_LG3_3$genotype <- mgr3
gdaysF_LG3_3

pg3<- ggplot(gdaysF_LG3_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG3") + ylim(180,380) + xlim(0,max(gdaysF_LG3$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q3<- qplot(cM,neg_log.qval., data=gdaysF_LG3, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG3$cM)))
#multiplot(pg3, q3, cols=1)

###LG4 for gdaysF
gdaysF_LG4_2 <- gdaysF_LG4[,1:3]
gdaysF_LG4_2
gdaysF_LG4_3 <- rbind(gdaysF_LG4_2, gdaysF_LG4_2, gdaysF_LG4_2)
med4 <- c(gdaysF_LG4$med0,gdaysF_LG4$med1, gdaysF_LG4$med2) 
mgr4 <- c(rep('SL/SL', length(gdaysF_LG4$med0)), rep('SL/LL', length(gdaysF_LG4$med1)), rep('LL/LL', length(gdaysF_LG4$med2)))
gdaysF_LG4_3$med <- med4
gdaysF_LG4_3$genotype <- mgr4
gdaysF_LG4_3

pg4<- ggplot(gdaysF_LG4_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG4") + ylim(180,380) + xlim(0,max(gdaysF_LG4$cM)) + xlab("cM")+ylab("Days") +theme(legend.position="bottom")

q4<- qplot(cM,neg_log.qval., data=gdaysF_LG4, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG4$cM)))
#multiplot(pg4, q4, cols=1)

#multiplot(pg1, pg2, pg3, pg4, cols=2)

#TO ARRANGE MULTIPLE PANELS SEE HERE:
# http://stackoverflow.com/questions/9490482/combined-plot-of-ggplot2-not-in-a-single-plot-using-par-or-layout-functio

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q1, pg1, q2, pg2, q3, pg3, q4, pg4, layout=layout)

###LG5 for gdaysF

gdaysF_LG5_2 <- gdaysF_LG5[,1:3]
gdaysF_LG5_2
gdaysF_LG5_3 <- rbind(gdaysF_LG5_2, gdaysF_LG5_2, gdaysF_LG5_2)
med5 <- c(gdaysF_LG5$med0,gdaysF_LG5$med1, gdaysF_LG5$med2) 
mgr5 <- c(rep('SL/SL', length(gdaysF_LG5$med0)), rep('SL/LL', length(gdaysF_LG5$med1)), rep('LL/LL', length(gdaysF_LG5$med2)))
gdaysF_LG5_3$med <- med5
gdaysF_LG5_3$genotype <- mgr5
gdaysF_LG5_3

pg5 <- ggplot(gdaysF_LG5_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG5") + ylim(180,380) + xlim(0,max(gdaysF_LG5$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q5<- qplot(cM,neg_log.qval., data=gdaysF_LG5, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG5$cM)))
#multiplot(pg1, q1, cols=1)

###LG6 for gdaysF

gdaysF_LG6_2 <- gdaysF_LG6[,1:3]
gdaysF_LG6_2
gdaysF_LG6_3 <- rbind(gdaysF_LG6_2, gdaysF_LG6_2, gdaysF_LG6_2)
med6 <- c(gdaysF_LG6$med0,gdaysF_LG6$med1, gdaysF_LG6$med2) 
mgr6 <- c(rep('SL/SL', length(gdaysF_LG6$med0)), rep('SL/LL', length(gdaysF_LG6$med1)), rep('LL/LL', length(gdaysF_LG6$med2)))
gdaysF_LG6_3$med <- med6
gdaysF_LG6_3$genotype <- mgr6
gdaysF_LG6_3

pg6 <- ggplot(gdaysF_LG6_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG6") + ylim(180,380) + xlim(0,max(gdaysF_LG6$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q6<- qplot(cM,neg_log.qval., data=gdaysF_LG6, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG6$cM)))
#multiplot(pg1, q1, cols=1)

###LG7 for gdaysF
gdaysF_LG7_2 <- gdaysF_LG7[,1:3]
gdaysF_LG7_2
gdaysF_LG7_3 <- rbind(gdaysF_LG7_2, gdaysF_LG7_2, gdaysF_LG7_2)
med7 <- c(gdaysF_LG7$med0,gdaysF_LG7$med1, gdaysF_LG7$med2) 
mgr7 <- c(rep('SL/SL', length(gdaysF_LG7$med0)), rep('SL/LL', length(gdaysF_LG7$med1)), rep('LL/LL', length(gdaysF_LG7$med2)))
gdaysF_LG7_3$med <- med7
gdaysF_LG7_3$genotype <- mgr7
gdaysF_LG7_3

pg7 <- ggplot(gdaysF_LG7_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG7") + ylim(180,380) + xlim(0,max(gdaysF_LG7$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q7<- qplot(cM,neg_log.qval., data=gdaysF_LG7, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG7$cM)))
#multiplot(pg1, q1, cols=1)

###LG8 for gdaysF
gdaysF_LG8_2 <- gdaysF_LG8[,1:3]
gdaysF_LG8_2
gdaysF_LG8_3 <- rbind(gdaysF_LG8_2, gdaysF_LG8_2, gdaysF_LG8_2)
med8 <- c(gdaysF_LG8$med0,gdaysF_LG8$med1, gdaysF_LG8$med2) 
mgr8 <- c(rep('SL/SL', length(gdaysF_LG8$med0)), rep('SL/LL', length(gdaysF_LG8$med1)), rep('LL/LL', length(gdaysF_LG8$med2)))
gdaysF_LG8_3$med <- med8
gdaysF_LG8_3$genotype <- mgr8
gdaysF_LG8_3

pg8 <- ggplot(gdaysF_LG8_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG8") + ylim(180,380) + xlim(0,max(gdaysF_LG8$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q8<- qplot(cM,neg_log.qval., data=gdaysF_LG8, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG8$cM)))
#multiplot(pg1, q1, cols=1)

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q5, pg5, q6, pg6, q7, pg7, q8, pg8, layout=layout)

###LG9 for gdaysF
gdaysF_LG9_2 <- gdaysF_LG9[,1:3]
gdaysF_LG9_2
gdaysF_LG9_3 <- rbind(gdaysF_LG9_2, gdaysF_LG9_2, gdaysF_LG9_2)
med9 <- c(gdaysF_LG9$med0,gdaysF_LG9$med1, gdaysF_LG9$med2) 
mgr9 <- c(rep('SL/SL', length(gdaysF_LG9$med0)), rep('SL/LL', length(gdaysF_LG9$med1)), rep('LL/LL', length(gdaysF_LG9$med2)))
gdaysF_LG9_3$med <- med9
gdaysF_LG9_3$genotype <- mgr9
gdaysF_LG9_3

pg9 <- ggplot(gdaysF_LG9_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG9") + ylim(180,380) + xlim(0,max(gdaysF_LG9$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q9<- qplot(cM,neg_log.qval., data=gdaysF_LG9, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG9$cM)))
#multiplot(pg1, q1, cols=1)

###LG10 for gdaysF
gdaysF_LG10_2 <- gdaysF_LG10[,1:3]
gdaysF_LG10_2
gdaysF_LG10_3 <- rbind(gdaysF_LG10_2, gdaysF_LG10_2, gdaysF_LG10_2)
med10 <- c(gdaysF_LG10$med0,gdaysF_LG10$med1, gdaysF_LG10$med2) 
mgr10 <- c(rep('SL/SL', length(gdaysF_LG10$med0)), rep('SL/LL', length(gdaysF_LG10$med1)), rep('LL/LL', length(gdaysF_LG10$med2)))
gdaysF_LG10_3$med <- med10
gdaysF_LG10_3$genotype <- mgr10
gdaysF_LG10_3

pg10 <- ggplot(gdaysF_LG10_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG10") + ylim(180,380) + xlim(0,max(gdaysF_LG10$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q10<- qplot(cM,neg_log.qval., data=gdaysF_LG10, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG10$cM)))
#multiplot(pg1, q1, cols=1)

###LG11 for gdaysF
gdaysF_LG11_2 <- gdaysF_LG11[,1:3]
gdaysF_LG11_2
gdaysF_LG11_3 <- rbind(gdaysF_LG11_2, gdaysF_LG11_2, gdaysF_LG11_2)
med11 <- c(gdaysF_LG11$med0,gdaysF_LG11$med1, gdaysF_LG11$med2) 
mgr11 <- c(rep('SL/SL', length(gdaysF_LG11$med0)), rep('SL/LL', length(gdaysF_LG11$med1)), rep('LL/LL', length(gdaysF_LG11$med2)))
gdaysF_LG11_3$med <- med11
gdaysF_LG11_3$genotype <- mgr11
gdaysF_LG11_3

pg11 <- ggplot(gdaysF_LG11_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG11") + ylim(180,380) + xlim(0,max(gdaysF_LG11$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q11<- qplot(cM,neg_log.qval., data=gdaysF_LG11, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG11$cM)))
#multiplot(pg1, q1, cols=1)

###LG12 for gdaysF
gdaysF_LG12_2 <- gdaysF_LG12[,1:3]
gdaysF_LG12_2
gdaysF_LG12_3 <- rbind(gdaysF_LG12_2, gdaysF_LG12_2, gdaysF_LG12_2)
med12 <- c(gdaysF_LG12$med0,gdaysF_LG12$med1, gdaysF_LG12$med2) 
mgr12 <- c(rep('SL/SL', length(gdaysF_LG12$med0)), rep('SL/LL', length(gdaysF_LG12$med1)), rep('LL/LL', length(gdaysF_LG12$med2)))
gdaysF_LG12_3$med <- med12
gdaysF_LG12_3$genotype <- mgr12
gdaysF_LG12_3

pg12 <- ggplot(gdaysF_LG12_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG12") + ylim(180,380) + xlim(0,max(gdaysF_LG12$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q12<- qplot(cM,neg_log.qval., data=gdaysF_LG12, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG12$cM)))
#multiplot(pg1, q1, cols=1)

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q9, pg9, q10, pg10, q11, pg11, q12, pg12, layout=layout)

###LG13 for gdaysF
gdaysF_LG13_2 <- gdaysF_LG13[,1:3]
gdaysF_LG13_2
gdaysF_LG13_3 <- rbind(gdaysF_LG13_2, gdaysF_LG13_2, gdaysF_LG13_2)
med13 <- c(gdaysF_LG13$med0,gdaysF_LG13$med1, gdaysF_LG13$med2) 
mgr13 <- c(rep('SL/SL', length(gdaysF_LG13$med0)), rep('SL/LL', length(gdaysF_LG13$med1)), rep('LL/LL', length(gdaysF_LG13$med2)))
gdaysF_LG13_3$med <- med13
gdaysF_LG13_3$genotype <- mgr13
gdaysF_LG13_3

pg13 <- ggplot(gdaysF_LG13_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG13") + ylim(180,380) + xlim(0,max(gdaysF_LG13$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q13<- qplot(cM,neg_log.qval., data=gdaysF_LG13, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG13$cM)))
#multiplot(pg1, q1, cols=1)

###LG14 for gdaysF
gdaysF_LG14_2 <- gdaysF_LG14[,1:3]
gdaysF_LG14_2
gdaysF_LG14_3 <- rbind(gdaysF_LG14_2, gdaysF_LG14_2, gdaysF_LG14_2)
med14 <- c(gdaysF_LG14$med0,gdaysF_LG14$med1, gdaysF_LG14$med2) 
mgr14 <- c(rep('SL/SL', length(gdaysF_LG14$med0)), rep('SL/LL', length(gdaysF_LG14$med1)), rep('LL/LL', length(gdaysF_LG14$med2)))
gdaysF_LG14_3$med <- med14
gdaysF_LG14_3$genotype <- mgr14
gdaysF_LG14_3

pg14 <- ggplot(gdaysF_LG14_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG14") + ylim(180,380) + xlim(0,max(gdaysF_LG14$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q14<- qplot(cM,neg_log.qval., data=gdaysF_LG14, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG14$cM)))
#multiplot(pg1, q1, cols=1)

###LG15 for gdaysF
gdaysF_LG15_2 <- gdaysF_LG15[,1:3]
gdaysF_LG15_2
gdaysF_LG15_3 <- rbind(gdaysF_LG15_2, gdaysF_LG15_2, gdaysF_LG15_2)
med15 <- c(gdaysF_LG15$med0,gdaysF_LG15$med1, gdaysF_LG15$med2) 
mgr15 <- c(rep('SL/SL', length(gdaysF_LG15$med0)), rep('SL/LL', length(gdaysF_LG15$med1)), rep('LL/LL', length(gdaysF_LG15$med2)))
gdaysF_LG15_3$med <- med15
gdaysF_LG15_3$genotype <- mgr15
gdaysF_LG15_3

pg15 <- ggplot(gdaysF_LG15_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG15") + ylim(180,380) + xlim(0,max(gdaysF_LG15$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q15<- qplot(cM,neg_log.qval., data=gdaysF_LG15, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG15$cM)))
#multiplot(pg1, q1, cols=1)

###LG16 for gdaysF
gdaysF_LG16_2 <- gdaysF_LG16[,1:3]
gdaysF_LG16_2
gdaysF_LG16_3 <- rbind(gdaysF_LG16_2, gdaysF_LG16_2, gdaysF_LG16_2)
med16 <- c(gdaysF_LG16$med0,gdaysF_LG16$med1, gdaysF_LG16$med2) 
mgr16 <- c(rep('SL/SL', length(gdaysF_LG16$med0)), rep('SL/LL', length(gdaysF_LG16$med1)), rep('LL/LL', length(gdaysF_LG16$med2)))
gdaysF_LG16_3$med <- med16
gdaysF_LG16_3$genotype <- mgr16
gdaysF_LG16_3

pg16 <- ggplot(gdaysF_LG16_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG16") + ylim(180,380) + xlim(0,max(gdaysF_LG16$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q16<- qplot(cM,neg_log.qval., data=gdaysF_LG16, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG16$cM)))
#multiplot(pg1, q1, cols=1)

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q13, pg13, q14, pg14, q15, pg15, q16, pg16, layout=layout)

###LG17 for gdaysF
gdaysF_LG17_2 <- gdaysF_LG17[,1:3]
gdaysF_LG17_2
gdaysF_LG17_3 <- rbind(gdaysF_LG17_2, gdaysF_LG17_2, gdaysF_LG17_2)
med17 <- c(gdaysF_LG17$med0,gdaysF_LG17$med1, gdaysF_LG17$med2) 
mgr17 <- c(rep('SL/SL', length(gdaysF_LG17$med0)), rep('SL/LL', length(gdaysF_LG17$med1)), rep('LL/LL', length(gdaysF_LG17$med2)))
gdaysF_LG17_3$med <- med17
gdaysF_LG17_3$genotype <- mgr17
gdaysF_LG17_3

pg17 <- ggplot(gdaysF_LG17_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG17") + ylim(180,380) + xlim(0,max(gdaysF_LG17$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q17<- qplot(cM,neg_log.qval., data=gdaysF_LG17, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG17$cM)))
#multiplot(pg1, q1, cols=1)

###LG18 for gdaysF
gdaysF_LG18_2 <- gdaysF_LG18[,1:3]
gdaysF_LG18_2
gdaysF_LG18_3 <- rbind(gdaysF_LG18_2, gdaysF_LG18_2, gdaysF_LG18_2)
med18 <- c(gdaysF_LG18$med0,gdaysF_LG18$med1, gdaysF_LG18$med2) 
mgr18 <- c(rep('SL/SL', length(gdaysF_LG18$med0)), rep('SL/LL', length(gdaysF_LG18$med1)), rep('LL/LL', length(gdaysF_LG18$med2)))
gdaysF_LG18_3$med <- med18
gdaysF_LG18_3$genotype <- mgr18
gdaysF_LG18_3

pg18 <- ggplot(gdaysF_LG18_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG18") + ylim(180,380) + xlim(0,max(gdaysF_LG18$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q18<- qplot(cM,neg_log.qval., data=gdaysF_LG18, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG18$cM)))
#multiplot(pg1, q1, cols=1)

###LG19 for gdaysF
gdaysF_LG19_2 <- gdaysF_LG19[,1:3]
gdaysF_LG19_2
gdaysF_LG19_3 <- rbind(gdaysF_LG19_2, gdaysF_LG19_2, gdaysF_LG19_2)
med19 <- c(gdaysF_LG19$med0,gdaysF_LG19$med1, gdaysF_LG19$med2) 
mgr19 <- c(rep('SL/SL', length(gdaysF_LG19$med0)), rep('SL/LL', length(gdaysF_LG19$med1)), rep('LL/LL', length(gdaysF_LG19$med2)))
gdaysF_LG19_3$med <- med19
gdaysF_LG19_3$genotype <- mgr19
gdaysF_LG19_3

pg19 <- ggplot(gdaysF_LG19_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdaysF, LG19") + ylim(180,380) + xlim(0,max(gdaysF_LG19$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q19<- qplot(cM,neg_log.qval., data=gdaysF_LG19, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdaysF_LG19$cM)))
#multiplot(pg1, q1, cols=1)

layout <- matrix(c(1,2,2,3,4,4,5,6,6), nrow = 3)
multiplot(q17, pg17, q18, pg18, q19, pg19, layout=layout)
