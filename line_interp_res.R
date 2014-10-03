# Goal: to generate trend curves that show the effect of survival QTL on the phenotype (survival)
library(lattice)
library(ggplot2)
gdays_Res <- read.csv('/Volumes/group_dv/personal/DValenzano/Oct2014/gdaysRes2_tab.csv', sep=',', header=TRUE)

gdays_Res_LG1 <- subset(gdays_Res, gdays_Res$LG == '1')
gdays_Res_LG2 <- subset(gdays_Res, gdays_Res$LG == '2')
gdays_Res_LG3 <- subset(gdays_Res, gdays_Res$LG == '3')
gdays_Res_LG4 <- subset(gdays_Res, gdays_Res$LG == '4')
gdays_Res_LG5 <- subset(gdays_Res, gdays_Res$LG == '5')
gdays_Res_LG6 <- subset(gdays_Res, gdays_Res$LG == '6')
gdays_Res_LG7 <- subset(gdays_Res, gdays_Res$LG == '7')
gdays_Res_LG8 <- subset(gdays_Res, gdays_Res$LG == '8')
gdays_Res_LG9 <- subset(gdays_Res, gdays_Res$LG == '9')
gdays_Res_LG10 <- subset(gdays_Res, gdays_Res$LG == '10')
gdays_Res_LG11 <- subset(gdays_Res, gdays_Res$LG == '11')
gdays_Res_LG12 <- subset(gdays_Res, gdays_Res$LG == '12')
gdays_Res_LG13 <- subset(gdays_Res, gdays_Res$LG == '13')
gdays_Res_LG14 <- subset(gdays_Res, gdays_Res$LG == '14')
gdays_Res_LG15 <- subset(gdays_Res, gdays_Res$LG == '15')
gdays_Res_LG16 <- subset(gdays_Res, gdays_Res$LG == '16')
gdays_Res_LG17 <- subset(gdays_Res, gdays_Res$LG == '17')
gdays_Res_LG18 <- subset(gdays_Res, gdays_Res$LG == '18')
gdays_Res_LG19 <- subset(gdays_Res, gdays_Res$LG == '19')


gdays_Res_LG1_2 <- gdays_Res_LG1[,1:3]
gdays_Res_LG1_2
gdays_Res_LG1_3 <- rbind(gdays_Res_LG1_2, gdays_Res_LG1_2, gdays_Res_LG1_2)
med1 <- c(gdays_Res_LG1$med0,gdays_Res_LG1$med1, gdays_Res_LG1$med2) 
mgr1 <- c(rep('SL/SL', length(gdays_Res_LG1$med0)), rep('SL/LL', length(gdays_Res_LG1$med1)), rep('LL/LL', length(gdays_Res_LG1$med2)))
gdays_Res_LG1_3$med <- med1
gdays_Res_LG1_3$genotype <- mgr1
gdays_Res_LG1_3

pg1 <- ggplot(gdays_Res_LG1_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG1") + ylim(180,380) + xlim(0,max(gdays_Res_LG1$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q1<- qplot(cM,neg_log.qval., data=gdays_Res_LG1, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG1$cM)))
#multiplot(pg1, q1, cols=1)

###LG2 for gdays_Res
gdays_Res_LG2_2 <- gdays_Res_LG2[,1:3]
gdays_Res_LG2_2
gdays_Res_LG2_3 <- rbind(gdays_Res_LG2_2, gdays_Res_LG2_2, gdays_Res_LG2_2)
med2 <- c(gdays_Res_LG2$med0,gdays_Res_LG2$med1, gdays_Res_LG2$med2) 
mgr2 <- c(rep('SL/SL', length(gdays_Res_LG2$med0)), rep('SL/LL', length(gdays_Res_LG2$med1)), rep('LL/LL', length(gdays_Res_LG2$med2)))
gdays_Res_LG2_3$med <- med2
gdays_Res_LG2_3$genotype <- mgr2
gdays_Res_LG2_3

pg2<- ggplot(gdays_Res_LG2_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG2") + ylim(180,380) + xlim(0,max(gdays_Res_LG2$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q2<- qplot(cM,neg_log.qval., data=gdays_Res_LG2, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG2$cM)))
#multiplot(pg2, q2, cols=1)

###LG3 for gdays_Res
gdays_Res_LG3_2 <- gdays_Res_LG3[,1:3]
gdays_Res_LG3_2
gdays_Res_LG3_3 <- rbind(gdays_Res_LG3_2, gdays_Res_LG3_2, gdays_Res_LG3_2)
med3 <- c(gdays_Res_LG3$med0,gdays_Res_LG3$med1, gdays_Res_LG3$med2) 
mgr3 <- c(rep('SL/SL', length(gdays_Res_LG3$med0)), rep('SL/LL', length(gdays_Res_LG3$med1)), rep('LL/LL', length(gdays_Res_LG3$med2)))
gdays_Res_LG3_3$med <- med3
gdays_Res_LG3_3$genotype <- mgr3
gdays_Res_LG3_3

pg3<- ggplot(gdays_Res_LG3_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG3") + ylim(180,380) + xlim(0,max(gdays_Res_LG3$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q3<- qplot(cM,neg_log.qval., data=gdays_Res_LG3, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG3$cM)))
#multiplot(pg3, q3, cols=1)

###LG4 for gdays_Res
gdays_Res_LG4_2 <- gdays_Res_LG4[,1:3]
gdays_Res_LG4_2
gdays_Res_LG4_3 <- rbind(gdays_Res_LG4_2, gdays_Res_LG4_2, gdays_Res_LG4_2)
med4 <- c(gdays_Res_LG4$med0,gdays_Res_LG4$med1, gdays_Res_LG4$med2) 
mgr4 <- c(rep('SL/SL', length(gdays_Res_LG4$med0)), rep('SL/LL', length(gdays_Res_LG4$med1)), rep('LL/LL', length(gdays_Res_LG4$med2)))
gdays_Res_LG4_3$med <- med4
gdays_Res_LG4_3$genotype <- mgr4
gdays_Res_LG4_3

pg4<- ggplot(gdays_Res_LG4_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG4") + ylim(180,380) + xlim(0,max(gdays_Res_LG4$cM)) + xlab("cM")+ylab("Days") +theme(legend.position="bottom")

q4<- qplot(cM,neg_log.qval., data=gdays_Res_LG4, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG4$cM)))
#multiplot(pg4, q4, cols=1)

#multiplot(pg1, pg2, pg3, pg4, cols=2)

#TO ARRANGE MULTIPLE PANELS SEE HERE:
# http://stackoverflow.com/questions/9490482/combined-plot-of-ggplot2-not-in-a-single-plot-using-par-or-layout-functio

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q1, pg1, q2, pg2, q3, pg3, q4, pg4, layout=layout)

###LG5 for gdays_Res

gdays_Res_LG5_2 <- gdays_Res_LG5[,1:3]
gdays_Res_LG5_2
gdays_Res_LG5_3 <- rbind(gdays_Res_LG5_2, gdays_Res_LG5_2, gdays_Res_LG5_2)
med5 <- c(gdays_Res_LG5$med0,gdays_Res_LG5$med1, gdays_Res_LG5$med2) 
mgr5 <- c(rep('SL/SL', length(gdays_Res_LG5$med0)), rep('SL/LL', length(gdays_Res_LG5$med1)), rep('LL/LL', length(gdays_Res_LG5$med2)))
gdays_Res_LG5_3$med <- med5
gdays_Res_LG5_3$genotype <- mgr5
gdays_Res_LG5_3

pg5 <- ggplot(gdays_Res_LG5_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG5") + ylim(180,380) + xlim(0,max(gdays_Res_LG5$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q5<- qplot(cM,neg_log.qval., data=gdays_Res_LG5, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG5$cM)))
#multiplot(pg1, q1, cols=1)

###LG6 for gdays_Res

gdays_Res_LG6_2 <- gdays_Res_LG6[,1:3]
gdays_Res_LG6_2
gdays_Res_LG6_3 <- rbind(gdays_Res_LG6_2, gdays_Res_LG6_2, gdays_Res_LG6_2)
med6 <- c(gdays_Res_LG6$med0,gdays_Res_LG6$med1, gdays_Res_LG6$med2) 
mgr6 <- c(rep('SL/SL', length(gdays_Res_LG6$med0)), rep('SL/LL', length(gdays_Res_LG6$med1)), rep('LL/LL', length(gdays_Res_LG6$med2)))
gdays_Res_LG6_3$med <- med6
gdays_Res_LG6_3$genotype <- mgr6
gdays_Res_LG6_3

pg6 <- ggplot(gdays_Res_LG6_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG6") + ylim(180,380) + xlim(0,max(gdays_Res_LG6$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q6<- qplot(cM,neg_log.qval., data=gdays_Res_LG6, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG6$cM)))
#multiplot(pg1, q1, cols=1)

###LG7 for gdays_Res
gdays_Res_LG7_2 <- gdays_Res_LG7[,1:3]
gdays_Res_LG7_2
gdays_Res_LG7_3 <- rbind(gdays_Res_LG7_2, gdays_Res_LG7_2, gdays_Res_LG7_2)
med7 <- c(gdays_Res_LG7$med0,gdays_Res_LG7$med1, gdays_Res_LG7$med2) 
mgr7 <- c(rep('SL/SL', length(gdays_Res_LG7$med0)), rep('SL/LL', length(gdays_Res_LG7$med1)), rep('LL/LL', length(gdays_Res_LG7$med2)))
gdays_Res_LG7_3$med <- med7
gdays_Res_LG7_3$genotype <- mgr7
gdays_Res_LG7_3

pg7 <- ggplot(gdays_Res_LG7_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG7") + ylim(180,380) + xlim(0,max(gdays_Res_LG7$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q7<- qplot(cM,neg_log.qval., data=gdays_Res_LG7, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG7$cM)))
#multiplot(pg1, q1, cols=1)

###LG8 for gdays_Res
gdays_Res_LG8_2 <- gdays_Res_LG8[,1:3]
gdays_Res_LG8_2
gdays_Res_LG8_3 <- rbind(gdays_Res_LG8_2, gdays_Res_LG8_2, gdays_Res_LG8_2)
med8 <- c(gdays_Res_LG8$med0,gdays_Res_LG8$med1, gdays_Res_LG8$med2) 
mgr8 <- c(rep('SL/SL', length(gdays_Res_LG8$med0)), rep('SL/LL', length(gdays_Res_LG8$med1)), rep('LL/LL', length(gdays_Res_LG8$med2)))
gdays_Res_LG8_3$med <- med8
gdays_Res_LG8_3$genotype <- mgr8
gdays_Res_LG8_3

pg8 <- ggplot(gdays_Res_LG8_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG8") + ylim(180,380) + xlim(0,max(gdays_Res_LG8$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q8<- qplot(cM,neg_log.qval., data=gdays_Res_LG8, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG8$cM)))
#multiplot(pg1, q1, cols=1)

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q5, pg5, q6, pg6, q7, pg7, q8, pg8, layout=layout)

###LG9 for gdays_Res
gdays_Res_LG9_2 <- gdays_Res_LG9[,1:3]
gdays_Res_LG9_2
gdays_Res_LG9_3 <- rbind(gdays_Res_LG9_2, gdays_Res_LG9_2, gdays_Res_LG9_2)
med9 <- c(gdays_Res_LG9$med0,gdays_Res_LG9$med1, gdays_Res_LG9$med2) 
mgr9 <- c(rep('SL/SL', length(gdays_Res_LG9$med0)), rep('SL/LL', length(gdays_Res_LG9$med1)), rep('LL/LL', length(gdays_Res_LG9$med2)))
gdays_Res_LG9_3$med <- med9
gdays_Res_LG9_3$genotype <- mgr9
gdays_Res_LG9_3

pg9 <- ggplot(gdays_Res_LG9_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG9") + ylim(180,380) + xlim(0,max(gdays_Res_LG9$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q9<- qplot(cM,neg_log.qval., data=gdays_Res_LG9, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG9$cM)))
#multiplot(pg1, q1, cols=1)

###LG10 for gdays_Res
gdays_Res_LG10_2 <- gdays_Res_LG10[,1:3]
gdays_Res_LG10_2
gdays_Res_LG10_3 <- rbind(gdays_Res_LG10_2, gdays_Res_LG10_2, gdays_Res_LG10_2)
med10 <- c(gdays_Res_LG10$med0,gdays_Res_LG10$med1, gdays_Res_LG10$med2) 
mgr10 <- c(rep('SL/SL', length(gdays_Res_LG10$med0)), rep('SL/LL', length(gdays_Res_LG10$med1)), rep('LL/LL', length(gdays_Res_LG10$med2)))
gdays_Res_LG10_3$med <- med10
gdays_Res_LG10_3$genotype <- mgr10
gdays_Res_LG10_3

pg10 <- ggplot(gdays_Res_LG10_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG10") + ylim(180,380) + xlim(0,max(gdays_Res_LG10$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q10<- qplot(cM,neg_log.qval., data=gdays_Res_LG10, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG10$cM)))
#multiplot(pg1, q1, cols=1)

###LG11 for gdays_Res
gdays_Res_LG11_2 <- gdays_Res_LG11[,1:3]
gdays_Res_LG11_2
gdays_Res_LG11_3 <- rbind(gdays_Res_LG11_2, gdays_Res_LG11_2, gdays_Res_LG11_2)
med11 <- c(gdays_Res_LG11$med0,gdays_Res_LG11$med1, gdays_Res_LG11$med2) 
mgr11 <- c(rep('SL/SL', length(gdays_Res_LG11$med0)), rep('SL/LL', length(gdays_Res_LG11$med1)), rep('LL/LL', length(gdays_Res_LG11$med2)))
gdays_Res_LG11_3$med <- med11
gdays_Res_LG11_3$genotype <- mgr11
gdays_Res_LG11_3

pg11 <- ggplot(gdays_Res_LG11_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG11") + ylim(180,380) + xlim(0,max(gdays_Res_LG11$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q11<- qplot(cM,neg_log.qval., data=gdays_Res_LG11, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG11$cM)))
#multiplot(pg1, q1, cols=1)

###LG12 for gdays_Res
gdays_Res_LG12_2 <- gdays_Res_LG12[,1:3]
gdays_Res_LG12_2
gdays_Res_LG12_3 <- rbind(gdays_Res_LG12_2, gdays_Res_LG12_2, gdays_Res_LG12_2)
med12 <- c(gdays_Res_LG12$med0,gdays_Res_LG12$med1, gdays_Res_LG12$med2) 
mgr12 <- c(rep('SL/SL', length(gdays_Res_LG12$med0)), rep('SL/LL', length(gdays_Res_LG12$med1)), rep('LL/LL', length(gdays_Res_LG12$med2)))
gdays_Res_LG12_3$med <- med12
gdays_Res_LG12_3$genotype <- mgr12
gdays_Res_LG12_3

pg12 <- ggplot(gdays_Res_LG12_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG12") + ylim(180,380) + xlim(0,max(gdays_Res_LG12$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q12<- qplot(cM,neg_log.qval., data=gdays_Res_LG12, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG12$cM)))
#multiplot(pg1, q1, cols=1)

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q9, pg9, q10, pg10, q11, pg11, q12, pg12, layout=layout)

###LG13 for gdays_Res
gdays_Res_LG13_2 <- gdays_Res_LG13[,1:3]
gdays_Res_LG13_2
gdays_Res_LG13_3 <- rbind(gdays_Res_LG13_2, gdays_Res_LG13_2, gdays_Res_LG13_2)
med13 <- c(gdays_Res_LG13$med0,gdays_Res_LG13$med1, gdays_Res_LG13$med2) 
mgr13 <- c(rep('SL/SL', length(gdays_Res_LG13$med0)), rep('SL/LL', length(gdays_Res_LG13$med1)), rep('LL/LL', length(gdays_Res_LG13$med2)))
gdays_Res_LG13_3$med <- med13
gdays_Res_LG13_3$genotype <- mgr13
gdays_Res_LG13_3

pg13 <- ggplot(gdays_Res_LG13_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG13") + ylim(180,380) + xlim(0,max(gdays_Res_LG13$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q13<- qplot(cM,neg_log.qval., data=gdays_Res_LG13, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG13$cM)))
#multiplot(pg1, q1, cols=1)

###LG14 for gdays_Res
gdays_Res_LG14_2 <- gdays_Res_LG14[,1:3]
gdays_Res_LG14_2
gdays_Res_LG14_3 <- rbind(gdays_Res_LG14_2, gdays_Res_LG14_2, gdays_Res_LG14_2)
med14 <- c(gdays_Res_LG14$med0,gdays_Res_LG14$med1, gdays_Res_LG14$med2) 
mgr14 <- c(rep('SL/SL', length(gdays_Res_LG14$med0)), rep('SL/LL', length(gdays_Res_LG14$med1)), rep('LL/LL', length(gdays_Res_LG14$med2)))
gdays_Res_LG14_3$med <- med14
gdays_Res_LG14_3$genotype <- mgr14
gdays_Res_LG14_3

pg14 <- ggplot(gdays_Res_LG14_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG14") + ylim(180,380) + xlim(0,max(gdays_Res_LG14$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q14<- qplot(cM,neg_log.qval., data=gdays_Res_LG14, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG14$cM)))
#multiplot(pg1, q1, cols=1)

###LG15 for gdays_Res
gdays_Res_LG15_2 <- gdays_Res_LG15[,1:3]
gdays_Res_LG15_2
gdays_Res_LG15_3 <- rbind(gdays_Res_LG15_2, gdays_Res_LG15_2, gdays_Res_LG15_2)
med15 <- c(gdays_Res_LG15$med0,gdays_Res_LG15$med1, gdays_Res_LG15$med2) 
mgr15 <- c(rep('SL/SL', length(gdays_Res_LG15$med0)), rep('SL/LL', length(gdays_Res_LG15$med1)), rep('LL/LL', length(gdays_Res_LG15$med2)))
gdays_Res_LG15_3$med <- med15
gdays_Res_LG15_3$genotype <- mgr15
gdays_Res_LG15_3

pg15 <- ggplot(gdays_Res_LG15_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG15") + ylim(180,380) + xlim(0,max(gdays_Res_LG15$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q15<- qplot(cM,neg_log.qval., data=gdays_Res_LG15, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG15$cM)))
#multiplot(pg1, q1, cols=1)

###LG16 for gdays_Res
gdays_Res_LG16_2 <- gdays_Res_LG16[,1:3]
gdays_Res_LG16_2
gdays_Res_LG16_3 <- rbind(gdays_Res_LG16_2, gdays_Res_LG16_2, gdays_Res_LG16_2)
med16 <- c(gdays_Res_LG16$med0,gdays_Res_LG16$med1, gdays_Res_LG16$med2) 
mgr16 <- c(rep('SL/SL', length(gdays_Res_LG16$med0)), rep('SL/LL', length(gdays_Res_LG16$med1)), rep('LL/LL', length(gdays_Res_LG16$med2)))
gdays_Res_LG16_3$med <- med16
gdays_Res_LG16_3$genotype <- mgr16
gdays_Res_LG16_3

pg16 <- ggplot(gdays_Res_LG16_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG16") + ylim(180,380) + xlim(0,max(gdays_Res_LG16$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q16<- qplot(cM,neg_log.qval., data=gdays_Res_LG16, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG16$cM)))
#multiplot(pg1, q1, cols=1)

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q13, pg13, q14, pg14, q15, pg15, q16, pg16, layout=layout)

###LG17 for gdays_Res
gdays_Res_LG17_2 <- gdays_Res_LG17[,1:3]
gdays_Res_LG17_2
gdays_Res_LG17_3 <- rbind(gdays_Res_LG17_2, gdays_Res_LG17_2, gdays_Res_LG17_2)
med17 <- c(gdays_Res_LG17$med0,gdays_Res_LG17$med1, gdays_Res_LG17$med2) 
mgr17 <- c(rep('SL/SL', length(gdays_Res_LG17$med0)), rep('SL/LL', length(gdays_Res_LG17$med1)), rep('LL/LL', length(gdays_Res_LG17$med2)))
gdays_Res_LG17_3$med <- med17
gdays_Res_LG17_3$genotype <- mgr17
gdays_Res_LG17_3

pg17 <- ggplot(gdays_Res_LG17_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG17") + ylim(180,380) + xlim(0,max(gdays_Res_LG17$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q17<- qplot(cM,neg_log.qval., data=gdays_Res_LG17, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG17$cM)))
#multiplot(pg1, q1, cols=1)

###LG18 for gdays_Res
gdays_Res_LG18_2 <- gdays_Res_LG18[,1:3]
gdays_Res_LG18_2
gdays_Res_LG18_3 <- rbind(gdays_Res_LG18_2, gdays_Res_LG18_2, gdays_Res_LG18_2)
med18 <- c(gdays_Res_LG18$med0,gdays_Res_LG18$med1, gdays_Res_LG18$med2) 
mgr18 <- c(rep('SL/SL', length(gdays_Res_LG18$med0)), rep('SL/LL', length(gdays_Res_LG18$med1)), rep('LL/LL', length(gdays_Res_LG18$med2)))
gdays_Res_LG18_3$med <- med18
gdays_Res_LG18_3$genotype <- mgr18
gdays_Res_LG18_3

pg18 <- ggplot(gdays_Res_LG18_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG18") + ylim(180,380) + xlim(0,max(gdays_Res_LG18$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q18<- qplot(cM,neg_log.qval., data=gdays_Res_LG18, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG18$cM)))
#multiplot(pg1, q1, cols=1)

###LG19 for gdays_Res
gdays_Res_LG19_2 <- gdays_Res_LG19[,1:3]
gdays_Res_LG19_2
gdays_Res_LG19_3 <- rbind(gdays_Res_LG19_2, gdays_Res_LG19_2, gdays_Res_LG19_2)
med19 <- c(gdays_Res_LG19$med0,gdays_Res_LG19$med1, gdays_Res_LG19$med2) 
mgr19 <- c(rep('SL/SL', length(gdays_Res_LG19$med0)), rep('SL/LL', length(gdays_Res_LG19$med1)), rep('LL/LL', length(gdays_Res_LG19$med2)))
gdays_Res_LG19_3$med <- med19
gdays_Res_LG19_3$genotype <- mgr19
gdays_Res_LG19_3

pg19 <- ggplot(gdays_Res_LG19_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG19") + ylim(180,380) + xlim(0,max(gdays_Res_LG19$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q19<- qplot(cM,neg_log.qval., data=gdays_Res_LG19, xlab="cM", ylab="-log(q)", ylim=c(0,3), xlim=c(0, max(gdays_Res_LG19$cM)))
#multiplot(pg1, q1, cols=1)

layout <- matrix(c(1,2,2,3,4,4,5,6,6), nrow = 3)
multiplot(q17, pg17, q18, pg18, q19, pg19, layout=layout)
