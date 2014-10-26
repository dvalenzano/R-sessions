# Goal: to separate median survival by sex in LG 3 for cross G

library(lattice)
library(ggplot2)

gdays <- read.csv('/Volumes/group_dv/personal/DValenzano/Sep2014/qtl-direction-analysis/ReAutoReqtlresults/gdays2_tab.csv', sep=',', header=TRUE)
gdaysM <- read.csv('/Volumes/group_dv/personal/DValenzano/Sep2014/qtl-direction-analysis/ReAutoReqtlresults/gdaysM2_tab.csv', sep=',', header=TRUE)
gdaysF <- read.csv('/Volumes/group_dv/personal/DValenzano/Sep2014/qtl-direction-analysis/ReAutoReqtlresults/gdaysF2_tab.csv', sep=',', header=TRUE)
gdays_Res <- read.csv('/Volumes/group_dv/personal/DValenzano/Oct2014/gdaysRes2_tab.csv', sep=',', header=TRUE)

gdays_LG3 <- subset(gdays, gdays$LG == '3')
gdaysM_LG3 <- subset(gdaysM, gdaysM$LG == '3')
gdaysF_LG3 <- subset(gdaysF, gdaysF$LG == '3')
gdays_Res_LG3 <- subset(gdays_Res, gdays_Res$LG == '3')


###SL/SL####

gdays_LG3ss <- gdaysM_LG3[,1:3]
gdays_LG3ss_1 <- rbind(gdays_LG3ss, gdays_LG3ss)
lg3ss <- c(gdaysM_LG3$med0, gdaysF_LG3$med0)
genders <- c(rep('males', length(gdaysM_LG3$med0)), rep('females', length(gdaysF_LG3$med0)))
gdays_LG3ss_1$ss <- lg3ss
gdays_LG3ss_1$gen <- genders

pg3ss<- ggplot(gdays_LG3ss_1, aes(x=cM, y=ss, group=gen, color=gen)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG3-SS/SS, ") + ylim(180,380) + xlim(0,max(gdays_LG3_1$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")
pg3ss


###HETS####

gdays_LG3ls <- gdaysM_LG3[,1:3]
gdays_LG3ls_1 <- rbind(gdays_LG3ls, gdays_LG3ls)
lg3ls <- c(gdaysM_LG3$med1, gdaysF_LG3$med1)
genders <- c(rep('males', length(gdaysM_LG3$med1)), rep('females', length(gdaysF_LG3$med1)))
gdays_LG3ls_1$ls <- lg3ls
gdays_LG3ls_1$gen <- genders

pg3ls<- ggplot(gdays_LG3ls_1, aes(x=cM, y=ls, group=gen, color=gen)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG3-SL/LL, ") + ylim(180,380) + xlim(0,max(gdays_LG3_1$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")
pg3ls


###LL/LL####

gdays_LG3ll <- gdaysM_LG3[,1:3]
gdays_LG3ll_1 <- rbind(gdays_LG3ll, gdays_LG3ll)
lg3ll <- c(gdaysM_LG3$med2, gdaysF_LG3$med2)
genders <- c(rep('males', length(gdaysM_LG3$med0)), rep('females', length(gdaysF_LG3$med0)))
gdays_LG3ll_1$ll <- lg3ll
gdays_LG3ll_1$gen <- genders

pg3ll<- ggplot(gdays_LG3ll_1, aes(x=cM, y=ll, group=gen, color=gen)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG3-LL/LL, ") + ylim(180,380) + xlim(0,max(gdays_LG3_1$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")
pg3ll


##FINAL PLOT###

q3ss<- qplot(cM,neg_log.qval., data=gdays_LG3, xlab="cM", ylab="-log(q)", ylim=c(0,5), xlim=c(0, max(gdays_LG3$cM)))
q3sl<- qplot(cM,neg_log.qval., data=gdays_LG3, xlab="cM", ylab="-log(q)", ylim=c(0,5), xlim=c(0, max(gdays_LG3$cM)))
q3ll<- qplot(cM,neg_log.qval., data=gdays_LG3, xlab="cM", ylab="-log(q)", ylim=c(0,5), xlim=c(0, max(gdays_LG3$cM)))

layout <- matrix(c(1,1,2,3,3,4,5,5,6), nrow = 3)
multiplot(pg3ss, q3ss, pg3ls, q3sl, pg3ll, q3ll, layout=layout)
