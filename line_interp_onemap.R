#Goal: To plot phenotype by genotype in G cross, using the OneMap analysis, family by family. 

library(lattice)
library(ggplot2)

gdays7m <- read.csv('/Volumes/group_dv/personal/DValenzano/Oct2014/g7mdays2_tab.csv', sep=',', header=TRUE)
gdays14m <- read.csv('/Volumes/group_dv/personal/DValenzano/Oct2014/g14mdays2_tab.csv', sep=',', header=TRUE)

q7m<- qplot(cM,neg_log.pval., data=gdays7m, xlab="cM", ylab="-log(p)", ylim=c(0,10), xlim=c(0, max(gdays7m$cM)))

gdays7m_2 <- gdays7m[,1:3]
gdays7m_2
gdays7m_3 <- rbind(gdays7m_2, gdays7m_2, gdays7m_2)
med7m <- c(gdays7m$med0,gdays7m$med1, gdays7m$med2) 
mgr7m <- c(rep('SL/SL', length(gdays7m$med0)), rep('SL/LL', length(gdays7m$med1)), rep('LL/LL', length(gdays7m$med2)))
gdays7m_3$med <- med7m
gdays7m_3$genotype <- mgr7m
gdays7m_3

pg7m<- ggplot(gdays7m_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays fam 7m (OneMap)") + ylim(180,380) + xlim(0,max(gdays7m$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

gdays14m_2 <- gdays14m[,1:3]
gdays14m_2
gdays14m_3 <- rbind(gdays14m_2, gdays14m_2, gdays14m_2)
med14m <- c(gdays14m$med0,gdays14m$med1, gdays14m$med2) 
mgr14m <- c(rep('SL/SL', length(gdays14m$med0)), rep('SL/LL', length(gdays14m$med1)), rep('LL/LL', length(gdays14m$med2)))
gdays14m_3$med <- med14m
gdays14m_3$genotype <- mgr14m
gdays14m_3

pg14m<- ggplot(gdays14m_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays fam 14m (OneMap)") + ylim(180,380) + xlim(0,max(gdays14m$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q14m<- qplot(cM,neg_log.pval., data=gdays14m, xlab="cM", ylab="-log(p)", ylim=c(0,10), xlim=c(0, max(gdays14m$cM)))

layout <- matrix(c(1,2,2,3,4,4), nrow = 3)
multiplot(q7m, pg7m, q14m, pg14m, layout=layout)
