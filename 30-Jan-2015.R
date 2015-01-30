c1p <- read.csv(file="/Volumes/group_dv/personal/DValenzano/Jan2015/cross1_pvals.csv", header=T, sep=',')
c2p <- read.csv(file="/Volumes/group_dv/personal/DValenzano/Jan2015/cross2_pvals.csv", header=T, sep=',')

c1p2 <- subset(c1p, select = -c(1))
c1p3 <- data.matrix(c1p2, rownames.force=NA)

c1p3[1:10,]

ecdf.c1pval<-ecdf(c1p3)

c1.qval<-apply(c1p3,2,function(x){
  x/ecdf.c1pval(x)
})

c1.qval[c1.qval>1]<-1
c1.qval2 <- data.frame(c1.qval)
c1.qval2$marker <- c1p$marker
c1.qval2[1:10,]

write.table(c1.qval2, file="/Volumes/group_dv/personal/DValenzano/Jan2015/c1-qvals.csv")
