#########
#c1p <- read.csv(file="/Volumes/group_dv/personal/DValenzano/month-by-month/Jan2015/cross1_pvals.csv", header=T, sep=',')
#c1p2 <- subset(c1p, select = -c(1))
#c1p3 <- data.matrix(c1p2, rownames.force=NA)
#c1p3[1:10,]
#ecdf.c1pval<-ecdf(c1p3)
#c1.qval<-apply(c1p3,2,function(x){
#  x/ecdf.c1pval(x)
#})
#c1.qval[c1.qval>1]<-1
#c1.qval2 <- data.frame(c1.qval)
#c1.qval2$marker <- c1p$marker
#c1.qval2[1:10,]
#write.table(c1.qval2, file="/Volumes/group_dv/personal/DValenzano/Jan2015/c1-qvals.csv")
#gdays <- read.csv('/Volumes/group_dv/personal/DValenzano/Sep2014/qtl-direction-analysis/ReAutoReqtlresults/gdays2_tab.csv', sep=',', header=TRUE)
#c1logq <- gdays[,1:4]
#write.table(c1logq, file="/Volumes/group_dv/personal/DValenzano/Jan2015/c1-logqvals.csv")
#########

cr1p <- read.csv(file="/Volumes/group_dv/personal/DValenzano/month-by-month/May2015/go_p_sorted.csv", header=T, sep=',')

cr1p2 <- subset(cr1p, select = -c(1:3))
cr1p3 <- data.matrix(cr1p2, rownames.force=NA)
cr1p3_sex <- cr1p3[,1]
cr1p3_surv <- cr1p3[,2]

#ecdf.cr1pval<-ecdf(cr1p3)
#cr1.qval<-apply(cr1p3,2,function(x){
#  x/ecdf.cr1pval(x)
#})

ecdf.cr1pval_sex<-ecdf(cr1p3_sex)
ecdf.cr1pval_surv<-ecdf(cr1p3_surv)

cr1_sex.qval<-lapply(cr1p3_sex,function(x){
  x/ecdf.cr1pval_sex(x)
})

cr1_surv.qval<-lapply(cr1p3_surv,function(x){
  x/ecdf.cr1pval_surv(x)
})

cr1_sex.qval[cr1_sex.qval>1]<-1
cr1_sex.qval2 <- data.frame(cr1_sex.qval)
cr1_sex.qval2$marker <- cr1p$marker
#cr1_sex.qval2$LG <- cr1p$LG
#cr1_sex.qval2$cM <- cr1p$cM

cr1_sex.qval2[1:10,]

write.table(cr1_sex.qval2, file="/Volumes/group_dv/personal/DValenzano/month-by-month/May2015/cr1_sex-qvals.csv")

