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

cr1_sex.qval<-lapply(cr1p3_sex,function(x){
  x/ecdf.cr1pval_sex(x)
})

cr1_sex.qval[cr1_sex.qval>1]<-1
cr1_sex.qval2 <- data.frame(cr1_sex.qval)

cr1_sex.qval2t <- t(cr1_sex.qval2)
qvalsex_gw <- cr1_sex.qval2t[,1]
min_logqvalsex_gw <- -log(qvalsex_gw)
df_sex <- data.frame(cr1p$marker, cr1p$LG, cr1p$cM, qvalsex_gw, min_logqvalsex_gw)

write.table(df_sex, file="/Volumes/group_dv/personal/DValenzano/month-by-month/May2015/cr1_sex-qvals.csv")

##### SAME ANALYSIS AS ABOVE, APPLIED TO SURVIVAL #####

ecdf.cr1pval_surv<-ecdf(cr1p3_surv)

cr1_surv.qval<-lapply(cr1p3_surv,function(x){
  x/ecdf.cr1pval_surv(x)
})

cr1_surv.qval[cr1_surv.qval>1]<-1
cr1_surv.qval2 <- data.frame(cr1_surv.qval)

cr1_surv.qval2t <- t(cr1_surv.qval2)
qvalsurv_gw <- cr1_surv.qval2t[,1]
min_logqvalsurv_gw <- -log(qvalsurv_gw)
df_surv <- data.frame(cr1p$marker, cr1p$LG, cr1p$cM, qvalsurv_gw, min_logqvalsurv_gw)

write.table(df_surv, file="/Volumes/group_dv/personal/DValenzano/month-by-month/May2015/cr1_surv-qvals.csv")


# SAME ANALYSIS AS ABOVE, LG by LG #

cr1p_lg1 <- subset(cr1p, LG=='1')
cr1p_lg2 <- subset(cr1p, LG=='2')
cr1p_lg3 <- subset(cr1p, LG=='3')
cr1p_lg4 <- subset(cr1p, LG=='4')
cr1p_lg5 <- subset(cr1p, LG=='5')
cr1p_lg6 <- subset(cr1p, LG=='6')
cr1p_lg7 <- subset(cr1p, LG=='7')
cr1p_lg8 <- subset(cr1p, LG=='8')
cr1p_lg9 <- subset(cr1p, LG=='9')
cr1p_lg10 <- subset(cr1p, LG=='10')
cr1p_lg11 <- subset(cr1p, LG=='11')
cr1p_lg12 <- subset(cr1p, LG=='12')
cr1p_lg13 <- subset(cr1p, LG=='13')
cr1p_lg14 <- subset(cr1p, LG=='14')
cr1p_lg15 <- subset(cr1p, LG=='15')
cr1p_lg16 <- subset(cr1p, LG=='16')
cr1p_lg17 <- subset(cr1p, LG=='17')
cr1p_lg18 <- subset(cr1p, LG=='18')
cr1p_lg19 <- subset(cr1p, LG=='19')

## LG1

cr1p2_lg1 <- subset(cr1p_lg1, select = -c(1:3))
cr1p3_lg1 <- data.matrix(cr1p2_lg1, rownames.force=NA)
cr1p3_lg1_sex <- cr1p3_lg1[,1]
cr1p3_lg1_surv <- cr1p3_lg1[,2]

#corrected qval for sex
ecdf.cr1pval_lg1_sex<-ecdf(cr1p3_lg1_sex)

cr1_lg1_sex.qval<-lapply(cr1p3_lg1_sex,function(x){
  x/ecdf.cr1pval_lg1_sex(x)
})

cr1_lg1_sex.qval[cr1_lg1_sex.qval>1]<-1
cr1_lg1_sex.qval2 <- data.frame(cr1_lg1_sex.qval)

cr1_lg1_sex.qval2t <- t(cr1_lg1_sex.qval2)
qvalsex_lg1 <- cr1_lg1_sex.qval2t[,1]
min_logqvalsex_lg1 <- -log(qvalsex_lg1)

#corrected qval for surv
ecdf.cr1pval_lg1_surv<-ecdf(cr1p3_lg1_surv)

cr1_lg1_surv.qval<-lapply(cr1p3_lg1_surv,function(x){
  x/ecdf.cr1pval_lg1_surv(x)
})

cr1_lg1_surv.qval[cr1_lg1_surv.qval>1]<-1
cr1_lg1_surv.qval2 <- data.frame(cr1_lg1_surv.qval)

cr1_lg1_surv.qval2t <- t(cr1_lg1_surv.qval2)
qvalsurv_lg1 <- cr1_lg1_surv.qval2t[,1]
min_logqvalsurv_lg1 <- -log(qvalsurv_lg1)

## LG2

cr1p2_lg2 <- subset(cr1p_lg2, select = -c(1:3))
cr1p3_lg2 <- data.matrix(cr1p2_lg2, rownames.force=NA)
cr1p3_lg2_sex <- cr1p3_lg2[,1]
cr1p3_lg2_surv <- cr1p3_lg2[,2]

#corrected qval for sex
ecdf.cr1pval_lg2_sex<-ecdf(cr1p3_lg2_sex)

cr1_lg2_sex.qval<-lapply(cr1p3_lg2_sex,function(x){
  x/ecdf.cr1pval_lg2_sex(x)
})

cr1_lg2_sex.qval[cr1_lg2_sex.qval>1]<-1
cr1_lg2_sex.qval2 <- data.frame(cr1_lg2_sex.qval)

cr1_lg2_sex.qval2t <- t(cr1_lg2_sex.qval2)
qvalsex_lg2 <- cr1_lg2_sex.qval2t[,1]
min_logqvalsex_lg2 <- -log(qvalsex_lg2)

#corrected qval for surv
ecdf.cr1pval_lg2_surv<-ecdf(cr1p3_lg2_surv)

cr1_lg2_surv.qval<-lapply(cr1p3_lg2_surv,function(x){
  x/ecdf.cr1pval_lg2_surv(x)
})

cr1_lg2_surv.qval[cr1_lg2_surv.qval>1]<-1
cr1_lg2_surv.qval2 <- data.frame(cr1_lg2_surv.qval)

cr1_lg2_surv.qval2t <- t(cr1_lg2_surv.qval2)
qvalsurv_lg2 <- cr1_lg2_surv.qval2t[,1]
min_logqvalsurv_lg2 <- -log(qvalsurv_lg2)

## LG3 
cr1p2_lg3 <- subset(cr1p_lg3, select = -c(1:3))
cr1p3_lg3 <- data.matrix(cr1p2_lg3, rownames.force=NA)
cr1p3_lg3_sex <- cr1p3_lg3[,1]
cr1p3_lg3_surv <- cr1p3_lg3[,2]

#corrected qval for sex
ecdf.cr1pval_lg3_sex<-ecdf(cr1p3_lg3_sex)

cr1_lg3_sex.qval<-lapply(cr1p3_lg3_sex,function(x){
  x/ecdf.cr1pval_lg3_sex(x)
})

cr1_lg3_sex.qval[cr1_lg3_sex.qval>1]<-1
cr1_lg3_sex.qval2 <- data.frame(cr1_lg3_sex.qval)

cr1_lg3_sex.qval2t <- t(cr1_lg3_sex.qval2)
qvalsex_lg3 <- cr1_lg3_sex.qval2t[,1]
min_logqvalsex_lg3 <- -log(qvalsex_lg3)

#corrected qval for surv
ecdf.cr1pval_lg3_surv<-ecdf(cr1p3_lg3_surv)

cr1_lg3_surv.qval<-lapply(cr1p3_lg3_surv,function(x){
  x/ecdf.cr1pval_lg3_surv(x)
})

cr1_lg3_surv.qval[cr1_lg3_surv.qval>1]<-1
cr1_lg3_surv.qval2 <- data.frame(cr1_lg3_surv.qval)

cr1_lg3_surv.qval2t <- t(cr1_lg3_surv.qval2)
qvalsurv_lg3 <- cr1_lg3_surv.qval2t[,1]
min_logqvalsurv_lg3 <- -log(qvalsurv_lg3)

## LG4

cr1p2_lg4 <- subset(cr1p_lg4, select = -c(1:3))
cr1p3_lg4 <- data.matrix(cr1p2_lg4, rownames.force=NA)
cr1p3_lg4_sex <- cr1p3_lg4[,1]
cr1p3_lg4_surv <- cr1p3_lg4[,2]

#corrected qval for sex
ecdf.cr1pval_lg4_sex<-ecdf(cr1p3_lg4_sex)

cr1_lg4_sex.qval<-lapply(cr1p3_lg4_sex,function(x){
  x/ecdf.cr1pval_lg4_sex(x)
})

cr1_lg4_sex.qval[cr1_lg4_sex.qval>1]<-1
cr1_lg4_sex.qval2 <- data.frame(cr1_lg4_sex.qval)

cr1_lg4_sex.qval2t <- t(cr1_lg4_sex.qval2)
qvalsex_lg4 <- cr1_lg4_sex.qval2t[,1]
min_logqvalsex_lg4 <- -log(qvalsex_lg4)

#corrected qval for surv
ecdf.cr1pval_lg4_surv<-ecdf(cr1p3_lg4_surv)

cr1_lg4_surv.qval<-lapply(cr1p3_lg4_surv,function(x){
  x/ecdf.cr1pval_lg4_surv(x)
})

cr1_lg4_surv.qval[cr1_lg4_surv.qval>1]<-1
cr1_lg4_surv.qval2 <- data.frame(cr1_lg4_surv.qval)

cr1_lg4_surv.qval2t <- t(cr1_lg4_surv.qval2)
qvalsurv_lg4 <- cr1_lg4_surv.qval2t[,1]
min_logqvalsurv_lg4 <- -log(qvalsurv_lg4)

## LG5

cr1p2_lg5 <- subset(cr1p_lg5, select = -c(1:3))
cr1p3_lg5 <- data.matrix(cr1p2_lg5, rownames.force=NA)
cr1p3_lg5_sex <- cr1p3_lg5[,1]
cr1p3_lg5_surv <- cr1p3_lg5[,2]

#corrected qval for sex
ecdf.cr1pval_lg5_sex<-ecdf(cr1p3_lg5_sex)

cr1_lg5_sex.qval<-lapply(cr1p3_lg5_sex,function(x){
  x/ecdf.cr1pval_lg5_sex(x)
})

cr1_lg5_sex.qval[cr1_lg5_sex.qval>1]<-1
cr1_lg5_sex.qval2 <- data.frame(cr1_lg5_sex.qval)

cr1_lg5_sex.qval2t <- t(cr1_lg5_sex.qval2)
qvalsex_lg5 <- cr1_lg5_sex.qval2t[,1]
min_logqvalsex_lg5 <- -log(qvalsex_lg5)

#corrected qval for surv
ecdf.cr1pval_lg5_surv<-ecdf(cr1p3_lg5_surv)

cr1_lg5_surv.qval<-lapply(cr1p3_lg5_surv,function(x){
  x/ecdf.cr1pval_lg5_surv(x)
})

cr1_lg5_surv.qval[cr1_lg5_surv.qval>1]<-1
cr1_lg5_surv.qval2 <- data.frame(cr1_lg5_surv.qval)

cr1_lg5_surv.qval2t <- t(cr1_lg5_surv.qval2)
qvalsurv_lg5 <- cr1_lg5_surv.qval2t[,1]
min_logqvalsurv_lg5 <- -log(qvalsurv_lg5)

## LG6

cr1p2_lg6 <- subset(cr1p_lg6, select = -c(1:3))
cr1p3_lg6 <- data.matrix(cr1p2_lg6, rownames.force=NA)
cr1p3_lg6_sex <- cr1p3_lg6[,1]
cr1p3_lg6_surv <- cr1p3_lg6[,2]

#corrected qval for sex
ecdf.cr1pval_lg6_sex<-ecdf(cr1p3_lg6_sex)

cr1_lg6_sex.qval<-lapply(cr1p3_lg6_sex,function(x){
  x/ecdf.cr1pval_lg6_sex(x)
})

cr1_lg6_sex.qval[cr1_lg6_sex.qval>1]<-1
cr1_lg6_sex.qval2 <- data.frame(cr1_lg6_sex.qval)

cr1_lg6_sex.qval2t <- t(cr1_lg6_sex.qval2)
qvalsex_lg6 <- cr1_lg6_sex.qval2t[,1]
min_logqvalsex_lg6 <- -log(qvalsex_lg6)

#corrected qval for surv
ecdf.cr1pval_lg6_surv<-ecdf(cr1p3_lg6_surv)

cr1_lg6_surv.qval<-lapply(cr1p3_lg6_surv,function(x){
  x/ecdf.cr1pval_lg6_surv(x)
})

cr1_lg6_surv.qval[cr1_lg6_surv.qval>1]<-1
cr1_lg6_surv.qval2 <- data.frame(cr1_lg6_surv.qval)

cr1_lg6_surv.qval2t <- t(cr1_lg6_surv.qval2)
qvalsurv_lg6 <- cr1_lg6_surv.qval2t[,1]
min_logqvalsurv_lg6 <- -log(qvalsurv_lg6)

## LG7

cr1p2_lg7 <- subset(cr1p_lg7, select = -c(1:3))
cr1p3_lg7 <- data.matrix(cr1p2_lg7, rownames.force=NA)
cr1p3_lg7_sex <- cr1p3_lg7[,1]
cr1p3_lg7_surv <- cr1p3_lg7[,2]

#corrected qval for sex
ecdf.cr1pval_lg7_sex<-ecdf(cr1p3_lg7_sex)

cr1_lg7_sex.qval<-lapply(cr1p3_lg7_sex,function(x){
  x/ecdf.cr1pval_lg7_sex(x)
})

cr1_lg7_sex.qval[cr1_lg7_sex.qval>1]<-1
cr1_lg7_sex.qval2 <- data.frame(cr1_lg7_sex.qval)

cr1_lg7_sex.qval2t <- t(cr1_lg7_sex.qval2)
qvalsex_lg7 <- cr1_lg7_sex.qval2t[,1]
min_logqvalsex_lg7 <- -log(qvalsex_lg7)

#corrected qval for surv
ecdf.cr1pval_lg7_surv<-ecdf(cr1p3_lg7_surv)

cr1_lg7_surv.qval<-lapply(cr1p3_lg7_surv,function(x){
  x/ecdf.cr1pval_lg7_surv(x)
})

cr1_lg7_surv.qval[cr1_lg7_surv.qval>1]<-1
cr1_lg7_surv.qval2 <- data.frame(cr1_lg7_surv.qval)

cr1_lg7_surv.qval2t <- t(cr1_lg7_surv.qval2)
qvalsurv_lg7 <- cr1_lg7_surv.qval2t[,1]
min_logqvalsurv_lg7 <- -log(qvalsurv_lg7)

## LG8

cr1p2_lg8 <- subset(cr1p_lg8, select = -c(1:3))
cr1p3_lg8 <- data.matrix(cr1p2_lg8, rownames.force=NA)
cr1p3_lg8_sex <- cr1p3_lg8[,1]
cr1p3_lg8_surv <- cr1p3_lg8[,2]

#corrected qval for sex
ecdf.cr1pval_lg8_sex<-ecdf(cr1p3_lg8_sex)

cr1_lg8_sex.qval<-lapply(cr1p3_lg8_sex,function(x){
  x/ecdf.cr1pval_lg8_sex(x)
})

cr1_lg8_sex.qval[cr1_lg8_sex.qval>1]<-1
cr1_lg8_sex.qval2 <- data.frame(cr1_lg8_sex.qval)

cr1_lg8_sex.qval2t <- t(cr1_lg8_sex.qval2)
qvalsex_lg8 <- cr1_lg8_sex.qval2t[,1]
min_logqvalsex_lg8 <- -log(qvalsex_lg8)

#corrected qval for surv
ecdf.cr1pval_lg8_surv<-ecdf(cr1p3_lg8_surv)

cr1_lg8_surv.qval<-lapply(cr1p3_lg8_surv,function(x){
  x/ecdf.cr1pval_lg8_surv(x)
})

cr1_lg8_surv.qval[cr1_lg8_surv.qval>1]<-1
cr1_lg8_surv.qval2 <- data.frame(cr1_lg8_surv.qval)

cr1_lg8_surv.qval2t <- t(cr1_lg8_surv.qval2)
qvalsurv_lg8 <- cr1_lg8_surv.qval2t[,1]
min_logqvalsurv_lg8 <- -log(qvalsurv_lg8)

## LG9

cr1p2_lg9 <- subset(cr1p_lg9, select = -c(1:3))
cr1p3_lg9 <- data.matrix(cr1p2_lg9, rownames.force=NA)
cr1p3_lg9_sex <- cr1p3_lg9[,1]
cr1p3_lg9_surv <- cr1p3_lg9[,2]

#corrected qval for sex
ecdf.cr1pval_lg9_sex<-ecdf(cr1p3_lg9_sex)

cr1_lg9_sex.qval<-lapply(cr1p3_lg9_sex,function(x){
  x/ecdf.cr1pval_lg9_sex(x)
})

cr1_lg9_sex.qval[cr1_lg9_sex.qval>1]<-1
cr1_lg9_sex.qval2 <- data.frame(cr1_lg9_sex.qval)

cr1_lg9_sex.qval2t <- t(cr1_lg9_sex.qval2)
qvalsex_lg9 <- cr1_lg9_sex.qval2t[,1]
min_logqvalsex_lg9 <- -log(qvalsex_lg9)

#corrected qval for surv
ecdf.cr1pval_lg9_surv<-ecdf(cr1p3_lg9_surv)

cr1_lg9_surv.qval<-lapply(cr1p3_lg9_surv,function(x){
  x/ecdf.cr1pval_lg9_surv(x)
})

cr1_lg9_surv.qval[cr1_lg9_surv.qval>1]<-1
cr1_lg9_surv.qval2 <- data.frame(cr1_lg9_surv.qval)

cr1_lg9_surv.qval2t <- t(cr1_lg9_surv.qval2)
qvalsurv_lg9 <- cr1_lg9_surv.qval2t[,1]
min_logqvalsurv_lg9 <- -log(qvalsurv_lg9)

## LG10

cr1p2_lg10 <- subset(cr1p_lg10, select = -c(1:3))
cr1p3_lg10 <- data.matrix(cr1p2_lg10, rownames.force=NA)
cr1p3_lg10_sex <- cr1p3_lg10[,1]
cr1p3_lg10_surv <- cr1p3_lg10[,2]

#corrected qval for sex
ecdf.cr1pval_lg10_sex<-ecdf(cr1p3_lg10_sex)

cr1_lg10_sex.qval<-lapply(cr1p3_lg10_sex,function(x){
  x/ecdf.cr1pval_lg10_sex(x)
})

cr1_lg10_sex.qval[cr1_lg10_sex.qval>1]<-1
cr1_lg10_sex.qval2 <- data.frame(cr1_lg10_sex.qval)

cr1_lg10_sex.qval2t <- t(cr1_lg10_sex.qval2)
qvalsex_lg10 <- cr1_lg10_sex.qval2t[,1]
min_logqvalsex_lg10 <- -log(qvalsex_lg10)

#corrected qval for surv
ecdf.cr1pval_lg10_surv<-ecdf(cr1p3_lg10_surv)

cr1_lg10_surv.qval<-lapply(cr1p3_lg10_surv,function(x){
  x/ecdf.cr1pval_lg10_surv(x)
})

cr1_lg10_surv.qval[cr1_lg10_surv.qval>1]<-1
cr1_lg10_surv.qval2 <- data.frame(cr1_lg10_surv.qval)

cr1_lg10_surv.qval2t <- t(cr1_lg10_surv.qval2)
qvalsurv_lg10 <- cr1_lg10_surv.qval2t[,1]
min_logqvalsurv_lg10 <- -log(qvalsurv_lg10)

## LG11

cr1p2_lg11 <- subset(cr1p_lg11, select = -c(1:3))
cr1p3_lg11 <- data.matrix(cr1p2_lg11, rownames.force=NA)
cr1p3_lg11_sex <- cr1p3_lg11[,1]
cr1p3_lg11_surv <- cr1p3_lg11[,2]

#corrected qval for sex
ecdf.cr1pval_lg11_sex<-ecdf(cr1p3_lg11_sex)

cr1_lg11_sex.qval<-lapply(cr1p3_lg11_sex,function(x){
  x/ecdf.cr1pval_lg11_sex(x)
})

cr1_lg11_sex.qval[cr1_lg11_sex.qval>1]<-1
cr1_lg11_sex.qval2 <- data.frame(cr1_lg11_sex.qval)

cr1_lg11_sex.qval2t <- t(cr1_lg11_sex.qval2)
qvalsex_lg11 <- cr1_lg11_sex.qval2t[,1]
min_logqvalsex_lg11 <- -log(qvalsex_lg11)

#corrected qval for surv
ecdf.cr1pval_lg11_surv<-ecdf(cr1p3_lg11_surv)

cr1_lg11_surv.qval<-lapply(cr1p3_lg11_surv,function(x){
  x/ecdf.cr1pval_lg11_surv(x)
})

cr1_lg11_surv.qval[cr1_lg11_surv.qval>1]<-1
cr1_lg11_surv.qval2 <- data.frame(cr1_lg11_surv.qval)

cr1_lg11_surv.qval2t <- t(cr1_lg11_surv.qval2)
qvalsurv_lg11 <- cr1_lg11_surv.qval2t[,1]
min_logqvalsurv_lg11 <- -log(qvalsurv_lg11)

## LG 12

cr1p2_lg12 <- subset(cr1p_lg12, select = -c(1:3))
cr1p3_lg12 <- data.matrix(cr1p2_lg12, rownames.force=NA)
cr1p3_lg12_sex <- cr1p3_lg12[,1]
cr1p3_lg12_surv <- cr1p3_lg12[,2]

#corrected qval for sex
ecdf.cr1pval_lg12_sex<-ecdf(cr1p3_lg12_sex)

cr1_lg12_sex.qval<-lapply(cr1p3_lg12_sex,function(x){
  x/ecdf.cr1pval_lg12_sex(x)
})

cr1_lg12_sex.qval[cr1_lg12_sex.qval>1]<-1
cr1_lg12_sex.qval2 <- data.frame(cr1_lg12_sex.qval)

cr1_lg12_sex.qval2t <- t(cr1_lg12_sex.qval2)
qvalsex_lg12 <- cr1_lg12_sex.qval2t[,1]
min_logqvalsex_lg12 <- -log(qvalsex_lg12)

#corrected qval for surv
ecdf.cr1pval_lg12_surv<-ecdf(cr1p3_lg12_surv)

cr1_lg12_surv.qval<-lapply(cr1p3_lg12_surv,function(x){
  x/ecdf.cr1pval_lg12_surv(x)
})

cr1_lg12_surv.qval[cr1_lg12_surv.qval>1]<-1
cr1_lg12_surv.qval2 <- data.frame(cr1_lg12_surv.qval)

cr1_lg12_surv.qval2t <- t(cr1_lg12_surv.qval2)
qvalsurv_lg12 <- cr1_lg12_surv.qval2t[,1]
min_logqvalsurv_lg12 <- -log(qvalsurv_lg12)

## LG13

cr1p2_lg13 <- subset(cr1p_lg13, select = -c(1:3))
cr1p3_lg13 <- data.matrix(cr1p2_lg13, rownames.force=NA)
cr1p3_lg13_sex <- cr1p3_lg13[,1]
cr1p3_lg13_surv <- cr1p3_lg13[,2]

#corrected qval for sex
ecdf.cr1pval_lg13_sex<-ecdf(cr1p3_lg13_sex)

cr1_lg13_sex.qval<-lapply(cr1p3_lg13_sex,function(x){
  x/ecdf.cr1pval_lg13_sex(x)
})

cr1_lg13_sex.qval[cr1_lg13_sex.qval>1]<-1
cr1_lg13_sex.qval2 <- data.frame(cr1_lg13_sex.qval)

cr1_lg13_sex.qval2t <- t(cr1_lg13_sex.qval2)
qvalsex_lg13 <- cr1_lg13_sex.qval2t[,1]
min_logqvalsex_lg13 <- -log(qvalsex_lg13)

#corrected qval for surv
ecdf.cr1pval_lg13_surv<-ecdf(cr1p3_lg13_surv)

cr1_lg13_surv.qval<-lapply(cr1p3_lg13_surv,function(x){
  x/ecdf.cr1pval_lg13_surv(x)
})

cr1_lg13_surv.qval[cr1_lg13_surv.qval>1]<-1
cr1_lg13_surv.qval2 <- data.frame(cr1_lg13_surv.qval)

cr1_lg13_surv.qval2t <- t(cr1_lg13_surv.qval2)
qvalsurv_lg13 <- cr1_lg13_surv.qval2t[,1]
min_logqvalsurv_lg13 <- -log(qvalsurv_lg13)

## LG14

cr1p2_lg14 <- subset(cr1p_lg14, select = -c(1:3))
cr1p3_lg14 <- data.matrix(cr1p2_lg14, rownames.force=NA)
cr1p3_lg14_sex <- cr1p3_lg14[,1]
cr1p3_lg14_surv <- cr1p3_lg14[,2]

#corrected qval for sex
ecdf.cr1pval_lg14_sex<-ecdf(cr1p3_lg14_sex)

cr1_lg14_sex.qval<-lapply(cr1p3_lg14_sex,function(x){
  x/ecdf.cr1pval_lg14_sex(x)
})

cr1_lg14_sex.qval[cr1_lg14_sex.qval>1]<-1
cr1_lg14_sex.qval2 <- data.frame(cr1_lg14_sex.qval)

cr1_lg14_sex.qval2t <- t(cr1_lg14_sex.qval2)
qvalsex_lg14 <- cr1_lg14_sex.qval2t[,1]
min_logqvalsex_lg14 <- -log(qvalsex_lg14)

#corrected qval for surv
ecdf.cr1pval_lg14_surv<-ecdf(cr1p3_lg14_surv)

cr1_lg14_surv.qval<-lapply(cr1p3_lg14_surv,function(x){
  x/ecdf.cr1pval_lg14_surv(x)
})

cr1_lg14_surv.qval[cr1_lg14_surv.qval>1]<-1
cr1_lg14_surv.qval2 <- data.frame(cr1_lg14_surv.qval)

cr1_lg14_surv.qval2t <- t(cr1_lg14_surv.qval2)
qvalsurv_lg14 <- cr1_lg14_surv.qval2t[,1]
min_logqvalsurv_lg14 <- -log(qvalsurv_lg14)

## LG15

cr1p2_lg15 <- subset(cr1p_lg15, select = -c(1:3))
cr1p3_lg15 <- data.matrix(cr1p2_lg15, rownames.force=NA)
cr1p3_lg15_sex <- cr1p3_lg15[,1]
cr1p3_lg15_surv <- cr1p3_lg15[,2]

#corrected qval for sex
ecdf.cr1pval_lg15_sex<-ecdf(cr1p3_lg15_sex)

cr1_lg15_sex.qval<-lapply(cr1p3_lg15_sex,function(x){
  x/ecdf.cr1pval_lg15_sex(x)
})

cr1_lg15_sex.qval[cr1_lg15_sex.qval>1]<-1
cr1_lg15_sex.qval2 <- data.frame(cr1_lg15_sex.qval)

cr1_lg15_sex.qval2t <- t(cr1_lg15_sex.qval2)
qvalsex_lg15 <- cr1_lg15_sex.qval2t[,1]
min_logqvalsex_lg15 <- -log(qvalsex_lg15)

#corrected qval for surv
ecdf.cr1pval_lg15_surv<-ecdf(cr1p3_lg15_surv)

cr1_lg15_surv.qval<-lapply(cr1p3_lg15_surv,function(x){
  x/ecdf.cr1pval_lg15_surv(x)
})

cr1_lg15_surv.qval[cr1_lg15_surv.qval>1]<-1
cr1_lg15_surv.qval2 <- data.frame(cr1_lg15_surv.qval)

cr1_lg15_surv.qval2t <- t(cr1_lg15_surv.qval2)
qvalsurv_lg15 <- cr1_lg15_surv.qval2t[,1]
min_logqvalsurv_lg15 <- -log(qvalsurv_lg15)

## LG16

cr1p2_lg16 <- subset(cr1p_lg16, select = -c(1:3))
cr1p3_lg16 <- data.matrix(cr1p2_lg16, rownames.force=NA)
cr1p3_lg16_sex <- cr1p3_lg16[,1]
cr1p3_lg16_surv <- cr1p3_lg16[,2]

#corrected qval for sex
ecdf.cr1pval_lg16_sex<-ecdf(cr1p3_lg16_sex)

cr1_lg16_sex.qval<-lapply(cr1p3_lg16_sex,function(x){
  x/ecdf.cr1pval_lg16_sex(x)
})

cr1_lg16_sex.qval[cr1_lg16_sex.qval>1]<-1
cr1_lg16_sex.qval2 <- data.frame(cr1_lg16_sex.qval)

cr1_lg16_sex.qval2t <- t(cr1_lg16_sex.qval2)
qvalsex_lg16 <- cr1_lg16_sex.qval2t[,1]
min_logqvalsex_lg16 <- -log(qvalsex_lg16)

#corrected qval for surv
ecdf.cr1pval_lg16_surv<-ecdf(cr1p3_lg16_surv)

cr1_lg16_surv.qval<-lapply(cr1p3_lg16_surv,function(x){
  x/ecdf.cr1pval_lg16_surv(x)
})

cr1_lg16_surv.qval[cr1_lg16_surv.qval>1]<-1
cr1_lg16_surv.qval2 <- data.frame(cr1_lg16_surv.qval)

cr1_lg16_surv.qval2t <- t(cr1_lg16_surv.qval2)
qvalsurv_lg16 <- cr1_lg16_surv.qval2t[,1]
min_logqvalsurv_lg16 <- -log(qvalsurv_lg16)

## LG17

cr1p2_lg17 <- subset(cr1p_lg17, select = -c(1:3))
cr1p3_lg17 <- data.matrix(cr1p2_lg17, rownames.force=NA)
cr1p3_lg17_sex <- cr1p3_lg17[,1]
cr1p3_lg17_surv <- cr1p3_lg17[,2]

#corrected qval for sex
ecdf.cr1pval_lg17_sex<-ecdf(cr1p3_lg17_sex)

cr1_lg17_sex.qval<-lapply(cr1p3_lg17_sex,function(x){
  x/ecdf.cr1pval_lg17_sex(x)
})

cr1_lg17_sex.qval[cr1_lg17_sex.qval>1]<-1
cr1_lg17_sex.qval2 <- data.frame(cr1_lg17_sex.qval)

cr1_lg17_sex.qval2t <- t(cr1_lg17_sex.qval2)
qvalsex_lg17 <- cr1_lg17_sex.qval2t[,1]
min_logqvalsex_lg17 <- -log(qvalsex_lg17)

#corrected qval for surv
ecdf.cr1pval_lg17_surv<-ecdf(cr1p3_lg17_surv)

cr1_lg17_surv.qval<-lapply(cr1p3_lg17_surv,function(x){
  x/ecdf.cr1pval_lg17_surv(x)
})

cr1_lg17_surv.qval[cr1_lg17_surv.qval>1]<-1
cr1_lg17_surv.qval2 <- data.frame(cr1_lg17_surv.qval)

cr1_lg17_surv.qval2t <- t(cr1_lg17_surv.qval2)
qvalsurv_lg17 <- cr1_lg17_surv.qval2t[,1]
min_logqvalsurv_lg17 <- -log(qvalsurv_lg17)

## LG18

cr1p2_lg18 <- subset(cr1p_lg18, select = -c(1:3))
cr1p3_lg18 <- data.matrix(cr1p2_lg18, rownames.force=NA)
cr1p3_lg18_sex <- cr1p3_lg18[,1]
cr1p3_lg18_surv <- cr1p3_lg18[,2]

#corrected qval for sex
ecdf.cr1pval_lg18_sex<-ecdf(cr1p3_lg18_sex)

cr1_lg18_sex.qval<-lapply(cr1p3_lg18_sex,function(x){
  x/ecdf.cr1pval_lg18_sex(x)
})

cr1_lg18_sex.qval[cr1_lg18_sex.qval>1]<-1
cr1_lg18_sex.qval2 <- data.frame(cr1_lg18_sex.qval)

cr1_lg18_sex.qval2t <- t(cr1_lg18_sex.qval2)
qvalsex_lg18 <- cr1_lg18_sex.qval2t[,1]
min_logqvalsex_lg18 <- -log(qvalsex_lg18)

#corrected qval for surv
ecdf.cr1pval_lg18_surv<-ecdf(cr1p3_lg18_surv)

cr1_lg18_surv.qval<-lapply(cr1p3_lg18_surv,function(x){
  x/ecdf.cr1pval_lg18_surv(x)
})

cr1_lg18_surv.qval[cr1_lg18_surv.qval>1]<-1
cr1_lg18_surv.qval2 <- data.frame(cr1_lg18_surv.qval)

cr1_lg18_surv.qval2t <- t(cr1_lg18_surv.qval2)
qvalsurv_lg18 <- cr1_lg18_surv.qval2t[,1]
min_logqvalsurv_lg18 <- -log(qvalsurv_lg18)

## LG19

cr1p2_lg19 <- subset(cr1p_lg19, select = -c(1:3))
cr1p3_lg19 <- data.matrix(cr1p2_lg19, rownames.force=NA)
cr1p3_lg19_sex <- cr1p3_lg19[,1]
cr1p3_lg19_surv <- cr1p3_lg19[,2]

#corrected qval for sex
ecdf.cr1pval_lg19_sex<-ecdf(cr1p3_lg19_sex)

cr1_lg19_sex.qval<-lapply(cr1p3_lg19_sex,function(x){
  x/ecdf.cr1pval_lg19_sex(x)
})

cr1_lg19_sex.qval[cr1_lg19_sex.qval>1]<-1
cr1_lg19_sex.qval2 <- data.frame(cr1_lg19_sex.qval)

cr1_lg19_sex.qval2t <- t(cr1_lg19_sex.qval2)
qvalsex_lg19 <- cr1_lg19_sex.qval2t[,1]
min_logqvalsex_lg19 <- -log(qvalsex_lg19)

#corrected qval for surv
ecdf.cr1pval_lg19_surv<-ecdf(cr1p3_lg19_surv)

cr1_lg19_surv.qval<-lapply(cr1p3_lg19_surv,function(x){
  x/ecdf.cr1pval_lg19_surv(x)
})

cr1_lg19_surv.qval[cr1_lg19_surv.qval>1]<-1
cr1_lg19_surv.qval2 <- data.frame(cr1_lg19_surv.qval)

cr1_lg19_surv.qval2t <- t(cr1_lg19_surv.qval2)
qvalsurv_lg19 <- cr1_lg19_surv.qval2t[,1]
min_logqvalsurv_lg19 <- -log(qvalsurv_lg19)

### save sex and surv on separate tables ###

qvalsex_lg <- c(qvalsex_lg1, qvalsex_lg2, qvalsex_lg3, qvalsex_lg4, qvalsex_lg5, qvalsex_lg6, qvalsex_lg7, qvalsex_lg8, 
                qvalsex_lg9, qvalsex_lg10, qvalsex_lg11, qvalsex_lg12, qvalsex_lg13, qvalsex_lg14, qvalsex_lg15, qvalsex_lg16, 
                qvalsex_lg17, qvalsex_lg18, qvalsex_lg19)

min_logqvalsex_lg <- c(min_logqvalsex_lg1, min_logqvalsex_lg2, min_logqvalsex_lg3, min_logqvalsex_lg4, min_logqvalsex_lg5, min_logqvalsex_lg6, min_logqvalsex_lg7, min_logqvalsex_lg8, 
                       min_logqvalsex_lg9, min_logqvalsex_lg10, min_logqvalsex_lg11, min_logqvalsex_lg12, min_logqvalsex_lg13, min_logqvalsex_lg14, min_logqvalsex_lg15, min_logqvalsex_lg16, 
                       min_logqvalsex_lg17, min_logqvalsex_lg18, min_logqvalsex_lg19)

qvalsurv_lg <- c(qvalsurv_lg1, qvalsurv_lg2, qvalsurv_lg3, qvalsurv_lg4, qvalsurv_lg5, qvalsurv_lg6, qvalsurv_lg7, qvalsurv_lg8, 
                 qvalsurv_lg9, qvalsurv_lg10, qvalsurv_lg11, qvalsurv_lg12, qvalsurv_lg13, qvalsurv_lg14, qvalsurv_lg15, qvalsurv_lg16, 
                 qvalsurv_lg17, qvalsurv_lg18, qvalsurv_lg19)

min_logqvalsurv_lg <- c(min_logqvalsurv_lg1, min_logqvalsurv_lg2, min_logqvalsurv_lg3, min_logqvalsurv_lg4, min_logqvalsurv_lg5, min_logqvalsurv_lg6, min_logqvalsurv_lg7, min_logqvalsurv_lg8, 
                        min_logqvalsurv_lg9, min_logqvalsurv_lg10, min_logqvalsurv_lg11, min_logqvalsurv_lg12, min_logqvalsurv_lg13, min_logqvalsurv_lg14, min_logqvalsurv_lg15, min_logqvalsurv_lg16, 
                        min_logqvalsurv_lg17, min_logqvalsurv_lg18, min_logqvalsurv_lg19)


df_sex2 <- data.frame(cr1p$marker, cr1p$LG, cr1p$cM, cr1p$sex, qvalsex_gw, min_logqvalsex_gw, qvalsex_lg, min_logqvalsex_lg)
df_surv2 <- data.frame(cr1p$marker, cr1p$LG, cr1p$cM, cr1p$days, qvalsurv_gw, min_logqvalsurv_gw, qvalsurv_lg, min_logqvalsurv_lg)

write.table(df_sex2, file="/Volumes/group_dv/personal/DValenzano/month-by-month/May2015/cr1_sex2-qvals.csv")

write.table(df_surv2, file="/Volumes/group_dv/personal/DValenzano/month-by-month/May2015/cr1_surv2-qvals.csv")

