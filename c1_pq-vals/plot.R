# This script generates plots for survival QTL and sex-QTL in cross 1 using genome-wide and linkage group by linkage group
# q values. The input files are produced with this python script:
# https://github.com/dvalenzano/Interactive-Sessions/blob/master/14-May-2015.py

c1sex <- read.csv(file="/Volumes/group_dv/personal/DValenzano/month-by-month/May2015/cr1_sex2.1-qvals.csv", sep=",", header=T)
c1surv <- read.csv(file="/Volumes/group_dv/personal/DValenzano/month-by-month/May2015/cr1_surv2.1-qvals.csv", sep=",", header=T)

c1surv_lg3 <- subset(c1surv, LG=='3')
c1sex_lg3 <- subset(c1sex, LG=='3')

layout(matrix(c(1,1,2,2),2,2,byrow=TRUE))

plot(c1surv_lg3$cM, c1surv_lg3$min_logqvalsurv_gw, xlab="cM", ylab="-log(q)", col=c("tomato2"), ylim=c(0,10), pch=16, main="genome-wide qvalue")
points(c1sex_lg3$cM, c1sex_lg3$min_logqvalsex_gw, xlab="cM", ylab="-log(q)", col=c("mediumblue"), pch=16)
legend(200,7.5,c("survival", "sex"), col=c("tomato2", "mediumblue"), cex=1.2, pch=c(16,16), bty="n")
plot(c1surv_lg3$cM, c1surv_lg3$min_logqvalsurv_lg, xlab="cM", ylab="-log(q)", col=c("tomato2"), ylim=c(0,10), pch=16, main="linkage-group by linkage-group qvalue")
points(c1sex_lg3$cM, c1sex_lg3$min_logqvalsex_lg, xlab="cM", ylab="-log(q)", col=c("mediumblue"), pch=16)
legend(200,7.5,c("survival", "sex"), col=c("tomato2", "mediumblue"), cex=1.2, pch=c(16,16), bty="n")
grid()
