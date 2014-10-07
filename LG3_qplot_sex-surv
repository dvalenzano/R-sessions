library(lattice)
library(ggplot2)

aadays <- read.csv('/Volumes/group_dv/personal/DValenzano/Oct2014/aadays3_tab.csv', sep=',', header=TRUE)
gdays <- read.csv('/Volumes/group_dv/personal/DValenzano/Oct2014/gdays3_tab.csv', sep=',', header=TRUE)

########G CROSS DOT PLOT OF SEX AND SURVIVAL QTL##########

aadays_LG3 <- subset(aadays, aadays$LG == '3')
gdays_LG3 <- subset(gdays, gdays$LG == '3')

# To merge the dot plot for surv-qtl and the that for sex, I need to put everything in the same column
negl_qvalg <- c(gdays_LG3$neg_log.qval., gdays_LG3$neg_log.qval._sex) 

gd0 <- rbind(gdays_LG3, gdays_LG3)
gd0$LOD <- negl_qvalg
gd0$LOD_gr <- c(rep('surv', length(gdays_LG3$neg_log.qval.)), rep('sex', length(gdays_LG3$neg_log.qval._sex)))  

g1 <- qplot(cM,LOD, data=gd0, xlab="cM", ylab="-log(q)", colour=LOD_gr, main="LG3, cross G Sex and Survival QTL", ylim=c(0,5))
dev.new()
g1+scale_colour_brewer(palette="Set1")

#########SAME THING FOR LG3 IN AA CROSS###########

negl_qvalaa <- c(aadays_LG3$neg_log.qval., aadays_LG3$neg_log.qval._sex) 

aad0 <- rbind(aadays_LG3, aadays_LG3)
aad0$LOD <- negl_qvalaa
aad0$LOD_gr <- c(rep('surv', length(aadays_LG3$neg_log.qval.)), rep('sex', length(aadays_LG3$neg_log.qval._sex)))  

aa1 <- qplot(cM,LOD, data=aad0, xlab="cM", ylab="-log(q)", colour=LOD_gr, main="LG3, cross AA Sex and Survival QTL", ylim=c(0,5))
dev.new()
aa1+scale_colour_brewer(palette="Set1")

aadays_LG3
