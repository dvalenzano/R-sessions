library(survival)

g <- read.csv(file="/Volumes/group_dv/personal/DValenzano/Sep2014/g_days2.csv", header=T, sep=',')

g2<- g[3:nrow(g),]
g2$status <- rep(1, length(g2$X))
#g2$time <- as.numeric(g2$X.2)

g2$time <- as.numeric(as.character(g2$X.2))

fit46347 <- survfit(formula=Surv(time,status)~X46347, data=g2)
plot(fit46347, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(3,3,3), col=c(2:4), main="G-cross marker 46347")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

prova <- data.frame(g2$X.2, g2$X46347)

prova_ss <- subset(prova, prova$g2.X46347==0)
prova_sl <- subset(prova, prova$g2.X46347==1)
prova_ll <- subset(prova, prova$g2.X46347==2)

ss <- as.numeric(as.character(prova_ss$g2.X.2))
sl <- as.numeric(as.character(prova_sl$g2.X.2))
ll <- as.numeric(as.character(prova_ll$g2.X.2))

100*(median(ll)/median(ss) -1)
100*(median(sl)/median(ss) -1)
100*(median(ll)/median(ls) -1)
