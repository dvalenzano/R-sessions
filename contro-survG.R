# Goal: to plot survival on control regions on LG3 (cross G)

library(survival)

g <- read.csv(file="/Volumes/group_dv/personal/DValenzano/Sep2014/g_days2.csv", header=T, sep=',')

g2<- g[3:nrow(g),]
g2$status <- rep(1, length(g2$X))
#g2$time <- as.numeric(g2$X.2)

g2$time <- as.numeric(as.character(g2$X.2))

fit46347 <- survfit(formula=Surv(time,status)~X46347, data=g2)
plot(fit46347, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(3,3,3), col=c(2:4), main="G-cross marker 46347")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fit7106 <- survfit(formula=Surv(time,status)~X7106, data=g2)
plot(fit7106, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(3,3,3), col=c(2:4), main="G-cross marker 7106")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fit40077 <- survfit(formula=Surv(time,status)~X40077, data=g2)
plot(fit40077, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(3,3,3), col=c(2:4), main="G-cross marker 40077")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

#### THEN IN MALES AND FEMALES SEPARATELY

#gsel_m <- read.csv('/Volumes/group_dv/personal/DValenzano/Dec2014/m_selectedG.csv', sep=',', header=TRUE)
#gsel_f <- read.csv('/Volumes/group_dv/personal/DValenzano/Dec2014/f_selectedG.csv', sep=',', header=TRUE)

#gsel_m$status <- rep(1, length(gsel_m$X))
#gsel_f$status <- rep(1, length(gsel_f$X))

#gms_46347 <- subset(gsel_m, X46347=='2')
#gfs_46347 <- subset(gsel_f, X46347=='2')

#g2m <- subset(g2, X.1 == '1')
#g2f <- subset(g2, X.1 == '2')

par(mfrow=c(3,1))

fit46347 <- survfit(formula=Surv(time,status)~X46347, data=g2)
plot(fit46347, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(3,3,3), col=c(2:4), main="G-cross marker 46347")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(3,3,3), col=c(2:4))

fitm <- survfit(formula=Surv(time,status)~X46347, data=g2m)
plot(fitm, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(3,3,3), col=c(2:4), main="G-cross males, marker 46347")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(3,3,3), col=c(2:4))

fitf <- survfit(formula=Surv(time,status)~X46347, data=g2f)
plot(fitf, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(3,3,3), col=c(2:4), main="G-cross females marker 46347")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(3,3,3), col=c(2:4))

#####

par(mfrow=c(3,1))

fit7106 <- survfit(formula=Surv(time,status)~X7106, data=g2)
plot(fit7106, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(3,3,3), col=c(2:4), main="G-cross marker 7106")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(3,3,3), col=c(2:4))

fitm <- survfit(formula=Surv(time,status)~X7106, data=g2m)
plot(fitm, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(3,3), col=c(3:4), main="G-cross males, marker 7106")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(3,3,3), col=c(2:4))

fitf <- survfit(formula=Surv(time,status)~X7106, data=g2f)
plot(fitf, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(3,3), col=c(2:3), main="G-cross females marker 7106")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(3,3,3), col=c(2:4))


######

par(mfrow=c(3,1))

fit40077 <- survfit(formula=Surv(time,status)~X40077, data=g2)
plot(fit40077, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(3,3,3), col=c(2:4), main="G-cross marker 40077")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(3,3,3), col=c(2:4))

fitm <- survfit(formula=Surv(time,status)~X40077, data=g2m)
plot(fitm, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(3,3,3), col=c(2:4), main="G-cross males, marker 40077")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(3,3,3), col=c(2:4))

fitf <- survfit(formula=Surv(time,status)~X40077, data=g2f)
plot(fitf, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(3,3,3), col=c(2:4), main="G-cross females marker 40077")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(3,3,3), col=c(2:4))

