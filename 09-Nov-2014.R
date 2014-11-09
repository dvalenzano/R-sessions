#Goal: To test a control region in AA cross LG3, where males are longer-lived than females. Here het males are not longer lived than het females

aanpeak_m <- read.csv('/Volumes/group_dv/personal/DValenzano/Nov2014/AA-cross/m_notpeakAA2.csv', sep=',', header=TRUE)
aanpeak_f <- read.csv('/Volumes/group_dv/personal/DValenzano/Nov2014/AA-cross/f_notpeakAA2.csv', sep=',', header=TRUE)

aanpeak_m$status <- rep(1, length(aanpeak_m$X))
aanpeak_f$status <- rep(1, length(aanpeak_f$X))

library(survival)


sigm = c('27839','30997','30674','25041','22860','18891','11124','22990','18523','21837','16997','15646','40193','17058','34540',
         '37447','20516','1421','8546','38791')



#First, plots marker by marker, in males and females
aams_27839 <- subset(aanpeak_m, X27839=='0')
aafs_27839 <- subset(aanpeak_f, X27839=='0')
aamf_27839 <- rbind(aams_27839,aafs_27839)

par(mfrow=c(3,3))

fitm <- survfit(formula=Surv(X.2,status)~X27839, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (27839)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X27839, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (27839)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_27839)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (27839)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

###########

aams_21837 <- subset(aanpeak_m, X21837=='0')
aafs_21837 <- subset(aanpeak_f, X21837=='0')
aamf_21837 <- rbind(aams_21837,aafs_21837)

#par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X21837, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (21837)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X21837, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (21837)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_21837)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (21837)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))


###########

aams_37447 <- subset(aanpeak_m, X37447=='0')
aafs_37447 <- subset(aanpeak_f, X37447=='0')
aamf_37447 <- rbind(aams_37447,aafs_37447)

#par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X37447, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (37447)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X37447, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (37447)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_37447)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (37447)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

###########
###########

aams_30997 <- subset(aanpeak_m, X30997=='0')
aafs_30997 <- subset(aanpeak_f, X30997=='0')
aamf_30997 <- rbind(aams_30997,aafs_30997)

par(mfrow=c(3,3))

fitm <- survfit(formula=Surv(X.2,status)~X30997, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (30997)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X30997, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (30997)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_30997)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (30997)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

###########

aams_30674 <- subset(aanpeak_m, X30674=='0')
aafs_30674 <- subset(aanpeak_f, X30674=='0')
aamf_30674 <- rbind(aams_30674,aafs_30674)

#par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X30674, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (30674)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X30674, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (30674)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_30674)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (30674)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

###########

aams_25041 <- subset(aanpeak_m, X25041=='0')
aafs_25041 <- subset(aanpeak_f, X25041=='0')
aamf_25041 <- rbind(aams_25041,aafs_25041)

#par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X25041, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (25041)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X25041, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (25041)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_25041)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (25041)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

###########
###########

aams_22860 <- subset(aanpeak_m, X22860=='0')
aafs_22860 <- subset(aanpeak_f, X22860=='0')
aamf_22860 <- rbind(aams_22860,aafs_22860)

par(mfrow=c(3,3))

fitm <- survfit(formula=Surv(X.2,status)~X22860, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (22860)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X22860, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (22860)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_22860)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (22860)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

###########

aams_18891 <- subset(aanpeak_m, X18891=='0')
aafs_18891 <- subset(aanpeak_f, X18891=='0')
aamf_18891 <- rbind(aams_18891,aafs_18891)

#par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X18891, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (18891)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X18891, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (18891)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_18891)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (18891)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

###########

aams_11124 <- subset(aanpeak_m, X11124=='0')
aafs_11124 <- subset(aanpeak_f, X11124=='0')
aamf_11124 <- rbind(aams_11124,aafs_11124)

#par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X11124, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (11124)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X11124, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (11124)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_11124)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (11124)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

###########
###########
aams_22990 <- subset(aanpeak_m, X22990=='0')
aafs_22990 <- subset(aanpeak_f, X22990=='0')
aamf_22990 <- rbind(aams_22990,aafs_22990)

par(mfrow=c(3,3))

fitm <- survfit(formula=Surv(X.2,status)~X22990, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (22990)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X22990, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (22990)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_22990)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (22990)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

###########

aams_18523 <- subset(aanpeak_m, X18523=='0')
aafs_18523 <- subset(aanpeak_f, X18523=='0')
aamf_18523 <- rbind(aams_18523,aafs_18523)

#par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X18523, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (18523)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X18523, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (18523)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_18523)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (18523)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))


###########

aams_20516 <- subset(aanpeak_m, X20516=='0')
aafs_20516 <- subset(aanpeak_f, X20516=='0')
aamf_20516 <- rbind(aams_20516,aafs_20516)

#par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X20516, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (20516)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X20516, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (20516)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_20516)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (20516)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

###########
###########

aams_1421 <- subset(aanpeak_m, X1421=='0')
aafs_1421 <- subset(aanpeak_f, X1421=='0')
aamf_1421 <- rbind(aams_1421,aafs_1421)

par(mfrow=c(3,3))

fitm <- survfit(formula=Surv(X.2,status)~X1421, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (1421)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X1421, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (1421)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_1421)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (1421)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

###########

aams_16997 <- subset(aanpeak_m, X16997=='0')
aafs_16997 <- subset(aanpeak_f, X16997=='0')
aamf_16997 <- rbind(aams_16997,aafs_16997)

#par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X16997, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (16997)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X16997, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (16997)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_16997)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (16997)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

###########

aams_8546 <- subset(aanpeak_m, X8546=='0')
aafs_8546 <- subset(aanpeak_f, X8546=='0')
aamf_8546 <- rbind(aams_8546,aafs_8546)

#par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X8546, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (8546)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X8546, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (8546)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_8546)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (8546)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

###########
###########

aams_15646 <- subset(aanpeak_m, X15646=='0')
aafs_15646 <- subset(aanpeak_f, X15646=='0')
aamf_15646 <- rbind(aams_15646,aafs_15646)

par(mfrow=c(3,3))

fitm <- survfit(formula=Surv(X.2,status)~X15646, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (15646)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X15646, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (15646)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_15646)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (15646)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

###########

aams_38791 <- subset(aanpeak_m, X38791=='0')
aafs_38791 <- subset(aanpeak_f, X38791=='0')
aamf_38791 <- rbind(aams_38791,aafs_38791)

#par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X38791, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (38791)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X38791, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (38791)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_38791)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (38791)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

##########
aams_40193 <- subset(aanpeak_m, X40193=='0')
aafs_40193 <- subset(aanpeak_f, X40193=='0')
aamf_40193 <- rbind(aams_40193,aafs_40193)

#par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X40193, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (40193)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X40193, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (40193)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_40193)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (40193)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

###########
###########

aams_17058 <- subset(aanpeak_m, X17058=='0')
aafs_17058 <- subset(aanpeak_f, X17058=='0')
aamf_17058 <- rbind(aams_17058,aafs_17058)

par(mfrow=c(2,3))

fitm <- survfit(formula=Surv(X.2,status)~X17058, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (17058)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X17058, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (17058)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_17058)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (17058)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

############
aams_34540 <- subset(aanpeak_m, X34540=='0')
aafs_34540 <- subset(aanpeak_f, X34540=='0')
aamf_34540 <- rbind(aams_34540,aafs_34540)

#par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X34540, data=aanpeak_m)
plot(fitm, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross males, npeak marker (34540)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitf <- survfit(formula=Surv(X.2,status)~X34540, data=aanpeak_f)
plot(fitf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross females, npeak marker (34540)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_34540)
plot(fitmf, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-cross, ll survival for npeak marker (34540)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

#########
##########
##########

##############MULTIPLE PLOTS IN MALES AND FEMALES of LL/LL alleles##############
par(mfrow=c(4,5))

fitmf_27839 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_25041)
plot(fitmf_27839, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (27839)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
leg.txt <- c("males", "females")
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_21837 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_21837)
#plot(fitmf_21837, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (21837)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
plot(fitmf_21837, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2))
title(expression("AA-LG3, ll surv npeak marker (" * phantom("21837") *")"), col.main="black")
title(expression(phantom("AA-LG3, ll surv npeak marker (") * "21837" ), col.main="red")
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_37447 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_37447)
plot(fitmf_37447, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (37447)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_30997 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_30997)
plot(fitmf_30997, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (30997)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_30674 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_30674)
plot(fitmf_30674, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (30674)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_25041 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_25041)
plot(fitmf_25041, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll survival for npeak marker (25041)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_22860 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_22860)
plot(fitmf_22860, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (22860)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_18891 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_18891)
plot(fitmf_18891, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (18891)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_11124 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_11124)
#plot(fitmf_11124, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (11124)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
plot(fitmf_11124, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2))
title(expression("AA-LG3, ll surv npeak marker (" * phantom("11124") *")"), col.main="black")
title(expression(phantom("AA-LG3, ll surv npeak marker (") * "11124" ), col.main="red")
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_22990 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_22990)
plot(fitmf_22990, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (22990)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_18523 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_18523)
plot(fitmf_18523, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (18523)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_20516 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_20516)
plot(fitmf_20516, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (20516)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_1421 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_1421)
plot(fitmf_1421, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (1421)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_16997 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_16997)
plot(fitmf_16997, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (16997)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_8546 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_8546)
plot(fitmf_8546, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (8546)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_15646 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_15646)
plot(fitmf_15646, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (15646)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_38791 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_38791)
plot(fitmf_38791, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (38791)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_40193 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_40193)
plot(fitmf_40193, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (40193)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_6974 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_17058)
plot(fitmf_17058, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="AA-LG3, ll surv npeak marker (17058)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

##############HETS COMPARISON BETWEEN MALES AND FEMALES#############

aams_27839_h <- subset(aanpeak_m, X27839=='1')
aafs_27839_h <- subset(aanpeak_f, X27839=='1')
aamf_27839_h <- rbind(aams_27839_h,aafs_27839_h)

aams_21837_h <- subset(aanpeak_m, X21837=='1')
aafs_21837_h <- subset(aanpeak_f, X21837=='1')
aamf_21837_h <- rbind(aams_21837_h,aafs_21837_h)

aams_37447_h <- subset(aanpeak_m, X37447=='1')
aafs_37447_h <- subset(aanpeak_f, X37447=='1')
aamf_37447_h <- rbind(aams_37447_h,aafs_37447_h)

aams_30997_h <- subset(aanpeak_m, X30997=='1')
aafs_30997_h <- subset(aanpeak_f, X30997=='1')
aamf_30997_h <- rbind(aams_30997_h,aafs_30997_h)

aams_30674_h <- subset(aanpeak_m, X30674=='1')
aafs_30674_h <- subset(aanpeak_f, X30674=='1')
aamf_30674_h <- rbind(aams_30674_h,aafs_30674_h)

aams_25041_h <- subset(aanpeak_m, X25041=='1')
aafs_25041_h <- subset(aanpeak_f, X25041=='1')
aamf_25041_h <- rbind(aams_25041_h,aafs_25041_h)

aams_22860_h <- subset(aanpeak_m, X22860=='1')
aafs_22860_h <- subset(aanpeak_f, X22860=='1')
aamf_22860_h <- rbind(aams_22860_h,aafs_22860_h)

aams_18891_h <- subset(aanpeak_m, X18891=='1')
aafs_18891_h <- subset(aanpeak_f, X18891=='1')
aamf_18891_h <- rbind(aams_18891_h,aafs_18891_h)

aams_11124_h <- subset(aanpeak_m, X11124=='1')
aafs_11124_h <- subset(aanpeak_f, X11124=='1')
aamf_11124_h <- rbind(aams_11124_h,aafs_11124_h)

aams_22990_h <- subset(aanpeak_m, X22990=='1')
aafs_22990_h <- subset(aanpeak_f, X22990=='1')
aamf_22990_h <- rbind(aams_22990_h,aafs_22990_h)

aams_18523_h <- subset(aanpeak_m, X18523=='1')
aafs_18523_h <- subset(aanpeak_f, X18523=='1')
aamf_18523_h <- rbind(aams_18523_h,aafs_18523_h)

aams_20516_h <- subset(aanpeak_m, X20516=='1')
aafs_20516_h <- subset(aanpeak_f, X20516=='1')
aamf_20516_h <- rbind(aams_20516_h,aafs_20516_h)

aams_1421_h <- subset(aanpeak_m, X1421=='1')
aafs_1421_h <- subset(aanpeak_f, X1421=='1')
aamf_1421_h <- rbind(aams_1421_h,aafs_1421_h)

aams_16997_h <- subset(aanpeak_m, X16997=='1')
aafs_16997_h <- subset(aanpeak_f, X16997=='1')
aamf_16997_h <- rbind(aams_16997_h,aafs_16997_h)

aams_8546_h <- subset(aanpeak_m, X8546=='1')
aafs_8546_h <- subset(aanpeak_f, X8546=='1')
aamf_8546_h <- rbind(aams_8546_h,aafs_8546_h)

aams_15646_h <- subset(aanpeak_m, X15646=='1')
aafs_15646_h <- subset(aanpeak_f, X15646=='1')
aamf_15646_h <- rbind(aams_15646_h,aafs_15646_h)

aams_38791_h <- subset(aanpeak_m, X38791=='1')
aafs_38791_h <- subset(aanpeak_f, X38791=='1')
aamf_38791_h <- rbind(aams_38791_h,aafs_38791_h)

aams_40193_h <- subset(aanpeak_m, X40193=='1')
aafs_40193_h <- subset(aanpeak_f, X40193=='1')
aamf_40193_h <- rbind(aams_40193_h,aafs_40193_h)

aams_17058_h <- subset(aanpeak_m, X17058=='1')
aafs_17058_h <- subset(aanpeak_f, X17058=='1')
aamf_17058_h <- rbind(aams_17058_h,aafs_17058_h)

aams_34540_h <- subset(aanpeak_m, X34540=='1')
aafs_34540_h <- subset(aanpeak_f, X34540=='1')
aamf_34540_h <- rbind(aams_34540_h,aafs_34540_h)


par(mfrow=c(4,5))

fitmf_27839 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_25041_h)
plot(fitmf_27839, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (27839)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
leg.txt <- c("males", "females")
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_21837 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_21837_h)
plot(fitmf_21837, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (21837)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_37447 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_37447_h)
plot(fitmf_37447, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (37447)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_30997 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_30997_h)
plot(fitmf_30997, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (30997)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_30674 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_30674_h)
plot(fitmf_30674, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (30674)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_25041 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_25041_h)
plot(fitmf_25041, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (25041)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_22860 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_22860_h)
plot(fitmf_22860, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (22860)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_18891 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_18891_h)
plot(fitmf_18891, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (18891)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_11124 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_11124_h)
plot(fitmf_11124, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (11124)")
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_22990 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_22990_h)
plot(fitmf_22990, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (22990)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_18523 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_18523_h)
plot(fitmf_18523, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (18523)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_20516 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_20516_h)
plot(fitmf_20516, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (20516)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_1421 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_1421_h)
plot(fitmf_1421, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (1421)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_16997 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_16997_h)
plot(fitmf_16997, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (16997)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_8546 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_8546_h)
plot(fitmf_8546, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (8546)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_15646 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_15646_h)
plot(fitmf_15646, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (15646)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_38791 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_38791_h)
plot(fitmf_38791, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (38791)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_40193 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_40193_h)
plot(fitmf_40193, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (40193)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_17058 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_17058_h)
plot(fitmf_17058, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (17058)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))

fitmf_34540 <- survfit(formula=Surv(X.2,status)~X.1, data=aamf_34540_h)
plot(fitmf_34540, xlim=c(0,550), ylab="Fraction survived", xlab="Time (days_h)", lwd=c(2.5,2.5), lty=c(1,2), main="AALG3 het nsurv snp (34540)")
#legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))
legend("topright", legend=leg.txt, lty=c(1,2), lwd=c(2.5,2.5))




