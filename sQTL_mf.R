# Goal: to plot survival in males and females at the survival QTL

gpeak_m <- read.csv('/Volumes/group_dv/personal/DValenzano/Oct2014/m_peakG.csv', sep=',', header=TRUE)
gpeak_f <- read.csv('/Volumes/group_dv/personal/DValenzano/Oct2014/f_peakG.csv', sep=',', header=TRUE)

gpeak_m$status <- rep(1, length(gpeak_m$X))
gpeak_f$status <- rep(1, length(gpeak_f$X))

library(survival)



#First, plots marker by marker, in males and females
gms_26385 <- subset(gpeak_m, X26385=='2')
gfs_26385 <- subset(gpeak_f, X26385=='2')
gmf_26385 <- rbind(gms_26385,gfs_26385)

par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X26385, data=gpeak_m)
plot(fitm, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross males, peak marker -2 (26385)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fitf <- survfit(formula=Surv(X.2,status)~X26385, data=gpeak_f)
plot(fitf, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross females, peak marker -2 (26385)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=gmf_26385)
plot(fitmf, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="G-cross, ll survival for peak marker -2 (26385)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))


##########

gms_25003 <- subset(gpeak_m, X25003=='2')
gfs_25003 <- subset(gpeak_f, X25003=='2')
gmf_25003 <- rbind(gms_25003,gfs_25003)

par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X25003, data=gpeak_m)
plot(fitm, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross males, peak marker -1 (25003)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fitf <- survfit(formula=Surv(X.2,status)~X25003, data=gpeak_f)
plot(fitf, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross females, peak marker -1 (25003)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=gmf_25003)
plot(fitmf, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="G-cross, ll survival for peak marker -1 (25003)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

##########


gms_46347 <- subset(gpeak_m, X46347=='2')
gfs_46347 <- subset(gpeak_f, X46347=='2')
gmf_46347 <- rbind(gms_46347,gfs_46347)

par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X46347, data=gpeak_m)
plot(fitm, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross males, peak marker (46347)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fitf <- survfit(formula=Surv(X.2,status)~X46347, data=gpeak_f)
plot(fitf, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross females, peak marker (46347)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=gmf_46347)
plot(fitmf, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="G-cross, ll survival for peak marker (46347)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

#############

gms_12535 <- subset(gpeak_m, X12535=='2')
gfs_12535 <- subset(gpeak_f, X12535=='2')
gmf_12535 <- rbind(gms_12535,gfs_12535)

par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X12535, data=gpeak_m)
plot(fitm, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross males, peak marker +1 (12535)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fitf <- survfit(formula=Surv(X.2,status)~X12535, data=gpeak_f)
plot(fitf, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross females, peak marker +1 (12535)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=gmf_12535)
plot(fitmf, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="G-cross, ll survival for peak marker +1 (12535)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

############

gms_31890 <- subset(gpeak_m, X31890=='2')
gfs_31890 <- subset(gpeak_f, X31890=='2')
gmf_31890 <- rbind(gms_31890,gfs_31890)

par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X31890, data=gpeak_m)
plot(fitm, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross males, peak marker +2 (31890)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fitf <- survfit(formula=Surv(X.2,status)~X31890, data=gpeak_f)
plot(fitf, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross females, peak marker +2 (31890)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=gmf_31890)
plot(fitmf, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="G-cross, ll survival for peak marker +2 (31890)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

################

gms_17025 <- subset(gpeak_m, X17025=='2')
gfs_17025 <- subset(gpeak_f, X17025=='2')
gmf_17025 <- rbind(gms_17025,gfs_17025)

par(mfrow=c(3,1))

fitm <- survfit(formula=Surv(X.2,status)~X17025, data=gpeak_m)
plot(fitm, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross males, peak marker +3 (17025)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fitf <- survfit(formula=Surv(X.2,status)~X17025, data=gpeak_f)
plot(fitf, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross females, peak marker +3 (17025)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fitmf <- survfit(formula=Surv(X.2,status)~X.1, data=gmf_17025)
plot(fitmf, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5), lty=c(1,2), main="G-cross, ll survival for peak marker +3 (17025)")
legend(400,.9, legend=c("males", "females"), lwd=c(2.5,2.5), lty=c(1,2))

# Eventually, I want to generate a final plot with survival in males only, for the ll genotype only
gms_26385$group = rep(1, length(gms_26385$X))
gms_25003$group = rep(2, length(gms_25003$X))
gms_46347$group = rep(3, length(gms_46347$X))
gms_12535$group = rep(4, length(gms_12535$X))
gms_13555$group = rep(5, length(gms_13555$X))
gms_31890$group = rep(6, length(gms_31890$X))
gms_17025$group = rep(7, length(gms_17025$X))

gm_ll <- rbind(gms_26385, gms_25003, gms_46347, gms_12535, gms_13555, gms_31890, gms_17025)
fitgm_ll <- survfit(formula=Surv(X.2,status)~group, data=gm_ll)
plot(fitgm_ll)
