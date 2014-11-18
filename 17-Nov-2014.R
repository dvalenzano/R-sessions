# Goal: to derive survival plots for the peak markers in cross G and AA for both females and males together

gpeak_m <- read.csv('/Volumes/group_dv/personal/DValenzano/Oct2014/m_peakG.csv', sep=',', header=TRUE)
gpeak_f <- read.csv('/Volumes/group_dv/personal/DValenzano/Oct2014/f_peakG.csv', sep=',', header=TRUE)

gpeak_m$status <- rep(1, length(gpeak_m$X))
gpeak_f$status <- rep(1, length(gpeak_f$X))

gpeak <- rbind(gpeak_m,gpeak_f)
gpeak$status <- c(gpeak_m$status, gpeak_f$status)

g_sig = c('26385', '25003', '46347', '12535', '13555', '31890', '17025')

library(survival)

###FIRST- G CROSS###

fit_g26385 <- survfit(formula=Surv(X.2,status)~X26385, data=gpeak)
plot(fit_g26385, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross, peak marker (26385)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fit_g25003 <- survfit(formula=Surv(X.2,status)~X25003, data=gpeak)
plot(fit_g25003, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross, peak marker (25003)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fit_g46347 <- survfit(formula=Surv(X.2,status)~X46347, data=gpeak)
plot(fit_g46347, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross, peak marker (46347)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fit_g12535 <- survfit(formula=Surv(X.2,status)~X12535, data=gpeak)
plot(fit_g12535, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross, peak marker (12535)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fit_g13555 <- survfit(formula=Surv(X.2,status)~X13555, data=gpeak)
plot(fit_g13555, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross, peak marker (13555)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fit_g31890 <- survfit(formula=Surv(X.2,status)~X31890, data=gpeak)
plot(fit_g31890, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross, peak marker (31890)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

fit_g17025 <- survfit(formula=Surv(X.2,status)~X17025, data=gpeak)
plot(fit_g17025, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(2:4), main="G-cross, peak marker (17025)")
legend(400,.9, legend=c("sl/sl","sl/ll", "ll/ll"), lwd=c(2.5,2.5, 2.5), col=c(2:4))

###NOW AA CROSS #####

aapeak_m <- read.csv('/Volumes/group_dv/personal/DValenzano/Nov2014/AA-cross/m_peakAA2.csv', sep=',', header=TRUE)
aapeak_f <- read.csv('/Volumes/group_dv/personal/DValenzano/Nov2014/AA-cross/f_peakAA2.csv', sep=',', header=TRUE)

aapeak_m$status <- rep(1, length(aapeak_m$X))
aapeak_f$status <- rep(1, length(aapeak_f$X))

aapeak <- rbind(aapeak_m,aapeak_f)
aapeak$status <- c(aapeak_m$status, aapeak_f$status)

aa_sig = c('33911', '8423', '19589', '39073', '25587', '44971', '8178', '4179', 
           '32767', '26568', '26780', '2663', '36581', '12275', '34490', '39114', 
           '10884', '2093', '6074', '18588', '32724', '32599')

fit_aa33911 <- survfit(formula=Surv(X.2,status)~X33911, data=aapeak)
plot(fit_aa33911, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (33911)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa8423 <- survfit(formula=Surv(X.2,status)~X8423, data=aapeak)
plot(fit_aa8423, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (8423)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa19589 <- survfit(formula=Surv(X.2,status)~X19589, data=aapeak)
plot(fit_aa19589, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (19589)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa39073 <- survfit(formula=Surv(X.2,status)~X39073, data=aapeak)
plot(fit_aa39073, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (39073)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa25587 <- survfit(formula=Surv(X.2,status)~X25587, data=aapeak)
plot(fit_aa25587, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (25587)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa44971 <- survfit(formula=Surv(X.2,status)~X44971, data=aapeak)
plot(fit_aa44971, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (44971)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa8178 <- survfit(formula=Surv(X.2,status)~X8178, data=aapeak)
plot(fit_aa8178, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (8178)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa4179 <- survfit(formula=Surv(X.2,status)~X4179, data=aapeak)
plot(fit_aa4179, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (4179)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa32767 <- survfit(formula=Surv(X.2,status)~X32767, data=aapeak)
plot(fit_aa32767, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (32767)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa26568 <- survfit(formula=Surv(X.2,status)~X26568, data=aapeak)
plot(fit_aa26568, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (26568)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa26780 <- survfit(formula=Surv(X.2,status)~X26780, data=aapeak)
plot(fit_aa26780, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (26780)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa2663 <- survfit(formula=Surv(X.2,status)~X2663, data=aapeak)
plot(fit_aa2663, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (2663)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa36581 <- survfit(formula=Surv(X.2,status)~X36581, data=aapeak)
plot(fit_aa36581, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (36581)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa12275 <- survfit(formula=Surv(X.2,status)~X12275, data=aapeak)
plot(fit_aa12275, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (12275)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa34490 <- survfit(formula=Surv(X.2,status)~X34490, data=aapeak)
plot(fit_aa34490, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (34490)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa39114 <- survfit(formula=Surv(X.2,status)~X39114, data=aapeak)
plot(fit_aa39114, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (39114)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa10884 <- survfit(formula=Surv(X.2,status)~X10884, data=aapeak)
plot(fit_aa10884, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (10884)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa2093 <- survfit(formula=Surv(X.2,status)~X2093, data=aapeak)
plot(fit_aa2093, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (2093)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa6074 <- survfit(formula=Surv(X.2,status)~X6074, data=aapeak)
plot(fit_aa6074, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (6074)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa18588 <- survfit(formula=Surv(X.2,status)~X18588, data=aapeak)
plot(fit_aa18588, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (18588)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa32724 <- survfit(formula=Surv(X.2,status)~X32724, data=aapeak)
plot(fit_aa32724, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (32724)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))

fit_aa32599 <- survfit(formula=Surv(X.2,status)~X32599, data=aapeak)
plot(fit_aa32599, xlim=c(0,500), ylab="Fraction survived", xlab="Time (days)", lwd=c(2.5,2.5,2.5), col=c(4,3,2), main="AA-cross, peak marker (32599)")
legend(400,.9, legend=c("ll/ll","sl/ll", "sl/sl"), lwd=c(2.5,2.5, 2.5), col=c(4,3,2))
