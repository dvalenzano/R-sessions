##GOAL: To generate the simulation_arXiv figures relative to S, R throughout the simulation, standard deviation and s_i, r_i
setwd('/Volumes/group_dv/personal/DValenzano/papers/simulation_arXiv/Figure3/')

#### First, one panel at the time ####

abcdz <- read.csv('./first-run/first2runs.csv', sep=',', head=T)
xabcdz <- c(1:(length(abcdz$het)/5))
abcdz0 <- subset(abcdz, group == 0)
abcdz1 <- subset(abcdz, group == 1)
abcdz5k <- subset(abcdz, group == 3)
abcdz10k <- subset(abcdz, group == 6)
abcdz60k <- subset(abcdz, group == "z")

x2 <- c(1:71, 16:70)

# Alternatively 

#x2.1 <- rep(x2, 4)
#abcd2 <- abcd
#abcd2$x <- x2.1
#abcd2.0 <- subset(abcd2, group == 0)
#abcd2.1 <- subset(abcd2, group == 1)
#abcd2.5k <- subset(abcd2, group == 3)
#abcd2.10k <- subset(abcd2, group == 6)

## AS A SCATTERPLOT ##
plot(xabcdz, abcd0$het, ylim = c(0, 1), ylab = "Frequency of 1s / S/R_i", xlab = "Age")
points(xabcdz, abcd1$het, col=2)
points(xabcdz, abcd5k$het, col=3)
points(xabcdz, abcd10k$het, col=4)
points(xabcdz, abcd60k$het, col=5)

## AS A LINE PLOT ##
plot(xabcdz, abcdz0$het, ylim = c(0, 1), ylab = "Frequency of 1s / S/R_i", xlab = "Age", type="l", lwd=3, col="gray48", xaxt="n", bty="n")
axis(1, at=1:126, labels=x2, tick=T, lwd.ticks=0)
lines(xabcdz, abcdz1$het, col=2, lwd=3)
lines(xabcdz, abcdz5k$het, col=3, lwd =3)
lines(xabcdz, abcdz10k$het, col=4, lwd=3)
lines(xabcdz, abcdz60k$het, col=5, lwd=3)
abline(v=16, lwd=2, lty=2)
abline(v=72, lwd=4, lty=1)
#legendxy<-x(0,-2)
#legend(legendxy,  legend=c("1", "1.3k", "5k", "10k"), col=c("gray48", 2,3,4), lty=1, lwd=3, bty="n")
#legend("topright",  c("1", "1.3k", "5k", "10k", "60k"), col=c("gray48", 2,3,4,5), lty=1, lwd=3, bty="n")

#nf <- layout(matrix(c(1,1,1,2,1,1,1,0), 2,4, byrow=TRUE), widths=lcm(4), heights=lcm(4))
#layout.show(nf)

#dev.off()
#plot(xabcd, abcd0$het, ylim = c(0, 1), ylab = "Frequency of 1s / S/R_i", xlab = "Age", type="l", lwd=3, col="gray48", xaxt="n", bty="n")
#axis(1, at=1:126, labels=x2, tick=T, lwd.ticks=0)
#lines(xabcd, abcd1$het, col=2, lwd=3)
#lines(xabcd, abcd5k$het, col=3, lwd =3)
#lines(xabcd, abcd10k$het, col=4, lwd=3)
#abline(v=16, lwd=2, lty=2)
#abline(v=72, lwd=2, lty=2)
#plot.new()
#legend(x="top", c("1", "1.3k", "5k", "10k"), col=c("gray48", 2,3,4), lty=1, lwd=3, bty="n")

###### PLOTTING STANDARD DEVIATION ######
sd_abcdz <- read.csv('./het-sd.csv', sep=',', head=T)
sd_xabcdz <- c(1:(length(sd_abcdz$het)/5))
sd_abcdz0 <- subset(sd_abcdz, group == 0)
sd_abcdz1 <- subset(sd_abcdz, group == 1)
sd_abcdz5k <- subset(sd_abcdz, group == 3)
sd_abcdz10k <- subset(sd_abcdz, group == 6)
sd_abcdz60k <- subset(sd_abcdz, group == "z")

plot(sd_xabcdz, sd_abcdz0$het, ylim = c(0.75, 1.2), ylab = "Genetic Variance", xlab = "Age", type="l", lwd=3, col="gray48", xaxt="n", bty="n")
axis(1, at=1:126, labels=x2, tick=T, lwd.ticks=0)
lines(sd_xabcdz, sd_abcdz1$het, col=2, lwd=3)
lines(sd_xabcdz, sd_abcdz5k$het, col=3, lwd =3)
lines(sd_xabcdz, sd_abcdz10k$het, col=4, lwd=3)
lines(sd_xabcdz, sd_abcdz60k$het, col=5, lwd=3)
abline(v=16, lwd=2, lty=2)
abline(v=72, lwd=4, lty=1)

########## FIGURE 3 ###########
dev.off()
pdf("Figure3.pdf", width=4, height=3.2)
fig3 <- read.csv('sc_25k.csv', sep=',', header=F)
x <- c(1:5000)
y1 <- fig3[1:(length(fig3)-20001)]
y2 <- rep(5000, 5000)
plot(x,y1, type = 'l', col="blue", lwd = 1, xlab="Stage", ylab="N", bty="n", ylim=c(0,6000), pin=c(3,2))
lines(x, y2, col="red", lwd=2)
dev.off()
#legend(2000, 3000, c("Population", "Resources"), col=c("blue", "red"), lwd=c(2.5, 2.5), lty=c(1,1), bty="n")
#################################


###### HERE I COMBINE SURV, REPR VALUES TO VARIANCE #######
pdf("prova0.pdf", width=3.5, height=2.9)
plot(xabcdz, abcdz0$het, ylim = c(0, 1), ylab = "Frequency of 1s / S/R_i", xlab = "Age", type="l", lwd=1.5, col="gray48", xaxt="n", bty="n")
axis(1, at=1:126, labels=x2, tick=T, lwd.ticks=0)
lines(xabcdz, abcdz1$het, col=2, lwd=1.5)
lines(xabcdz, abcdz5k$het, col=3, lwd =1.5)
lines(xabcdz, abcdz10k$het, col=4, lwd=1.5)
lines(xabcdz, abcdz60k$het, col=5, lwd=1.5)
abline(v=16, lwd=1, lty=2)
abline(v=72, lwd=2, lty=1)
dev.off()

### Alternative variance plot, with only variance at the 60k stage
pdf(file="prova.pdf", width=3.5, height=2.9)
plot(sd_xabcdz, sd_abcdz60k$het, ylim = c(0.75, 1.2), ylab = "Genetic Variance", xlab = "Age", type="l", lwd=1.5, xaxt="n", bty="n")
axis(1, at=1:126, labels=x2, tick=T, lwd.ticks=0)
abline(v=16, lwd=1, lty=2)
abline(v=72, lwd=2, lty=1)
dev.off()


###### HERE I PLOT RATE OF SURVIVAL AND REPRODUCTION #######
rate <- read.csv('../Figure5/rate_surv-repr.csv', sep=',', head=T)
rate_x <- c(1:(length(rate$rate)/4))
rate_s <- subset(rate, group == "s")
rate_sc <- subset(rate, group == "sc")
rate_r <- subset(rate, group == "r")
rate_r[rate_r$rate == 0]<- NA
rate_rc <- subset(rate, group == "rc")

x3<- c(1:71)

# Survival
pdf(file="../Figure5/rate-repr_surv.pdf", width=7, height=2.9)
par(mfrow=c(1,2))
plot(rate_x, rate_s$rate, ylim = c(0.98, 1), ylab = "Frequency of 1s", xlab = "Age", type="l", lwd=1.5, bty="n")
#axis(1, at=1:71, labels=x3, lwd.ticks=0)
lines(rate_x, rate_sc$rate, col=2, lwd=1.5)
abline(v=16, lwd=1.5, lty=2)
#dev.off()

# Reproduction
#pdf(file="../Figure5/rate-repr.pdf", width=3.5, height=2.9)
plot(rate_x, rate_r$rate, ylim = c(0, 0.4), ylab = "Frequency of 1s", xlab = "Age", type="l", lwd=1.5, bty="n")
lines(rate_x, rate_rc$rate, col=2, lwd=1.5)
abline(v=16, lwd=1.5, lty=2)
dev.off()
