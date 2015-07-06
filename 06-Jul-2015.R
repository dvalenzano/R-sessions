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

nf <- layout(matrix(c(1,1,1,2,1,1,1,0), 2,4, byrow=TRUE), widths=lcm(4), heights=lcm(4))
layout.show(nf)

dev.off()
plot(xabcd, abcd0$het, ylim = c(0, 1), ylab = "Frequency of 1s / S/R_i", xlab = "Age", type="l", lwd=3, col="gray48", xaxt="n", bty="n")
axis(1, at=1:126, labels=x2, tick=T, lwd.ticks=0)
lines(xabcd, abcd1$het, col=2, lwd=3)
lines(xabcd, abcd5k$het, col=3, lwd =3)
lines(xabcd, abcd10k$het, col=4, lwd=3)
abline(v=16, lwd=2, lty=2)
abline(v=72, lwd=2, lty=2)
#plot.new()
legend(x="top", c("1", "1.3k", "5k", "10k"), col=c("gray48", 2,3,4), lty=1, lwd=3, bty="n")


########## FIGURE 4A ###########
fig4a <- read.csv('sc_25k.csv', sep=',', header=F)
x <- c(1:5000)
y1 <- fig3a[1:(length(fig3a)-20001)]
y2 <- rep(5000, 5000)
plot(x,y1, type = 'l', col="blue", lwd = 1, xlab="Stage", ylab="", bty="n", ylim=c(0,6000), pin=c(3,2))
lines(x, y2, col="red", lwd=2)
legend(2000, 3000, c("Population", "Resources"), col=c("blue", "red"), lwd=c(2.5, 2.5), lty=c(1,1), bty="n")
#################################

########## FIGURE 3B ##########
