x <- seq(0, 100, 0.1)

fx <- function(x, a, b) 
  {
  return(ifelse (x < a, 0.98^x, 
         (0.95^x)/b))
}  

y <- list(
  d = fx(x, 100, 2), 
  e = fx(x, 2, 2), 
  f = fx(x, 10, 2),
  g = fx(x, 40, 2), 
  h = fx(x, 60, 2), 
  i = fx(x, 80, 2)
  )

dev.off()

par( mfrow = c(2,3) )
#dev.new(width=5, height=4, unit="cm")
plot(x, y$d, lwd=1, ylim=c(0.0, 1.0), pch=16, type="l", xlab="Time (Age)", ylab="Fraction Alive", main="no drop in survival")
abline(v=18, lty=2, col="red")
polygon(c(x[x>=18], max(x), 18), c(y$d[x>=18], 0, 0), col="#999933", border=NA)

plot(x, y$e, lwd=1, ylim=c(0.0, 1.0), pch=16, type="l", xlab="Time (Age)", ylab="Fraction Alive", main="20% drop at age 2")
abline(v=18, lty=2, col="red")
polygon(c(x[x>=18], max(x), 18), c(y$e[x>=18], 0, 0), col="#999933", border=NA)

plot(x, y$f, lwd=1, ylim=c(0.0, 1.0), pch=16, type="l", xlab="Time (Age)", ylab="Fraction Alive", main="20% drop at age 10")
abline(v=18, lty=2, col="red")
polygon(c(x[x>=18], max(x), 18), c(y$f[x>=18], 0, 0), col="#999933", border=NA)

plot(x, y$g, lwd=1, ylim=c(0.0, 1.0), pch=16, type="l", xlab="Time (Age)", ylab="Fraction Alive", main="20% drop at age 40")
abline(v=18, lty=2, col="red")
polygon(c(x[x>=18], max(x), 18), c(y$g[x>=18], 0, 0), col="#999933", border=NA)

plot(x, y$h, lwd=1, ylim=c(0.0, 1.0), pch=16, type="l", xlab="Time (Age)", ylab="Fraction Alive", main="20% drop at age 60")
abline(v=18, lty=2, col="red")
polygon(c(x[x>=18], max(x), 18), c(y$h[x>=18], 0, 0), col="#999933", border=NA)

plot(x, y$i, lwd=1, ylim=c(0.0, 1.0), pch=16, type="l", xlab="Time (Age)", ylab="Fraction Alive", main="20% drop at age 80")
abline(v=18, lty=2, col="red")
polygon(c(x[x>=18], max(x), 18), c(y$i[x>=18], 0, 0), col="#999933", border=NA)

