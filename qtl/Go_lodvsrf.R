#this scripts plots the inverted mustache graph of cross Go 

library(qtl)
load("~/Go_estRf.Rdata")
rf<-pull.rf(Go_estRf)
lod<-pull.rf(Go_estRf, what="lod")
jpeg('Go_lodvsrfnodup.jpg')
plot(as.numeric(rf), as.numeric(lod), xlab="Recombination fraction", ylab="lod score")
dev.off()


