library(qtl)
Go_homo<-read.cross("csv", "~/", "Go_homoF2_converted.csv", genotypes=c("1", "2", "3"))
save(Go_homo, file="Go_homo.Rdata")
