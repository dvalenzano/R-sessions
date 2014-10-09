lg3c = read.csv('/Volumes/group_dv/personal/DValenzano/Oct2014/LG3merge/lg3c_rinput.csv',  sep=',', header=TRUE)
#rf = subset(lg3c, lg3c$group=='rf')
#plot(rf$neg_logpq, ylim=c(0,6))

names(lg3c)
# [1] "Marker"    "cM"        "neg_logpq" "group"

glg3c <- qplot(cM,neg_logpq, data=lg3c, xlab="cM", ylab="-log(q)", colour=group, main="LG3 consensus map", ylim=c(0,25))
dev.new() 
glg3c+scale_colour_brewer(palette="Set1")


f7m <- subset(lg3c, lg3c$group=='f7m')
f7f <- subset(lg3c, lg3c$group=='f7f')
f14ef <- subset(lg3c, lg3c$group=='f14ef')
rqtl <- subset(lg3c, lg3c$group=='rf')

#plot(f7m$cM, f7m$neg_logpq, ylim=c(0,10))
#plot(f7f$cM, f7f$neg_logpq, ylim=c(0,10))
#plot(f14ef$cM, f14ef$neg_logpq, ylim=c(0,10))
#plot(rqtl$cM, rqtl$neg_logpq, ylim=c(0,10))


############NOW I PLOT SEX AND SURVIVAL ON THE CONSOLIDATED LG3########

lg3c_ss = read.csv('/Volumes/group_dv/personal/DValenzano/Oct2014/LG3merge/lg3c_rinput_sex-surv.csv',  sep=',', header=TRUE)
names(lg3c_ss)
lg3c_ss[1:50,]

sx <- subset(lg3c_ss, lg3c_ss$group=='sx')
all <- subset(lg3c_ss, lg3c_ss$group=='all')
m <- subset(lg3c_ss, lg3c_ss$group=='m')
f <- subset(lg3c_ss, lg3c_ss$group=='f')
Res <- subset(lg3c_ss, lg3c_ss$group=='Res')

sa <- rbind(sx, all)
sm <- rbind(sx, m)
sf <- rbind(sx, f)
sRes <- rbind(sx, Res)

psa <- qplot(cM,neg_logpq, data=sa, xlab="cM", ylab="-log(q)", colour=group, main="LG3 consensus map, sex and survival QTL", ylim=c(0,5))
dev.new() 
psa+scale_colour_brewer(palette="Set1")

psm <- qplot(cM,neg_logpq, data=sm, xlab="cM", ylab="-log(q)", colour=group, main="LG3 consensus map, sex and survival QTL", ylim=c(0,5))
dev.new() 
psm+scale_colour_brewer(palette="Set1")

psf <- qplot(cM,neg_logpq, data=sf, xlab="cM", ylab="-log(q)", colour=group, main="LG3 consensus map, sex and survival QTL", ylim=c(0,5))
dev.new() 
psf+scale_colour_brewer(palette="Set1")

psR <- qplot(cM,neg_logpq, data=sRes, xlab="cM", ylab="-log(q)", colour=group, main="LG3 consensus map, sex and survival QTL", ylim=c(0,5))
dev.new() 
psR+scale_colour_brewer(palette="Set1")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
cbPalette2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#9999CC")

glg3css <- qplot(cM,neg_logpq, data=lg3c_ss, xlab="cM", ylab="-log(q)", colour=group, main="LG3 consensus map", ylim=c(0,5))
dev.new() 
glg3css+scale_colour_brewer(palette="Spectral")
#glg3css+geom_point() + scale_colour_manual(values=cbPalette)



lg3c_ss[1:50,]

