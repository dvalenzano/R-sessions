# Goal: to calculate FDR on survival QTL for cross AAo and Go. This is done both genome-wide and linkage group by linkage group.
# There is a brother python code called 25-Jan-2015.py
load("/Volumes/group_dv/personal/DValenzano/Dec2014/RF/pval4dario.rdata")

ecdf.pval<-ecdf(go.pval)
go.qval<-apply(go.pval,2,function(x){
   x/ecdf.pval(x)
})

write.table(go.pval, file="/Volumes/group_dv/personal/DValenzano/Dec2014/go-pval.csv")
write.table(aao.pval, file="/Volumes/group_dv/personal/DValenzano/Jan2015/aao-pval.csv")

colnames(go.qval) <- colnames(go.pval)
go.qval[go.qval>1]<-1

ecdf.pval<-ecdf(aao.pval)
aao.qval<-apply(aao.pval,2,function(x){
   x/ecdf.pval(x)
})
colnames(aao.qval) <- colnames(aao.pval)
aao.qval[aao.qval>1]<-1

### Now we run the adjusted pvalue method 

lg3p <- read.csv(file="/Volumes/group_dv/personal/DValenzano/Dec2014/RF/go_dayslg3_pval.csv", header=T, sep=',')
p <- lg3p$pval
padj <- p.adjust(p, method="BH")
padj
lg3p$p.adj <- padj

par(mrow=c(1,1))
lg3p$neg.logp <- (-1)*log10(lg3p$pval)
plot(lg3p$cM, lg3p$neg.logp, lwd=2.5, ylim=c(0,10), pch=19, ylab="-log(p value)", xlab='cM', main="Survival QTL, Cross G, LG3")

write.table(lg3p, file="/Volumes/group_dv/personal/DValenzano/Dec2014/RF/lg3_pval.csv")

### 25-Jan-2015 ## NOW WE CALCULATE THE ADJ. PVALS GENOME-WIDE ########

lgp <- read.csv(file="/Volumes/group_dv/personal/DValenzano/Dec2014/RF/go_days_pval.csv", header=T, sep=',')
gop <- lgp$pval
gopadj <- p.adjust(gop, method="BH")
gopadj
min(gopadj)
lgp$p.adj <- gopadj
write.table(lgp, file="/Volumes/group_dv/personal/DValenzano/Jan2015/go_days_padj.csv")

# here there is a python intermezzo

gop2 <- read.csv(file="/Volumes/group_dv/personal/DValenzano/Jan2015/go_pq.csv", header=T, sep=',')
gop2[1:10,]

gop2_lg1 <- subset(gop2, LG=='1')
gop2_lg1$p.adj_lg <- p.adjust(gop2_lg1$pval, method="BH")

gop2_lg2 <- subset(gop2, LG=='2')
gop2_lg2$p.adj_lg <- p.adjust(gop2_lg2$pval, method="BH")

gop2_lg3 <- subset(gop2, LG=='3')
gop2_lg3$p.adj_lg <- p.adjust(gop2_lg3$pval, method="BH")

gop2_lg4 <- subset(gop2, LG=='4')
gop2_lg4$p.adj_lg <- p.adjust(gop2_lg4$pval, method="BH")

gop2_lg5 <- subset(gop2, LG=='5')
gop2_lg5$p.adj_lg <- p.adjust(gop2_lg5$pval, method="BH")

gop2_lg6 <- subset(gop2, LG=='6')
gop2_lg6$p.adj_lg <- p.adjust(gop2_lg6$pval, method="BH")

gop2_lg7 <- subset(gop2, LG=='7')
gop2_lg7$p.adj_lg <- p.adjust(gop2_lg7$pval, method="BH")

gop2_lg8 <- subset(gop2, LG=='8')
gop2_lg8$p.adj_lg <- p.adjust(gop2_lg8$pval, method="BH")

gop2_lg9 <- subset(gop2, LG=='9')
gop2_lg9$p.adj_lg <- p.adjust(gop2_lg9$pval, method="BH")

gop2_lg10 <- subset(gop2, LG=='10')
gop2_lg10$p.adj_lg <- p.adjust(gop2_lg10$pval, method="BH")

gop2_lg11 <- subset(gop2, LG=='11')
gop2_lg11$p.adj_lg <- p.adjust(gop2_lg11$pval, method="BH")

gop2_lg12 <- subset(gop2, LG=='12')
gop2_lg12$p.adj_lg <- p.adjust(gop2_lg12$pval, method="BH")

gop2_lg13 <- subset(gop2, LG=='13')
gop2_lg13$p.adj_lg <- p.adjust(gop2_lg13$pval, method="BH")

gop2_lg14 <- subset(gop2, LG=='14')
gop2_lg14$p.adj_lg <- p.adjust(gop2_lg14$pval, method="BH")

gop2_lg15 <- subset(gop2, LG=='15')
gop2_lg15$p.adj_lg <- p.adjust(gop2_lg15$pval, method="BH")

gop2_lg16 <- subset(gop2, LG=='16')
gop2_lg16$p.adj_lg <- p.adjust(gop2_lg16$pval, method="BH")

gop2_lg17 <- subset(gop2, LG=='17')
gop2_lg17$p.adj_lg <- p.adjust(gop2_lg17$pval, method="BH")

gop2_lg18 <- subset(gop2, LG=='18')
gop2_lg18$p.adj_lg <- p.adjust(gop2_lg18$pval, method="BH")

gop2_lg19 <- subset(gop2, LG=='19')
gop2_lg19$p.adj_lg <- p.adjust(gop2_lg19$pval, method="BH")

gop2_lg <- c(gop2_lg1$p.adj_lg,gop2_lg2$p.adj_lg,gop2_lg3$p.adj_lg,gop2_lg4$p.adj_lg,gop2_lg5$p.adj_lg,gop2_lg6$p.adj_lg,gop2_lg7$p.adj_lg,gop2_lg8$p.adj_lg,gop2_lg9$p.adj_lg,
             gop2_lg10$p.adj_lg,gop2_lg11$p.adj_lg,gop2_lg12$p.adj_lg,gop2_lg13$p.adj_lg,gop2_lg14$p.adj_lg,gop2_lg15$p.adj_lg,gop2_lg16$p.adj_lg,gop2_lg17$p.adj_lg,
             gop2_lg18$p.adj_lg,gop2_lg19$p.adj_lg)

gop2$p.adg_lg <- gop2_lg
gop2[1:20,]
min(gop2$p.adg_lg)

write.table(gop2, file="/Volumes/group_dv/personal/DValenzano/Jan2015/go_pq2.csv")


##### 25-Jan-2015 ###### CALCULATE FDR IN CROSS AA #####

laap <- read.csv(file="/Volumes/group_dv/personal/DValenzano/Jan2015/aao_days_pval.csv", header=T, sep=',')
aaop <- laap$pval
aaopadj <- p.adjust(aaop, method="BH")
aaopadj
min(aaopadj)
laap$p.adj <- aaopadj
write.table(laap, file="/Volumes/group_dv/personal/DValenzano/Jan2015/aao_days_padj.csv")

### Now I calculate the corrected p lg by lg ####

aap2 <- read.csv(file="/Volumes/group_dv/personal/DValenzano/Jan2015/aao_pq.csv", header=T, sep=',')
aap2[1:10,]

aap2_lg1 <- subset(aap2, LG=='1')
aap2_lg1$p.adj_lg <- p.adjust(aap2_lg1$pval, method="BH")

aap2_lg2 <- subset(aap2, LG=='2')
aap2_lg2$p.adj_lg <- p.adjust(aap2_lg2$pval, method="BH")

aap2_lg3 <- subset(aap2, LG=='3')
aap2_lg3$p.adj_lg <- p.adjust(aap2_lg3$pval, method="BH")

aap2_lg4 <- subset(aap2, LG=='4')
aap2_lg4$p.adj_lg <- p.adjust(aap2_lg4$pval, method="BH")

aap2_lg5 <- subset(aap2, LG=='5')
aap2_lg5$p.adj_lg <- p.adjust(aap2_lg5$pval, method="BH")

aap2_lg6 <- subset(aap2, LG=='6')
aap2_lg6$p.adj_lg <- p.adjust(aap2_lg6$pval, method="BH")

aap2_lg7 <- subset(aap2, LG=='7')
aap2_lg7$p.adj_lg <- p.adjust(aap2_lg7$pval, method="BH")

aap2_lg8 <- subset(aap2, LG=='8')
aap2_lg8$p.adj_lg <- p.adjust(aap2_lg8$pval, method="BH")

aap2_lg9 <- subset(aap2, LG=='9')
aap2_lg9$p.adj_lg <- p.adjust(aap2_lg9$pval, method="BH")

aap2_lg10 <- subset(aap2, LG=='10')
aap2_lg10$p.adj_lg <- p.adjust(aap2_lg10$pval, method="BH")

aap2_lg11 <- subset(aap2, LG=='11')
aap2_lg11$p.adj_lg <- p.adjust(aap2_lg11$pval, method="BH")

aap2_lg12 <- subset(aap2, LG=='12')
aap2_lg12$p.adj_lg <- p.adjust(aap2_lg12$pval, method="BH")

aap2_lg13 <- subset(aap2, LG=='13')
aap2_lg13$p.adj_lg <- p.adjust(aap2_lg13$pval, method="BH")

aap2_lg14 <- subset(aap2, LG=='14')
aap2_lg14$p.adj_lg <- p.adjust(aap2_lg14$pval, method="BH")

aap2_lg15 <- subset(aap2, LG=='15')
aap2_lg15$p.adj_lg <- p.adjust(aap2_lg15$pval, method="BH")

aap2_lg16 <- subset(aap2, LG=='16')
aap2_lg16$p.adj_lg <- p.adjust(aap2_lg16$pval, method="BH")

aap2_lg17 <- subset(aap2, LG=='17')
aap2_lg17$p.adj_lg <- p.adjust(aap2_lg17$pval, method="BH")

aap2_lg18 <- subset(aap2, LG=='18')
aap2_lg18$p.adj_lg <- p.adjust(aap2_lg18$pval, method="BH")

aap2_lg19 <- subset(aap2, LG=='19')
aap2_lg19$p.adj_lg <- p.adjust(aap2_lg19$pval, method="BH")

aap2_lg <- c(aap2_lg1$p.adj_lg,aap2_lg2$p.adj_lg,aap2_lg3$p.adj_lg,aap2_lg4$p.adj_lg,aap2_lg5$p.adj_lg,aap2_lg6$p.adj_lg,aap2_lg7$p.adj_lg,aap2_lg8$p.adj_lg,aap2_lg9$p.adj_lg,
             aap2_lg10$p.adj_lg,aap2_lg11$p.adj_lg,aap2_lg12$p.adj_lg,aap2_lg13$p.adj_lg,aap2_lg14$p.adj_lg,aap2_lg15$p.adj_lg,aap2_lg16$p.adj_lg,aap2_lg17$p.adj_lg,
             aap2_lg18$p.adj_lg,aap2_lg19$p.adj_lg)

aap2$p.adg_lg <- aap2_lg
aap2[1:20,]
min(aap2$p.adg_lg)

write.table(aap2, file="/Volumes/group_dv/personal/DValenzano/Jan2015/aao_pq2.csv")


################
