# Microbiome hyerarchical clustering - done on Mar 3, 2016

library(MASS)
library(Hmisc)
library(corrplot)
library(pvclust)

setwd("/Volumes/group_dv/personal/DValenzano/papers/uBiome0/spearman data/")
s0 <- read.csv("data_original.csv", head=T)
sn <- read.csv("data_normalized.csv", head=T)
# remove first row and column (not names)
sn2 <- sn[-c(1),]
sn2 <- sn2[, -c(1)]
sn2_cor <- cor(sn2)

# This below provides p-values
sn2_rcor <- rcorr(as.matrix(sn2))
sn2_rcor

corrplot(sn2_cor, type="upper", order="hclust", tl.col="black", tl.srt=45)

col<- colorRampPalette(c("blue", "white", "red"))(20)

heatmap(x = sn2_cor, col = col, symm = TRUE)
# this was done using: 
# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
# check: http://stackoverflow.com/questions/10680658/how-can-i-create-a-correlation-matrix-in-r
# check: http://research.stowers-institute.org/efg/R/Visualization/cor-cluster/

plot( varclus(sn2_cor, similarity="spearman") )
plot( varclus(sn2_cor, similarity="pearson") )

library(pvclust)

#cluster.bootstrap <- pvclust(sn2_cor, nboot=1000, method.dist="abscor")
#plot(cluster.bootstrap)
#pvrect(cluster.bootstrap)

cluster.bootstrap <- pvclust(sn2_cor, nboot=10000,
                             method.dist="correlation")
pdf("HC.pdf", width=10, height=8)
plot(cluster.bootstrap)
pvrect(cluster.bootstrap)
dev.off()


###########################################################################
#### Same as above, on the original data (i.e. not normalized) ############
###########################################################################

s02 <- s0[-c(1),]
s02 <- s02[, -c(1)]
s02_cor <- cor(s02)

# This below provides p-values
s02_rcor <- rcorr(as.matrix(s02))
s02_rcor

corrplot(s02_cor, type="upper", order="hclust", tl.col="black", tl.srt=45)

col<- colorRampPalette(c("blue", "white", "red"))(20)

heatmap(x = s02_cor, col = col, symm = TRUE)

cluster.bootstrap <- pvclust(s02_cor, nboot=10000,
                             method.dist="correlation")
pdf("HC_s0.pdf", width=10, height=8)
plot(cluster.bootstrap)
pvrect(cluster.bootstrap)
dev.off()

###########################################################################
#### Same as above, normalizing differently the original data #############
###########################################################################

sn3 <- s0[-c(1),]
sn3 <- sn3[, -c(1)]
sn3 <- scale(sn3)
sn3_cor <- cor(sn3)

# This below provides p-values
sn3_rcor <- rcorr(as.matrix(sn3))
sn3_rcor

corrplot(sn3_cor, type="upper", order="hclust", tl.col="black", tl.srt=45)

col<- colorRampPalette(c("blue", "white", "red"))(20)

heatmap(x = sn3_cor, col = col, symm = TRUE)

cluster.bootstrap <- pvclust(sn3_cor, nboot=1000,
                             method.dist="correlation")
pdf("HC_s0.pdf", width=10, height=8)
plot(cluster.bootstrap)
pvrect(cluster.bootstrap)
dev.off()

###########################################################################
############################ 10 week OTUs #################################
###########################################################################

wk.10 <- read.csv('/Volumes/group_dv/personal/DValenzano/papers/uBiome0/10wk_table_even4000.csv', head=T, sep=',')
wk.10 <- wk.10[-c(47)]

wk.10 <- wk.10[-c(1),]
wk.10 <- wk.10[, -c(1)]

wk.10[] <- lapply(wk.10, function(x) as.numeric(as.character(x)))


wk.10n <- scale(wk.10)
wk.10_cor <- cor(wk.10n)
wk.10_rcor <- rcorr(as.matrix(wk.10n))
wk.10_rcor

corrplot(wk.10_cor, type="upper", order="hclust", tl.col="black", tl.srt=45)

col<- colorRampPalette(c("blue", "white", "red"))(20)

heatmap(x = wk.10_cor, col = col, symm = TRUE)

cluster.bootstrap <- pvclust(wk.10_cor, nboot=10000,
                             method.dist="correlation")
#pdf("HC_s0.pdf", width=10, height=8)
plot(cluster.bootstrap)
pvrect(cluster.bootstrap)
#dev.off()

###################

wk.10 <- read.csv('/Volumes/group_dv/personal/DValenzano/papers/uBiome0/spearman data/10wk_table_even4000n.csv', head=T)
names(wk.10)

#wk.10 <- wk.10[-c(1),]
wk.10 <- wk.10[, -c(1)]

wk.10_cor <- cor(wk.10)
wk.10_rcor <- rcorr(as.matrix(wk.10))
wk.10_rcor

corrplot(wk.10_cor, type="upper", order="hclust", tl.col="black", tl.srt=45)

col<- colorRampPalette(c("blue", "white", "red"))(20)

heatmap(x = wk.10_cor, col = col, symm = TRUE)

cluster.bootstrap <- pvclust(wk.10_cor, nboot=1000,
                             method.dist="correlation")
pdf("HC_wk10n.pdf", width=10, height=8)
plot(cluster.bootstrap)
pvrect(cluster.bootstrap)
dev.off()

###################
setwd("/Volumes/group_dv/personal/DValenzano/papers/uBiome0/spearman data/")
wk.16 <- read.csv('/Volumes/group_dv/personal/DValenzano/papers/uBiome0/16wk_table_5509n.csv', head=T)
names(wk.16)

#wk.16 <- wk.16[-c(1),]
wk.16 <- wk.16[, -c(1)]

wk.16_cor <- cor(wk.16)
wk.16_rcor <- rcorr(as.matrix(wk.16))
wk.16_rcor

corrplot(wk.16_cor, type="upper", order="hclust", tl.col="black", tl.srt=45)

col<- colorRampPalette(c("blue", "white", "red"))(20)

heatmap(x = wk.16_cor, col = col, symm = TRUE)

cluster.bootstrap <- pvclust(wk.16_cor, nboot=1000,
                             method.dist="correlation")
pdf("HC_wk16n.pdf", width=10, height=8)
plot(cluster.bootstrap)
pvrect(cluster.bootstrap)
dev.off()

