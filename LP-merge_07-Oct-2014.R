library(LPmerge)

setwd(dir="/Volumes/group_dv/personal/DValenzano/Oct2014/LG3merge/")

g7m <- read.csv('/Volumes/group_dv/personal/DValenzano/Oct2014/LG3merge/LG3_f7m.csv', sep=",", header=TRUE)
g7f <- read.csv('/Volumes/group_dv/personal/DValenzano/Oct2014/LG3merge/LG3_f7f.csv', sep=",", header=TRUE)
g14ef <- read.csv('/Volumes/group_dv/personal/DValenzano/Oct2014/LG3merge/LG3_f14ef.csv', sep=",", header=TRUE)
grqtl <- read.csv('/Volumes/group_dv/personal/DValenzano/Oct2014/LG3merge/LG3_rqtl.csv', sep=",", header=TRUE)

maps <- list(g7m, g7f, g14ef, grqtl)

g7m_df <- data.frame(maps[[1]]$Marker, maps[[1]]$cM)
g7f_df <- data.frame(maps[[2]]$Marker, maps[[2]]$cM)
g14ef_df <- data.frame(maps[[3]]$Marker, maps[[3]]$cM)
grqtl_df <- data.frame(maps[[4]]$marker, maps[[4]]$cM)

colnames(g7m_df)[1] <- "marker"
colnames(g7m_df)[2] <- "cM"
colnames(g7f_df)[1] <- "marker"
colnames(g7f_df)[2] <- "cM"
colnames(g14ef_df)[1] <- "marker"
colnames(g14ef_df)[2] <- "cM"
colnames(grqtl_df)[1] <- "marker"
colnames(grqtl_df)[2] <- "cM"

maps_2 <- list(g7m_df, g7f_df, g14ef_df, grqtl_df)
names(maps_2)<- c("g7m_df", "g7f_df", "g14ef_df", "grqtl_df")
str(maps_2)

print(link.map.lengths <- unlist(lapply(maps_2, function(x){max(x$cM)})))

pop.size <- c(47, 19, 21, 202)

weighted <- LPmerge(maps_2,max.interval=1:20,weights=pop.size)

# Looks like the best is the number 24, with a mean 34.71, sd 15.95, and Consensus map length of 258.06, which is close to that of the two reference maps
weighted[[12]]
write.table(weighted[[12]], "LG3_om_rf_consensus.csv")
