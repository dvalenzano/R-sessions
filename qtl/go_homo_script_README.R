> library(qtl)
> load("~/2014-01-06_F2_lm/Go_homo.Rdata") #this was made from Go_homoF2_converted.csv in go_homo_readcross.R.  Go_homoF2_converted.csv was made from NfGo_pg.csv, which dario sent me on 1/3/14.
plot.missing(Go_homo)

summary(Go_homo)
    F2 intercross

    No. individuals:    207 

    No. phenotypes:     10 
    Percent phenotyped: 100 100 100 
                        100 100 
                        68.1 68.1 
                        85.5 100 
                        8.7 

    No. chromosomes:    1 
        Autosomes:      1 

    Total markers:      6013 
    No. markers:        6013 
    Percent genotyped:  75.7 
    Genotypes (%):      AA:25.3 
                        AB:49.6 
                        BB:25 
                        not BB:0 
                        not AA:0 
Warning message:
In summary.cross(Go_homo) :
  Some chromosomes > 1000 cM in length; there may be a problem with the genetic map.
  (Perhaps it is in basepairs?)

### make plots of this data. saved as go_homo_b4_cleaning.png
par(mfrow=c(1,2), las=1)
plot(ntyped(Go_homo), ylab="No. typed markers", main="No. genotypes by individual")
plot(ntyped(Go_homo, "mar"), ylab="No. typed individuals",  main="No. genotypes by marker")

# omit markers w/ lots of missing data
nt.bymar<-ntyped(Go_homo, "mar")
todrop<-names(nt.bymar[nt.bymar<21]) #use 10% cutoff like Dario did in AAn_script.R
Go_homo<-drop.markers(Go_homo, todrop)

### this shows that there are some individuals with <1000 typed markers

#now I omit the individuals with few or no genotypes:
Go_homo <- subset(Go_homo, ind=(ntyped(Go_homo)>1500))

> summary(Go_homo)
    F2 intercross

    No. individuals:    197 

    No. phenotypes:     10 
    Percent phenotyped: 100 100 100 100 100 69 69 87.3 
                        100 8.6 

    No. chromosomes:    1 
        Autosomes:      1 

    Total markers:      5986 
    No. markers:        5986 
    Percent genotyped:  79.8 
    Genotypes (%):      AA:25.3  AB:49.6  BB:25 
                        not BB:0  not AA:0 
Warning message:
In summary.cross(Go_homo) :
  Some chromosomes > 1000 cM in length; there may be a problem with the genetic map.
  (Perhaps it is in basepairs?)


#Histogram of proportion of markers for which pairs of individuals have matching genotypes
par(mfrow=c(1,1))
cg <- comparegeno(Go_homo)
hist(cg[lower.tri(cg)], breaks=seq(0,1,len=101), xlab="no. matching genotypes", main = "Cross AAo: distribution of matching genotypes")
rug(cg[lower.tri(cg)])

#saved as go_homo_distrib_of_matching_geno.png
#there are some markers w/ >0.8

#### look for duplicate markers
dupmar<-findDupMarkers(Go_homo, exact.only=FALSE)
dupmar
$`26467`
[1] "6493"

$`14030`
[1] "5619"

$`12263`
[1] "14731"

$`11137`
[1] "40773"
lapply(dupmar, write, "Go_dupmar.txt", append=TRUE) #writes names of duplicated markers to file
Go_homo_nodup<-drop.markers(Go_homo, unlist(dupmar)) #unlist(dup) creates vector with just the names of duplicate markers. this dropped 4 markers

save(Go_homo_nodup, file="Go_homo_nodup.Rdata")


#Recheck histogram of proportion of markers for which pairs of individuals have matching genotypes
par(mfrow=c(1,1))
cg <- comparegeno(Go_homo_nodup)
hist(cg[lower.tri(cg)], breaks=seq(0,1,len=101), xlab="no. matching genotypes", main = "Cross AAo: distribution of matching genotypes")
rug(cg[lower.tri(cg)])

# RESULT: it's the same

#check how many pairs of individuals have >80% matching genotypes. there are 6 pairs. 
wh<-which(cg>0.8, arr=TRUE)
wh<-wh[wh[,1]<wh[,2],]
wh
     row col
[1,]   1   2
[2,]   1   4
[3,]   2   4
[4,]   1  16
[5,]   2  16
[6,]   4  16

# here's how they look like:
> table(g[1,], g[2,])
   
      1   2   3
  1   3  22   3
  2  24 652  23
  3   1  21   1
> table(g[1,], g[4,])
   
       1    2    3
  1    8   81    4
  2   40 1817   17
  3    2   65    2
> table(g[2,], g[4,])
   
       1    2    3
  1    3   53    1
  2   40 1360    9
  3    1   53    1
> table(g[1,], g[16,])
   
       1    2    3
  1   11   96    4
  2  102 1856  161
  3    4   72    4
> table(g[2,], g[16,])
   
       1    2    3
  1    6   54    5
  2   76 1355  119
  3    4   56    3
> table(g[4,], g[16,])
   
       1    2    3
  1   32  115    8
  2  212 4028  351
  3    1   46    2

#### it turns out that individual 2 is an F1 (G027)

## > pull.pheno(Go_homo_nodup, "Catalog.ID")
  [1] G016 G027 G037 G038 G082 G083
  [7] G090 G102 G105 G110 G112 G113
 [13] G116 G118 G121 G125 G130 G136
1: g016: female fam 1
2: G027: IS AN F1 !!!! 
 4: g038: male fam 1, y tail
  16: g125 male fam 5, red tail

#checked in the G cross spreadsheet on googledocs that dario shared with me and these 3 aren't F1s


#### will drop individual 2, but won't drop the other individuals. Individuals 1 and 4 were from the same family, so it is not unreasonable that they share a high amount of matching genotypes. #### EDIT::::: REFER TO BELOW. I ACTUALLY DO END UP DROPPING ALL OF THESE INDIVIDUALS

Go_homo_nodup.2<-subset(Go_homo_nodup, ind=-2)
pull.pheno(Go_homo_nodup.2, "Catalog.ID")

#CHECK THAT IT WORKED. IT DID
 [1] G016 G037 G038 G082 G083 G090
  [7] G102 G105 G110 G112 G113 G116

> summary(Go_homo_nodup.2)
    F2 intercross

    No. individuals:    196 

#Recheck histogram of proportion of markers for which pairs of individuals have matching genotypes
par(mfrow=c(1,1))
cg <- comparegeno(Go_homo_nodup.2)
hist(cg[lower.tri(cg)], breaks=seq(0,1,len=101), xlab="no. matching genotypes", main = "Cross AAo: distribution of matching genotypes")
rug(cg[lower.tri(cg)])
# This only leaves 3 pairs of individuals w/ matching genotypes>0.8, as expected. saved as Go_homo_nodup.2_matchinggeno.png


###### look for markers w/ distorted segregation patterns

gt<-geno.table(Go_homo_nodup.2)
gt[gt$P.value < 0.05/totmar(Go_homo_nodup.2),]
# remove markers w/ p<1e-10
todrop<-rownames(gt[gt$P.value<1e-10,])
Go_homo_nodup.2<-drop.markers(Go_homo_nodup.2, todrop)

#this leaves us with Total markers:      5757, down from 5982

#genotype frequencies by individual
Go <- pull.geno(Go_homo_nodup.2)
Gofreq <- apply(Go, 1, function(a) table(factor(a, levels=1:3)))
Gofreq <- t(t(Gofreq) / colSums(Gofreq))
par(mfrow=c(1,3), las=1)
for(i in 1:3)
  plot(Gofreq[i,], ylab="Genotype frequency", main=c("AA", "AB", "BB")[i], ylim=c(0,1))

t <- pull.geno(test)
tfreq <- apply(t, 1, function(a) table(factor(a, levels=1:3)))
tfreq <- t(t(tfreq) / colSums(tfreq))
par(mfrow=c(1,3), las=1)
for(i in 1:3)
  plot(tfreq[i,], ylab="Genotype frequency", main=c("AA", "AB", "BB")[i], ylim=c(0,1))
#saved as Go_homo_nodup.2_genofreq.png

# i have a feeling these individuals w/ AB>0.85 may be the markers from previous that had matching genotypes. So I subset them into their own cross object and plot their genotpye frequencies to see if they give AB>0.85, since I don't know how else to identify from the plot Go_homo_nodup.2_genofreq.png what is the catalog IDs of these outlier individuals.

> test<-subset(Go_homo_nodup.2, ind=c(1,3,15)) # it is 3 and 15 instead of 4 and 16 since we've dropped individual 2
> pull.pheno(test, "Catalog.ID")
[1] G016 G038 G125
207 Levels: G016 G027 G037 G038 G041 ... G457

#plot the genotypes proportions
> t <- pull.geno(test)
> tfreq <- apply(t, 1, function(a) table(factor(a, levels=1:3)))
> tfreq <- t(t(tfreq) / colSums(tfreq))
> par(mfrow=c(1,3), las=1)
> for(i in 1:3)
+     plot(tfreq[i,], ylab="Genotype frequency", main=c("AA", "AB", "BB")[i], ylim=c(0,1))
#saved as go_test_result.png. THESE ARE INDEED THE OUTLIERS!

#here i drop individuals 1,3, 15 from Go_homo_nodup.2
Go_homo_nodup.3<-subset(Go_homo_nodup.2, ind=-c(1,3,15))
                        
pull.pheno(Go_homo_nodup.3, "Catalog.ID")

summary(Go_homo_nodup.3)
 F2 intercross

    No. individuals:    193 

    No. phenotypes:     10 
    Percent phenotyped: 100 100 100 100 100 69.4 
                        69.4 88.1 100 8.8 

    No. chromosomes:    1 
        Autosomes:      1 

    Total markers:      5757 
    No. markers:        5757 
    Percent genotyped:  80.8 
    Genotypes (%):      AA:24.6  AB:49.6  BB:25.7 
                        not BB:0  not AA:0 

# replot genotype frequencies. they look better now. saved as Go_homo_nodup.3.png 
Go <- pull.geno(Go_homo_nodup.3)
Gofreq <- apply(Go, 1, function(a) table(factor(a, levels=1:3)))
Gofreq <- t(t(Gofreq) / colSums(Gofreq))
par(mfrow=c(1,3), las=1)
for(i in 1:3)
  plot(Gofreq[i,], ylab="Genotype frequency", main=c("AA", "AB", "BB")[i], ylim=c(0,1))

##### SAVE THIS CROSS OBJECT #######
> save(Go_homo_nodup.3, file="Go_homo_nodup.3.Rdata")

######## EST.RF via Go_estRf.sh #########
####### output: Go_estRf.Rdata
# plot lod vs rf
# script in Go_lodvsrf.R

#check for switched alleles
> checkAlleles(Go_estRf, threshold=5)
No apparent problems.

########## FORM LINKAGE GROUPS (ON CLUSTER): ############

#the below parameters for max.rf and min.lod are similar to what we used for Gn (max.rf=0.1, min.lod=7), as shown in G2_01_script.R.
 lg<-formLinkageGroups(Go_estRf, max.rf=0.1, min.lod=6)
> table(lg[,2])

  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
416 408 370 370 366 357 352 343 338 290 278 274 273 268 245 223 175 166 145  29 
 21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40 
 20   8   3   2   2   2   2   1   1   1   1   1   1   1   1   1   1   1   1   1 
 41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59 
  1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1

 lg<-formLinkageGroups(Go_estRf, max.rf=0.12, min.lod=7) #will go with this one b/c it gives 19 LGs. it may be better to go with the other parameters b/c "it's better to combine linkage groups after the fact rather than to have to split linkage groups apart", but for each linkage group there aren't too many more markers in contrast to the number of markers on each LG in the other parameters
> table(lg[,2])

  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
417 408 387 379 370 359 353 345 338 309 290 274 273 268 245 225 176 165 150   2 
 21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40 
  1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
 41  42  43  44 
  1   1   1   1 

> lg<-formLinkageGroups(Go_estRf, max.rf=0.11, min.lod=7) 
> table(lg[,2])

  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
417 408 379 370 366 359 352 344 338 290 280 274 273 268 245 222 176 165 148  29 
 21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40 
 20   2   2   2   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
 41  42  43  44  45  46  47  48  49  50  51  52 
  1   1   1   1   1   1   1   1   1   1   1   1


summary(Go)
    F2 intercross

    No. individuals:    193 

    No. phenotypes:     10 
    Percent phenotyped: 100 100 100 100 100 69.4 69.4 88.1 100 8.8 

    No. chromosomes:    44 
        Autosomes:      1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 
                        23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 
                        42 43 44 

    Total markers:      5757 
    No. markers:        417 408 387 379 370 359 353 345 338 309 290 274 273 268 
                        245 225 176 165 150 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
                        1 1 1 1 1 1 1 
    Percent genotyped:  80.8 
    Genotypes (%):      AA:24.6  AB:49.6  BB:25.7  not BB:0  not AA:0 
Warning message:
In summary.cross(Go) :
  Some chromosomes > 1000 cM in length; there may be a problem with the genetic map.
  (Perhaps it is in basepairs?)
> summary.amp(Go)
