library(qqman)
# manhattan plot using base graphics
manhattan4 <- function(dataframe, limitchromosomes=NULL,pt.col=c('gray10','gray50'),pt.bg=c('gray10','gray50'),
                       pt.cex=0.45,pch=21,cex.axis=0.7,gridlines=F,gridlines.col='gray83',gridlines.lty=1,gridlines.lwd=1,ymax=8, ymax.soft=T, annotate=NULL,annotate.cex=0.7,annotate.font=3,
                       suggestiveline=-log10(1e-5), suggestiveline.col='blue', suggestiveline.lwd=1.5, suggestiveline.lty=1, 
                       genomewideline=-log10(5e-8), genomewideline.col='red', genomewideline.lwd=1.5, genomewideline.lty=1, 
                       highlight=NULL,highlight.col=c('green3','magenta'),highlight.bg=c('green3','magenta'),  ...) {
  #============================================================================================
  ######## Check data and arguments
  d = dataframe
  if (!("CHR" %in% names(d) & "BP" %in% names(d) & "P" %in% names(d))) stop("Make sure your data frame contains columns CHR, BP, and P")
  d = d[( !is.na(d$CHR) & !is.na(d$BP) & !is.na(d$P) ), ]
  
  if (TRUE %in% is.na(suppressWarnings(as.numeric(d$CHR)))) warning('non-numeric, non-NA entries in CHR column of dataframe. attempting to remove..')
  if (TRUE %in% is.na(suppressWarnings(as.numeric(d$BP)))) warning('non-numeric, non-NA entries in BP column of dataframe. attempting to remove..')
  if (TRUE %in% is.na(suppressWarnings(as.numeric(d$P)))) warning('non-numeric, non-NA entries in P column of dataframe. attempting to remove..')
  
  d = d[!is.na(suppressWarnings(as.numeric(d$CHR))),] # remove rows with non-numeric, non-NA entries
  d = d[!is.na(suppressWarnings(as.numeric(d$BP))),]
  d = d[!is.na(suppressWarnings(as.numeric(d$P))),]
  
  
  if (!is.null(annotate)){
    if ('SNP' %in% names(d)){
      missing_annotate = annotate[!(annotate %in% d$SNP)]
      annotate = annotate[annotate %in% d$SNP]
      
      if (length(missing_annotate)>0){
        print('These SNPs were not annotated because of missing data:')
        print(missing_annotate)
      }
    } else {
      stop("D'oh! Dataframe must have a column $SNP with rs_ids to use annotate feature.")
    }
  }
  if (!is.numeric(annotate.cex) | annotate.cex<0) annotate.cex=0.7
  if (!is.numeric(annotate.font)) annotate.font=3
  
  if (is.character(gridlines.col[1]) & !(gridlines.col[1] %in% colors())) gridlines.col = 'gray83'
  if (!is.numeric(pt.cex) | pt.cex<0) pt.cex=0.45
  if (is.character(pt.col) & (FALSE %in% (pt.col %in% colors()))) pt.col = c('gray10','gray50')
  if (is.character(pt.bg) & (FALSE %in% (pt.bg %in% colors()))) pt.bg = F
  if (is.character(highlight.col) & (FALSE %in% (highlight.col %in% colors()))) highlight.col = c('green3','magenta')
  if (is.character(highlight.bg) & (FALSE %in% (highlight.bg %in% colors()))) highlight.bg = F
  if (is.character(suggestiveline.col[1]) & !(suggestiveline.col[1] %in% colors())) suggestiveline.col = 'blue'
  if (is.character(genomewideline.col[1]) & !(genomewideline.col[1] %in% colors())) genomewideline.col = 'red'
  
  if(!is.null(limitchromosomes)){
    if (TRUE %in% is.na(suppressWarnings(as.numeric(limitchromosomes)))){
      stop('limitchromosomes argument is not numeric') 
    } else {  
      d = d[d$CHR %in% as.numeric(limitchromosomes), ]
    }
  }
  
  
  ######################
  
  # Set positions, ticks, and labels for plotting
  d=subset(d[order(d$CHR, d$BP), ], (P>0 & P<=1)) # sort, and keep only 0<P<=1
  d$logp = -log10(d$P)
  d$pos=NA
  
  
  # Ymax
  if(is.na(suppressWarnings(as.numeric(ymax)))){  # not numeric
    ymax = ceiling(max(-log10(d$P)))
    warning('non-numeric ymax argument.')
  } else if (as.numeric(ymax) < 0){           # negative
    ymax = ceiling(max(-log10(d$P)))
    warning('negative ymax argument.')
  }
  if (ymax.soft==T){ #if soft, ymax is just the lower limit for ymax
    ymax = max(ymax, ceiling(max(-log10(d$P))))
    
    # make ymax larger if top annotate SNP is very high
    if (!is.null(annotate)){
      annotate.max = max(d[which(d$SNP %in% annotate),]$logp)
      if ((ymax - annotate.max) < 0.18*ymax){
        ymax = annotate.max + 0.18*ymax
      }
    }
  } #else, ymax = ymax
  
  ## Fix for the bug where one chromosome is missing. Adds index column #####
  d$index=NA
  ind = 0
  for (i in unique(d$CHR)){
    ind = ind + 1
    d[d$CHR==i,]$index = ind
  }
  ########
  
  nchr=length(unique(d$CHR))
  if (nchr==1) {
    d$pos=d$BP
    ticks=floor(length(d$pos))/2+1
    xlabel = paste('Linkage Group',unique(d$CHR),'position')
    labs = ticks
  } else {
    ticks = rep(NA,length(unique(d$CHR))+1)
    ticks[1] = 0
    for (i in 1:max(d$index)) {
      d[d$index==i, ]$pos   =    (d[d$index==i, ]$BP - d[d$index==i,]$BP[1]) +1 +ticks[i]
      ticks[i+1] = max(d[d$index==i,]$pos)
    }
    xlabel = 'Linkage Group'
    labs = append(unique(d$CHR),'')
  }
  
  # Initialize plot
  xmax = max(d$pos) * 1.03
  xmin = max(d$pos) * -0.03
  #ymax = ceiling(ymax * 1.03)
  ymin = -ymax*0.03
  plot(0,col=F,xaxt='n',bty='n',xaxs='i',yaxs='i',xlim=c(xmin,xmax), ylim=c(0,ymax),
       xlab=xlabel,ylab=expression(-log[10](italic(p))),las=1,cex.axis=cex.axis, ...)
  
  # stagger labels
  blank = rep('',length(labs))
  lowerlabs = rep('',length(labs))
  upperlabs = rep('',length(labs))
  
  for (i in 1:length(labs)){
    if (i %% 2 == 0){
      lowerlabs[i] = labs[i]
    } else{
      upperlabs[i] = labs[i]
    }
  }
  
  axis(1,at=ticks,labels=blank,lwd=0,lwd.ticks=1,cex.axis=cex.axis)
  axis(1,at=ticks,labels=upperlabs,lwd=0,lwd.ticks=0,cex.axis=cex.axis,line=-0.25)
  axis(1,at=ticks,labels=lowerlabs,lwd=0,lwd.ticks=0,cex.axis=cex.axis,line=0.25)
  
  yvals = par('yaxp')
  yinterval = par('yaxp')[2] / par('yaxp')[3]
  axis(2,at= (seq(0,(ymax+yinterval/2),yinterval) - yinterval/2),labels=F,lwd=0,lwd.ticks=1,cex.axis=cex.axis)
  
  # Gridlines
  if (isTRUE(gridlines)){
    
    abline(v=ticks,col=gridlines.col[1],lwd=gridlines.lwd,lty=gridlines.lty) #at ticks
    abline(h=seq(0,ymax,yinterval),col=gridlines.col[1],lwd=gridlines.lwd,lty=gridlines.lty) # at labeled ticks
    #abline(h=(seq(0,ymax,yinterval) - yinterval/2),col=gridlines.col[1],lwd=1.0) # at unlabeled ticks
  }
  
  # Points, with optional highlighting
  pt.col = rep(pt.col,max(d$CHR))[1:max(d$CHR)]
  pt.bg = rep(pt.bg,max(d$CHR))[1:max(d$CHR)]
  d.plain = d
  if (!is.null(highlight)) {
    if(class(highlight)!='character' & class(highlight)!='list'){
      stop('"highlight" must be a char vector (for 1 color) or list (for multi color).')
    }
    
    
    if ('SNP' %in% names(d)){
      missing_highlight = highlight[!(highlight %in% d$SNP)]
      highlight = highlight[highlight %in% d$SNP]
      
      if (length(missing_highlight)>0){
        print('These SNPs were not highlightd because of missing data:')
        print(missing_highlight)
      }
      
    } else {
      stop("D'oh! Dataframe must have a column $SNP with rs_ids to use highlight feature.")
    }
    
    if (class(highlight)=='character'){ #if char vector, make list for consistency in plotting below
      highlight = list(highlight)
    }
    
    highlight.col = rep(highlight.col,length(highlight))[1:length(highlight)]
    highlight.bg = rep(highlight.bg,length(highlight))[1:length(highlight)]
    
    for (i in 1:length(highlight)){
      d.plain = d.plain[which(!(d.plain$SNP %in% highlight[[i]])), ]
    }
  }
  
  icol=1
  for (i in unique(d.plain$CHR)) {
    with(d.plain[d.plain$CHR==i, ],points(pos, logp, col=pt.col[icol],bg=pt.bg[icol],cex=pt.cex,pch=pch,...))
    icol=icol+1
  }
  
  if (!is.null(highlight)){   
    for (i in 1:length(highlight)){
      d.highlight=d[which(d$SNP %in% highlight[[i]]), ]
      with(d.highlight, points(pos, logp, col=highlight.col[i],bg=highlight.bg[i],cex=pt.cex,pch=pch,...)) 
    }
  }
  
  # Significance lines
  if (is.numeric(suggestiveline)) abline(h=suggestiveline, col=suggestiveline.col[1],lwd=suggestiveline.lwd,lty=suggestiveline.lty)
  if (is.numeric(genomewideline)) abline(h=genomewideline, col=genomewideline.col[1],lwd=genomewideline.lwd,lty=genomewideline.lty)
  
  # Annotate
  if (!is.null(annotate)){
    d.annotate = d[which(d$SNP %in% annotate),]
    text(d.annotate$pos,(d.annotate$logp + 0.019*ymax),labels=d.annotate$SNP,srt=90,cex=annotate.cex,adj=c(0,0.48),font=annotate.font)      
  }
  
  # Box
  box()
}

setwd(dir='/Volumes/group_dv/personal/DValenzano/Jul2014/RF_qtl/')
g7m <- read.csv('/Volumes/group_dv/personal/DValenzano/Jul2014/rinput_g7m.csv', head=T, sep=',')
g14m <- read.csv('/Volumes/group_dv/personal/DValenzano/Jul2014/rinput_g14m.csv', head=T, sep=',')
g8m <- read.csv('/Volumes/group_dv/personal/DValenzano/Jul2014/rinput_g8m.csv', head=T, sep=',')
g1_1m <- read.csv('/Volumes/group_dv/personal/DValenzano/Jul2014/rinput_g1_1m.csv', head=T, sep=',')
g7f <- read.csv('/Volumes/group_dv/personal/DValenzano/Jul2014/rinput_g7f.csv', head=T, sep=',')
g14f <- read.csv('/Volumes/group_dv/personal/DValenzano/Jul2014/rinput_g14f.csv', head=T, sep=',')
g8f <- read.csv('/Volumes/group_dv/personal/DValenzano/Jul2014/rinput_g8f.csv', head=T, sep=',')


highlight.7m.rf_rqtl <- subset(g7m, GROUP=='rf_rqtl' )
highlight.7m.rf <- subset(g7m, GROUP=='rf' )
highlight.14m.rf_rqtl <- subset(g14m, GROUP=='rf_rqtl' )
highlight.14m.rf <- subset(g14m, GROUP=='rf' )
highlight.8m.rf_rqtl <- subset(g8m, GROUP=='rf_rqtl' )
highlight.8m.rf <- subset(g8m, GROUP=='rf' )
highlight.1_1m.rf_rqtl <- subset(g1_1m, GROUP=='rf_rqtl' )
highlight.1_1m.rf <- subset(g1_1m, GROUP=='rf' )
highlight.7f.rf_rqtl <- subset(g7f, GROUP=='rf_rqtl' )
highlight.7f.rf <- subset(g7f, GROUP=='rf' )
highlight.14f.rf_rqtl <- subset(g14f, GROUP=='rf_rqtl' )
highlight.14f.rf <- subset(g14f, GROUP=='rf' )
highlight.8f.rf_rqtl <- subset(g8f, GROUP=='rf_rqtl' )
highlight.8f.rf <- subset(g8f, GROUP=='rf' )

g7m_days <- subset(g7m[,1:4], P != 'na')
names(g7m_days) <- c('SNP', 'CHR', 'BP', 'P')
g14m_days <- subset(g14m[,1:4], P != 'na')
names(g14m_days) <- c('SNP', 'CHR', 'BP', 'P')
g8m_days <- subset(g8m[,1:4], P != 'na')
names(g8m_days) <- c('SNP', 'CHR', 'BP', 'P')
g1_1m_days <- subset(g1_1m[,1:4], P != 'na')
names(g1_1m_days) <- c('SNP', 'CHR', 'BP', 'P')
g7f_days <- subset(g7f[,1:4], P != 'na')
names(g7f_days) <- c('SNP', 'CHR', 'BP', 'P')
g14f_days <- subset(g14f[,1:4], P != 'na')
names(g14f_days) <- c('SNP', 'CHR', 'BP', 'P')
g8f_days <- subset(g8f[,1:4], P != 'na')
names(g8f_days) <- c('SNP', 'CHR', 'BP', 'P')

par(mfrow=c(3,2))
manhattan4(g7m_days, main= "cross G, fam7m", highlight=c(as.character(highlight.7m.rf_rqtl$SNP),as.character(highlight.7m.rf$SNP)), genomewideline=F, suggestiveline=F)
manhattan4(g14m_days, main= "cross G, fam14m", highlight=c(as.character(highlight.14m.rf_rqtl$SNP),as.character(highlight.14m.rf$SNP)), genomewideline=F, suggestiveline=F)
manhattan4(g8m_days, main= "cross G, fam8m", highlight=c(as.character(highlight.8m.rf_rqtl$SNP),as.character(highlight.8m.rf$SNP)), genomewideline=F, suggestiveline=F)
manhattan4(g1_1m_days, main= "cross G, fam1.1m", highlight=c(as.character(highlight.1_1m.rf_rqtl$SNP),as.character(highlight.1_1m.rf$SNP)), genomewideline=F, suggestiveline=F)
manhattan4(g7f_days, main= "cross G, fam7f", highlight=c(as.character(highlight.7f.rf_rqtl$SNP),as.character(highlight.7f.rf$SNP)), genomewideline=F, suggestiveline=F)
manhattan4(g14f_days, main= "cross G, fam14f", highlight=c(as.character(highlight.14f.rf_rqtl$SNP),as.character(highlight.14f.rf$SNP)), genomewideline=F, suggestiveline=F)
#manhattan4(g8f_days, main= "cross G, fam8f", highlight=c(as.character(highlight.8f.rf_rqtl$SNP),as.character(highlight.8f.rf$SNP)), genomewideline=F, suggestiveline=F)

###AND THE SAME FOR AA CROSS###
aa1m <- read.csv('/Volumes/group_dv/personal/DValenzano/Jul2014/rinput_aa1m.csv', head=T, sep=',')
aa1f <- read.csv('/Volumes/group_dv/personal/DValenzano/Jul2014/rinput_aa1f.csv', head=T, sep=',')
aa3m <- read.csv('/Volumes/group_dv/personal/DValenzano/Jul2014/rinput_aa3m.csv', head=T, sep=',')

highlight.1m.rf_rqtl <- subset(aa1m, GROUP=='rf_rqtl' )
highlight.1m.rf <- subset(aa1m, GROUP=='rf' )
highlight.3m.rf_rqtl <- subset(aa3m, GROUP=='rf_rqtl' )
highlight.3m.rf <- subset(aa3m, GROUP=='rf' )
highlight.1f.rf_rqtl <- subset(aa1f, GROUP=='rf_rqtl' )
highlight.1f.rf <- subset(aa1f, GROUP=='rf' )

aa1m_days <- subset(aa1m[,1:4], P != 'na')
names(aa1m_days) <- c('SNP', 'CHR', 'BP', 'P')
aa3m_days <- subset(aa3m[,1:4], P != 'na')
names(aa3m_days) <- c('SNP', 'CHR', 'BP', 'P')
aa1f_days <- subset(aa1f[,1:4], P != 'na')
names(aa1f_days) <- c('SNP', 'CHR', 'BP', 'P')

par(mfrow=c(3,1))
manhattan4(aa1m_days, main= "cross AA, fam1m", highlight=c(as.character(highlight.1m.rf_rqtl$SNP),as.character(highlight.1m.rf$SNP)), genomewideline=F, suggestiveline=F)
manhattan4(aa3m_days, main= "cross AA, fam3m", highlight=c(as.character(highlight.3m.rf_rqtl$SNP),as.character(highlight.3m.rf$SNP)), genomewideline=F, suggestiveline=F)
manhattan4(aa1f_days, main= "cross AA, fam1f", highlight=c(as.character(highlight.1f.rf_rqtl$SNP),as.character(highlight.1f.rf$SNP)), genomewideline=F, suggestiveline=F)

par(mfrow=c(3,3))
manhattan4(g7m_days, main= "cross G, fam7m", highlight=c(as.character(highlight.7m.rf_rqtl$SNP),as.character(highlight.7m.rf$SNP)), genomewideline=F, suggestiveline=F)
manhattan4(g14m_days, main= "cross G, fam14m", highlight=c(as.character(highlight.14m.rf_rqtl$SNP),as.character(highlight.14m.rf$SNP)), genomewideline=F, suggestiveline=F)
manhattan4(aa1m_days, main= "cross AA, fam1m", highlight=c(as.character(highlight.1m.rf_rqtl$SNP),as.character(highlight.1m.rf$SNP)), genomewideline=F, suggestiveline=F)
manhattan4(g8m_days, main= "cross G, fam8m", highlight=c(as.character(highlight.8m.rf_rqtl$SNP),as.character(highlight.8m.rf$SNP)), genomewideline=F, suggestiveline=F)
manhattan4(g1_1m_days, main= "cross G, fam1.1m", highlight=c(as.character(highlight.1_1m.rf_rqtl$SNP),as.character(highlight.1_1m.rf$SNP)), genomewideline=F, suggestiveline=F)
manhattan4(aa3m_days, main= "cross AA, fam3m", highlight=c(as.character(highlight.3m.rf_rqtl$SNP),as.character(highlight.3m.rf$SNP)), genomewideline=F, suggestiveline=F)
manhattan4(g7f_days, main= "cross G, fam7f", highlight=c(as.character(highlight.7f.rf_rqtl$SNP),as.character(highlight.7f.rf$SNP)), genomewideline=F, suggestiveline=F)
manhattan4(g14f_days, main= "cross G, fam14f", highlight=c(as.character(highlight.14f.rf_rqtl$SNP),as.character(highlight.14f.rf$SNP)), genomewideline=F, suggestiveline=F)
manhattan4(aa1f_days, main= "cross AA, fam1f", highlight=c(as.character(highlight.1f.rf_rqtl$SNP),as.character(highlight.1f.rf$SNP)), genomewideline=F, suggestiveline=F)

manhattan4(subset(g14m_days, CHR==5), main= "cross G, fam14m", highlight=c(as.character(highlight.14m.rf_rqtl$SNP),as.character(highlight.14m.rf$SNP)), genomewideline=F, suggestiveline=F)
