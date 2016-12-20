### Plot percent variance of PCs with oscillation status
# INPUT
# dat - data matrix (each row is a CpG)
# zts - ZT time for each column

source("colors.R")
source("ggtheme.R")
require(ggplot2);require(scales)

plotPCA <- function(dat,zts){
  pca <- prcomp(t(dat))
  tmp <- pooledHR(t(pca$x[,1:4]),zts)
  pcavar <- (pca$sdev^2/sum(pca$sdev^2))[1:4]
  cbind(tmp[,c(1,7)],pcavar)
  rownames(tmp) <- c("1","2","3","4")
  plotdf <- data.frame("pcavar"=pcavar,"osc"=tmp[,1],"PC"=rownames(tmp),"acro"=tmp[,7])
  plotdf$lab <- "Non-oscillating"
  plotdf$lab[plotdf$osc<0.05] <- "Oscillating"
  plotdf$lab <- factor(plotdf$lab,levels=c("Oscillating","Non-oscillating"))
  
  
  return(ggplot(plotdf,aes(PC,pcavar,fill=lab))+geom_bar(stat="identity",color=colors$blue)+
           scale_fill_manual(values=c("Oscillating"=colors$blue,"Non-oscillating"=NA))+
           ylab("Percent of Variance")+
           labs(fill="")+guides(color=F)+
           scale_y_continuous(labels = percent)+
           geom_text(aes(PC,pcavar/2,label=paste(round(acro),"/",(round(acro)+12)%%24,sep=""),color=lab),size=8*5/14)+
           scale_color_manual(values=c("Oscillating"="white","Non-oscillating"=NA))+
           theme(legend.position=c(0.7,0.9),
                 text=element_text(size=8, color="black"),
                 axis.line.x = element_line(color="black", size=0.25),
                 axis.line.y = element_line(color="black", size=0.25),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 axis.text=element_text(size=8)))
}

plotPCAscores <- function(dat,zts,sigcol=colkey$sig,nonsigcol=colkey$nonsig){
  
  Nzts <- length(zts)
  
  require(reshape2);require(ggplot2)
  pca <- prcomp(t(dat))
  
  VE <- pca$sdev[1:4]^2/sum(pca$sdev^2)*100
  circP <- pooledHR(t(pca$x[,1:4]),zts)[,1]
  pcve <- paste(c("PC1","PC2","PC3","PC4"),"(",formatC(VE,digits=3),"%)")
  
  plotdf <- melt(pca$x[,1:4])
  plotdf$pval <- rep(circP,each=Nzts)
  plotdf$PCVE <- rep(pcve,each=Nzts)
  
  maxy <- max(plotdf$value)
  
  #pvalue text
  pd2 <- data.frame("PCVE"=pcve,"circP"=paste("P =",formatC(circP)))
  
  p <- ggplot(plotdf,aes(Var1%%24,value,color=pval<0.05))+geom_point(size=0.5)+
    scale_color_manual(values=c("TRUE"=sigcol,"FALSE"=nonsigcol))+
    facet_wrap(~PCVE)+getUnifiedGGTheme()+ylab("Score")+
    geom_smooth(size=0.25,se=F,fullrange=T,method="lm",
                formula= y ~ sin(x/24*2*pi)+cos(x/24*2*pi))+
    geom_text(data=pd2,aes(x=18,y=maxy*0.8,color=NA,label=circP),color="black",size=7*5/14)+
    scale_x_continuous(limits=c(0,24),breaks=seq(0,24,4))+xlab("ZT")
  return(p)
}

pooledHR <- function(data,zts,per=24,nc=parallel::detectCores()-1){
  runtime <- system.time({
    cat(sprintf("Running pooledHR() on %i cores ",as.numeric(nc)))
    require(cluster);require(foreach);require(doParallel)
    registerDoParallel(cores=nc)
    data <- as.matrix(data)
    sinterm <- sin(2*pi/per*zts); costerm <- cos(2*pi/per*zts)
    results <- mclapply(1:nrow(data),function(i){
      # if(i%%10000==0){write.table(i,".update.txt")}
      lmfit <- lm(data[i,] ~ sinterm+costerm)
      lmnull <- lm(data[i,] ~ 1)
      c(anova(lmfit,lmnull)$`Pr(>F)`[2],summary(lmfit)$r.squared,lmfit$coefficients)
    },mc.cores=nc)
  })[3]
  results <- t(sapply(results,function(x) x))
  # write.table("done",".update.txt")
  results <- cbind(results,apply(results,1,function(x) sqrt(x[4]^2+x[5]^2)))
  results <- cbind(results,apply(results,1,function(x) (atan2(x[4],x[5])/2/pi*per+per)%%per))
  results[which(is.na(results[,1])),1] <- 1
  cat(sprintf("finished in %f seconds\n",runtime))
  return(results)
}