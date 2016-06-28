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