# INPUT:
# df - data.frame with df$X and df$Y values
# logscl - plot X and Y on a log10 scale?

source("ggtheme.R")

require(ggplot2)

plot2dkde <- function(df,
                      ylim=NULL,
                      xlim=NULL,
                      xlab="Circadian Amplitude",
                      ylab="Expression/month",
                      logscl=T){
  
  if(logscl){
    hpy <- diff(range(log10(df$Y)))/11
    hpx <- diff(range(log10(df$X)))/11
  }else{
    hpy <- diff(range(df$Y))/11
    hpx <- diff(range(df$X))/11
  }
  
  p <- ggplot(df,aes(X,Y))+
    geom_rect(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,fill="grey90")+
    stat_density2d(aes(fill = ..level..), geom="polygon",alpha=1,h=c(hpx,hpy))+
    getUnifiedGGTheme()+
    ylab(ylab)+
    xlab(xlab)+
    coord_cartesian(ylim=ylim,xlim=xlim)+
    geom_smooth(method="lm",lwd=1,color=colors$blue)+
    scale_fill_gradientn(colors=c(rev(c("grey10","grey20","grey30","grey40","grey50","grey60","grey70","grey80"))))
  
  if(logscl){p <- p + scale_x_log10() + scale_y_log10()}
  
  return(p)
  
}