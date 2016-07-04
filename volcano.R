source("colors.R")
source("ggtheme.R")

plotVolcano <- function(
  coefficients, pvalues, 
  nonSignificant=colors$grey, 
  negative=colors$blue, 
  positive=colors$red,
  xAxisLabel="Coefficient",
  yAxisLabel=expression(-log[10]*'(p-value)'),
  pAdjustmentMethod="bonferroni",
  isPercent=F)
{
  require(ggplot2)
  require(data.table)
  require(scales)
  pd <- data.table(X=coefficients, Y=-log10(pvalues))
  pd[, Fill := as.numeric(p.adjust(pvalues, pAdjustmentMethod) < 0.05)]
  pd[Fill == TRUE & X < 0, Fill := Fill + 1]
  rng <- max(abs(pd$X))*1.05
  
  p <- ggplot(pd, aes(X, Y, 
                      color=as.factor(Fill))) + 
    geom_point(size=.15, alpha=1) + 
    geom_hline(yintercept=-log10(0.05/length(pvalues)),linetype=3)+
    xlab(xAxisLabel) + 
    ylab(yAxisLabel) + 
    scale_color_manual(values=c(nonSignificant, positive, negative), guide=FALSE) + 
    geom_vline(xintercept=0)+
    getUnifiedGGTheme()+
    theme(axis.line.y=element_blank())
    # JG>> Why do we need the following??
    # MC>>> Added information for asymmetry (Gabe can delete if unnecessary)
    # +
    # geom_text(data=data.frame("X"=c(rng/2,-rng/2),
    #                           "Y"=c(0, 0),
    #                           "Label"=paste(c(sum(pd$Fill==1),sum(pd$Fill==2)),"\n",c("HyperM","HypoM"))),
    #           aes(X,Y,label=Label),
    #           color="black",
    #           size=3,
    #           vjust=-.2
    #           )
 
  
  
  if(isPercent){
    p+scale_x_continuous(limits = c(-rng,rng),labels = percent)
  } else {
    p+scale_x_continuous(limits = c(-rng,rng))
  }
}
