source("colors.R")

plotPermutationsCaseCtrl <- function(realCase, permCase, realCtrl, permCtrl,
                                 colCase=colors$blue, colCtrl=colors$red,
                                 colBorder=colors$grey
                                 ) {

  # start  <- min(permCase, permCtrl, realCase, realCtrl)*0.9
  start  <- seq(0,1,0.05)[ceiling(min(realCase,permCase,realCtrl,permCtrl)/0.05)]
  end    <- max(permCase, permCtrl, realCase, realCtrl)*1.1
  breaks <- seq(start, end, length.out=50)

  caseH <- hist(permCase, plot=FALSE, breaks=breaks)
  ctrlH <- hist(permCtrl, plot=FALSE, breaks=breaks)

  xend <- end # max x-axis limit
  yend <- max(caseH$counts, ctrlH$counts)*1.1

  par(mar=c(2.5,3,2,1), lwd=0.25)
  plot(caseH, ylim=c(0,yend), las=1, xaxt='n', yaxt='n', ylab="", xlab="",
       col=adjustcolor(colCase, 0.5), main="", xlim=c(start,xend),
       border=colBorder, 
       )
  plot(ctrlH, add=TRUE, col=adjustcolor(colCtrl, 0.5), border=colBorder)

  par(lwd=1)
  ticks <- pretty(c(start, xend))
  labels <- format(ticks, digits=1)
  axis(side=1, pos=0, at=ticks, labels=labels, cex.axis=0.72, mgp=c(3,0.5,0))
  ticks <- pretty(c(caseH$counts, ctrlH$counts))
  axis(side=2, at=ticks, labels=format(ticks, big.mark=","), las=1, pos=start,
       cex.axis=0.72
       )

  mtext(expression(paste(average," ", R^2)), 1, line=1.5)
  mtext("Count", 2, line=2)

  abline(v=realCase, lwd=1, col=colCase)
  abline(v=realCtrl, lwd=1, col=colCtrl)

  pCase <- mean(permCase >= realCase)
  pCtrl <- mean(permCtrl >= realCtrl)
  mtext(3, at=realCase, text=paste("p = ", pCase), col=colCase, ps=7)
  mtext(3, at=realCtrl, text=paste("p = ", pCtrl), col=colCtrl, ps=7)

  # legx <- xend-((xend-start)/2.5)
  # legy <- yend
  # yoff <- legy/3.5

  # legend(c(legx,xend), c(legy,legy-yoff), legend=c("permuted", "observed"),
  #        title="real data", pt.cex=c(2,1), pch=c(15,NA), lty=c(0, 1), lwd=c(0, 2),
  #        col=c(adjustcolor(colCase, 0.5), colCase), cex=0.72
  #        )

  # legend(c(legx, xend), c(legy-yoff, legy-yoff*2), legend=c("permuted", "observed"),
  #        title="control data", pt.cex=c(2,1), pch=c(15,NA), lty=c(0, 1),
  #        lwd=c(0, 2), col=c(adjustcolor(colCtrl, 0.5), colCtrl), cex=0.72
  #        )

}




plotPermutationsNoCtrl <- function(realCase, 
                                   permCase,
                                   colCase=colors$blue
) {
  
  start  <- min(permCase, realCase)*0.9
  end    <- max(permCase, realCase)*1.1
  breaks <- seq(start, end, length.out=50)
  
  caseH <- hist(permCase, plot=FALSE, breaks=breaks)
  
  xend <- end # max x-axis limit
  yend <- max(caseH$counts)*1.1
  
  par(mar=c(3,4,2,1))
  plot(caseH, ylim=c(0,yend), las=1, xaxt='n', yaxt='n', ylab="Count",
       col=adjustcolor(colCase, 0.5), main="", xlim=c(start,xend)
  )
  
  axis(side=1, pos=0, ps=7)
  ticks <- pretty(c(caseH$counts))
  axis(side=2, at=ticks, labels=format(ticks, big.mark=","), las=1, pos=start)
  
  mtext(expression(paste(average," ", R^2)), 1, line=2)
  
  abline(v=realCase, lwd=2, col=colCase)
  
  pCase <- mean(permCase >= realCase)
  mtext(3, at=realCase, text=paste("p = ", pCase), col=colCase, ps=7)
  
}
