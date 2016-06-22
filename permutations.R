source("colors.R")

plotPermutationsCaseCtrl <- function(realCase, permCase, realCtrl, permCtrl,
                                 colCase=colors$blue, colCtrl=colors$red
                                 ) {

  start  <- min(permCase, permCtrl, realCase, realCtrl)*0.9
  end    <- max(permCase, permCtrl, realCase, realCtrl)*1.1
  breaks <- seq(start, end, length.out=50)

  caseH <- hist(permCase, plot=FALSE, breaks=breaks)
  ctrlH <- hist(permCtrl, plot=FALSE, breaks=breaks)

  xend <- end # max x-axis limit
  yend <- max(caseH$counts, ctrlH$counts)*1.1

  par(mar=c(3,4,2,1))
  plot(caseH, ylim=c(0,yend), las=1, xaxt='n', yaxt='n', ylab="Count",
       col=adjustcolor(colCase, 0.5), main="", xlim=c(start,xend)
       )
  plot(ctrlH, add=TRUE, col=adjustcolor(colCtrl, 0.5))

  axis(side=1, pos=0, ps=7)
  ticks <- pretty(c(caseH$counts, ctrlH$counts))
  axis(side=2, at=ticks, labels=format(ticks, big.mark=","), las=1, pos=start)

  mtext(expression(paste(average," ", R^2)), 1, line=2)

  abline(v=realCase, lwd=3, col=colCase)
  abline(v=realCtrl, lwd=3, col=colCtrl)

  pCase <- mean(permCase >= realCase)
  pCtrl <- mean(permCtrl >= realCtrl)
  mtext(3, at=realCase, text=paste("p = ", pCase), col=colCase, ps=7)
  mtext(3, at=realCtrl, text=paste("p = ", pCtrl), col=colCtrl, ps=7)

  legx <- xend-((xend-start)/2.5)
  legy <- yend
  yoff <- legy/3.5

  legend(c(legx,xend), c(legy,legy-yoff), legend=c("permuted", "observed"),
         title="real data", pt.cex=c(2,1), pch=c(15,NA), lty=c(0, 1), lwd=c(0, 2),
         col=c(adjustcolor(colCase, 0.5), colCase)
         )

  legend(c(legx, xend), c(legy-yoff, legy-yoff*2), legend=c("permuted", "observed"),
         title="control data", pt.cex=c(2,1), pch=c(15,NA), lty=c(0, 1),
         lwd=c(0, 2), col=c(adjustcolor(colCtrl, 0.5), colCtrl)
         )
}

