source("colors.R")

library(plotrix)

plotPermutationsCaseCtrl <- function(realCase, permCase,
                                     realCtrl=NULL, permCtrl=NULL,
                                     xlab=expression(paste(average," ", R^2)),
                                     xstep=0.05, xmax=1,
                                     colCase=colors$blue, colCtrl=colors$red,
                                     colBorder=colors$grey
                                     ) {

  # start  <- min(permCase, permCtrl, realCase, realCtrl)*0.9
  xstart  <- seq(0,xmax,xstep)[ceiling(min(realCase,permCase,realCtrl,permCtrl)/xstep)]
  xend    <- seq(0,xmax,xstep)[ceiling(max(realCase,permCase,realCtrl,permCtrl)/xstep)+1]
  breaks <- seq(xstart, xend, length.out=50)

  caseH <- hist(permCase, plot=FALSE, breaks=breaks)
  if(!is.null(permCtrl)) {
    ctrlH <- hist(permCtrl, plot=FALSE, breaks=breaks)
  } else {
    ctrlH <- NULL
  }

  yend <- max(caseH$counts, ctrlH$counts)*1.1

  par(mar=c(2.5,3,2,1), lwd=0.25)
  plot(caseH, ylim=c(0,yend), las=1, xaxt='n', yaxt='n', ylab="", xlab="",
       col=adjustcolor(colCase, 0.5), main="", xlim=c(xstart,xend),
       border=colBorder, 
       )

  if(!is.null(ctrlH)) {
    plot(ctrlH, add=TRUE, col=adjustcolor(colCtrl, 0.5), border=colBorder)
  }

  par(lwd=1)
  ticks <- seq(xstart, xend, xstep)
  labels <- format(ticks, digits=1)
  labels[1] <- ""
  axis(side=1, pos=0, at=ticks, labels=labels, cex.axis=0.72, mgp=c(3,0.5,0))
  ticks <- pretty(c(caseH$counts, ctrlH$counts))
  axis(side=2, at=ticks, labels=format(ticks, big.mark=","), las=1, pos=xstart,
       cex.axis=0.72
       )

  mtext(xlab, 1, line=1.5)
  mtext("Count", 2, line=2)

  abline(v=realCase, lwd=1, col=colCase)
  pCase <- mean(permCase >= realCase)
  mtext(3, at=realCase, text=paste("p = ", pCase), col=colCase, ps=7)

  if(!is.null(ctrlH)) {
    abline(v=realCtrl, lwd=1, col=colCtrl)
    pCtrl <- mean(permCtrl >= realCtrl)
    mtext(3, at=realCtrl, text=paste("p = ", pCtrl), col=colCtrl, ps=7)
  }

  # axis break
  if(xstart!=0) {
    axis.break(1, xstart+0.01, pos=1)
  }


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
