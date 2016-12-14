source("colors.R")

plotPvaluesCaseCtrl <- function(pvalsCase, pvalsCtrl=NULL,
                                xlab="oscillation p-value",
                                legendLabels=c("real", "control"),
                                legtitle="% significant",
                                colCase=colors$blue, colCtrl=colors$red,
                                colBorder=colors$grey,
                                ylab="Count"
                                ) {

  breaks <- seq(0, 1, length.out=50)

  caseH <- hist(pvalsCase, plot=FALSE, breaks=breaks)

  if(!is.null(pvalsCtrl)) {
    ctrlH <- hist(pvalsCtrl, plot=FALSE, breaks=breaks)
  } else {
    ctrlH <- NULL
  }

  par(mar=c(2.5,3,2,1), lwd=0.25)
  plot(caseH, ylim=range(0, caseH$counts, ctrlH$counts), las=1, xaxt='n',
       yaxt='n', xlab="", ylab="", col=adjustcolor(colCase, 0.5), main="",
       border=colBorder
       )

  if(!is.null(ctrlH)) {
    plot(ctrlH, add=TRUE, col=adjustcolor(colCtrl, 0.5), border=colBorder)
  }

  par(lwd=1)
  ticks <- seq(0,1,0.25)
  axis(side=1, pos=0, at=ticks, cex.axis=0.72, mgp=c(3,0.5,0))
  ticks <- pretty(c(0,caseH$counts, ctrlH$counts))
  axis(side=2, at=ticks, labels=format(ticks, big.mark=","), las=1, pos=0,
       cex.axis=0.72
  )
  mtext(xlab, 1, line=1.5)
  mtext(ylab, 2, line=2)

  legx <- 1-(1/3)
  legy <- max(caseH$counts, ctrlH$counts)
  yoff <- legy/7

  if(!is.null(ctrlH)) {
    pCase <- round(mean(pvalsCase <= 0.05), 3)*100
    pCtrl <- round(mean(pvalsCtrl <= 0.05), 3)*100
    sigs <- c(pCase, pCtrl)
    legend("topright", legend=paste(legendLabels, sigs, "%"),
           fill=c(adjustcolor(colCase, 0.5), adjustcolor(colCtrl, 0.5)),
           title=legtitle
           )
  } else {
    pCase <- round(mean(pvalsCase <= 0.05), 3)*100
    sig <- pCase
    mtext(paste(sig,"% Significant",sep=""),side=3)
    # legend("topright", legend=paste(legendLabels[1], sig, "%"),
    #        fill=adjustcolor(colCase, 0.5), title=legtitle
    #        )
  }

}

##Pvalue histogram with overlayed multiple test correction significance
plotPvaluesMTC <- function(pvalsCase, #bonferroni significant Pvalues
                           pvalsCtrl, #bonferroni nonsignficant Pvalues
                           colCase=colors$blue,
                           colCtrl=colors$red,
                           colBorder=colors$grey,
                           ylab="Count"
) {
  
  breaks <- seq(0, 1, length.out=50)
  
  caseH <- hist(pvalsCase, plot=FALSE, breaks=breaks)
  ctrlH <- hist(pvalsCtrl, plot=FALSE, breaks=breaks)
  
  pCase <- round(sum(pvalsCase <= 0.05)/length(c(pvalsCtrl,pvalsCase)), 3)*100
  pCtrl <- round(sum(c(pvalsCase,pvalsCtrl) <= 0.05)/length(c(pvalsCtrl,pvalsCase)), 3)*100
  sigs <- c(pCase, pCtrl)
  
  par(mar=c(5,4.5,1,2),lwd=0.25)
  maintitle = paste("\n",sigs[2]," % P-value < 0.05\n",sigs[1]," % Bonferroni < 0.05",sep="")
  barplot(rbind(ctrlH$counts,caseH$counts),space=0,ylim=range(0, caseH$counts+ctrlH$counts), las=1,
          yaxt='n', cex.lab=1.2, xlab="aging p-value", ylab=ylab, xaxt='n',
          col=adjustcolor(c(colCase,colCtrl), 0.5),main=maintitle,cex.main=1,border=colBorder
  )
  
  legx <- 20
  legy <- max(caseH$counts+ctrlH$counts)
  yoff <- legy/2
  
  ticks <- pretty(caseH$counts+ctrlH$counts)
  
  axis(side=1, pos=0, cex.axis=0.7,at = seq(0,50,length.out = 5),labels=seq(0,4)/4)
  axis(side=2,pos=0,at = ticks,las=2,
       labels = format(ticks, big.mark=",", scientific=FALSE),cex.axis=0.7)
  
  legend(x=legx,y=yoff, legend=c("Bonferroni < 0.05", "Bonferroni > 0.05"),
         fill=c(adjustcolor(colCtrl, 0.5), adjustcolor(colCase, 0.5)), cex=0.7
  )
  
}
