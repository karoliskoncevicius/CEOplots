source("colors.R")

plotPvaluesCaseCtrl <- function(pvalsCase, pvalsCtrl, colCase=colors$blue,
                            colCtrl=colors$red
                            ) {

  breaks <- seq(0, 1, length.out=50)

  caseH <- hist(pvalsCase, plot=FALSE, breaks=breaks)
  ctrlH <- hist(pvalsCtrl, plot=FALSE, breaks=breaks)

  par(mar=c(5,4.5,1,2))
  plot(caseH, ylim=range(0, caseH$counts, ctrlH$counts), las=1, xaxt='n',
       yaxt='n', cex.lab=1.2, xlab="oscillation p-value", ylab="Count",
       col=adjustcolor(colCase, 0.5), main=""
       )
  plot(ctrlH, add=TRUE, col=adjustcolor(colCtrl, 0.5))

  axis(side=1, pos=0, cex.axis=0.7)
  axis(side=2, las=1, pos=0, cex.axis=0.7)

  legx <- 1-(1/3)
  legy <- max(caseH$counts, ctrlH$counts)
  yoff <- legy/7

  pCase <- round(mean(pvalsCase <= 0.05), 3)*100
  pCtrl <- round(mean(pvalsCtrl <= 0.05), 3)*100
  sigs <- c(pCase, pCtrl)
  legend("topright", legend=paste0(c("real (", "control ("), sigs, "% significant)"),
         fill=c(adjustcolor(colCase, 0.5), adjustcolor(colCtrl, 0.5)), cex=0.7
         )

}
