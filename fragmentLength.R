source("colors.R")

plotFragmentLength <- function(ints, lengths, start=200, end=700, res="ACGT",
                               xlim=c(0,2000), ylim=c(-0.5,3),
                               xlab="length", ylab="average intensity",
                               col=colors$orange, lineCol=colors$green
                               ) {

  col <- adjustcolor(col, 0.99)

  par(mar=c(2.5,3,2,1))
  plot(lengths, ints, col=col, xlim=xlim, ylim=ylim, pch=19, las=1, xaxt="n",
       yaxt="n", xlab="", ylab="", cex=0.3
       )

  axis(side=1, cex.axis=0.72, mgp=c(3,0.5,0))
  axis(side=2, cex.axis=0.72, las=1)

  mtext(xlab, 1, line=1.5)
  mtext(ylab, 2, line=2)

  text(xlim[1]+diff(xlim)/2, ylim[1]+diff(ylim)*0.7, res, font=2)

  abline(v=start, lty=2, col=lineCol, las=1)
  abline(v=end, lty=2, col=lineCol, las=1)
}
