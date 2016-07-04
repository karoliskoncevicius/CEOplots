# INPUT:
# ints - matrix of intensities with samples in columns.
# lots - lot/batch/plate information for each sample (length = # of samples)
# cols - colors for each lot (length = number of unique lots)

source("colors.R")
require(scales)

plotDensities <- function(ints, lots, cols,xlab="array intensity",leglab="Lot",ispct=F) {
  colors <- rep(cols, table(lots))
  dens   <- apply(ints, 2, density)

  xlim=range(sapply(dens, "[[", "x"))
  ylim=range(sapply(dens, "[[", "y"))

  par(mar=c(3,3,2,1))
  plot(NA, xlim=xlim, ylim=ylim, las=1, xaxt='n')
  mapply(points, dens, type="l", col=adjustcolor(colors,alpha.f = 0.3))

  mtext("density", 2, line=2)
  mtext(xlab, 1, line=2)
  if(ispct){axis(side=1, at=c(0,0.25,0.5,0.75,1),labels = percent(c(0,0.25,0.5,0.75,1)))}

  legend("topright", legend=unique(lots), fill=cols, title=leglab)
}

