# INPUT:
# ints - list of intensity vectors
# labs - label for each sample (length = # of samples)
# cols - colors for each labael (length = number of unique labels)
# ...  - additional parameters passed to plot()

source("colors.R")
require(scales)
require(plyr)

plotDensities <- function(ints, labs, cols, xlab="array intensity",
                          leglab="Lot", ispct=F, lpos="topright", ...
                          ) {
  colors <- mapvalues(labs, from=levels(factor(labs)), to=cols)
  dens   <- lapply(ints, density)

  xlim <- range(sapply(dens, "[[", "x"))
  ylim <- range(sapply(dens, "[[", "y"))

  par(mar=c(3,3,2,1))
  if(ispct) {
    plot(NA, xlim=xlim, ylim=ylim, las=1, xaxt='n', ...)
    axis(side=1, at=c(0,0.25,0.5,0.75,1),labels = percent(c(0,0.25,0.5,0.75,1)))
  } else {
    plot(NA, xlim=xlim, ylim=ylim, las=1, ...)
  }
  mapply(points, dens, type="l", col=adjustcolor(colors, alpha.f=0.3))

  mtext("density", 2, line=2)
  mtext(xlab, 1, line=2)

  legend(lpos, legend=unique(labs), fill=cols, title=leglab)
}

