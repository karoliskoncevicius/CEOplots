# INPUT:
# ints - matrix of intensities with samples in columns.
# lots - lot/batch/plate information for each sample (length = # of samples)
# cols - colors for each lot (length = number of unique lots)
# ...  - additional information passed to boxplot (like ylim)

source("colors.R")

plotBoxplots <- function(ints, lots, cols, ...) {
  colors <- rep(cols, table(lots))

  par(mar=c(2.5,3,2,1))
  boxplot(ints, col=colors, cex=0.1, xaxt="n", outline=FALSE, las=1,
          boxlwd=0.25, whisklty=1, whisklwd=0.25, pch=19, cex=0.2, ...
          )

  mtext("array intensity", 2, line=2)

  # legend("topright", legend=unique(lots), fill=cols, title="Lot")
}

