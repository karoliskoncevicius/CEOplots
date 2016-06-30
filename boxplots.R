source("colors.R")

plotBoxplots <- function(ints, lots, cols) {
  colors <- rep(cols, table(lots))

  par(mar=c(2.5,3,2,1))
  boxplot(ints, col=colors, cex=0.1, xaxt="n", outline=TRUE, las=1,
          boxlwd=0.25, whisklty=1, whisklwd=0.25, pch=19, cex=0.2
          )

  mtext("array intensity", 2, line=2)

  # legend("topright", legend=unique(lots), fill=cols, title="Lot")
}

