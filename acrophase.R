source("colors.R")
source("ggtheme.R")

library(ggplot2); library(gridExtra)


# NOTE, the acrophase plot can be increased as follows:
# require(grid)
# print(plotAcros(...), vp=viewport(x=0.3, y=0.3, width=2.6, height=2.6))


plotAcros <- function(acrs, pvals, blue=colors$blue, red=colors$red) {

  acrs <- ceiling(acrs)

  frac <- tapply(pvals <= 0.05, acrs, mean) / 0.05
  acr  <- as.numeric(names(frac))
  pd   <- data.frame(acr, frac)

  p1 <- ggplot() +
    geom_bar(stat="identity", position="dodge", alpha=0.9, width=0.9, data=pd, size=0.5, aes(x=acr-0.5, y=frac, color=round(frac,1) > 2, fill=round(frac,1) > 2)) +
    geom_text(aes(x=seq(0, 23, 3), y=-1.3, label=seq(0, 23, 3)), color="black", size=2) +
    geom_text(data=pd[round(pd$frac,1) > 2,], vjust=-0, hjust=-0, size=2, aes(x=acr - 0.5, y=frac+1.5, label=paste0(round(frac,1)))) +
    coord_polar(start=0) +
    scale_x_continuous("", breaks = seq(1, 24), labels = seq(1, 24), limits=c(0, 24)) +
    scale_fill_manual("", labels=c("< 10%", "> 10%"), values=c(blue, red), guide=FALSE) +
    scale_color_manual("", labels=c("< 10%", "> 10%"), values=c(blue, red), guide=FALSE) +
    ylim(-5, 15) +
    getUnifiedGGTheme(
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.line=element_blank(),
          axis.ticks=element_blank()      
      )
    p1
}

