source("colors.R")

library(ggplot2); library(gridExtra)

plotAcros <- function(acrs, pvals, type, blue=colors$blue, red=colors$red) {

  acrs <- round(acrs)

  frac <- tapply(pvals <= 0.05, acrs, mean) / 0.05
  acr  <- as.numeric(names(frac))
  pd   <- data.frame(acr, frac, type=type)

  p1 <- ggplot() +
    geom_bar(stat="identity", position="dodge", alpha=0.9, width=0.9, data=pd, size=0.5, aes(x=acr + 0.5, y=frac, color=round(frac,1) > 2, fill=round(frac,1) > 2)) +
    geom_text(aes(x=seq(1, 24, 3)+0.5, y=-0.9, label=seq(0, 23, 3)), color="black", size=2) +
    geom_text(data=pd[round(pd$frac,1) > 2,], vjust=-0, hjust=-0, size=2, aes(x=acr + 0.5, y=frac+1.5, label=paste0(round(frac,1)))) +
    coord_polar(start=0) +
    scale_x_continuous("", breaks = seq(1, 25), labels = seq(1, 25), limits=c(1, 25)) +
    scale_fill_manual("", labels=c("< 5%", "> 5%"), values=c(blue, red), guide=FALSE) +
    scale_color_manual("", labels=c("< 5%", "> 5%"), values=c(blue, red), guide=FALSE) +
    ylim(-5, 8) +

    theme(text=element_text(size=7, color="red"),
          panel.grid = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank())

    p1
}

