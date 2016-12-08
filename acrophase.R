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

# plot uncorrected acrophases
plotUncorrectedAcros <- function(
  acrs,  # Acrophases
  pvals, # P-values
  sigThreshold=0.05, # p-value significance threshold
  day=colors$green, # Color for daytime bars
  night=colors$purple, # Color for night-time bars
  background="grey", # Color for non-significant bars
  addNonSignificant=FALSE,
  addCountLabels=TRUE
  ) 
{
  require(data.table)
  require(ggplot2)
  require(dplyr)

  # Prepare the data by making a table of values
  pd <- data.table(acrs=(round(acrs) %% 24+1), Significant = pvals < sigThreshold)
  pdSignificant <- 
    data.table(ZT=1:24, Count=pd[Significant==TRUE, table(acrs)]) %>%
      .[, Density := as.numeric(Count / sum(Count))]
  pdNonSignificant <- 
    data.table(ZT=1:24, Count=pd[Significant==FALSE, table(acrs)]) %>%
      .[, Density := as.numeric(Count / sum(Count))]
  pd <- merge(pdSignificant, pdNonSignificant, by="ZT")
  pd[, Color := ifelse(ZT <= 11, "Night", "Day")]
  setkey(pd, ZT)

  # determine y axis range
  ylim <- range(pd[, range(Density.x)], pd[, range(Density.y)])
  ylim[1] <- -ylim[2]

  # determine y position of hour labels
  ylabel <- -0.05
  p0 <- 
    ggplot() + 
      geom_text(
        aes(x=seq(3, 24, 3), 
          y=-0.05, 
          label=seq(3, 24, 3)), 
        color="black", 
        size=2)
  if (addNonSignificant) {
    p0 <- p0 + 
      geom_bar(data=pd, aes(x=ZT, y=Density.y), 
          stat="identity", fill=background, alpha=0.7, color=background, size=0.4)
  }
  p0 <- p0 + 
    geom_bar(data=pd, aes(x=ZT, y=Density.x, fill=Color, color=Color), 
      stat="identity", alpha=0.6, size=0.4)
  if (addCountLabels) {
    p0 <- p0 + 
      geom_segment(data=pd[Density.x > Density.y],
        aes(x=(ZT-5)%%24+1, xend=ZT, y=Density.x, yend=Density.x),
        color=colors$red, size=0.2, linetype=2
        ) + 
      geom_text(data=pd[Density.x > Density.y],
        aes(x=(ZT-5)%%24+1, y=Density.x, label=as.character(Count.x)),
        color="black", size=2, hjust=1
        )    
  }
  p0 + 
    scale_x_continuous("", breaks = seq(1, 24), labels = NULL) +
    scale_y_continuous("", 
      breaks=pd[Density.x > Density.y, Density.x],
      limits=ylim) +
    scale_color_manual("", values=c(day, night)) + 
    scale_fill_manual("", values=c(day, night)) + 
    guides(color=FALSE, fill=FALSE) +
    coord_polar(start=pi/24 + 2*pi) + 
    getUnifiedGGTheme(
        axis.ticks = element_blank(),      
        axis.text.y=element_blank(),    
        axis.title.y=element_blank(),
        axis.line=element_blank(),
        axis.ticks=element_blank(),    
        panel.border = element_blank(),
        legend.key = element_blank()        
    )
}