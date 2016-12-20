source("colors.R")
source("ggtheme.R")

library(ggplot2); library(gridExtra)


# Will draw an acrophase distribution rose plot
plotAcros <- function(
  acrs,  # Acrophases
  pvals, # P-values
  sigThreshold=0.05, # p-value significance threshold
  day=colors$orange, # Color for daytime bars
  night=colors$purple # Color for night-time bars
  ) 
{
  require(data.table)
  require(ggplot2)
  require(dplyr)

  pd <- data.table(acrs=(round(acrs) %% 24), Significant = pvals < sigThreshold)
  pd <- pd[Significant==TRUE, .N, acrs] %>%
    setnames(c("ZT", "Count")) %>%
    setkey("ZT") %>%
    .[, Color := ifelse(ZT <= 11, "Night", "Day")]

  #find limits and labels for the y axis
  ymax <- pd[, max(Count)]
  ymax <- round(ymax) * 1.2
  breaks <- seq(ymax %/% 4, ymax, ymax %/% 4)
  if (ymax < 1000) breaks <- round(breaks,-1)
  if (ymax > 1000) breaks <- round(breaks,-2)
  ymax <- breaks[4]
  breaks <- breaks[-4]

  # draw
  ggplot() + 
  geom_hline(yintercept=c(0, breaks),
    color="#DCDCDC", size=0.2) + 
  geom_text(aes(x=0, y=breaks, 
    label=breaks), 
    color="black", size=2, vjust=-0.2) + 
  geom_bar(data=pd, aes(x=ZT, y=Count, color=Color, fill=Color),
    stat="identity", alpha=0.6, size=0.2, width=0.8) + 
  geom_text(aes(x=seq(0, 23, 3), y=0-ymax*0.12, 
    label=c("24/0", "3", "6", "9", "12", "15", "18", "21")), 
    color="black", size=2)+ 
  guides(color=FALSE, fill=FALSE) + 
  scale_x_continuous("", breaks = seq(0, 23, 3), 
    labels = c("24/0", "3", "6", "9", "12", "15", "18", "21")) + 
  scale_y_continuous("", 
    limits=c(-ymax*0.6, ymax)) + 
  scale_color_manual("", values=c(day, night)) + 
  scale_fill_manual("", values=c(day, night)) + 
  annotate("text", x=0, y=-ymax*0.6, label="ZT", size=2.5) + 
  annotate("text", x=22.9, y=ymax*0.55, label="# of OmCs", size=2, angle=90) + 
  coord_polar(start=2*pi - pi/24) +
  getUnifiedGGTheme() + 
  theme(
    plot.margin=margin(-3-17, -3-17, -19-17, -13-17),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y=element_blank()
  )
}