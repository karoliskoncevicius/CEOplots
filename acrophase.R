source("colors.R")
source("ggtheme.R")

library(ggplot2); library(gridExtra)


# Will draw an acrophase distribution rose plot
plotAcros <- function(
  acrs,  # Acrophases
  pvals, # P-values
  sigThreshold=0.05, # p-value significance threshold
  day=colors$orange, # Color for daytime bars
  night=colors$purple, # Color for night-time bars
  ylabel="# of OmCs"
){
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
  int <- pretty(c(0,ymax))[2]
  breaks <- if(ymax%/%int>5){seq(int*2,ymax,int*2)}else{seq(int,ymax,int)}
  labels <- formatC(breaks, format="d", big.mark=',')
  
  # draw
  ggplot() + 
    geom_hline(yintercept=c(0, breaks),
               color="#DCDCDC", size=0.2) + 
    geom_text(aes(x=0-0.5, y=breaks, label=labels), 
              color="black", size=2, vjust=-0.2) + 
    geom_bar(data=pd, aes(x=ZT, y=Count, color=Color, fill=Color),
             stat="identity", alpha=0.6, size=0.2, width=0.8) + 
    geom_text(aes(x=seq(0, 23, 3)-0.5, y=0-ymax*0.12, 
                  label=c("24/0", "3", "6", "9", "12", "15", "18", "21")), 
              color="black", size=2)+ 
    guides(color=FALSE, fill=FALSE) + 
    scale_x_continuous(limits=c(NA,23.5),"", breaks = seq(0, 24, 3)) + 
    scale_y_continuous("", 
                       limits=c(-ymax*0.6, ymax),labels="",breaks=0) + 
    scale_color_manual("", values=c(day, night)) + 
    scale_fill_manual("", values=c(day, night)) + 
    annotate("text", x=0-0.5, y=-ymax*0.6, label="ZT", size=2.5) +
    annotate("text", x=22.9-0.5, y=ymax*0.55, label=ylabel, size=2, angle=90) +
    coord_polar() +
    getUnifiedGGTheme() + 
    theme(
      plot.margin=margin(-3-17, -3-17, -19-17, -13-17),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y=element_blank()
    )
}