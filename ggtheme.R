
# This function can be augmented with parameters
# that will adjust the theme

getUnifiedGGTheme <- function(...) { 
  require(ggplot2)
  theme(
    text=element_text(size=7, color="black", margin = margin(), debug = FALSE),
    title=element_text(size=7, color="black", margin=margin(2, 2, 0, 0)),
    plot.title=element_text(size=7, color="black"),
    plot.margin=margin(2, 2, 2, 2),
    axis.line.x = element_line(color="black", size=0.25),
    axis.line.y = element_line(color="black", size=0.25),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=7,color="black"),
    ...
    )
}
