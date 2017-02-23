colors <- list()
# colors$red    <- "#FF0032"
colors$red    <- rgb(190/255,29/255,44/255)
colors$blue   <- "#1771FF"
# colors$orange <- "#FF7500"
colors$orange <- rgb(244/255,117/255,33/255)
# colors$purple <- "#5B04B6"
colors$purple <- rgb(92/255,60/255,152/255)
colors$yellow <- "#FFFF00"
# colors$green  <- "#00B692"
colors$green  <- rgb(11/255,182/255,147/255)
# colors$grey   <- "#D2D2D2"
colors$grey   <- rgb(128/255,130/255,133/255)

#color assignments
colkey <- list()
colkey$wake   <- colors$yellow
colkey$sleep  <- colors$purple
colkey$nonsig <- colors$grey
colkey$sig    <- colors$red
colkey$lung   <- colors$orange
colkey$liver  <- colors$green
colkey$hypo   <- colors$blue
colkey$hyper  <- colors$red
colkey$A9     <- colors$orange
colkey$A15    <- colors$purple
colkey$A25    <- colors$green
colkey$macro  <- colors$red
colkey$cntrl  <- colors$grey
colkey$real <- colors$red
colkey$permuted <- colors$grey
colkey$neutral <- colors$grey
