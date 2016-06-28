source("colors.R")

plotSurface <- function(ints, coords, ZT, centromere,
                        palette=c(colors$blue, colors$blue, "#EBEBEB", colors$red, colors$red)
                        ) {
  ordered <- order(coords)
  coords <- coords[ordered]
  ints   <- ints[ordered,]

  ordered <- order(ZT)
  ZT   <- ZT[ordered]
  ints <- ints[,ordered]

  ZT <- ZT %% 24
  ZT[ZT==0] <- 24
  ints <- t(apply(ints, 1, tapply, ZT, mean))
  ZT   <- as.numeric(colnames(ints))

  # 2X
  ZT   <- c(ZT, ZT)
  ints <- cbind(ints, ints)

  # Fix Centromere
  maxGap1  <- which.min(abs(coords-centromere[1]))
  maxGap2  <- which.min(abs(coords-centromere[2]))

  scaleX <- max(coords)/100
  x <- coords/scaleX
  y <- ((1:length(ZT))/length(ZT))*50
  z <- as.matrix(blur(as.im(ints), sigma=c(2,20), normalise=TRUE, bleed=FALSE))
  z <- z*(-1) # invert!
  z <- z - min(z, na.rm=TRUE)
  z <- z/max(z, na.rm=TRUE)
  z <- z * 15
  z[maxGap1,] <- NA
  z[maxGap2,] <- NA

  colorzjet <- colorRampPalette(palette)(100)

  par3d("windowRect"=c(0,0,800,800))
  view3d(theta=-100, phi=30, fov=30)
  rgl.surface(x, y, z, color=colorzjet[ findInterval(z, seq(0, 15, length=100))], specular="black", lit=FALSE)
  labels <- ZT
  ticks  <- y
  axes3d("z--", labels=FALSE, at=ticks, col="black")
  axes3d("z--", labels=labels, at=ticks, col="black")
  inds   <- seq(1, length(x), 50)
  ticks  <- pretty(1:max(coords), 5)/scaleX
  labels <- paste0(round(ticks*scaleX/1000000), "Mbp")
  axis3d("x--", at=ticks, col="black", labels=FALSE)
  mtext3d(labels, at=ticks, edge="x", line=2, col="black")
  grid3d("z+")
}

