
source("colors.R", chdir=TRUE)
source("ggtheme.R", chdir=TRUE)


# Assuming Matts padlock data as input having the following structure:
# List of 7
#  $ pctM           :List of 3
#   ..$ A9 : num [1:8011, 1:36] 0.779 0.836 0.7 0.823 0.499 ...
#   .. ..- attr(*, "dimnames")=List of 2
#   .. .. ..$ : NULL
#   .. .. ..$ : chr [1:36] "0" "2" "4" "6" ...
#   ..$ A15: num [1:8011, 1:29] 0.586 0.691 0.438 0.79 0.466 ...
#   .. ..- attr(*, "dimnames")=List of 2
#   .. .. ..$ : NULL
#   .. .. ..$ : chr [1:29] "0" "2" "4" "6" ...
#   ..$ A25: num [1:8011, 1:29] 0.572 0.68 0.432 0.766 0.521 ...
#   .. ..- attr(*, "dimnames")=List of 2
#   .. .. ..$ : NULL
#   .. .. ..$ : chr [1:29] "0" "2" "4" "6" ...
#  $ zts            :List of 3
#   ..$ A9 : num [1:36] 0 2 4 6 8 10 12 14 16 18 ...
#   ..$ A15: num [1:29] 0 2 4 6 8 10 12 14 16 18 ...
#   ..$ A25: num [1:29] 0 2 4 6 8 10 12 14 16 18 ...
#  $ coverage       :List of 3
#   ..$ A9 : int [1:8011, 1:36] 4379 4380 4383 4373 3238 22047 22026 22036 10093 10086 ...
#   .. ..- attr(*, "dimnames")=List of 2
#   .. .. ..$ : NULL
#   .. .. ..$ : chr [1:36] "0" "2" "4" "6" ...
#   ..$ A15: int [1:8011, 1:29] 4791 4799 4797 4798 3368 24712 24690 24693 10794 10784 ...
#   .. ..- attr(*, "dimnames")=List of 2
#   .. .. ..$ : NULL
#   .. .. ..$ : chr [1:29] "0" "2" "4" "6" ...
#   ..$ A25: int [1:8011, 1:29] 4341 4345 4350 4354 1246 15152 15135 15146 6030 6020 ...
#   .. ..- attr(*, "dimnames")=List of 2
#   .. .. ..$ : NULL
#   .. .. ..$ : chr [1:29] "0" "2" "4" "6" ...
#  $ genomicElements: int [1:8011, 1:48] 0 0 0 0 0 0 0 0 0 0 ...
#   ..- attr(*, "dimnames")=List of 2
#   .. ..$ : chr [1:8011] "chr7_100076117_100076118_-" "chr7_100076135_100076136_-" "chr7_100076153_100076154_-" "chr7_100076177_100076178_-" ...
#   .. ..$ : chr [1:48] "TSS" "exons" "introns" "genes" ...
#  $ osc            :List of 3
#   ..$ A9 : num [1:8011, 1:7] 0.79 0.762 0.951 0.479 0.364 ...
#   .. ..- attr(*, "dimnames")=List of 2
#   .. .. ..$ : NULL
#   .. .. ..$ : chr [1:7] "pval" "rsq" "int" "sin" ...
#   ..$ A15: num [1:8011, 1:7] 0.747 0.726 0.554 0.9 0.189 ...
#   .. ..- attr(*, "dimnames")=List of 2
#   .. .. ..$ : NULL
#   .. .. ..$ : chr [1:7] "pval" "rsq" "int" "sin" ...
#   ..$ A25: num [1:8011, 1:7] 0.1475 0.1401 0.0616 0.6415 0.093 ...
#   .. ..- attr(*, "dimnames")=List of 2
#   .. .. ..$ : NULL
#   .. .. ..$ : chr [1:7] "pval" "rsq" "int" "sin" ...
#  $ apvs           :List of 3
#   ..$ A9 : Named num [1:31549] 0.601 0.616 0.996 0.515 0.314 ...
#   .. ..- attr(*, "names")= chr [1:31549] "489" "491" "516" "518" ...
#   ..$ A15: Named num [1:29687] 0.6609 0.0218 0.0609 0.5417 0.4478 ...
#   .. ..- attr(*, "names")= chr [1:29687] "489" "491" "516" "518" ...
#   ..$ A25: Named num [1:27872] 4.57e-09 2.63e-08 4.72e-02 3.12e-01 2.82e-01 ...
#   .. ..- attr(*, "names")= chr [1:27872] "489" "491" "516" "518" ...
#  $ aging          :'data.frame':	8011 obs. of  2 variables:
#   ..$ pval: num [1:8011] 3.73e-01 3.13e-02 6.45e-01 3.96e-03 1.07e-11 ...
#   ..$ dir : num [1:8011] -0.0929 -0.2223 0.0482 -0.2945 0.6296 ...
oscilationFitsAndTheirAging <- function(dat) {
	require(data.table)
	require(ggplot2)
	require(foreach)
	require(gridExtra)
  require(scales)
	getFittedCurve <- function(locus, age, dat) {
		t <- 0:24
		sinterm <- dat$osc[[age]][locus, "sin"]
		costerm <- dat$osc[[age]][locus, "cos"]
		intercept <- mean(dat$pctM[[age]][locus,])
		values <- sinterm * sin(2*pi*t/24) + costerm * cos(2*pi*t/24)
		data.table(X=t, Y=values, M=intercept)
	}

	# First, select the top most significant oscillating loci
	# from Age 9 and draw their curves

	lidx1 <- which(
			dat$osc$A9[, "acro"] >= 12 &
			dat$osc$A9[, "acro"] < 24)
	lidx1 <- lidx1[ order(dat$osc$A9[lidx1, "pval"])[1:300]]

	lidx2 <- which(
			dat$osc$A9[, "acro"] < 12 &
			dat$osc$A9[, "acro"] >= 0)
	lidx2 <- lidx2[ order(dat$osc$A9[lidx2, "pval"])[1:300]]

	dt <- 
		foreach(locus=lidx1, .combine=rbind) %do% {
			curve <- getFittedCurve(locus, "A9", dat)
			data.table(Locus=locus, curve)
		}
	dt[, Acro := "[12-24)"]
	dt2 <- 
		foreach(locus=lidx2, .combine=rbind) %do% {
			curve <- getFittedCurve(locus, "A9", dat)
			data.table(Locus=locus, curve)
		}
	dt2[, Acro := "[0-12)"]
	dt <- rbind(dt, dt2)
	rm(dt2)


	# Select loci at various p thresholds
	# And compute their Age change
	dt2 <- foreach(p=seq(0, 1, 0.02), .combine=rbind) %do% {
		lidx1 <- which(
				dat$osc$A9[, "acro"] >= 12 &
				dat$osc$A9[, "acro"] < 24 &
				dat$osc$A9[, "pval"] < p + 0.02 & 
				dat$osc$A9[, "pval"] >= p)
		res <- foreach(locus=lidx1, .combine=rbind) %do% {
			ageChange <- mean(dat$pctM[["A25"]][locus,]) - 
							mean(dat$pctM[["A9"]][locus,])
			data.table(Locus=locus, AgeChange=ageChange, P=p, Acro="[12-24)")
		}

		lidx2 <- which(
				dat$osc$A9[, "acro"] < 12 &
				dat$osc$A9[, "acro"] >= 0 &
				dat$osc$A9[, "pval"] < p + 0.02 & 
				dat$osc$A9[, "pval"] >= p)
		res2 <- foreach(locus=lidx2, .combine=rbind) %do% {
			ageChange <- mean(dat$pctM[["A25"]][locus,]) - 
							mean(dat$pctM[["A9"]][locus,])
			data.table(Locus=locus, AgeChange=ageChange, P=p, Acro="[0-12)")
		}
		rbind(res, res2)
	}

	# Plot the fits and age change
	r1 <- dt[, range(Y)]
	r2 <- dt2[, list(mean(AgeChange), sd(AgeChange)), list(Acro, P)][, range(V1)]
	yrange <- range(c(r1, r2))

	p1 <- ggplot(dt) + 
		geom_line(aes(X, Y, color=Acro, group=Locus), alpha=0.1, size=0.25) + 
		xlab("ZT") + ylab("Methylation change") + 
	  scale_y_continuous(limits=yrange,labels=percent) + 
		geom_hline(yintercept=0, color="#AAAAAA", size=0.4) + 
		scale_color_manual(values=c(colors$blue, colors$red), guide=FALSE) + 
		getUnifiedGGTheme(plot.margin=margin(l=0))+
	  scale_x_continuous(limits=c(0,24),breaks = seq(0,24,4))


	p2 <- ggplot(dt2[, list(mean(AgeChange), sd(AgeChange)), list(Acro, P)],
		aes(P, V1, color=Acro)) + 
		geom_point(size=0.3) + 
		# geom_smooth(size=0.3) +
		geom_hline(yintercept=0, color="#AAAAAA", size=0.4) + 
		ylim(yrange) + 
		xlab("P-value bin") + 
		ylab("") + 
		scale_color_manual("Acrophase", values=c(colors$blue, colors$red)) + 
		theme(legend.position=c(0.5, 0.8),
				legend.direction="vertical",
				legend.key.size=unit(0.2, "cm")) + 
		getUnifiedGGTheme(
			plot.margin=margin(l=-10))

	grid.arrange(p1, p2, ncol=2)
}