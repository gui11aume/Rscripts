violin.plot <- function (x, median.bar="grey50", x.pos, hbars=TRUE, ...) {
# x: a list/data.frame of values.
# median.bar: color of the median bar, NA for none.
# x.pos: positions of the plots on the x axis.
# ...: density, angle, border, col, and lty are passed to polygon().
# Other parameters are passed to plot().

   require(sm)

# -- Test calls -- #
# violin.plot(list(rnorm(100), 1+rnorm(100)), col=1:2)
# violin.plot(data.frame(x=rnorm(100), y=1+rnorm(100)), col=1:2)

   # Get the number of violin plots to draw.
   m <- length(x);

   # Automatically space violins.
   if (missing(x.pos)) {
      x.pos <- 1:m;
   }

   # Cut the tails of the distribution.
   cuttails <- function(x) {
      Q <- quantile(x, c(.01, .99), na.rm=TRUE)
      return(x[x > Q[1] & x < Q[2]])
   }
   x <- lapply(X=x, FUN=cuttails)

   # Get densities.
   densities <- lapply(X=x, FUN=sm.density, display="none")

   # Compute nice spacing parameters between plots.
   y.sup <- sapply(X=densities, FUN=function (x) max(x$estimate))
   x.min <- min(sapply(X=densities, FUN=function (x) min(x$eval.points)))
   x.max <- max(sapply(X=densities, FUN=function (x) max(x$eval.points)))

   dotargs = list(...);
   # The density, angle, border, col, and lty parameters are passed
   # to polygon(). The rest is passed to plot().
   topol <- c("density", "angle", "border", "col", "lty")
   polargs <- dotargs[names(dotargs) %in% topol]
   plotargs <- dotargs[!names(dotargs) %in% topol]


   # -- plot() paramters -- #

   BOT = x.min
   LFT = min(x.pos)-.6
   TOP = x.max
   RGT = max(x.pos)+.6

   # Overwrite 'type', 'xaxt', 'x', and 'y'.
   plotargs[["type"]] <- "n"
   plotargs[["xaxt"]] <- "n"
   plotargs[["x"]] <- c(LFT, RGT)
   plotargs[["y"]] <- c(BOT, TOP)

   # Set the axis labels to empty if not specified.
   if (is.null(plotargs[["xlab"]])) {
      plotargs[["xlab"]] <- "";
   }
   if (is.null(plotargs[["ylab"]])) {
      plotargs[["ylab"]] <- "";
   }

   # Done!
   do.call(what=plot, args=plotargs)

   ###################################################
   # Add grey background and horizontal white lines.
   # This part is specific of the figures and not tunable.
   # Remove it for a more general plotting functin.
   rect(xleft=LFT, xright=RGT, ybottom=BOT, ytop=TOP,
      col="grey95", border=NA)
   if (hbars) {
      ythick = seq(from=5*ceiling(BOT/5), to=5*floor(TOP/5), by=5)
      ythin = seq(from=2.5*ceiling(BOT/2.5), to=2.5*floor(TOP/2.5), by=2.5)
      segments(x0=LFT, x1=RGT, y0=ythick, y1=ythick, lwd=2, col="white")
      segments(x0=LFT, x1=RGT, y0=ythin, y1=ythin, lwd=1, col="white")
   }
   ###################################################


   # -- polygon() paramters -- #

   # Manually recycle the parameters passed to 'polygon()'.
   # NB: 'polargs.i' is a list of lists of arguments.
   polargs.i <- rep(
      do.call(
         what=mapply,
         args=(c(list(FUN=list, SIMPLIFY=FALSE), polargs))
      ), length.out=m
   );

   # Plot the violins.
   medians <- sapply(X=x, FUN=median, na.rm=TRUE)
   quant <- sapply(X=x, FUN=quantile, probs=c(.1, .25, .75, .9),
                   na.rm=TRUE)
   for (i in 1:m) { 
      polargs.i[[i]][["border"]] <- NA
      # Set the color with transparency.
      if ("col" %in% names(polargs.i[[i]])) {
         polargs.i[[i]][["col"]] <-
            rgb(t(col2rgb(polargs.i[[i]][["col"]])/256), alpha=.5)
      }
      else {
         polargs.i[[i]][["col"]] <- "#00000030"
      }

      x. <- c(densities[[i]]$estimate,
         -rev(densities[[i]]$estimate)) / (2.1*y.sup[i]) + x.pos[i]
      y. <- c(densities[[i]]$eval.points, rev(densities[[i]]$eval.points))
      do.call(what=polygon, args=c(list(x=x., y=y.), polargs.i[[i]]))

      x. <- c(.7*densities[[i]]$estimate,
         -rev(.7*densities[[i]]$estimate)) / (2.1*y.sup[i]) + x.pos[i]
      do.call(what=polygon, args=c(list(x=x., y=y.), polargs.i[[i]]))

      rect(xleft=x.pos[i]-.03, xright=x.pos[i]+.03,
           ybottom=quant[2,i], ytop=quant[3,i], col="black")
      segments(x0=x.pos[i], x1=x.pos[i], y0=quant[1,i], y1=quant[4,i])
      points(x.pos[i], medians[i], pch=18, cex=.6, col="white")
   }
}
