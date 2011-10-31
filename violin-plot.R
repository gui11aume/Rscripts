violin.plot <- function (x, x.scaling=1, autoscale=0.995, 
   individual.scaling=FALSE, median.bar="grey50", x.pos, ...) {
# x: a list/data.frame of values.
# x.scaling: an x scaling factor to apply to the densities.
# autoscale: the minimum proportion of the data to be displayed.
# individual.scaling: whether violins should have the same max width.
# median.bar: color of the median bar, NA for none.
# x.pos: positions of the plots on the x axis.
# ...: density, angle, border, col, and lty are passed to polygon().
# Other parameters are passed to plot().

# -- Test calls -- #
# violin.plot(list(rnorm(100), 1+rnorm(100)), col=1:2)
# violin.plot(data.frame(x=rnorm(100), y=1+rnorm(100)), col=1:2)

   if (autoscale < 0 || autoscale > 1) {
      stop ("'autoscale' outside the interval [0,1]");
   }

   # Get the number of violin plots to draw.
   m <- length(x);

   # Automatically space violins.
   if (missing(x.pos)) {
      x.pos <- 1:m;
   }

   # Get densities.
   densities <- lapply(X=x, FUN=density, na.rm=TRUE);

   # Compute nice spacing parameters between plots.
   y.sup <- sapply(X=densities, FUN=function (x) max(x$y));
   x.min <- min(sapply(X=densities, FUN=function (x) min(x$x)));
   x.max <- max(sapply(X=densities, FUN=function (x) max(x$x)));
   if (individual.scaling)  {
      # Different width scaling per violin (remember that
      # y of the density is the width of the violin).
      y.sup <- y.sup/.8;
   }
   else {
      # Same width scaling for all violin.
      y.sup <- rep(max(y.sup), length(y.sup));
   }

   dotargs = list(...);
   # The density, angle, border, col, and lty parameters are passed
   # to polygon(). The rest is passed to plot().
   topol <- c("density", "angle", "border", "col", "lty");
   polargs <- dotargs[names(dotargs) %in% topol];
   plotargs <- dotargs[!names(dotargs) %in% topol];


   # -- plot() paramters -- #

   # Overwrite 'type', 'xaxt', 'x', and 'y'.
   plotargs[["type"]] <- "n";
   plotargs[["xaxt"]] <- "n";
   plotargs[["x"]] <- c(min(x.pos)-.6*x.scaling, max(x.pos)+.6*x.scaling);
   plotargs[["y"]] <- c(x.min, x.max);

   # Set the axis labels to empty if not specified.
   if (is.null(plotargs[["xlab"]])) {
      plotargs[["xlab"]] <- "";
   }
   if (is.null(plotargs[["ylab"]])) {
      plotargs[["ylab"]] <- "";
   }

   # Find a nice y zoom if ylim is not specified.
   if (is.null(plotargs[["ylim"]])) {
      ymin <- min(sapply(X=x, FUN=quantile, prob=(1-autoscale)/2,
         na.rm=TRUE));
      ymax <- max(sapply(X=x, FUN=quantile, prob=0.5+autoscale/2,
         na.rm=TRUE));
      plotargs[["ylim"]] <- c(ymin, ymax);
   }

   # Done!
   do.call(what=plot, args=plotargs);


   # -- polygon() paramters -- #

   # Manually recycle the parameters passed to 'polygon()'.
   # NB: 'polargs.i' is a list of lists of arguments.
   polargs.i <- rep(
      do.call(
         what=mapply,
         args=(c(list(FUN=list, SIMPLIFY=FALSE), polargs))
      ), length.out=m
   );

   # Plot the x-axis.
   axis(side=1, at=1:m, labels=names(x))

   # Plot the violins.
   for (i in 1:m) { 
      x. <- x.scaling*(c(densities[[i]]$y,
         -rev(densities[[i]]$y))) / (2.1*y.sup[i]) + x.pos[i]
      y. <- c(densities[[i]]$x, rev(densities[[i]]$x))
      do.call(what=polygon, args=c(list(x=x., y=y.), polargs.i[[i]]));

      # Plot the median bar.
      if (!is.na(median.bar)) {
         medians <- sapply(X=x, FUN=median, na.rm=TRUE)
         # Left to center dots.
         segments(x0=x.pos[i], x1=x.pos[i]-.4, y0=medians[i],
            y1=medians[i], lwd = 8, lty = "11", col=median.bar)
         # Center to right dots.
         segments(x0=x.pos[i], x1 = x.pos[i]+.4, y0=medians[i],
            y1=medians[i], lwd = 8, lty = "11", col=median.bar)
      }
   }
}
