venn.plot <-function(areaA, areaB, areaAB, areaTot, main,
   labels=TRUE) {
   #
   require(grid);
      

   # DRY function to get the distance between centers that
   # gives the specified shared area.
   find.d <- function(R = 1, r, a) {

      # Define a function to compute the shared area
      # given the distance (will be used for bisection).
      get.area <- function(d) {

         if (d > (r + R)) {
            return (0);
         }
         else if (d < (R-r)) {
            return (pi * r^2);
         }
         # x is the part of the distance in the small disc.
         # l is the half segment defined by the intersection.
         # ... a bit of a geometric black box, I admit.
         x <- (d - (R-r^2)/d) / 2;
         l <- sqrt(r^2 - x^2);
         alpha <- acos((d-x)/R);
         beta <- acos(x/r);
         return (alpha*R^2 + beta*r^2 - d*l);

      }

      # Bisect to find the distance.
      # Initial parameter values.
      d <- 1; epsilon <- 0.5;
      while (abs((this.a <- get.area(d)) - a) > 0.001) {
         d <- ifelse(this.a > a, d + epsilon, d - epsilon);
         epsilon <- epsilon /2;
      }

      return (d);

   }

   # Normalize circle radii.
   r <- sqrt(min(c(areaA, areaB)) / max(c(areaA, areaB)));
   a <- pi * areaAB / max(c(areaA, areaB));
   d <- find.d(R=1, r=r, a=a)

   grid.newpage();

   # Get the coordinates of the center.
   center.x <- unit(0.5, "npc") + unit(1-r, "inch");
   center.y <- unit(0.5, "npc");

   # Plot the discs.
   grid.circle(
      x = center.x - unit(d, "inch"),
      y = center.y,
      r = unit(2, "inch"),
      gp = gpar(fill = "red", alpha = 0.5)
   );
   grid.circle(
      x = center.x + unit(d, "inch"),
      y = center.y,
      r = unit(2*r, "inch"),
      gp = gpar(fill = "blue", alpha = 0.5)
   );

   # Write the title.
   if (!missing(main)) {
      grid.text(
         label = main,
         just = "center",
         x = unit(0.5, "npc"),  # (real page center)
         y = unit(0.5, "npc") + unit(2.8, "inch"),
         gp = gpar(cex = 1.5)
      );
   }

   # Write the labels if required.
   if (labels) {
      if (areaAB > 0) {
         grid.text(
            label = as.character(areaAB),
            x = center.x + unit(1-r, "inch"),
            y = center.y
         );
      }
      xA <- if(d - (r-1) > .15)
            center.x - unit(r+1, "inch")
         else
            center.x - unit(d+2+.5*r, "inch");
      xB <- if(d + (r-1) > .15)
            center.x + unit(r + 1, "inch")
         else
            center.x + unit(d + 2.5*r, "inch");
      grid.text(
         label = as.character(areaA),
         x = xA,
         y = center.y
      );
      grid.text(
         label = as.character(areaB),
         x = xB,
         y = center.y
      );
   }


   # Plot the expected overlap if required.
   if (!missing(areaTot)) {
      # Compute expected intersection.
      a.expt <- pi * areaA * areaB / areaTot^2;
      d.expt <- find.d(R=1, r=r, a=a.expt);
      grid.circle(
         x = center.x + unit(d.expt, "inch"),
         y = center.y,
         r = unit(2*r, "inch"),
         gp = gpar(lty = 2)
      );

   }

}
