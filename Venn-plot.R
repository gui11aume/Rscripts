venn.plot <-function(areaA, areaB, areaAB, areaTot, main, labels=TRUE) {
   #
   require(grid);
      
   if (!all(c(areaA, areaB, areaAB) > 0)) {
      stop("Venn plot requires strictly positive values");
   }

   if (!missing(areaTot) && (areaTot < areaA + areaB - areaAB)) {
      stop("specified value of areaTot is inconsistent");
   }

   # DRY function to get the distance between centers that
   # gives the specified shared area.
   find.d <- function(R, r, a) {

      # Define a function to compute the shared area
      # given the distance (will be used for bisection).
      get.area <- function(d) {

         if (d > (r + R)) {
            return (0);
         }
         else if (d < abs(R-r)) {
            return (pi * min(R, r)^2);
         }
         # x is the part of the distance in the small disc.
         # l is the half segment defined by the intersection.
         # ... a bit of a geometric black box, I admit.
         x <- (d - (R^2-r^2)/d) / 2;
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

   # Normalize circle radius: the largest is set to 1,
   # so the larger area is pi.
   R <- sqrt(areaA / max(c(areaA, areaB)));
   r <- sqrt(areaB / max(c(areaA, areaB)));
   a <- pi * areaAB / max(c(areaA, areaB));
   d <- find.d(R=R, r=r, a=a)

   grid.newpage();

   # ---------------------------------------------
   # A note on the scaling.                       
   #                                              
   # The width of the viewport is 1 "snpc" so the 
   # larger radius is 0.25 "snpc": all dimensions 
   # have to be divided by 4.                     
   # ---------------------------------------------

   # Get the coordinates of the center.
   center.x <- unit(0.5, "npc") + unit((R-r)/8, "snpc");
   center.y <- unit(0.5, "npc");

   # Plot the discs.
   grid.circle(
      x = center.x - unit(d/8, "snpc"),
      y = center.y,
      r = unit(R/4, "snpc"),
      gp = gpar(fill = "red", alpha = 0.5)
   );
   grid.circle(
      x = center.x + unit(d/8, "snpc"),
      y = center.y,
      r = unit(r/4, "snpc"),
      gp = gpar(fill = "blue", alpha = 0.5)
   );

   # Write the title.
   if (!missing(main)) {
      grid.text(
         label = main,
         just = "center",
         x = unit(0.5, "npc"),  # (real page center)
         y = unit(1, "npc") - unit(1, "cm"),
         gp = gpar(cex = 1.5)
      );
   }

   # Write the labels if required.
   if (labels) {
      if (areaAB > 0) {
         grid.text(
            label = as.character(areaAB),
            x = center.x + unit((R-r)/8, "snpc"),
            y = center.y
         );
      }
      if (areaA - areaAB > 0) {
         xA <- if(d - (r-1) > .15)
               center.x - unit((r+R)/8, "snpc")
            else
               center.x - unit((d+2.5*R)/8, "snpc");
         grid.text(
            label = as.character(areaA - areaAB),
            x = xA,
            y = center.y
         );
      }
      if (areaB - areaAB > 0) {
         xB <- if(d + (r-1) > .15)
               center.x + unit((r+R)/8, "snpc")
            else
               center.x + unit((d+2.5*r)/8, "snpc");
         grid.text(
            label = as.character(areaB - areaAB),
            x = xB,
            y = center.y
         );
      }
   }


   # Plot the expected overlap if required.
   if (!missing(areaTot)) {
      # Compute expected intersection.
      a.expt <- pi * areaA * areaB / areaTot^2;
      d.expt <- find.d(R=R, r=r, a=a.expt);
      grid.circle(
         x = center.x + unit((2*d.expt-d)/8, "snpc"),
         y = center.y,
         r = unit(r/4, "snpc"),
         gp = gpar(lty = 2)
      );

   }

}
