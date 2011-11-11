venn.plot <-function(areaA, areaB, areaAB) {
   #
   require(grid);
      
   # Normalize circle radii.
   r = sqrt(min(c(areaA, areaB)) / max(c(areaA, areaB)));
   a = pi * areaAB / max(c(areaA, areaB));

   get.area <- function(d) {
      # Quite a pain this geometry!
      # Assume R = 1 throughout.
      if (d > (r + 1)) {
         return (0);
      }
      x <- (d - (1-r^2)/d) / 2;
      l <- sqrt(r^2 - x^2);
      alpha <- acos(d-x);
      beta <- acos(x/r);
      return (alpha + beta*r^2 - d*l);
   }

   # Initial parameter values.
   d <- 1; epsilon <- 0.5;
   # Find the distance by bisecting.
   while (abs((this.a <- get.area(d)) - a) > 0.001) {
      d <- ifelse(this.a > a, d + epsilon, d - epsilon);
      epsilon <- epsilon /2;
   }

   # Plot the discs.
   grid.circle(x=0.5-d/8, y=0.5, r=0.25, gp=gpar(fill='red', alpha=0.5));
   grid.circle(x=0.5+d/8, y=0.5, r=r/4, gp=gpar(fill='blue', alpha=0.5));

}
