GO.pvals <- function(GOtable,
    reject=c("bilateral", "unilateral")) {
# Author: Guillaume Filion.
# Date: 2011-11-10.
# Compute Hypergeometric p-values from a GO count table,
# typically the intersections of target genes and genes
# with a given GO category. Assume that the left-most
# column and bottom row are total counts.

   # https://github.com/gui11aume/vtrackR.git
   require(vtrackR);

   # Get the type of rejection region (default: unilateral).
   reject <- match.arg(arg=reject);

   # The total-less table is (m x n).
   m <- nrow(GOtable)-1;
   n <- ncol(GOtable)-1;

   # Total genes in the background.
   grandtotal <- GOtable[m+1,n+1];

   pvals <- matrix(NA, nrow=m, ncol=n);
   # The sign of the p-value says whether there is an
   # enrichment or a depletion.
   sign <- matrix(1, nrow=m, ncol=n);

   # Loop over columns.
   for (i in 1:n) {
      phigher <- phyper(
            q = GOtable[1:m,i],
            m = GOtable[1:m,n+1],
            n = grandtotal-GOtable[1:m,n+1],
            k = GOtable[m+1,i],
            lower.tail = FALSE
      );
      if (reject == "bilateral") {
         # Symmetrize the p-value if bilateral test.
         plower <- phyper(
               q = GOtable[1:m,i]-1,
               m = GOtable[1:m,n+1],
               n = grandtotal-GOtable[1:m,n+1],
               k = GOtable[m+1,i],
               lower.tail = TRUE
         );
         sign[phigher < plower,i] <- - 1;
         pvals[,i] = 2 * apply(cbind(plower, phigher), MARGIN=1, min);
      }
      else {
         pvals[,i] <- phigher;
      }
      # To compute the correct p.value we have to add the
      # probability that "q = observed value".
      pvals[,i] = pvals[,i] + dhyper(
            x = GOtable[1:m,i],
            m = GOtable[1:m,n+1],
            n = grandtotal-GOtable[1:m,n+1],
            k = GOtable[m+1,i]
      );
   }
   
   rownames(pvals) <- rownames(GOtable)[-(m+1)];
   colnames(pvals) <- colnames(GOtable)[-(n+1)];

   return(vtag(sign*pvals));

}
