# Equivalent to proxscal:

pd_pscale <- function
(mdsmat, 
 ndim = 2)
  
{
  set.seed(4321) 
  RT <- smacofSym(mdsmat[[1]], ndim = ndim, type = "ratio", 
                  weightmat = mdsmat[[2]], init = "random", verbose = FALSE, 
                  relax = FALSE, modulus = 1, itmax = 100, eps = 1e-4, 
                  spline.degree = 2, spline.intKnots = 2)
  cat(c("Stress for ",ndim," dimensions is: ", RT$stress, "\n"))
  
  # Distances:
  DIST  <- as.matrix(RT$confdist)
  colnames(DIST) <- colnames(mdsmat[[1]])
  
  # Coordinates:
  COORD <- RT$conf
  rownames(COORD) <-  colnames(mdsmat[[1]])
  COORD <- as.data.frame(COORD)
  
  PSCALE <- list(dist = DIST, coord = COORD)
  return(PSCALE)
}
