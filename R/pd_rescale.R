# Create rescale in one go:

pd_rescale <- function
(mdsmat, 
 distmat, 
 n_issues, 
 n_years)
{
  pd_mdscprod <- function
  (distmatr, wmat, n_issues, n_years)
  {
    product = 0
    frow = n_issues + 1
    lrow = ncol(distmatr)
    for (col in (1:n_issues)){
      for (row in (frow:lrow)){
        firstval = distmatr[col,row]
        secondval = wmat[col,row]
        if (!is.na(firstval) & !is.na(secondval)){product = product + firstval*secondval}
      }
    }
    product = product/n_years
    name = product
  }
  
  pd_mdsresc <- function
  (COORD, rf)
  {
    
    RCOORD <- COORD
    RCOORD[,1] <- COORD[,1]/rf
    RCOORD[,2] <- COORD[,2]/rf
    return(RCOORD)
    
  } 
  
  raw <- pd_mdscprod(distmatr = mdsmat[[1]], wmat = mdsmat[[2]], n_issues = n_issues, n_years = n_years) #for the initial dist
  final <- pd_mdscprod(distmatr = pscalemat[[1]], wmat = mdsmat[[2]], n_issues = n_issues, n_years = n_years) #for the new dist
  rf <- final/raw # the rescale factor
  # Rescale coordinates:
  RS_COORD <- pd_mdsresc(distmat[[2]], rf)
  return(RS_COORD)
}
