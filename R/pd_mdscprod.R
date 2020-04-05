# Equivalent of mdscprod:

pd_mdscprod <- function
(distmat, wmat, n_issues, years)
{
  product = 0
  frow = n_issues + 1
  lrow = ncol(distmat)
  for (col in (1:n_issues)){
    for (row in (frow:lrow)){
      firstval = distmat[col,row]
      secondval = wmat[col,row]
      if (!is.na(firstval) & !is.na(secondval)){product = product + firstval*secondval}
    }
  }
  product = product/years
  name = product
}