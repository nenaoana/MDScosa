# Create rescale in one go:

pd_rescale <- function
(OLD_MAT, NEW_MAT, n_issues, years)
{
  raw <- pd_mdscprod(distmat = OLD_MAT[[1]], wmat = OLD_MAT[[2]], n_issues = n_issues, years = years) #for the initial dist
  final <- pd_mdscprod(distmat = NEW_MAT[[1]], wmat = OLD_MAT[[2]], n_issues = n_issues, years = years) #for the new dist
  rf <- final/raw # the rescale factor
  # Rescale coordinates:
  RS_COORD <- pd_mdsresc(NEW_MAT[[2]], rf)
  return(RS_COORD)
}