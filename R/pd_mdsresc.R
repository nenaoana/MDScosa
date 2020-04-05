# Equivalent of mdsresc:

pd_mdsresc <- function
(COORD, rf)
{
  
  RCOORD <- COORD
  RCOORD[,1] <- COORD[,1]/rf
  RCOORD[,2] <- COORD[,2]/rf
  return(RCOORD)
  
} 