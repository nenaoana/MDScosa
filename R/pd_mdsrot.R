# Rotate Axes:

pd_mdsrot <- function
(rescalemat, 
 WF, 
 EL)
{
  # Coord of OBS 1 and 2:
  x1 = mean(rescalemat[WF, 1])
  y1 = mean(rescalemat[WF, 2])
  x2 = mean(rescalemat[EL, 1])
  y2 = mean(rescalemat[EL, 2])
  
  # Center configuration:
  
  dim1c = rescalemat[, 1] - ((x1+x2)/2)
  dim2c = rescalemat[, 2] - ((y1+y2)/2)
  
  rescalemat_c <- rescalemat
  rescalemat_c[,1] <- dim1c
  rescalemat_c[,2] <- dim2c
  
  # New coordinates of obs1 and obs2:
  
  x1c = mean(rescalemat_c[WF, 1])
  y1c = mean(rescalemat_c[WF, 2])
  x2c = mean(rescalemat_c[EL, 1])
  y2c = mean(rescalemat_c[EL, 2])
  
  # determine appropriate rotation (based on coord of obs2):
  
  i=0
  if (x2c>0 & y2c>0) {i=1}
  if (x2c<0 & y2c<0) {i=2}
  if (x2c>0 & y2c<0) {i=3}
  if (x2c<0 & y2c>0) {i=4}
  
  # obs2: both coordinates positive --> clockwise rotation
  if (i==1) {
    dim1r=dim1c*(x2c/sqrt(x2c^2 +y2c^2))-dim2c*(-1*(y2c/sqrt(x2c^2+y2c^2)))
    dim2r=dim1c*(-1*(y2c/sqrt(x2c^2+y2c^2)))+dim2c*(x2c/sqrt(x2c^2+y2c^2))
  }
  
  # obs2: both coordinates negative  --> clockwise rotation + inversion
  if (i==2) {
    dim1r=dim1c*(x1c/sqrt(x1c^2+y1c^2))-dim2c*(-1*(y1c/sqrt(x1c^2+y1c^2)))
    dim2r=dim1c*(-1*(y1c/sqrt(x1c^2+y1c^2)))+dim2c*(x1c/sqrt(x1c^2+y1c^2))
    dim1r=dim1r * (-1)
  }
  
  # obs2: positive on 1st dim, negative on 2nd dim --> anticlockwise rotation
  if (i==3) {
    dim1r=dim1c*(x2c/sqrt(x2c^2+y1c^2))-dim2c*(y1c/sqrt(x2c^2+y1c^2))
    dim2r=dim1c*(y1c/sqrt(x2c^2+y1c^2))+dim2c*(x2c/sqrt(x2c^2+y1c^2))
  }
  
  # obs2: negative on 1st dim, positive on 2nd dim --> anticlockwise rotation + inversion
  if (i==4) {
    dim1r=dim1c*(x1c/sqrt(x1c^2+y2c^2))-dim2c*(y2c/sqrt(x1c^2+y2c^2))
    dim2r=dim1c*(y2c/sqrt(x1c^2+y2c^2))+dim2c*(x1c/sqrt(x1c^2+y2c^2))
    dim1r = dim1r*(-1)
  }
  
  # check
  if (i==0) stop('i==0??? - check the command again, you might have messed something up!')
  
  rescalemat_r <- rescalemat_c
  rescalemat_r[,1] <- dim1r
  rescalemat_r[,2] <- dim2r
  return(rescalemat_r)
} 
