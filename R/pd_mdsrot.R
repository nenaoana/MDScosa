# Rotate Axes:

pd_mdsrot <- function
(RS_COORD, WF , EL)
{
  # Coord of OBS 1 and 2:
  x1 = mean(RS_COORD[WF, 1])
  y1 = mean(RS_COORD[WF, 2])
  x2 = mean(RS_COORD[EL, 1])
  y2 = mean(RS_COORD[EL, 2])
  
  # Center configuration:
  
  dim1c = RS_COORD[, 1] - ((x1+x2)/2)
  dim2c = RS_COORD[, 2] - ((y1+y2)/2)
  
  RS_COORD_c <- RS_COORD
  RS_COORD_c[,1] <- dim1c
  RS_COORD_c[,2] <- dim2c
  
  # New coordinates of obs1 and obs2:
  
  x1c = mean(RS_COORD_c[WF, 1])
  y1c = mean(RS_COORD_c[WF, 2])
  x2c = mean(RS_COORD_c[EL, 1])
  y2c = mean(RS_COORD_c[EL, 2])
  
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
  
  RS_COORD_r <- RS_COORD_c
  RS_COORD_r[,1] <- dim1r
  RS_COORD_r[,2] <- dim2r
  return(RS_COORD_r)
} 
