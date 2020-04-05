\name{pd_mdsrot}
\alias{pd_mdsrot}
\title{
Function for rotating coordinates.
}
\description{
Function for rotating the rescaled coordinates.
}
\usage{
pd_mdsrot <- (rescalemat, 
 WF, 
 EL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rescalemat}{Distance and weight matrix. Usually the result of function pd_mdsmat.
}
\item{rescalemat}{Matrix with rescaled and rotated coordinates. Usually the result of pd_pscale.
}
\item{WF}{The label of the welfare issue in rescalemat.}
\item{EL}{The label of the ecolib issue in rescalemat.}
}
\author{
Ioana-Elena Oana
}
