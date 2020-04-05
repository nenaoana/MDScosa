\name{pd_pscale}
\alias{pd_pscale}
\title{
Function for creating distances and coordinates.
}
\description{
Function for creating distances and coordinates from weight and distances matrices obtained with pd_mdsmat.
}
\usage{
pd_pscale <- (mdsmat, 
 ndim = 2)}
\arguments{
  \item{mdsmat}{Distance and weight matrices. Usually the result of function pd_mdsmat.
}
\item{ndim}{The number of dimensions, default is 2, but can be changes for testing the stess. Keep in mind though that the following steps (plotting, etc.) are only for the case of ndim = 2.
}
}
\author{
Ioana-Elena Oana
}
