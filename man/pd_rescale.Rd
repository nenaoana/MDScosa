\name{pd_rescale}
\alias{pd_rescale}
\title{
Function for rescaling coordinates.
}
\description{
Function for rescaling the coordinates obtained with pd_mdsmat and pd_pscale.
}
\usage{
pd_rescale <- (mdsmat, 
 distmat, 
 n_issues, 
 n_years)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mdsmat}{Distance and weight matrices. Usually the result of function pd_mdsmat.
}
\item{distmat}{Matrix of distances and coordinates. Usually the result of pd_pscale.
}
\item{n_issues}{Number of issues left after deletion with the pd_mdsmat function.
}
\item{n_years}{Number of years.
}
}
\author{
Ioana-Elena Oana
}
