\name{pd_labelattr}
\alias{pd_labelattr}
\title{
Function for obtaining party and issue labels when these are listed as attributes.
}
\description{
Function for obtaining party and issue labels when these are listed as attributes.
}
\usage{
pd_labelattr <- (data,
 party_var,
 issue_var,
 coordmat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{Matrix with rescaled and rotated coordinates. Usually the result of pd_rescale and pd_mdsrot.
}
\item{party_var}{Name of the party variable that contains labels as attributes.
}
\item{issue_var}{Name of the issue variable that contains labels as attributes.
}
\item{coordmat}{Matrix with rescaled and rotated coordinates. Usually the result of pd_rescale and pd_mdsrot.
}
}
\author{
Ioana-Elena Oana
}
