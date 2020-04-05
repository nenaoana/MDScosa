\name{pd_mdsmat}
\alias{pd_mdsmat}
\title{
Matrices of weights and distances.
}
\description{
Creates matrices of weights and distances, and sets minimum number of observations for parties and issues.
}
\usage{
pd_mdsmat <- (
    data,
    party_var,
    issue_var,
    proximity_var,
    year_var,
    years=NULL,
    noweight = FALSE,
    row = FALSE,
    cell = FALSE,
    min = 1,
    imin = NULL
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{A dataset containing COSA election campaign data.
}
\item{party_var}{Name of the party variable.
}
\item{issue_var}{Name of the issue variable.
}
\item{proximity_var}{Name of the proximity variable (i.e. direction).
}
\item{years}{Years to be included as a vector of characters.
}
\item{noweight}{If noweight is TRUE, the matrix of weights is not computed.
}
\item{row}{If TRUE, specifies that weights are computed separately for each party (and within each year if the option year is specified).
}
\item{cell}{If TRUE, weights are computed separately for each election.
}
\item{min}{Minimum number of observations for a party in an election.}
\item{imin}{Minimum percentage of observations for an issue to be included.}
}
\author{
Ioana-Elena Oana
}
