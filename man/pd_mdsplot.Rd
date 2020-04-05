\name{pd_mdsplot}
\alias{pd_mdsplot}
\title{
Creates an MDS plot
}
\description{
Creates an MDS plot based on COSA data.
}
\usage{
pd_mdsplot <- (
    coordmat,
    labels = NULL,
    point_col = "black",
    point_size = 1.5,
    text_col = "black",
    text_size = 3,
    line_col = "red",
    title = "MDS",
    WF,
    EL,
    CL,
    DF
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{coordmat}{Matrix with rescaled and rotated coordinates. Usually the result of pd_rescale and pd_mdsrot.
}
\item{labels}{Labels of issues and parties in the plot. This needs to follow the order in coordmat. If left NULL, it automatically takes labels in coordmat.
}
\item{point_col}{Colour of points.
}
\item{point_size}{Size of points.
}
\item{text_col}{Colour of label text.
}
\item{text_size}{Size of label text.
}
\item{line_col}{Color of line.
}
\item{title}{Title of the plot.
}
\item{WF}{The label of the welfare issue in coordmat.}
\item{EL}{The label of the ecolib issue in coordmat.}
\item{CL}{The label of the cultlib issue in coordmat.}
\item{DF}{The label of the defense issue in coordmat.}
}
\author{
Ioana-Elena Oana
}
