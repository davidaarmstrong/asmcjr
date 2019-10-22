\name{rc.errors}
\alias{rc.errors}
\title{Calculating the Number of Errors in Nominate Model}
\description{Calculates the number of errors and the PRE for a W-Nominate model.}

\usage{
rc.errors(obj, data, rcnum)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{An object of class \code{nomObject}.}
  \item{data}{\code{rollcall} object used to estimate \code{nomObject}.  }
  \item{rcnum}{Number identifying the roll call to be plotted.}
  
}
\value{
The number of errors and PRE are returned.
}
