\name{plot.wnom.coords}
\alias{plot.wnom.coords}
\title{Plotting method for Individuals from W-Nominate Model}
\description{Produces a plot of the legislators for a \code{wnominate} object.}

\usage{
plot.wnom.coords(obj, shapevar = NULL, dropNA=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{An object of class \code{nomObject}.}
  \item{shapevar}{Name of the variable that identifies the shapes in the resulting ggplot.}
  \item{dropNA}{Whether or not those who didn't vote Yea or Nay. }
  
}
\value{
A \code{ggplot} 
}
