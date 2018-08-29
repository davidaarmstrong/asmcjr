\name{bamPrep}
\alias{bamPrep}
\title{Prepare data for Bayesian Aldrich-McKelvey Scaling}
\description{
Prepares data to be passed down to the \code{\link{BAM}} function.
}
\usage{
bamPrep(x, nmin = 1, missing = NULL, self = 1, midpt = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A \code{data.frame} or \code{matrix} containing self-placement (if available) and idiosyncratic placements of stimuli.  No other variables should be in the dataset}
  \item{nmin}{Minimum number of valid stimuli placements needed for an observation to be retained in the data.}
  \item{missing}{Vector of values that will be recoded to missing in addition to the usual \code{NA} values. }
  \item{self}{Integer giving column containing self-placements.}
  \item{midpt}{Value around which all columns will be centered.  If \code{NULL} (the default), algorithm will use the empirical midpoint for each variable. }
}
\value{
A list with two elements:
\item{stims}{A matrix giving the centered values of the idiosyncratic placement of the stimuli.}
\item{self}{A vector of self-placement values (if availabel).}
}
\seealso{
\code{\link{BAM}}
}
