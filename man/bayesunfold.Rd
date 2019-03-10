\name{bayesunfold}
\alias{bayesunfold}
\title{Bakker-Poole Bayesian Multidimensional Scaling: Unfolding Analysis of Ratio-Scale Data}

\description{
Performs the Bayesian multidimensional unfolding procedure described in Bakker and Poole, "Bayesian Metric Multidimensional Scaling" (2013).
The method runs L-BFGS to get the target configuration.
}

\usage{
bayesunfold(T, cutoff = 5, dims = 2, nsamp = 1500, burnin = 500, ...)
}

\arguments{
  \item{T}{A rectangular matrix of ratio-scale preferential choice data (e.g., feeling thermometer rankings of political stimuli) in which the individuals are the rows and the stimuli are the columns}
  \item{dims}{Number of dimensions to estimate}
  \item{cutoff}{Number of placements required for an individual to be included in the analysis}
  \item{nsamp}{Number of iterations of the slice sampler to be run after the burn-in period}
  \item{burnin}{Number of iterations of the slice sampler to be discarded}
  \item{...}{other arguments to be passed down to the \code{smacofRect} function. }
}

\value{
A list with the following elements:
\item{lbfgs.stimuli}{An object of class \code{mcmc} that holds the chain values (rows) for each of the paramters (columns) of the model}
\item{samples}{An object of class \code{alpha} that holds the chain values for the alpha parameter}
\item{bayes.result}{Bayes result}
}


\references{
Bakker, Ryan and Keith T. Poole. 2013. Bayesian Metric Multidimensional Scaling. Political Analysis 21: 125-140.
}


\author{
Dave Armstrong, Ryan Bakker, Christopher Hare and Keith T. Poole
}

\examples{
data(ANES1968)
T <- ANES1968[,1:12]
result <- bayesunfold(T, cutoff=5)
}

\keyword{ideal point estimation}
