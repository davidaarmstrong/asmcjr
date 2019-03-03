\name{bayesunfold}
\alias{bayesunfold}
\title{bayesunfold}
\description{Bayesian Metric unfolding.}
\usage{
bayesunfold(input, cutoff = 5, dims = 2, nsamp = 1500, burnin = 500, ...)
}
\arguments{
  \item{input}{An data frame or matrix of individuals by stimuli placements.}
  \item{cutoff}{Minimum number of stimuli that need to be rated to be included in result.}
  \item{dims}{Number of dimensions to estimate.}
  \item{nsamp}{Number of samples saved from the slice sampler.}
  \item{burnin}{Number of burnin iterations before sampling starts}
  \item{...}{Other arguments passed down to \code{\link{smacofRect}}.  Must include at least \code{type} and \code{conditionality}.  See help file for details.}
}
\description{
The \code{bayesunfold} routine uses L-BFGS to get the appropriate point configuration and then slice sampling to sample from the posteriors. 

We have had some trouble getting this to work reliably on the macOS.  As of this writing, using the current stable version 3.5.2 produced a segfault error at the slice sampling phase.  We found that compiling R-devel from scratch and then installing the relevant packages is a reliable solution.  This also produced reliable results on Linux (Debian).  
}
\value{
A list that will include the following:

\item{lbfgs.result}{The L-BFGS scaling result.}
\item{samples}{The samples from the posterior distribution of placements as an \code{mcmc.list}.}
\item{stimuli}{The stimulus placements in 1-\code{dims} dimensions along with lower and upper credible intervals. }
\item{individuals}{The individual placements in 1-\code{dims} dimensions along with lower and upper credible intervals.}
\item{sigma_squared_hat}{Residual variance. }
\item{sigma_squared_hat_sd}{Posterior SD of residual variance. }
}
