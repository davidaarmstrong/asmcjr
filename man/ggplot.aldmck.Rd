\name{ggplot.aldmck}
\alias{ggplot.aldmck}
\title{Alternative plotting function for Aldrich-McKelvel Results}
\description{Uses \code{ggplot2} to produce alternative visualizations for results of the \code{aldmck} function.  }
\usage{
ggplot.aldmck(result, groupVar = NULL, addStim = FALSE, weights = c("all", "positive", "negative"), xlab = NULL, main = NULL, ylab = NULL, ...)
}
\arguments{
  \item{result}{An object of class \code{aldmck} that is the result of an Aldrich-McKelvey scaling analysis.}
  \item{groupVar}{Optional grouping variable that will paritition the observations to produce different density cuvres for \code{aldmck} results.}
  \item{addStim}{Logical indicating whether the stimulus locations should be added to the density plot.}
  \item{weights}{Which observations should be used in plotting.  The default is to use all available observations.  The \code{"positive"} and \code{"negative"} options allow users to plot only those observations with either positive or negative weights, respectively.}
  \item{xlab}{Optional x-label for the plot.}
  \item{main}{Optional main title for the plot.}
  \item{ylab}{Optional y-label for the plot.}
  \item{\dots}{Other arguments to be passed down, currently unimplemented.}
}
\value{
A \code{ggplot} of the stimulus locations and density of ideal points.
}
\seealso{
\code{\link{aldmck}}
}
