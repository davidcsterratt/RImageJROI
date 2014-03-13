\name{plot.IJZIP}
\alias{plot.IJZIP}
\title{Plot IJZIP object}
\usage{
  \method{plot}{IJZIP} (x, add = FALSE, col.line = "black",
    col.polyline = "black", col.point = "black", xlab = "",
    ylab = "", main = "", ...)
}
\arguments{
  \item{x}{The IJZIP object}

  \item{add}{Whether to add to an existing plot}

  \item{col.line}{the color of 'line' type ROI elements}

  \item{col.polyline}{the color of 'polyline', 'freeline'
  and 'angle' type ROI elements}

  \item{col.point}{the color of 'point' type ROI elements}

  \item{main}{an overall title for the plot: see
  \code{title}}

  \item{xlab}{a title for the x axis: see \code{title}}

  \item{ylab}{a title for the y axis: see \code{title}}

  \item{...}{Arguments to be passed to methods, such as
  graphical parameters (see \code{\link{par}})}
}
\description{
  Plot IJZIP object
}
\examples{
file <- file.path(system.file(package = "RImageJROI"), "extdata", "ijroi", "ijzip.zip")
dat <- read.ijzip(file)
plot(dat)
}
\author{
  David Sterratt, Mikko Vihtakari
}
\seealso{
  \code{\link{read.ijzip}}
}

