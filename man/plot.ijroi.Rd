\name{plot.ijroi}
\alias{plot.ijroi}
\title{Plot ijroi object}
\usage{
  \method{plot}{ijroi} (x, add = FALSE, xlab = "",
    ylab = "", main = "", asp = 1, ...)
}
\arguments{
  \item{x}{The \code{ijroi} object.}

  \item{add}{Whether to add to an existing plot.}

  \item{main}{an overall title for the plot:
  \code{\link{title}}.}

  \item{xlab}{a title for the x axis: \code{\link{title}}.}

  \item{ylab}{a title for the y axis: \code{\link{title}}.}

  \item{asp}{numeric defining the aspect ratio y/x: see
  \code{\link{plot.window}}. Defaults to 1.}

  \item{...}{Additional parameters.}
}
\description{
  Plots ImageJ ROI objects using the
  \link[=graphics]{'base' graphics} package.
}
\details{
  ImageJ ROI objects created with following tools are
  plotted using following graphics commands: \itemize{
  \item{Rectangle tool ("rect")} \code{\link{rect}}.
  \item{Oval selections ("oval")} \code{\link{polygon}}.
  \item{Elliptical selections ("freehand")}
  \code{\link{lines}}. Note that these are exported as
  "freehand" type and do not draw correctly at the moment.
  \item{Point Tool and Multi-Point Tool ("point")}
  \code{\link{points}}. \item{Straight Line ("line")}
  \code{\link{lines}}. \item{Segmented Line ("polyline")}
  \code{\link{lines}}. \item{Freehand Line ("freeline")}
  \code{\link{lines}}. } All graphics allow the additional
  parameters from appropriate functions. Aspect ratio
  (\code{asp}) is 1 by default leading to correct
  representation of ImageJ objects. If correct
  representation is not important, set \code{asp = NA} to
  use the R base-graphics default setting.
}
\examples{
# 'oval' ROIs are plotted using polygon()
file <- file.path(system.file(package = "RImageJROI"), "extdata", "ijroi", "oval.roi")
x <- read.ijroi(file)
plot(x, border = "red")

# 'polygon' ROIs are plotted using lines()
file <- file.path(system.file(package = "RImageJROI"), "extdata", "ijroi", "polygon.roi")
x <- read.ijroi(file)
plot(x, col = "red")

# 'rect' ROIs are plotted using rect()
file <- file.path(system.file(package = "RImageJROI"), "extdata", "ijroi", "rect.roi")
x <- read.ijroi(file)
plot(x, border = "red")

# 'line' ROIs (among others listed in 'details') are plotted using lines()
file <- file.path(system.file(package = "RImageJROI"), "extdata", "ijroi", "line.roi")
x <- read.ijroi(file)
plot(x, col = "red")

# 'polyline' ROIs are plotted using lines()
file <- file.path(system.file(package = "RImageJROI"), "extdata", "ijroi", "segmented_line.roi")
x <- read.ijroi(file)
plot(x, col = "red")

# Objects created using 'Elliptical selections' tool are saved as
# 'freehand' and do not come out right.
file <- file.path(system.file(package = "RImageJROI"), "extdata", "ijroi", "elliptical.roi")
x <- read.ijroi(file)
plot(x, col = "red")

# 'point' ROIs are plotted using points()
file <- file.path(system.file(package = "RImageJROI"), "extdata", "ijroi", "multi_point.roi")
x <- read.ijroi(file)
plot(x, col = "red")

# 'freeline' ROIs are plotted using lines()
file <- file.path(system.file(package = "RImageJROI"), "extdata", "ijroi", "freehand_line.roi")
x <- read.ijroi(file)
plot(x, col = "red")
}
\author{
  David Sterratt, Mikko Vihtakari
}
\seealso{
  \code{\link{read.ijroi}}, \code{\link{read.ijzip}},
  \code{\link{plot.ijzip}}
}

