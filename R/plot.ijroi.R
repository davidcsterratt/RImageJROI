##' @title Plot IJROI object
##' @param x The IJROI object
##' @param add Whether to add to an existing plot
##' @param main an overall title for the plot: see \code{title}
##' @param xlab a title for the x axis: see \code{title}
##' @param ylab a title for the y axis: see \code{title}
##' @param ... Additional parameters
##' @method plot IJROI
##' @export
##' @author David Sterratt
plot.IJROI <- function(x, add=FALSE,
                       xlab = "", ylab = "", main = "", ...) {
  with(x, {
    if (!add) 
      plot(NA, NA, xlim=range(coords[,1]), ylim=range(coords[,2],),
           xlab = xlab, ylab = ylab, main = main)

    if (type == types["rect"]) {
      rect(left, bottom, right, top, ...)
    }
    if (type == types["oval"]) {
      theta <- seq(0, 2*pi, len=360)
      polygon(left + width/2*(1 + sin(theta)),
              top + height/2*(1 + cos(theta)), ...)
    }
    if (type == types["line"]) {
      warning("Plotting line not yet supported")
    }
    if (type %in% types[c("polygon", "freehand", "traced")]) {
      coords <- rbind(coords, coords[1,])
      lines(coords, ...)
    }
    if (type %in% types[c("polyline", "freeline", "angle")]) {
      lines(coords, ...)
    }
    if (type %in% types[c("point")]) {
      points(coords, ...)
    }
    
    
  })
}
