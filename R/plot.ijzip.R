##' @title Plot IJZIP object
##' @param x The IJZIP object
##' @param add Whether to add to an existing plot
##' @param col.line the color of 'line' type ROI elements
##' @param col.polyline the color of 'polyline', 'freeline' and 'angle' type ROI elements
##' @param col.point the color of 'point' type ROI elements
##' @param main an overall title for the plot: see \code{title}
##' @param xlab a title for the x axis: see \code{title}
##' @param ylab a title for the y axis: see \code{title}
##' @param ... Arguments to be passed to methods, such as graphical
##' parameters (see \code{\link{par}})
##' @author Mikko Vihtakari
##' @seealso \code{\link{read.ijzip}}
##' @examples
##' file <- file.path(system.file(package = "RImageJROI"), "extdata", "ijroi", "ijzip.zip")
##' dat <- read.ijzip(file)  
##' plot(dat)
##' @method plot IJZIP
##' @export 
##' 
plot.IJZIP <- function(x, add=FALSE, col.line = "black", col.polyline = "black", col.point = "black", xlab = "", ylab = "", main = "", ...) {

## Base plot
    if (!add) {
      plot(NA, NA, xlim=range(unlist(lapply(x, function(i) i$coords[,1])), na.rm = TRUE), ylim=range(unlist(lapply(x, function(i) i$coords[,2])), na.rm = TRUE), axes = FALSE, xlab = xlab, ylab = ylab, main = main)
    }

for(i in seq_along(x)){  

## Lines

if (x[[i]]$strType %in% c("line")) lines(x[[i]]$coords, col = ifelse(length(col.line) == 1, col.line, col.line[i]), ...)

## Polyline, freeline, angle
  
if (x[[i]]$strType %in% c("polyline", "freeline", "angle")) lines(x[[i]]$coords, col = ifelse(length(col.polyline) == 1, col.polyline, col.polyline[i]), ...)
  
## Points 
  
if (x[[i]]$strType %in% c("point")) points(x[[i]]$coords, col = ifelse(length(col.point) == 1, col.point, col.point[i]), ...)

## Polygon, freehand, traced
if (x[[i]]$strType %in% c("polygon", "freehand", "traced")) {
coords <- rbind(x[[i]]$coords, x[[i]]$coords[1,])
      lines(coords, ...)
    }

## Rect
if (x[[i]]$strType %in% c("rect")) {
      rect(x[[i]]$left, x[[i]]$bottom, x[[i]]$right, x[[i]]$top, ...)
    }

## Oval
    if (x[[i]]$strType %in% c("oval")) {
      theta <- seq(0, 2*pi, len=360)
      polygon(x[[i]]$left + x[[i]]$width/2*(1 + sin(theta)), x[[i]]$top + x[[i]]$height/2*(1 + cos(theta)), ...)
    }
  
}

axis(1)
axis(2, las = 2)
}

