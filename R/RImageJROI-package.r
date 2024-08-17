#' Read and write 'ImageJ' Region of Interest (ROI) files
#'
#' Provides functions to read and write 'ImageJ'
#' (http://imagej.nih.gov/ij/) Region of Interest (ROI) files, to plot
#' the ROIs and to convert them as spatstat (http://spatstat.org/)
#' spatial patterns.
#'
#' ImageJ ROI files can be read into R using the
#' \code{\link{read.ijroi}} and \code{\link{read.ijzip}} functions,
#' resulting in \code{ijroi} and \code{ijzip} objects.
#'
#' The objects can be plotted using generic
#' \code{\link[=plot.ijroi]{plot}} command and converted to
#' \link[spatstat.geom]{spatstat.geom} spatial patterns by using
#' \code{\link{ij2spatstat}} function.
#'
#' The \code{ijroi} and \code{ijzip} objects can be written to file
#' using the \code{\link{write.ijroi}} and \code{\link{write.ijzip}}
#' functions.
#'
#' @name RImageJROI
#' @docType package
NULL
