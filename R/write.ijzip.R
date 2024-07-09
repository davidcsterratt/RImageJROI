
#' @title Write ImageJ zip file containing several ROI files
#'
#' @description A wrapper function, which writes a zip file containing ImageJ ROI files using \code{\link{write.ijroi}} function.
#' @param file zip file to write that will contain a collection of ImageJ ROI files
#' @param roi a collection of roi list
#' @param verbose Whether to report information
#' @seealso \code{\link{write.ijroi}}
#' @return
#' @export
#'
#' @examples
write.ijzip <- function(file, roi, verbose = TRUE) {
  location <- tempdir(check = T)
  
  num_roi <- length(roi)
  for(i in 1:num_roi) {
    write.ijroi(file = paste0(location, "/", roi[[i]]$name, ".roi"), 
                roi = roi[[i]], verbose = verbose)
  }
  
  zip(zipfile = file, files = dir(path = location, full.names = T, no.. = T, pattern = "*.roi"))
  
  unlink(location, recursive = T)
}






