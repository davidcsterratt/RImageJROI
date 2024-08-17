#' @title Write ImageJ zip file containing several ROI files
#'
#' @description Write or add to a zip archive containing ImageJ ROI
#'   files using the \code{\link{write.ijroi}} function.
#' @param file zip archive to write that will contain a collection of
#'   ImageJ ROI files
#' @param roi A list of ROIs
#' @param verbose Whether to report information
#' @importFrom utils zip
#' @seealso \code{\link{write.ijroi}}
#' @export
write.ijzip <- function(file, roi, verbose = TRUE) {
  # Delete any existing archive
  unlink(file)

  # Create temporary directory
  location <- tempfile('roi')
  dir.create(location)

  # Add files to the archive one by one, to preserve the list order
  for(i in 1:length(roi)) {
    file_name <- file.path(location, paste0(roi[[i]]$name, ".roi"))
    write.ijroi(file = file_name,
                roi = roi[[i]], verbose = verbose)
    zip(zipfile = file,
      file_name,
      flags = "-9Xjq") # quiet mode
  }

  # Remove temporary location
  unlink(location, recursive = TRUE)
}






