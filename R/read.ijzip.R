#' @title Read ImageJ zip file containing several ROI files
#' 
#' @description A wrapper function, which reads a zip file containing ImageJ ROI files using \code{\link{read.ijroi}} function. 
#' @param file zip file containing a collection of ImageJ ROI files
#' @param names Logical, indicating whether the ROI file names should be used as names for the elements in the list (see Return). If FALSE a sequence of names specifying the type of ROI is automatically generated.
#' @param list.files logical, indicating whether a data.frame of ROI files in \code{file} should be returned instead of a list of results. Defaults to FALSE. If TRUE equals to \code{unzip(file, list = TRUE)}.
#' @param print.all logical indicating whether to print all information from \code{\link{read.ijroi}} function as opposed to a subset of relevant information? Defaults to \code{FALSE}.
#' @param verbose Whether to report information (see \code{\link{read.ijroi}}).
#' @return An object of class \code{IJZIP} containing a list of the coordinates and types of ImageJ ROIs. Each element is named after option specified in \code{names}.
#' @author Mikko Vihtakari
#' @seealso \code{\link{read.ijroi}}, \code{\link{plot.IJZIP}}.
#' @examples
#' file <- file.path(system.file(package = "RImageJROI"), "extdata", "ijroi", "ijzip.zip")
#' dat <- read.ijzip(file)
#' plot(dat)
#' @export

read.ijzip <- function(file, verbose = FALSE, names = TRUE, list.files = FALSE, print.all = FALSE){

## Read files in the zip file
files <- unzip(file, list = TRUE)

if(any(sapply(strsplit(files$Name, "\\."), '[', 2) == "roi") == FALSE) stop("The zip file contains other files than ImageJ ROIs")

if(list.files == FALSE){
  ## Find a suitable location to unizip the zip file
  location <- tempfile()
  
  ## Unzip the zip file to a temporary folder
  unzip(file, exdir = location)
  
  # Read ROIs
  if(print.all){
    roi.dat <- sapply(seq_along(files$Name), function(i){
      tmp <- read.ijroi(paste(location, files$Name, sep = "/")[i], verbose = verbose)
      if(is.null(tmp$coords)){
      tmp$coords <- data.frame(x = NA, y = NA)} else {
        colnames(tmp[["coords"]]) <- c("x", "y")}
      tmp2 <- list(c(tmp))
      names(tmp2) <- tmp[["name"]]
      return(tmp2)})
  } else {
    roi.dat <- sapply(seq_along(files$Name), function(i){
    tmp <- read.ijroi(paste(location, files$Name, sep = "/")[i], verbose = verbose)
    tmp <- tmp[names(tmp) %in% c("name", "coords", "strType", "bottom", "left", "top", "right", "width", "height", "xrange", "yrange")]
    if(is.null(tmp$coords)){
      tmp$coords <- data.frame(x = c(tmp[["left"]], tmp[["right"]]), y = c(tmp[["bottom"]], tmp[["top"]]))} else {
        colnames(tmp[["coords"]]) <- c("x", "y")}
    tmp2 <- list(c(tmp["coords"], tmp["bottom"], tmp["left"], tmp["top"], tmp["right"], tmp["width"], tmp["height"], tmp["xrange"], tmp["yrange"], tmp["strType"]))
    names(tmp2) <- tmp[["name"]]
    tmp2})
  }
  
  ## Remove the temporary folder
  unlink(location, recursive = TRUE)
  
## Rename elements of the returned list
  if (names == FALSE){
    rep.names <- unlist(lapply(seq_along(roi.dat), function(i) roi.dat[[i]]$strType))
    rep.names <- make.unique(rep.names)
    rep.numbers <- sapply(strsplit(rep.names, "\\."), '[', 2)
    rep.numbers[is.na(rep.numbers)] <- 0
    rep.names <- paste(sapply(strsplit(rep.names, "\\."), '[', 1), as.character(as.numeric(rep.numbers)+1), sep = "_")
    names(roi.dat) <- rep.names
  }
class(roi.dat) <- "IJZIP"
  return(roi.dat)}

if (list.files == TRUE) return(files)
}