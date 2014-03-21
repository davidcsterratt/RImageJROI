#' @title Read ImageJ zip file containing several ROI files
#' 
#' @description A wrapper function, which reads a zip file containing ImageJ ROI files using \code{\link{read.ijroi}} function. 
#' @param file zip file containing a collection of ImageJ ROI files
#' @param names Logical, indicating whether the ROI file names should be used as names for the elements in the list (see Return). If FALSE a sequence of names specifying the type of ROI is automatically generated.
#' @param list.files logical, indicating whether a data.frame of ROI files in \code{file} should be returned instead of a list of results. Defaults to FALSE. If TRUE equals to \code{unzip(file, list = TRUE)}.
#' @param print.all logical indicating whether to print all information from \code{\link{read.ijroi}} function as opposed to a subset of relevant information? Defaults to \code{FALSE}.
#' @param verbose Whether to report information (see \code{\link{read.ijroi}}).
#' @return An object of class \code{ijzip} containing a list of the coordinates and types of ImageJ ROIs. Each element is named after option specified in \code{names}.
#' @author Mikko Vihtakari
#' @seealso \code{\link{read.ijroi}}, \code{\link{plot.ijzip}}.
#' @examples
#' file <- file.path(system.file(package = "RImageJROI"), "extdata", "ijroi", "ijzip.zip")
#' x <- read.ijzip(file)
#' plot(x)
#' @export

read.ijzip <- function(file, names = TRUE, list.files = FALSE, print.all = FALSE, verbose = FALSE){

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
      xclude.always <- c("version", "types")
      tmp <- tmp[!names(tmp) %in% xclude.always]
      
      xclude.if.0 <- c("n", "strokeWidth", "shapeRoiSize", "strokeColor", "fillColor", "style", "headSize", "arcSize", "position")
      xclude.these <- unlist(lapply(tmp[names(tmp) %in% xclude.if.0], function(k) c(k == 0 | is.na(k))))
      xclude.these <- names(xclude.these[xclude.these == TRUE])
      tmp <- tmp[!names(tmp) %in% xclude.these]
      
      if(tmp$type == 1 | tmp$type ==2 | tmp$type == 10) {xclude.these.too <- c("x1", "y1", "x2", "y2")} else {
        xclude.these.too <- c("bottom", "left", "top", "right", "width", "height")}
      tmp <- tmp[!names(tmp) %in% xclude.these.too]
    
    if(is.null(tmp$coords)){
      Xcoords <- unlist(c(tmp[names(tmp) %in% c("left", "x1")], tmp[names(tmp) %in% c("right", "x2")]))
      Ycoords <- unlist(c(tmp[names(tmp) %in% c("top", "y1")], tmp[names(tmp) %in% c("bottom", "y2")]))
      tmp$coords <- data.frame(x = Xcoords, y = Ycoords)} else {
        colnames(tmp[["coords"]]) <- c("x", "y")}
    tmp2 <- list(tmp)
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
class(roi.dat) <- "ijzip"
return(roi.dat)}

if (list.files == TRUE) return(files)
}