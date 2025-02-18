#' Write an ImageJ ROI file.
#'
#' @param file Name of ImageJ ROI file to write
#' @param roi  A structure of class \code{ijroi} containing the ROI information
#' @param verbose Whether to report information
#' @export
#' @author Le Gao
#' @seealso \code{\link{read.ijroi}} for reading an ROI file
write.ijroi <- function(file, roi, verbose = TRUE) {
  # define internal helper functions
  putByte <- function(con, input) {
    pos <- seek(con)
    writeBin(object = as.raw(input), con = con, size = raw(0), useBytes = F, endian = "big")
    if(verbose) {
      message(paste0("Pos ", pos, "-", pos, ": Byte ", input))
    }
  }

  putShort <- function(con, input) {
    pos <- seek(con)
    writeBin(object = as.integer(input), con = con, size = 2, endian = "big")
    if(verbose) {
      message(paste0("Pos ", pos, "-", pos+1, ": Short ", input))
    }
  }

  putInt <- function(con, input) {
    pos <- seek(con)
    writeBin(object = as.integer(input), con = con, size = 4, endian = "big")
    if(verbose) {
      message(paste0("Pos ", pos, "-", pos+3, ": Integer ", input))
    }
  }

  putFloat <- function(con, input) {
    pos <- seek(con)
    writeBin(object = input, con = con, size = 4, endian = "big")
    if(verbose) {
      message(paste0("Pos ", pos, "-", pos+3, ": Float ", input))
    }
  }
  subtypes <- list(TEXT    = 1,
                   ARROW   = 2,
                   ELLIPSE = 3,
                   IMAGE   = 4)

  types <- list(polygon  = 0,
                rect     = 1,
                oval     = 2,
                line     = 3,
                freeline = 4,
                polyline = 5,
                noRoi    = 6,
                freehand = 7,
                traced   = 8,
                angle    = 9,
                point    = 10)

  con <- base::file(file, "wb")

  putByte(con, 73) # 0 I
  putByte(con, 111) # 1 o
  putByte(con, 117) # 2 u
  putByte(con, 116) # 3 t

  putShort(con, roi$version) # 4-5 version
  putByte(con, roi$type) # 6 type
  putByte(con, 0) # 7 unused

  putShort(con, roi$top) # 8-9 top
  putShort(con, roi$left) # 10-11 left
  putShort(con, roi$bottom) # 12-13 bottom
  putShort(con, roi$right) # 14-15 right

  putShort(con, roi$n) # 16-17 NCoordinates
  putFloat(con, roi$x1) # 18-21 x1
  putFloat(con, roi$y1) # 22-25 y1
  putFloat(con, roi$x2) # 26-29 x2
  putFloat(con, roi$y2) # 30-33 y2

  putShort(con, roi$strokeWidth) # 34-35 stroke width
  putInt(con, roi$shapeRoiSize) # 36-39 ShapeRoi size
  
  #putInt(con, roi$strokeColor) # 40-43 stroke color
  putByte(con, roi$strokeColor_a) # 40 alpha channel
  putByte(con, roi$strokeColor_r) # 41 red channel
  putByte(con, roi$strokeColor_g) # 42 green channel
  putByte(con, roi$strokeColor_b) # 43 blue channel
  
  #putInt(con, roi$fillColor) # 44-47 fill color
  putByte(con, roi$fillColor_a) # 44 alpha channel
  putByte(con, roi$fillColor_r) # 45 red channel
  putByte(con, roi$fillColor_g) # 46 green channel
  putByte(con, roi$fillColor_b) # 47 blue channel
  
  putShort(con, roi$subtype) # 48-49 subtype
  putShort(con, roi$options) # 50-51 options

  if((roi$type == types["freehand"]) &&
     (roi$subtype == subtypes["ELLIPSE"])) {
    putFloat(con, roi$aspectRatio)
  } else {
    putByte(con, roi$style) # 52-52 arrow style
    putByte(con, roi$headSize) # 53-53 arrow head size
    putShort(con, roi$arcSize) # 54-55 rounded rect arc size
  }


  putInt(con, roi$position) # 56-59 position
  putShort(con, 0) # 60-61 reserved (zeros) unused
  putShort(con, 0) # 62-63 Unused

  # 64- x-coordinates (short), followed by y-coordinates
  if(verbose) {
    message("Writing coordinate data")
  }

  isComposite <- (roi$shapeRoiSize > 0)
  if(isComposite) {
    stop("Composite ROIs not supported")
  }


  if(roi$type %in% types[c("polygon", "freehand", "traced", "polyline", "freeline", "angle", "point")]) {
    if(roi$n > 0) {
      for(i in 1:roi$n) {
        putShort(con, roi$coords[i,1] - roi$left)
      }

      for(i in 1:roi$n) {
        putShort(con, roi$coords[i,2] - roi$top)
      }
    }
  }

  close(con)
}


