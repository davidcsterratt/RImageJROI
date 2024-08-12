context("Writing a list of ROIs to a zip archive")

path <- file.path(system.file(package = "RImageJROI"), "extdata", "ijroi")

testthat::test_that("ROI zip files read and wrote are consistent", {
  roizip_infile <- file.path(path, "ijzip.zip")
  roizip_outfile <- file.path(tempdir(), "ijzip.zip")
  unlink(roizip_outfile)

  r <- RImageJROI::read.ijzip(roizip_infile)

  RImageJROI::write.ijzip(file = roizip_outfile, roi = r, verbose = FALSE)
  r_2 <- RImageJROI::read.ijzip(file = roizip_outfile, verbose = FALSE)

  testthat::expect_identical(r, r_2)
})
