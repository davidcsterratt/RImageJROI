
path <- file.path(system.file(package = "RImageJROI"), "extdata", "ijroi")

testthat::test_that(
  desc = "ROI zip files read and wrote are consistent",
  code = {
    path_roizip <- file.path(path, "ijzip.zip")
    
    r <- RImageJROI::read.ijzip(path_roizip)
    RImageJROI::write.ijzip(file = "/tmp/ijzip.zip", roi = r, verbose = F)
    r_2 <- RImageJROI::read.ijzip(file = "/tmp/ijzip.zip", verbose = F)
    
    testthat::expect_identical(r, r_2)
  })
