

path <- file.path(system.file(package = "RImageJROI"), "extdata", "ijroi")

testthat::test_that(
  desc = "ROI files read and wrote are consistent",
  code = {
    name_roi <- dir(path, pattern = "*.roi")
    
    for(i in 1:length(name_roi)) {
      r <- RImageJROI::read.ijroi(file.path(path, name_roi[i]))
      RImageJROI::write.ijroi(file = paste0("/tmp/", name_roi[i]), roi = r, verbose = F)
      r_2 <- RImageJROI::read.ijroi(file = paste0("/tmp/", name_roi[i]), verbose = F)
      testthat::expect_identical(r, r_2)
    }
  })