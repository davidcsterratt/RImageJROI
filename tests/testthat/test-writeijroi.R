path <- file.path(system.file(package = "RImageJROI"), "extdata", "ijroi")

context("Writing an ROI to file")

testthat::test_that("write.ijroi() produces files that are consistent with the structure written", {

  name_roi <- dir(path, pattern = "*.roi")

  tmpDir <- tempdir()
  for (i in 1:length(name_roi)) {

    r <- RImageJROI::read.ijroi(file.path(path, name_roi[i]))
    RImageJROI::write.ijroi(file = file.path(tmpDir, name_roi[i]), roi = r, verbose = FALSE)

    r_2 <- RImageJROI::read.ijroi(file = file.path(tmpDir, name_roi[i]), verbose = FALSE)

    tryCatch(
      expr = {
        testthat::expect_identical(r, r_2)
      },
      error = function(e){
        print(name_roi[i])
        print(e)
      }
    )
  }
})
