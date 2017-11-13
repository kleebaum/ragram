test_that("Test zncc", {
  master <- matrix(1:144, nrow = 12, ncol = 12)
  slave <- matrix(2:145, nrow = 12, ncol = 12)
  znccR <- zncc(raster(master), raster(slave), 5, 5, 5, 5, 3)
  znccCpp <- znccCpp(master, slave, 5, 5, 5, 5, 3)
  expect_equal(znccR, znccCpp)
})

test_that("Test parallelization", {
  data(kili)
  master <- aggregate(master, 5)
  slave <- aggregate(slave, 5)
  dispMapParallel <- disparityMap(master, slave, window.size = 3, search.area.size = 5, 
    run.parallel = T)
  dispMapSequencial <- disparityMap(master, slave, window.size = 3, search.area.size = 5)
  expect_equal(dim(dispMapParallel), dim(dispMapSequencial))
  expect_equal(dispMapParallel, dispMapSequencial)
})