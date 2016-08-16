test_that('Test zncc', {
    master <- matrix(1:81, nrow=9, ncol=9)
    slave <- matrix(2:82, nrow=9, ncol=9)
    znccR <- zncc(raster(master), raster(slave), 5, 5, 5, 5, 3)
    znccCpp <- znccCpp(master, slave, 5, 5, 5, 5, 3)
    expect_equal(znccR, znccCpp)
})