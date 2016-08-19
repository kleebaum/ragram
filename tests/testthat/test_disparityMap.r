test_that('Test zncc', {
    master <- matrix(1:144, nrow=12, ncol=12)
    slave <- matrix(2:145, nrow=12, ncol=12)
    znccR <- zncc(raster(master), raster(slave), 5, 5, 5, 5, 3)
    znccCpp <- znccCpp(master, slave, 5, 5, 5, 5, 3)
    #disparityMap(raster(master), raster(slave))
    expect_equal(znccR, znccCpp)
})