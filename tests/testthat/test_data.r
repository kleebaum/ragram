test_that('Test data(kili)', {
    data(kili)
    expect_is(kili, 'SARSet')
    expect_true(master@data@inmemory)
    expect_true(slave@data@inmemory)
})

test_that('Test data(kili) angles', {
    anglesKili <- angles(kili)
    expect_is(anglesKili, 'list')
    expect_false(length(anglesKili)==0)
    expect_is(angles(master), 'SpatialPointsDataFrame')
    #expect_is(angles(master, interpolate=T, aggregate.fact=1000), 'SpatialPointsDataFrame')
})

test_that('Test data(kili) anglesDif', {
    expect_error(anglesDif(master, slave))
    #expect_is(anglesDif(master, slave, interpolate=T, aggregate.fact=1000), 'SpatialPointsDataFrame')
})

test_that('Test data(kili) disparityMapExpected', {
    expect_error(disparityMapExpected(master, slave))
    #expect_is(disparityMapExpected(master, slave, interpolate=T, aggregate.fact=1000), 'SpatialPointsDataFrame')
})