test_that('Test sarRecord()', {
    sar.object.a <- sarRecord()
    sar.object.b <- new('SAR')
    expect_equal(sar.object.a, sar.object.b)
})

test_that('Test sarRecord(satellite="sentinel")', {
    s1.object <- sarRecord(satellite='sentinel')
    expect_is(s1.object, 'RasterLayer')
    expect_is(s1.object, 'SAR')
    expect_is(s1.object, 'Sentinel')
    expect_equal(s1.object@satellite, 'sentinel-1')
})

test_that('Test sarRecord(raster())', {
    sar.object.a <- sarRecord(raster())
    sar.object.b <- sarRecord()
    expect_equal(sar.object.a, sar.object.b)
})

test_that('Test sarRecord("123")', {
    sar.object.a <- sarRecord('123')
    sar.object.b <- sarRecord(raster())
    expect_equal(sar.object.a, sar.object.b)
})