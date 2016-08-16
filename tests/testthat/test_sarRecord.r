test_that('Test sarRecord()', {
    sar.object.a <- sarRecord()
    sar.object.b <- new('SAR')
    expect_equal(sar.object.a, sar.object.b)
})

test_that('Test sarRecord(satellite="sentinel")', {
    s1.object <- sarRecord(satellite='sentinel')
    expect_equal(s1.object@satellite, 'sentinel-1')
})

test_that('Test sarRecord(raster=raster())', {
    sar.object.a <- sarRecord(raster=raster())
    sar.object.b <- sarRecord()
    expect_equal(sar.object.a, sar.object.b)
})

test_that('Test sarRecord(adress="123")', {
    sar.object.a <- sarRecord(address = '123')
    sar.object.b <- sarRecord(raster=raster())
    expect_equal(sar.object.a, sar.object.b)
})