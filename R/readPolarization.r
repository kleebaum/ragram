setGeneric('readPolarization', function(object, raster, ...) {
    polPossibilities <- c('vv', 'vh', 'hh', 'hv')
    for(pol in c(polPossibilities, toupper(polPossibilities))) {
        if(length(grep(pol, raster@data@names))>0)
            return(pol)
    }
    return('unrecognized')
    standardGeneric('readPolarization')
})