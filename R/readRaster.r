setGeneric('readRaster', function(object) {
    path <- standardGeneric('readRaster')
    files <- list.files(path, pattern='*.tif*')
    if(nchar(object@imgSubstring)>0) {
        files <- grep(object@imgSubstring, files, invert=F, value=T)
    }
    if(length(object@polarization) > 0) {
        files <- grep(object@polarization, files, value=T)
        if(length(files)==0) {
            cat('Could not find measurement with polarization', object@polarization, ':-( \n')
            return(raster())
        }
    }
    if(length(files)==1) {
        return(raster(file.path(path, files)))
    } else if(length(files)>1) {
        cat('There is more than one file, which one (number) to choose? \n')
        sapply(1:length(files), function(i) {
            cat(i, ': ', files[i], '\n', sep='')
        })
        i_chosen <- as.numeric(readLines(con=stdin(),1))
        return(raster(file.path(path, files[i_chosen])))
    }
    return(raster())
})

setMethod('readRaster', 'Sentinel', function(object) {
    file.path(object@address, 'measurement')
})

setMethod('readRaster', 'TSX', function(object) {
    file.path(object@address, 'IMAGEDATA')
})