setGeneric('readRaster', function(object) {
    path <- standardGeneric('readRaster')
    files <- list.files(path, pattern='*.tif*')
    if(length(object@polarization) > 0) {
        files <- grep(object@polarization, files, value=T)
        if(length(files)==0) {
            cat('Could not find measurement with polarization', object@polarization, ':-( \n')
            return(raster())
        }
    }
    if(!object@warped) {
        files <- grep('warped', files, invert=T, value=T)
    } else {
        files.warped <- grep('warped', files, value=T)
        if(length(files.warped)==0) {
            cat('The image gets warped. This might take a few minutes.\n')
            dstfile <- paste0(sub("\\.[[:alnum:]]+$", "", as.character(files)), '-warped.tiff')
            gdalwarp(file.path(path, files), 
                     file.path(path, dstfile),
                     r='bilinear',
                     #s_srs ="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                     of="GTiff", t_srs='+init=epsg:4326',
                     dstalpha=T, dstnodata='NA', overwrite = T)
            return(raster(file.path(path, dstfile)))
        } else {
            files <- files.warped
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
