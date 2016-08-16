setGeneric('sarProject', function(object, 
                                  prefix='-warped',
                                  dstfile=paste0(sub('\\.[[:alnum:]]+$', '', as.character(object@file@name)),prefix,'.tiff'),
                                  of='GTiff',
                                  r='bilinear',
                                  t_srs='+init=epsg:4326',
                                  dstalpha=T, dstnodata='NA', 
                                  overwrite = F, ...) {
    cat('The image gets projected using gdalwarp. This might take a few minutes.\n')
    standardGeneric('sarProject')
})

setMethod('sarProject', 'SAR', function(object, dstfile, of, r, t_srs, dstalpha, overwrite, ...) {
    gdalwarp(object@file@name, 
             dstfile, of=of, r=r, t_srs=t_srs, dstalpha=dstalpha, 
             dstnodata=dstnodata, overwrite=overwrite, ...)
})