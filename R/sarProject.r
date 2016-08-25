#' Project SAR object
#' 
#' Projects SAR object using \code{\link{gdalwarp}}.
#' 
#' @param object SAR object or character giving the address of a raster (e.g. GeoTiff).
#' @param suffix Character. This suffix is added to the image name in front of file extension.
#' @param dstfile Character. The destination file name.
#' @param ... Arguments of \code{\link{gdalwarp}}.
#' @return Character. The destination file name.
#' @export
#' @examples
#' # object@geolocationPoints <- spTransform(object@geolocationPoints, CRS(t_srs))
setGeneric('sarProject', function(object, 
                                  suffix='-latlon',
                                  dstfile=paste0(sub('\\.[[:alnum:]]+$', '', as.character(object@file@name)),suffix,'.tiff'),
                                  of='GTiff',
                                  r='bilinear',
                                  t_srs='+init=epsg:4326',
                                  dstalpha=T, dstnodata='NA', 
                                  overwrite = F, ...) {
    
    srcfile <- standardGeneric('sarProject')
    
    cat('The image gets projected using gdalwarp. This might take a few minutes.\n')
    gdalwarp(srcfile, dstfile, of=of, r=r, t_srs=t_srs, dstalpha=dstalpha, 
             dstnodata=dstnodata, overwrite=overwrite, ...)
    
    cat('The geolocation points can be manually projected using "spTransform".\n')
    
    cat('Projected SAR record is located here:\n')
    return(dstfile)
})

setMethod('sarProject', 'SAR', function(object, ...) {
    return(object@file@name)
})