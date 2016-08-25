#' Project SAR object
#' 
#' Projects SAR object using \code{\link[gdalUtils]{gdalwarp}}.
#' 
#' @param object SAR object or character giving the address of a raster (e.g. GeoTiff).
#' @param suffix Character. This suffix is added to the image name in front of file extension.
#' @param dstfile Character. The destination file name.
#' @param of Character. Select the output format. The default is GeoTIFF (GTiff). Use the short format name.
#' @param r Character. resampling_method. ("near"|"bilinear"|"cubic"|"cubicspline"|"lanczos"|"average"|"mode"|"max"|"min"|"med"|"q1"|"q3").
#' @param t_srs Character. target spatial reference set. The coordinate systems that can be passed are anything supported by the OGRSpatialReference.SetFromUserInput() call, which includes EPSG PCS and GCSes (ie. EPSG:4296), PROJ.4 declarations (as above), or the name of a .prf file containing well known text.
#' @param dstalpha Logical. Create an output alpha band to identify nodata (unset/transparent) pixels.
#' @param dstnodata Character. Set nodata values for output bands.
#' @param overwrite Logical. (GDAL >= 1.8.0) Overwrite the target dataset if it already exists.
#' @param ... Arguments of \code{\link[gdalUtils]{gdalwarp}}.
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

#' @rdname sarProject
#' @export
setMethod('sarProject', 'SAR', function(object, ...) {
    return(object@file@name)
})