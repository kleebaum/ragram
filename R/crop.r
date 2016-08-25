#' Crop
#' 
#' Crop a geographic subset (see \code{\link[raster]{crop}}).
#' 
#' @param x object object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param y Extent object, or any object from which an Extent object can be extracted.
#' @param ... Additional arguments as for \code{\link[raster]{writeRaster}}.
#' @seealso \code{\link[raster]{crop}}
#' @rdname crop
#' @export
setMethod('crop', 'SAR',
          function(x, y, ...) {
              rasterCropped=callNextMethod(x, y, ...)
              newSar <- sarRecord(rasterCropped, satellite=x@satellite)
              newSar@polarization=x@polarization
              newSar@orbit=x@orbit
              newSar@node=x@node
              newSar@geolocationPoints=x@geolocationPoints
              
              newSar@border <- as(y, 'SpatialPolygons')
              newSar@border@proj4string <- x@crs
              newSar@cornerLon <- c(y[1:2], y[1:2])
              newSar@cornerLat <- c(y[4:3], y[3:4])
              return(newSar)
          })