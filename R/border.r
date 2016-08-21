#' Border
#' 
#' Returns border provided by metadata as a \code{\link{SpatialPolygons}} object.
#' 
#' @param object object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @export
#' @seealso \code{\link{plotBorder}}
#' @examples
#' data(kili)
#' 
#' border(master)
#' plot(border(master))
#' plotBorder(master)
setGeneric('border', 
           function(object, ...) {
               polygons <- standardGeneric('border')
               SpatialPolygons(polygons, proj4string = object@crs)
           })

setMethod('border', 'SAR', function(object, ...) {
    if(length(object@border@polygons)==0) {
        cornerCoords <- data.frame(object@cornerLon, object@cornerLat)
        cornerCoords <- cornerCoords[chull(cornerCoords), ]
        polygon <- Polygon(cornerCoords)
        return(list(Polygons(list(polygon), ID=1)))
    } else {
        return(object@border@polygons)
    }
})

setMethod('border', 'SARSet', function(object, ...) {
    polygons <- list()
    for(i in 1:length(object)) {
        cornerCoords <- data.frame(object[[i]]@cornerLon, 
                                   object[[i]]@cornerLat)
        cornerCoords <- cornerCoords[chull(cornerCoords), ]
        polygon <- Polygon(cornerCoords)
        polygons[[i]] <- Polygons(list(polygon), ID=i)
    }
    polygons
})