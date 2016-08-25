#' Intersection area between two SAR records
#' 
#' Returns the intersection of borders (provided by metadata) as a \code{\link[sp]{SpatialPolygons}} object.
#' 
#' @param object object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param slave object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @export
#' @return \code{\link[sp]{SpatialPolygons}}
setGeneric('intersection',
           function(object, slave, ...) {
               standardGeneric('intersection')
           })

#' @rdname intersection
#' @export
setMethod('intersection', 'SAR', 
          function(object, ...) {
              border(object)
          })

#' @rdname intersection
#' @export
setMethod('intersection', c('SAR', 'SAR'),
          function(object, slave, ...) {
              intersect(border(object), border(slave))
          })