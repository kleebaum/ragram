#' Retrieve geolocation points
#' 
#' Returns GCPs/TPs as a \code{\link{SpatialPointsDataFrame}} object.
#'  
#' @param object object of the \code{\link{SAR-class}} or a subclass 
#' (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}) or 
#' an object of the \code{\link{SARSet-class}}. 
#' @export
#' @seealso \code{\link{GeolocationPoints-class}}
#' @return either a single \code{\link{SpatialPointsDataFrame}} or a list of them
#' @examples
#' data(kili)
#' 
#' geolocationPoints(master)
#' master@geolocationPoints
#' plot(geolocationPoints(master))
#' 
#' geolocationPoints(kili)
setGeneric('geolocationPoints',
           function(object, ...) {
               standardGeneric('geolocationPoints')
           })

#' @rdname geolocationPoints
#' @export
setMethod('geolocationPoints', 'SAR',
          function(object, ...) {
              object@geolocationPoints
          })

#' @rdname geolocationPoints
#' @export
setMethod('geolocationPoints', 'SARSet',
          function(object, ...) {
              lapply(object, geolocationPoints, ...)
          })