#' spTransform for GeolocationPoints
#' 
#' Transforms GeolocationPoints to coordinate reference system 
#' (see \code{\link[sp]{spTransform}}).
#' 
#' @param object Object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param CRS Object of class \code{\link[sp]{CRS-class}}, or of class character in which case it is converted to \code{\link[sp]{CRS-class}}.
#' @param ... Further arguments to \code{\link[sp]{spTransform}}.
#' @seealso \code{\link{sarProject}}, \code{\link[sp]{spTransform}}, \code{\link[sp]{CRS-class}}
#' @rdname spTransform
#' @export 
setMethod('spTransform', c('GeolocationPoints', 'CRS'),
          function(x, CRSobj, ...) {
              new('GeolocationPoints', callNextMethod(x, CRSobj, ...))
          })