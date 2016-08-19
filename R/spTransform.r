#' @export
setMethod('spTransform', c('GeolocationPoints', 'CRS'),
          function(x, CRSobj, ...) {
              new('GeolocationPoints', callNextMethod(x, CRSobj, ...))
          })