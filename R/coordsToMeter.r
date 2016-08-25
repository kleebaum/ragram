#' Convert coordinates to meter
#' 
#' Converts an extent of longitudinal/latitudinal coordinates to meters.
#' @param coords extent or numeric vector of form c(lon1, lon2, lat1, lat2).
#' @return  numeric vector of form 
#' c(longitudinal distance in meter, latitudinal distance in meter).
#' @export
setGeneric('coordsToMeter', function(coords=c(0,0,0,0)) {
    coords <- standardGeneric('coordsToMeter')
    lon1 <- coords[1]
    lon2 <- coords[2]
    lat1 <- coords[3]
    lat2 <- coords[4]
    delta.lon <- lon2 - lon1
    delta.lat <- lat2 - lat1
    m.lon <- delta.lon*111195*cos(mean(lat1, lat2)/360*2*pi)
    m.lat <- delta.lat*111195
    return(c(m.lon, m.lat))
})

#' @rdname coordsToMeter
#' @export
setMethod('coordsToMeter', c('numeric'),
          function(coords) {
              return(coords)
          })

#' @rdname coordsToMeter
#' @export
setMethod('coordsToMeter', c('Extent'),
          function(coords) {
              return(coords)
          })