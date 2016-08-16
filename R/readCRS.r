setGeneric('readCRS', function(object, ...) {
    standardGeneric('readCRS')
})

setMethod('readCRS', 'TSX',
          function(object, ...) {
              return(CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
          })

setMethod('readCRS', 'Sentinel',
          function(object, ...) {
              return(CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
          })