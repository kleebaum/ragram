setGeneric('readCRS', function(object, ...) {
    standardGeneric('readCRS')
})

setMethod('readCRS', 'TSX',
          function(object, ...) {
              return(CRS('+init=epsg:4326'))
          })

setMethod('readCRS', 'Sentinel',
          function(object, ...) {
              return(CRS('+init=epsg:4326'))
          })