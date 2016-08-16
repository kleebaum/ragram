setMethod('initialize', 'SAR', 
          function(.Object, address='', raster=NULL, polarization=NULL, imgSubstring='', ...) {
              .Object@polarization <- tolower(polarization)
              .Object@imgSubstring <- imgSubstring
              
              if(nchar(address)==0) {
                  if(is.null(raster)) {
                      cat('No address and no raster given. A default raster is created.\n')
                      raster <- raster()
                  }
              } else if(!file.exists(address)) {
                  if(is.null(raster)) {
                      cat('Address can not be found and no raster is given. A default raster is created.\n')
                      raster <- raster()
                  }
              } else {
                  .Object@address <- address
                  .Object <- readMetadata(.Object)
                  .Object@geolocationPoints <- readGeolocationPoints(.Object)
                  
                  if(is.null(raster)) {
                      raster <- readRaster(.Object)
                  }
                  
                  if(length(.Object@polarization)==0) {
                      .Object@polarization <- readPolarization(.Object, raster)
                  }
                  
                  .Object@border <- border(.Object)
              }
              callNextMethod(.Object, raster, ...)
          })

setMethod('initialize', 'Sentinel',
          function(.Object, ...) {
              callNextMethod(.Object, ...)
          })

setMethod('initialize', 'TSX',
          function(.Object, ...) {
              callNextMethod(.Object, ...)
          })

setMethod('initialize', 'SARSet',
          function(.Object, elements) {
              cornerLat <- sapply(elements, function(sar) {sar@cornerLat})
              cornerLon <- sapply(elements, function(sar) {sar@cornerLon})
              .Object@cornerLat <- c(min(cornerLat), max(cornerLat))
              .Object@cornerLon <- c(min(cornerLon), max(cornerLon))
              
              .Object@centerLat <- mean(sapply(elements, function(sar) {sar@centerLat}))
              .Object@centerLon <- mean(sapply(elements, function(sar) {sar@centerLon}))
              
              .Object@extent <- extent(c(.Object@cornerLon, .Object@cornerLat))
              
              satellites <- sapply(elements, function(sar) {sar@satellite})
              .Object@satellite <- unique(satellites)
              
              callNextMethod(.Object, elements)
          })