setGeneric('readCornerCoords', function(object, metadata, ...) {
    standardGeneric('readCornerCoords')
})

setMethod('readCornerCoords', 'Sentinel',
          function(object, ...) {
              for(i in 1:xmlSize(metadata)) {
                  if(xmlAttrs(metadata[[i]])[1]=='measurementFrameSet') {
                      measurementFrameSet <- metadata[[i]]
                      coordsString <- xmlValue(measurementFrameSet[[1]][[1]][[1]][[1]][[1]][[1]][[1]])
                      coords <- sapply(strsplit(coordsString, ' '), strsplit, split=',')
                      object@cornerLat <- as.numeric(c(coords[[1]][1], coords[[2]][1], coords[[3]][1], coords[[4]][1]))
                      object@cornerLon <- as.numeric(c(coords[[1]][2], coords[[2]][2], coords[[3]][2], coords[[4]][2]))
                  }
              }
              return(object)
          })

setMethod('readCornerCoords', 'TSX',
          function(object, ...) {
              sceneInfo <- metadata[['productInfo']][['sceneInfo']]
              
              cornerLat <- c()
              cornerLon <- c()
              for(i in 1:xmlSize(sceneInfo)) {
                  if(xmlName(sceneInfo[[i]])=='sceneCornerCoord') {
                      cornerLat[length(cornerLat)+1] <- as.numeric(xmlValue(sceneInfo[[i]][['lat']]))
                      cornerLon[length(cornerLon)+1] <- as.numeric(xmlValue(sceneInfo[[i]][['lon']]))
                  }
              }
              object@cornerLat <- cornerLat
              object@cornerLon <- cornerLon
              return(object)
          })