setGeneric('readMetadata', function(object, ...) {
    metadata <- standardGeneric('readMetadata')
    object <- readCornerCoords(object, metadata)
    object@orbit <- readOrbitNumber(object, metadata)
    object@node <- readNode(object, metadata)
    object@centerLon <- mean(object@cornerLon)
    object@centerLat <- mean(object@cornerLat)
    object@crs <- readCRS(object)
    return(object)  
})

setMethod('readMetadata', 'Sentinel', 
          function(object, ...) {
              xmlRoot(xmlParse(file.path(object@address, 'manifest.safe')))[['metadataSection']]
          })

setMethod('readMetadata', 'TSX', 
          function(object, ...) {
              xmlFile <- list.files(object@address, pattern='*.xml')[1]
              xmlRoot(xmlParse(file.path(object@address, xmlFile)))
          })