setGeneric('readOrbitNumber', function(object, metadata, ...) {
    standardGeneric('readOrbitNumber')
})

setMethod('readOrbitNumber', 'TSX',
          function(object, ...) {
              as.numeric(xmlValue(metadata[['productInfo']][['missionInfo']][['relOrbit']]))
          })

setMethod('readOrbitNumber', 'Sentinel',
          function(object, ...) {
              for(i in 1:xmlSize(metadata)) {
                  if(xmlAttrs(metadata[[i]])[1]=='measurementOrbitReference') {
                      return(as.numeric(xmlValue(metadata[[i]][['metadataWrap']][['xmlData']][['orbitReference']][['relativeOrbitNumber']])))
                  }
              }
              return(NULL)
          })