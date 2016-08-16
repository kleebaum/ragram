setGeneric('readNode', function(object, metadata, ...) {
    standardGeneric('readNode')
})

setMethod('readNode', 'Sentinel',
          function(object, ...) {
              for(i in 1:xmlSize(metadata)) {
                  if(xmlAttrs(metadata[[i]])[1]=='measurementOrbitReference') {
                      return(xmlValue(metadata[[i]][['metadataWrap']][['xmlData']][['orbitReference']][['extension']][['orbitProperties']][['pass']]))
                  }
              }
              return('unknown')
          })

setMethod('readNode', 'TSX',
          function(object, ...) {
              xmlValue(metadata[['productInfo']][['missionInfo']][['orbitDirection']])
          })