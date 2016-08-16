setGeneric('readGeolocationPoints', function(object) {
    standardGeneric('readGeolocationPoints')
})

setMethod('readGeolocationPoints', 'SAR', function(object) {
    return(new('GeolocationPoints'))
})

setMethod('readGeolocationPoints', 'Sentinel', function(object) {
    path <- file.path(object@address, 'annotation')
    files <- list.files(path, pattern='*.xml')
    annotation <- xmlRoot(xmlParse(file.path(path, files[1])))
    
    geolocationGridPointList <- annotation[['geolocationGrid']][[1]]
    size <- xmlSize(geolocationGridPointList)
    geoloc <- array(0, 
                    c(size, 7), 
                    dimnames=list(NULL, 
                                  c('row', 'col', 'lon', 'lat', 'height', 'thetaIn', 'thetaEl')))
    for (i in 1:size) {
        geoloc[i,1] <- as.numeric(xmlValue(geolocationGridPointList[[i]][['line']]))
        geoloc[i,2] <- as.numeric(xmlValue(geolocationGridPointList[[i]][['pixel']]))
        geoloc[i,3] <- as.numeric(xmlValue(geolocationGridPointList[[i]][['longitude']]))
        geoloc[i,4] <- as.numeric(xmlValue(geolocationGridPointList[[i]][['latitude']]))
        geoloc[i,5] <- as.numeric(xmlValue(geolocationGridPointList[[i]][['height']]))
        geoloc[i,6] <- as.numeric(xmlValue(geolocationGridPointList[[i]][['incidenceAngle']]))
        geoloc[i,7] <- as.numeric(xmlValue(geolocationGridPointList[[i]][['elevationAngle']]))
    }
    
    new('GeolocationPoints', SpatialPointsDataFrame(
        data.frame('lon'=geoloc[,3], 'lat'=geoloc[,4]),
        data=data.frame('row'=geoloc[,1], 
                        'col'=geoloc[,2], 
                        'height'=geoloc[,5], 
                        'thetaIn'=geoloc[,6], 
                        'thetaEl'=geoloc[,7])),
        proj4string=readCRS(object))
})

setMethod('readGeolocationPoints', 'TSX', function(object) {
    path <- file.path(object@address, 'ANNOTATION')
    files <- list.files(path, pattern='*.xml')
    annotation <- xmlRoot(xmlParse(file.path(path, files[1])))
    geolocationGrid <- data.frame()
    j <- 1
    for(i in 1:xmlSize(annotation[['geolocationGrid']])) {
        if(xmlName(annotation[['geolocationGrid']][[i]])=='gridPoint') {
            geolocationGrid[j,1] <- as.numeric(xmlValue(annotation[['geolocationGrid']][[i]][['lat']]))
            geolocationGrid[j,2] <- as.numeric(xmlValue(annotation[['geolocationGrid']][[i]][['lon']]))
            geolocationGrid[j,3] <- as.numeric(xmlValue(annotation[['geolocationGrid']][[i]][['inc']]))
            geolocationGrid[j,4] <- as.numeric(xmlValue(annotation[['geolocationGrid']][[i]][['elev']]))
            geolocationGrid[j,5] <- as.numeric(xmlValue(annotation[['geolocationGrid']][[i]][['height']]))
            j <- j+1
        }
    }
    
    new('GeolocationPoints', SpatialPointsDataFrame(
        data.frame('lon'=geolocationGrid[,2], 'lat'=geolocationGrid[,1]),
        data=data.frame('height'=geolocationGrid[,5],
                        'thetaIn'=geolocationGrid[,3],
                        'thetaEl'=geolocationGrid[,4])),
        proj4string=readCRS(object))
})