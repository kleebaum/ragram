setGeneric('geolocationGam',
           function(object, z='thetaIn', ...) {
               standardGeneric('geolocationGam')
           })

setMethod('geolocationGam', 'GeolocationPoints', 
          function(object, ...) {
              x <- object@coords[,1]
              y <- object@coords[,2]
              z <- object@data[,z]
              return(gam(z ~ s(x, y, bs="ts")))
          })

setMethod('geolocationGam', 'SAR', 
          function(object, ...) {
              geolocationGam(object@geolocationPoints, ...)
          })

setGeneric('plotGam',
           function(object, z='thetaIn', 
                    plot.type='contour', col=rgb(1,0,0,0), 
                    ...) {
               standardGeneric('plotGam')
           })

setMethod('plotGam', 'GeolocationPoints', function(object, ...) {
        geolocationGam <- geolocationGam(object)
        vis.gam(geolocationGam, plot.type = plot.type, ...)
})

setMethod('plotGam', 'SAR', function(object, ...) {
    plotGam(object@geolocationPoints, z=z,  
            plot.type=plot.type, col=col, ...)
        plotBorder(object, col=col, add=T, ...)
})

setMethod('plotGam', 'SARSet', function(object, ...) {
    lapply(object@elements, function(sar, ...) {
        plotGam(sar, z=z, 
                plot.type=plot.type, col=col, ...)
    })
})

# setGeneric('predictGam', function(object, x, y,
#                                   z='thetaIn', projected=F, ...) {
#     standardGeneric('predictGam')
# })
# 
# setMethod('predictGam', 'GeolocationPoints', function(object, ...) {
#     if(projected) {
#         geolocationGam <- geolocationGam(object,
#                                          x='lon', y='lat')
#         predict(geolocationGam, data.frame(x=x, y=y))
#     } else {
#         geolocationGam <- geolocationGam(object)
#         predict(geolocationGam, data.frame(x=x, y=y))
#     }
# })
# 
# setMethod('predictGam', 'SAR', function(object, ...) {
#     predictGam(object@geolocationPoints, x=x, y=y, 
#                z=z, projected=projected, ...)
# })
# 
# plotDifGam <- function(sarMaster, sarSlave, z='thetaIn',
#                        xlim=NULL, ylim=NULL, 
#                        xlab='longitude', ylab='latitude', 
#                        col.regions=colorRampPalette(colors)(255), 
#                        useRaster=F, interpolate=T, 
#                        round.digits=1, ...) {
#     if(is.null(xlim) || is.null(ylim)) {
#         xlim<-c(min(sarMaster@cornerLon, sarSlave@cornerLon)-0.2, 
#                 max(sarMaster@cornerLon, sarSlave@cornerLon)+0.2)
#         ylim<-c(min(sarMaster@cornerLat, sarSlave@cornerLat)-0.2, 
#                 max(sarMaster@cornerLat, sarSlave@cornerLat)+0.2)
#     }
#     
#     masterLon <- sarMaster@geolocationPoints@lon
#     masterLat <- sarMaster@geolocationPoints@lat
#     masterValues <- slot(sarMaster@geolocationPoints, z)
#     
#     slaveValues <- predictGam(sarSlave, masterLon, masterLat, 
#                               projected = T)
#     
#     intersectionArea <- gIntersection(border(sarMaster), border(sarSlave))
#     
#     spatialPoints = SpatialPoints(data.frame(masterLon, masterLat),
#                                   proj4string = sarMaster@crs)
#     
#     selection <- which(unname(over(spatialPoints, intersectionArea)==T))
#     
#     differences <- data.frame('lon'=masterLon[selection], 
#                               'lat'=masterLat[selection],
#                               'dif'=(masterValues-slaveValues)[selection])
#     
#     difGam <- gam((masterValues-slaveValues) ~ s(masterLon, masterLat))
#     vis.gam(difGam, plot.type = "contour", 
#             color = "topo",
#             xlim=extent(intersectionArea)[1:2]+c(-0.3, 0.3),
#             ylim=extent(intersectionArea)[3:4]+c(-0.3, 0.3),
#             xlab=xlab, ylab=ylab)
#     plot(intersectionArea, add=T)
#     plot(spatialPoints[selection], add=T)
#     
#     levelplot(differences$dif ~ 
#                   round(differences$lon, round.digits)*
#                   round(differences$lat, round.digits),
#               xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim,
#               col.regions=col.regions, interpolate=interpolate,
#               panel=function(...) {
#                   if(useRaster)
#                       panel.levelplot.raster(...)
#                   else
#                       panel.levelplot(...)
#                   plotBorder(sarSlave, panel = T)
#                   plotBorder(sarMaster, panel = T)
#               }
#     )
# }