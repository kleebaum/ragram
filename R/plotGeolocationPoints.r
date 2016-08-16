setGeneric('plotGeolocationPoints',
           function(object, 
                    projected=F, levelplot=F, z='thetaIn', 
                    col.regions=colorRampPalette(colors)(255),
                    round.digits=1, ...) {
               standardGeneric('plotGeolocationPoints')
           })

setMethod('plotGeolocationPoints', 'GeolocationPoints',
          function(object, ...) {
              if(projected) {
                  x <- round(object@coords[,1], round.digits)
                  y <- round(object@coords[,2], round.digits)
              } else {
                  x <- object@data[,'col']
                  y <- object@data[,'row']
              }
              if(levelplot) {
                  levelplot(object@data[,z] ~ x * y, 
                            col.regions=col.regions, 
                            xlab=expression('r'[g]), ylab='az', ...)
              } else {
                  if(projected)
                    plot(object, ...)
                  else
                    plot(x, y, xlab=expression('r'[g]), ylab='az', ...)  
              }}
          )

setMethod('plotGeolocationPoints', 'SAR',
          function(object, ...) {
              plotGeolocationPoints(object@geolocationPoints, 
                                    projected=projected, levelplot=levelplot,
                                    z=z, col.regions=col.regions, 
                                    round.digits=round.digits, ...)
          })

setMethod('plotGeolocationPoints', 'SARSet',
          function(object, ...) {
              lapply(object@elements, function(sar, ...) {
                  plotGeolocationPoints(sar, projected=projected, 
                                        levelplot=levelplot,
                                        z=z, col.regions=col.regions, 
                                        round.digits=round.digits, ...)
              })
          })

levelplotHeight <- function(object, ...) {
    plotGeolocationPoints(object, z='height', levelplot=T, ...)
}