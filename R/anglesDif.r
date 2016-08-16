#' Angles differences of two SAR records
#' 
#' Returns the differences in angles within the intersection area of two SAR records.
#' Uses the angles provided by GCPs/TPs of both records. 
#' The angles for the slave image are interpolated using Kriging (\code{\link{krige}}).
#' Returns the results as a \code{\link{SpatialPointsDataFrame}} object.
#'  
#' @param object object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param slave object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param z either incidence (default) or elevation angles 'thetaEl'.
#' @param variogram.fit TRUE if a Gaussian variogram should be fitted
#' @param plot.fit TRUE if the fitted variogram should be plotted
#' @param interpolate TRUE if angles should be interpolated.
#' @param aggregate.fact integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). 
#' Or two integers (horizontal and vertical aggregation factor) or three integers (when also aggregating over layers). 
#' See \code{\link{aggregate}} method of raster package. If aggregate.fact equals 1, no aggregation is done, 
#' but then the interpolation might take a very long time...
#' @export
#' @seealso \code{\link{angles}}, \code{\link{plotAnglesDif}}, \code{\link{GeolocationPoints-class}}
#' @examples
#' data(kili)
#' anglesDif(master, slave)
setGeneric('anglesDif',
           function(object, slave=NULL, z='thetaIn',
                    variogram.fit=T, plot.fit=F, 
                    interpolate=F, aggregate.fact=100, ...) {
               standardGeneric('anglesDif')
           })

#' @export
setMethod('anglesDif', 'SAR',
          function(object, slave=NULL, ...) {
              cat('Cannot calculate difference of angles from a single SAR object.\n') 
              cat('Please pass a second one or a SARSet object.')
          })

#' @export
setMethod('anglesDif', c('SAR', 'SAR'),
          function(object, slave, ...) {
              
              intersection <- intersection(object, slave)
              slave@geolocationPoints@proj4string <- intersection@proj4string
              
              if(interpolate) {
                  
                  objectKriging <- angles(object, interpolate=T, aggregate.fact=aggregate.fact)
                  slaveKriging <- anglesKriging(object=slave@geolocationPoints,
                                                slave=objectKriging,
                                                z=z, variogram.fit=variogram.fit, 
                                                plot.fit=plot.fit)
                  
                  selection <- which(unname(over(objectKriging,
                                                 intersection)==T))
                  
                  differences <- data.frame('thetaDif'=(objectKriging@data[selection,1]-
                                                            slaveKriging@data[selection,1]),
                                            'var'=slaveKriging@data[selection,2])
                  
                  SpatialPointsDataFrame(coordinates(objectKriging)[selection,],
                                         data=differences, proj4string = object@crs)
                  
              } else {
                  slaveKriging <- anglesKriging(object=slave@geolocationPoints,
                                                slave=object@geolocationPoints,
                                                z=z, variogram.fit=variogram.fit, 
                                                plot.fit=plot.fit)
                  
                  object@geolocationPoints@proj4string <- intersection@proj4string
                  selection <- which(unname(over(object@geolocationPoints,
                                                 intersection)==T))
                  
                  if(length(selection)==0) {
                      stop('There are no GCPs in intersection area. Try again with interpolate=T.')
                  }
                  
                  differences <- data.frame('thetaDif'=(object@geolocationPoints@data[selection,z]-
                                                            slaveKriging@data[selection,1]),
                                            'var'=slaveKriging@data[selection,2])
                  
                  SpatialPointsDataFrame(coordinates(object@geolocationPoints)[selection,],
                                         data=differences, proj4string = object@crs)
              }
          })

#' @export
setMethod('anglesDif', 'SARSet',
          function(object, slave=NULL, ...) {
              anglesDifList <- list() 
              for(i in 1:length(object)) {
                  master <- object[[i]]
                  for(j in i:length(object)) {
                      slave <- object[[j]]
                      if(!identical(master, slave)) {
                          anglesDifList[[length(anglesDifList)+1]] <- anglesDif(master, slave, z, 
                                                                                variogram.fit, plot.fit, ...)
                      }
                  }
              }
              anglesDifList
          })