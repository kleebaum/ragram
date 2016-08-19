#' Angles differences of two SAR records
#' 
#' Returns the differences in angles within the intersection area of two SAR records.
#' Uses the angles provided by GCPs/TPs of both records. 
#' The angles for the slave image are interpolated using Kriging (\code{\link{krige}}).
#' Returns the results as a \code{\link{SpatialPointsDataFrame}} object.
#'  
#' @param object Object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param slave Object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param z Character. Either incidence ('thetaIn', default) or elevation angles 'thetaEl'.
#' @param variogram.fit Logical. Fit a Gaussian variogram?
#' @param plot.fit Logical. Plot the fitted variogram?
#' @param interpolate Logical. Interpolate angles?
#' @param aggregate.fact Integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). 
#' Or two integers (horizontal and vertical aggregation factor) or three integers (when also aggregating over layers). 
#' See \code{\link{aggregate}} method of raster package.
#' @return \code{\link{SpatialPointsDataFrame}}
#' @export
#' @seealso \code{\link{angles}}, \code{\link{plotAnglesDif}}, \code{\link{GeolocationPoints-class}}
#' @examples
#' data(kili)
#' 
#' anglesDif(kili[[1]], kili[[5]])
#' plotAnglesDif(kili[[1]], kili[[5]])
#' 
#' anglesDif(master, slave, interpolate=T, aggregate.fact=100)
#' plotAnglesDif(master, slave, interpolate=T, aggregate.fact=100)
#' 
#' anglesDif.sp <- anglesDif(master, slave, interpolate = T, aggregate.fact=10)
#' anglesDif.raster <- rasterFromXYZ(anglesDif.sp)
#' plot(anglesDif.raster)
setGeneric('anglesDif',
           function(object, slave=NULL, z='thetaIn',
                    variogram.fit=T, plot.fit=F, 
                    interpolate=F, aggregate.fact=1000, ...) {
               standardGeneric('anglesDif')
           })

#' @export
setMethod('anglesDif', 'SAR',
          function(object, slave=NULL, ...) {
              cat('Cannot calculate difference of angles from a single SAR object.\n') 
              cat('Please provide a second SAR object or a SARSet object.')
          })

#' @export
setMethod('anglesDif', c('SAR', 'SAR'),
          function(object, slave, ...) {
              
              intersection <- intersection(object, slave)
              
              if(interpolate) {
                  object@geolocationPoints <- new('GeolocationPoints',
                                                  angles(object, z=z, interpolate=T, 
                                                         variogram.fit=variogram.fit, plot.fit=plot.fit,
                                                         aggregate.fact=aggregate.fact))
                  
              } 
              slaveKriging <- anglesKriging(object=slave@geolocationPoints,
                                            slave=object@geolocationPoints,
                                            z=z, variogram.fit=variogram.fit, 
                                            plot.fit=plot.fit)
              
              selection <- which(unname(over(object@geolocationPoints,
                                             intersection)==T))
              
              if(length(selection)==0) {
                  stop('There are no GCPs in intersection area. Try again with interpolate=T.')
              }
              
              differences <- data.frame('thetaDif'=(object@geolocationPoints@data[selection,z]-
                                                        slaveKriging@data[selection,z]),
                                        'var'=slaveKriging@data[selection,2])
              
              SpatialPointsDataFrame(coordinates(object@geolocationPoints)[selection,],
                                     data=differences, proj4string = object@crs)
              
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