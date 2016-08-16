#' Angles
#' 
#' Returns angles provided by GCPs/TPs as a \code{\link{SpatialPointsDataFrame}} object.
#'  
#' @param object object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param z either incidence (default) or elevation angles 'thetaEl'.
#' @param interpolate TRUE if angles should be interpolated.
#' @param variogram.fit logical. Fit a Gaussian variogram?
#' @param plot.fit logical. Plot the fitted variogram?
#' @param aggregate logical. Should the raster be aggregated? 
#' If no aggregation is done the interpolation might take a very long time.
#' @param aggregate.fact integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). 
#' Or two integers (horizontal and vertical aggregation factor) or three integers (when also aggregating over layers). 
#' See \code{\link{aggregate}} method of raster package.
#' @export
#' @seealso \code{\link{plotAngles}}, \code{\link{GeolocationPoints-class}}
#' @examples
#' data(kili)
#' angles(master)
#' plot(angles(master))
setGeneric('angles', 
           function(object, z='thetaIn', interpolate=F, 
                    variogram.fit=T, plot.fit=F, 
                    aggregate=F, aggregate.fact=100, ...) {
               standardGeneric('angles')
           })

#' @name angles
#' @rdname angles
#' @export
setMethod('angles', 'GeolocationPoints', 
          function(object, ...) {
              SpatialPointsDataFrame(coordinates(object), data=object@data[z],
                                     proj4string = object@proj4string)
          })

#' @name angles
#' @rdname angles
#' @export
setMethod('angles', 'SAR', 
          function(object, ...) {
              if(interpolate) {
                  if(aggregate) {
                      cat('Raster gets aggregated by factor ', aggregate.fact, '.\n', sep='')
                      object.aggr <- aggregate(object, aggregate.fact)
                      new.coords <- data.frame(xyFromCell(object.aggr, 1:ncell(object.aggr)))
                  } else {
                      new.coords <- data.frame(xyFromCell(object, 1:ncell(object)))
                  }
                  cat('Values are interpolated (using kriging) for coordinates of aggregated raster.\n')
                  colnames(new.coords) <- c('lon', 'lat')
                  new.points <- SpatialPoints(new.coords, proj4string = object@geolocationPoints@proj4string)
                  kriging <- anglesKriging(object=object@geolocationPoints, 
                                          slave=new.points, z=z,
                                          variogram.fit=variogram.fit,
                                          plot.fit=plot.fit)
                  return(kriging)
              } else {
                  return(angles(object@geolocationPoints, z, interpolate, ...))
              }
          })

#' @name angles
#' @rdname angles
#' @export
setMethod('angles', 'SARSet',
          function(object, ...) {
              lapply(object, angles, z, interpolate, 
                     variogram.fit, plot.fit, aggregate.fact,
                     ...)
          })