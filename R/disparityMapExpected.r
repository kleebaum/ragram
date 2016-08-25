#' Expected (theoretical) disparity between two SAR records
#' 
#' Returns the expected disparity within the intersection area of two SAR records.
#' Uses the angles provided by GCPs/TPs of both records. 
#' The angles for the slave image are interpolated using Kriging (\code{\link{krige}}).
#' Returns the results as a \code{\link{SpatialPointsDataFrame}} object.
#'  
#' @param object Object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param slave Object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param h Numeric. Relative height.
#' @param z either incidence (default) or elevation angles 'thetaEl'.
#' @param z Character. Either incidence ('thetaIn', default) or elevation angles 'thetaEl'.
#' @param variogram.fit Logical. Fit a Gaussian variogram?
#' @param plot.fit Logical. Plot the fitted variogram?
#' @param interpolate Logical. Interpolate angles?
#' @param aggregate.fact Integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). 
#' Or two integers (horizontal and vertical aggregation factor) or three integers (when also aggregating over layers). 
#' See \code{\link{aggregate}} method of raster package.
#' @export
#' @seealso \code{\link{disparityMap}}, \code{\link{angles}}, \code{\link{plotAnglesDif}}, \code{\link{GeolocationPoints-class}}
#' @examples
#' data(kili)
#' 
#' disparityMapExpected(kili[[1]], kili[[5]], h=1)
#' 
#' disparityMapExpected(master, slave, interpolate = TRUE, aggregate.fact = 100)
disparityMapExpected <- function(object, slave, h=1, z='thetaIn',
                                 variogram.fit=T, plot.fit=F, 
                                 interpolate=F, aggregate.fact=1000, ...) {
    
    intersection <- intersection(object, slave)
    
    if(interpolate) {
        object@geolocationPoints <- new('GeolocationPoints', angles(object, z=z, interpolate=T, 
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
    
    disparities <- data.frame('disparities'=
                                  disparity(h,
                                            object@geolocationPoints@data[selection,z]/360*2*pi,
                                            slaveKriging@data[selection,z]/360*2*pi),
                              'var'=slaveKriging@data[selection,2])
    
    SpatialPointsDataFrame(coordinates(object@geolocationPoints)[selection,],
                           data=disparities, proj4string = object@crs)
    
}