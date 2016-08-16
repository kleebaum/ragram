#' Expected (theoretical) disparity between two SAR records
#' 
#' Returns the expected disparity within the intersection area of two SAR records.
#' Uses the angles provided by GCPs/TPs of both records. 
#' The angles for the slave image are interpolated using Kriging (\code{\link{krige}}).
#' Returns the results as a \code{\link{SpatialPointsDataFrame}} object.
#'  
#' @param object object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param slave object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param h numeric. Relative height.
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
disparityMapExpected <- function(object, slave, h=10, z='thetaIn',
                                 variogram.fit=T, plot.fit=F, 
                                 interpolate=T, aggregate.fact=100, ...) {
    
    intersection <- intersection(object, slave)
    #slave@geolocationPoints@proj4string <- intersection@proj4string
    
    if(interpolate) {
        
        objectKriging <- angles(object, interpolate=T, 
                                aggregate.fact=aggregate.fact)
        
        #objectKriging@proj4string <- intersection@proj4string
        #slave@geolocationPoints@proj4string <- intersection@proj4string
        
        slaveKriging <- anglesKriging(object=slave@geolocationPoints,
                                      slave=objectKriging,
                                      z=z, variogram.fit=variogram.fit, 
                                      plot.fit=plot.fit)
        
        selection <- which(unname(over(objectKriging,
                                       intersection)==T))
        
        disparities <- data.frame('disparities'=
                                      disparity(h,
                                                objectKriging@data[selection,1]/360*2*pi,
                                                slaveKriging@data[selection,1]/360*2*pi),
                                  'var'=slaveKriging@data[selection,2])
        
        SpatialPointsDataFrame(coordinates(objectKriging)[selection,],
                               data=disparities, proj4string = object@crs)
        
    } else {
        slaveKriging <- anglesKriging(object=slave@geolocationPoints,
                                      slave=object@geolocationPoints,
                                      z=z, variogram.fit=variogram.fit, 
                                      plot.fit=plot.fit)
        
        #object@geolocationPoints@proj4string <- intersection@proj4string
        selection <- which(unname(over(object@geolocationPoints,
                                       intersection)==T))
        
        if(length(selection)==0) {
            stop('There are no GCPs in intersection area. Try again with interpolate=T.')
        }
        
        disparities <- data.frame('disparities'=
                                      disparity(h,
                                                object@geolocationPoints@data[selection,z]/360*2*pi,
                                                slaveKriging@data[selection,1]/360*2*pi),
                                  'var'=slaveKriging@data[selection,2])
        
        SpatialPointsDataFrame(coordinates(object@geolocationPoints)[selection,],
                               data=disparities, proj4string = object@crs)
    }
}