#' Spatial interpolation of angles using kriging
#' 
#' Angles provided by GCPs/TPs are known.
#' Returns interpolated angles as a \code{\link{SpatialPointsDataFrame}} object.
#'  
#' @param object Object of class \code{\link{SpatialPointsDataFrame}}.
#' @param spatialPoints Object of \code{\link{SpatialPoints}} class.
#' @param z Character. Either incidence ('thetaIn', default) or elevation angles 'thetaEl'.
#' @param formula Formula used for kriging (see \code{\link{krige}} of gstat package).
#' @param variogram.fit Logical. Fit a Gaussian variogram?
#' @param plot.fit Logical. Plot the fitted variogram?
#' @export
#' @seealso \code{\link{angles}}, \code{\link{anglesDif}}, \code{\link{plotAngles}}, \code{\link{GeolocationPoints-class}}
#' @examples
#' data(kili)
#' anglesKriging(master@geolocationPoints, slave@geolocationPoints)
setGeneric('anglesKriging', function(object, slave, z='thetaIn',
                                     formula=as.formula(paste(z, '~ lon + lat')),
                                     variogram.fit=T, plot.fit=F, ...) {
    standardGeneric('anglesKriging')
})

#' @rdname anglesKriging
#' @export
setMethod('anglesKriging', c('GeolocationPoints', 'SpatialPoints'),
          function(object, slave, ...) {
              variogram <- variogram(formula, object)
              
              if(variogram.fit) {
                  psill <- diff(range(variogram$gamma))
                  range.var <- diff(range(variogram$dist))
                  variogram.model <- vgm(psill=psill, model="Gau", range=range.var, nugget=0)
                  
                  variogram.fit <- fit.variogram(variogram, variogram.model)
                  if(plot.fit) {
                      print(plot(variogram, model=variogram.fit, ...))
                  }
                  slave@proj4string <- object@proj4string
                  kriging <- krige(formula, object, slave, variogram.fit)
                  if(is.na(kriging$var1.pred[1])) {
                      cat('Fitting variogram was not successful, try without...\n')
                      cat('variogram.fit is set to FALSE\n')
                      kriging <- krige(formula, object, slave)
                  }
              } else {
                  kriging <- krige(formula, object, slave)
              }
              colnames(kriging@data) <- c(z, paste0(z, '.var')) #c(paste0(z, '.pred'), paste0(z, '.var'))
              return(kriging)
          })

#' @rdname anglesKriging
#' @export
setMethod('anglesKriging', c('SAR', 'SAR'),
          function(object, slave, ...) {
              anglesKriging(object@geolocationPoints, slave@geolocationPoints, ...)
          })

#' @rdname anglesKriging
#' @export
setMethod('anglesKriging', c('SAR', 'SpatialPoints'),
          function(object, slave, ...) {
              anglesKriging(object@geolocationPoints, slave, ...)
          })

#' @rdname anglesKriging
#' @export
setMethod('anglesKriging', c('SpatialPointsDataFrame', 'SAR'),
          function(object, slave, ...) {
              anglesKriging(object, slave@geolocationPoints, ...)
          })