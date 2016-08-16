#' Spatial interpolation of angles using kriging
#' 
#' Angles provided by GCPs/TPs are known.
#' Returns interpolated angles as a \code{\link{SpatialPointsDataFrame}} object.
#'  
#' @param object object of class \code{\link{SpatialPointsDataFrame}}.
#' @param spatialPoints object of \code{\link{SpatialPoints}} class.
#' @param z either incidence (default) or elevation angles 'thetaEl'.
#' @param formula formula used for kriging (see \code{\link{krige}} of gstat package).
#' @param variogram.fit logical. Fit a Gaussian variogram?
#' @param plot.fit logical. Plot the fitted variogram?
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

#' @name anglesKriging
#' @rdname anglesKriging
#' @export
setMethod('anglesKriging', c('GeolocationPoints', 'SpatialPoints'),
          function(object, slave, ...) {
              variogram <- variogram(formula, object)
              
              if(variogram.fit) {
                  psill <- diff(range(variogram$gamma))
                  range.var <- diff(range(variogram$dist))
                  variogram.model <- vgm(psill, "Gau", range.var, 0)
                  
                  variogram.fit <- fit.variogram(variogram, variogram.model)
                  if(plot.fit) {
                      print(plot(variogram, model=variogram.fit, ...))
                  }
                  slave@proj4string <- object@proj4string
                  kriging <- krige(formula, object, slave, variogram.fit, maxdist=1000)
                  return(kriging)
                  if(is.na(kriging$var1.pred[1])) {
                      cat('Fitting variogram was not successful, try without...\n')
                      cat('variogram.fit is set to FALSE\n')
                      kriging <- krige(formula, object, slave)
                  }
              } else {
                  cat('hier')
                  kriging <- krige(formula, object, slave)
              }
              colnames(kriging@data) <- c(paste0(z, '.pred'), paste0(z, '.var'))
              return(kriging)
          })

#' @name anglesKriging
#' @rdname anglesKriging
#' @export
setMethod('anglesKriging', c('SAR', 'SAR'),
          function(object, slave, ...) {
              anglesKriging(object@geolocationPoints, slave@geolocationPoints, ...)
          })

#' @name anglesKriging
#' @rdname anglesKriging
#' @export
setMethod('anglesKriging', c('SAR', 'SpatialPoints'),
          function(object, slave, ...) {
              anglesKriging(object@geolocationPoints, slave, ...)
          })

#' @name anglesKriging
#' @rdname anglesKriging
#' @export
setMethod('anglesKriging', c('SpatialPointsDataFrame', 'SAR'),
          function(object, slave, ...) {
              anglesKriging(object, slave@geolocationPoints, ...)
          })