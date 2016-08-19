#' Plot disparity map of two SAR records
#' 
#' Plots the expected disparity map of two SAR records. 
#' Either the expected disparity for a relative height \eqn{h} or the calculated disparity is used.
#'  
#' @param object object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param z either incidence (default) or elevation angles 'thetaEl'.
#' @param xlab title of the x axis.
#' @param ylab title of the y axis.
#' @param breaks number of breaks of the color ramp.
#' @param col.regions color ramp.
#' @param variogram.fit TRUE if a Gaussian variogram should be fitted.
#' @param plot.fit TRUE if the fitted variogram should be plotted.
#' @param plot.legend TRUE if the legend is plotted.
#' @param legend.lab title of the legend.
#' @param asp aspect, default is 1/cos((mean(range(ylim)) * pi)/180).
#' @param ... graphical parameters. Any argument that can be passed to plot, such as axes=FALSE and main='title'.
#' @export
#' @seealso  \code{\link{disparity}}, \code{\link{angles}}, \code{\link{GeolocationPoints-class}}
#' @examples
#' data(kili)
#' 
#' plotDisparityMapExpected(kili[[1]], kili[[2]])
#' plot(disparityMapExpected(kili[[1]], kili[[2]]))
#' 
#' plotDisparityMapExpected(kili)
setGeneric('plotDisparityMapExpected', function(object, slave, z='thetaIn',
                                        xlab='Longitude', ylab='Latitude', 
                                        h=1, breaks=255,
                                        col.regions=colorRampPalette(colors)(breaks), 
                                        cex=1, cex.axis=1, grid=T,
                                        variogram.fit=T, plot.fit=F, 
                                        plot.legend=T, pch=20, 
                                        legend.lab='disparity [m]',
                                        interpolate=F, aggregate.fact=100,
                                        ...) {
    standardGeneric('plotDisparityMapExpected')
})

#' @export
setMethod('plotDisparityMapExpected', 'SAR',
          function(object, slave=NULL, ...) {
              plotAnglesDif(object, slave, ...)
          }
)

#' @name plotDisparityMapExpected
#' @rdname plotDisparityMapExpected
#' @export
setMethod('plotDisparityMapExpected', c('SAR', 'SAR'),
          function(object, slave, z, xlab, ylab, h,
                   breaks, col.regions, 
                   cex, cex.axis, grid,
                   variogram.fit, plot.fit, 
                   plot.legend, pch, legend.lab, interpolate, aggregate.fact,
                   xlim=c(min(object@extent@xmin, slave@extent@xmin),
                          max(object@extent@xmax, slave@extent@xmax)),
                   ylim=c(min(object@extent@ymin, slave@extent@ymin),
                          max(object@extent@ymax, slave@extent@ymax)),
                   asp=1/cos((mean(range(ylim)) * pi)/180), 
                   zlim=c(min(disparityMap@data[,1]), 
                          max(disparityMap@data[,1])),
                   ...) {
              plotAnglesDif(master, slave,
                            z, xlab, ylab,
                            breaks, col.regions, 
                            cex, cex.axis, grid,
                            variogram.fit, plot.fit,
                            plot.legend, pch, legend.lab,
                            interpolate, aggregate.fact,
                            disparity=T, h, 
                            ...)
          }
)

#' @name plotDisparityMapExpected
#' @rdname plotDisparityMapExpected
#' @export
setMethod('plotDisparityMapExpected', 'SARSet',
          function(object, slave=NULL, ...) {
              plotAnglesDif(master, slave,
                            z, xlab, ylab,
                            breaks, col.regions, 
                            cex, cex.axis, grid,
                            variogram.fit, plot.fit,
                            plot.legend, pch, legend.lab,
                            interpolate, aggregate.fact,
                            disparity=T, h, 
                            ...)
          })