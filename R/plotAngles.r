#' Plot angles
#' 
#' Plots the angles provided by GCPs/TPs as points.
#'  
#' @param object object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param z either incidence (default) or elevation angles 'thetaEl'.
#' @param xlab title of the x axis.
#' @param ylab title of the y axis.
#' @param breaks number of breaks of the color ramp.
#' @param col.regions color ramp.
#' @param plot.legend TRUE if the legend is plotted.
#' @param legend.lab title of the legend.
#' @param asp aspect, default is 1/cos((mean(range(ylim)) * pi)/180).
#' @param variogram.fit TRUE if a Gaussian variogram should be fitted.
#' @param plot.fit TRUE if the fitted variogram should be plotted.
#' @param interpolate TRUE if angles should be interpolated.
#' @param ... graphical parameters. Any argument that can be passed to plot, such as axes=FALSE and main='title'.
#' @export
#' @seealso  \code{\link{angles}}, \code{\link{GeolocationPoints-class}}
#' @examples
#' data(kili)
#' plotAngles(master)
#' plot(angles(master))
setGeneric('plotAngles', 
           function(object, z='thetaIn',
                    xlab='Longitude', ylab='Latitude', 
                    breaks=255,
                    col.regions=colorRampPalette(colors)(breaks), 
                    cex=1, cex.axis=1, grid=T, 
                    plot.legend=T, pch=20, 
                    legend.lab=expression(paste(theta[i], ' [deg]')),
                    variogram.fit=T, plot.fit=F, 
                    interpolate=F, aggregate.fact=100,
                    ...) {
               standardGeneric('plotAngles')
           })

#' @name plotAngles
#' @rdname plotAngles
#' @export
setMethod('plotAngles', 'SpatialPointsDataFrame',
          function(object, z, xlab, ylab, 
                   breaks, col.regions, 
                   cex, cex.axis, grid,
                   plot.legend, pch, legend.lab,
                   variogram.fit, plot.fit, 
                   interpolate, aggregate.fact,
                   xlim=object@bbox[1,],
                   ylim=object@bbox[2,],
                   asp=1/cos((mean(range(ylim)) * pi)/180), 
                   zlim=c(min(object@data[,z]), 
                          max(object@data[,z])),
                   ...) {
              plot(xlim, ylim, type = 'n', cex.axis=cex.axis, cex.lab=cex,
                   xlab=xlab, ylab=ylab, asp=asp, ...)
              if(grid)
                  grid()
              
              break.points <- seq(zlim[1], zlim[2], length.out = (breaks-2))
              if(zlim[1]>min(object@data[,z]))
                  break.points <- c(min(object@data[,z]), break.points)
              if(zlim[2]<max(object@data[,z]))
                  break.points <- c(break.points, max(object@data[,z]))
              mycol <- col.regions[as.numeric(cut(object@data[,z], 
                                                  breaks=break.points,
                                                  include.lowest=T))]
              
              points(coordinates(object), pch=pch, col=mycol)
              if(plot.legend) {
                  plotLegend(legend.lab=legend.lab, zlim=zlim, cex=cex, cex.axis=cex.axis,
                             nlevel=breaks, ...)
              }
          })

#' @name plotAngles
#' @rdname plotAngles
#' @export
setMethod('plotAngles', 'SAR', 
          function(object, z,
                   xlab, ylab, breaks, col.regions, 
                   cex, cex.axis, grid, plot.legend, pch, 
                   legend.lab,
                   variogram.fit, plot.fit, 
                   interpolate, aggregate.fact,
                   xlim=c(object@extent@xmin, object@extent@xmax),
                   ylim=c(object@extent@ymin, object@extent@ymax),
                   asp=1/cos((mean(range(ylim)) * pi)/180), 
                   zlim=c(min(my.angles@data[,z]), 
                          max(my.angles@data[,z])), ...) {
              if(interpolate) {
                  my.angles <- angles(object, z, interpolate, variogram.fit, 
                                   plot.fit, aggregate = T, aggregate.fact)
                  colnames(my.angles@data)[1] <- z
              } else {
                  my.angles <- object@geolocationPoints
              }
              plotAngles(my.angles, z,
                         xlab, ylab, breaks, col.regions, 
                         cex, cex.axis, grid,
                         plot.legend, pch, 
                         legend.lab,
                         variogram.fit, plot.fit, 
                         interpolate, aggregate.fact,
                         xlim, ylim, asp, zlim,
                         ...)
              plot(border(object), add=T)
          })

#' @name plotAngles
#' @rdname plotAngles
#' @export
setMethod('plotAngles', 'SARSet',
          function(object, ...) {
              lapply(object, function(sar) {
                  plotAngles(sar, z,
                             xlab, ylab, breaks, col.regions, 
                             cex, cex.axis, grid, 
                             plot.legend, pch, legend.lab,
                             variogram.fit, plot.fit, 
                             interpolate, aggregate.fact,
                             ...)
              })
          })

plotIncidenceAngles <- function(object, ...) {
    plotAngles(object, ...)
}

plotElevationAngles <- function(object, ...) {
    plotAngles(object, z = 'thetaEl', ...)
}
# 
# levelplotAngles <- function(object) {
#     levelplot(object@data[,z]~
#                   round(object@coords[,1],round.digits) *
#                   round(object@coords[,2],round.digits),
#               xlab=xlab, ylab=ylab, ...,
#               col.regions=col.regions, interpolate=interpolate,
#               panel=function(...) {
#                   if(useRaster)
#                       panel.levelplot.raster(...)
#                   else
#                       panel.levelplot(...)
#                   if(!is.null(sarObject))
#                       plotBorder(sarObject, panel=T)
#               }, ...
#     )
# }

# setMethod('plotAngles', 'SARSet',
#           function(object, ...) {
#               z <- sapply(object@elements, function(sar) sar@geolocationPoints@data[,z])
#               x <- sapply(object@elements, function(sar) round(sar@geolocationPoints@coords[,1], round.digits))
#               y <- sapply(object@elements, function(sar) round(sar@geolocationPoints@coords[,2], round.digits))
#               levelplot(z ~ x*y,
#                         xlab=xlab, ylab=ylab, 
#                         col.regions=col.regions, interpolate=interpolate,
#                         panel=function(...) {
#                             if(useRaster)
#                                 panel.levelplot.raster(...)
#                             else
#                                 panel.levelplot(...)
#                             plotBorder(object, panel = T)
#                         }, ...
#               )
#           })