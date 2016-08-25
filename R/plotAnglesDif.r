#' Plot differences of two SAR records
#' 
#' Plots the difference in angles of two provided by GCPs/TPs as points.
#'  
#' @param object Object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param z Character. Either incidence ('thetaIn', default) or elevation angles 'thetaEl'.
#' @param xlab Character. Title of the x axis.
#' @param ylab Character. Title of the y axis.
#' @param breaks Integer. Number of breaks of the color ramp.
#' @param col.regions Color ramp.
#' @param plot.legend Logical. Plot the legend?
#' @param legend.lab Character. Title of the legend.
#' @param asp Numeric vector. Aspect, default is 1/cos((mean(range(ylim)) * pi)/180).
#' @param variogram.fit Logical. Fit a Gaussian variogram?
#' @param plot.fit. Logical. Plot the fitted variogram?
#' @param interpolate Logical. Interpolate angles?
#' @param aggregate.fact Integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). 
#' Or two integers (horizontal and vertical aggregation factor) or three integers (when also aggregating over layers). 
#' See \code{\link{aggregate}} method of raster package. If no aggregation is done, 
#' the interpolation might take a very long time...
#' @param asp Numeric vector. Aspect, default is 1/cos((mean(range(ylim)) * pi)/180).
#' @param disparity Logical. Should the expected disparity be plotted?
#' @param h Numeric. Relative height for plotting the expected disparity.
#' @param ... graphical parameters. Any argument that can be passed to plot, such as axes=FALSE and main='title'.
#' @export
#' @seealso  \code{\link{anglesDif}}, \code{\link{angles}}, \code{\link{plotAngles}}, \code{\link{GeolocationPoints-class}}
#' @examples
#' data(kili)
#' 
#' plotAnglesDif(kili[[1]], kili[[3]])
#' 
#' plotAnglesDif(kili)
#' 
#' plotAnglesDif(master, slave, interpolate=T)
#' plot(anglesDif(master, slave, interpolate=T, aggregate.fact=100))
#' 
#' plotAnglesDif(kili[[1]], kili[[5]], disparity = T,
#' legend.lab = 'Disparity [m]', h=100)
setGeneric('plotAnglesDif', function(object, slave, z='thetaIn',
                                     xlab='Longitude', ylab='Latitude', 
                                     breaks=255,
                                     col.regions=colorRampPalette(colors)(breaks), 
                                     cex=1, cex.axis=1, grid=T,
                                     variogram.fit=T, plot.fit=F, 
                                     plot.legend=T, pch=20, 
                                     legend.lab=expression(paste(Delta, theta[i], ' [deg]')),
                                     interpolate=F, aggregate.fact=100,
                                     disparity=F, h=100,
                                     ...) {
    standardGeneric('plotAnglesDif')
})

#' @export
setMethod('plotAnglesDif', 'SAR',
          function(object, slave=NULL, ...) {
              cat('Impossible to plot the differences in angles for a single SAR object.\n')
              cat('Please provide a second one or a SARSet object.')
          }
)

#' @name plotAnglesDif
#' @rdname plotAnglesDif
#' @export
setMethod('plotAnglesDif', c('SAR', 'SAR'),
          function(object, slave, z, xlab, ylab, 
                   breaks, col.regions, 
                   cex, cex.axis, grid,
                   variogram.fit, plot.fit, 
                   plot.legend, pch, legend.lab, 
                   interpolate, aggregate.fact,
                   disparity, h,
                   xlim=c(min(object@extent@xmin, slave@extent@xmin),
                          max(object@extent@xmax, slave@extent@xmax)),
                   ylim=c(min(object@extent@ymin, slave@extent@ymin),
                          max(object@extent@ymax, slave@extent@ymax)),
                   asp=1/cos((mean(range(ylim)) * pi)/180), 
                   zlim=c(min(angleDif@data[,1]), 
                          max(angleDif@data[,1])),
                   ...) {
              if(disparity) {
                  angleDif <- disparityMapExpected(object, slave, h, z, 
                                                   variogram.fit, plot.fit,
                                                   interpolate, aggregate.fact)
              } else {
                  angleDif <- anglesDif(object, slave, z, variogram.fit, plot.fit, 
                                        interpolate, aggregate.fact)
              }
              plot(xlim, ylim, type = 'n', cex.axis=cex.axis, cex.lab=cex,
                   xlab=xlab, ylab=ylab, asp=asp, ...)
              if(grid)
                  grid()
              plot(border(object), add=T)
              plot(border(slave), add=T)
              
              break.points <- seq(zlim[1], zlim[2], length.out = (breaks-2))
              if(zlim[1]>min(angleDif@data[,1]))
                  break.points <- c(min(angleDif@data[,1]), break.points)
              if(zlim[2]<max(angleDif@data[,1]))
                  break.points <- c(break.points, max(angleDif@data[,1]))
              mycol <- col.regions[as.numeric(cut(angleDif@data[,1], 
                                                  breaks=break.points,
                                                  include.lowest=T))]
              
              points(coordinates(angleDif), pch=pch, col=mycol)
              if(plot.legend) {
                  plotLegend(legend.lab=legend.lab, nlevel=breaks,
                             zlim=zlim, cex=cex, cex.axis=cex.axis,
                             ...)
              }
              return(angleDif)
          }
)

#' @name plotAnglesDif
#' @rdname plotAnglesDif
#' @export
setMethod('plotAnglesDif', 'SARSet',
          function(object, slave=NULL, ...) {
              for(i in 1:length(object)) {
                  master <- object[[i]]
                  for(j in i:length(object)) {
                      slave <- object[[j]]
                      if(!identical(master, slave)) {
                          print(plotAnglesDif(master, slave,
                                              z, xlab, ylab,
                                              breaks, col.regions, 
                                              cex, cex.axis, grid,
                                              variogram.fit, plot.fit,
                                              plot.legend, pch, legend.lab,
                                              interpolate, aggregate.fact,
                                              disparity, h, 
                                              ...))
                      }
                  }
              }
          })

levelplotAnglesDif <- function(object, slave, z='thetaIn',
                               xlab='Longitude', ylab='Latitude', 
                               col.regions=colorRampPalette(colors)(255), 
                               round.digits=2, cex=1, cex.axis=1, 
                               variogram.fit=T, plot.fit=T, 
                               interpolate, aggregate.fact=100,
                               useRaster=T, 
                               xlim=c(min(object@extent@xmin, slave@extent@xmin),
                                      max(object@extent@xmax, slave@extent@xmax)),
                               ylim=c(min(object@extent@ymin, slave@extent@ymin),
                                      max(object@extent@ymax, slave@extent@ymax)),
                               asp=1/cos((mean(range(ylim)) * pi)/180), ...) {
    angleDif <- anglesDif(object, slave, z, variogram.fit, plot.fit,
                          interpolate, aggregate.fact)
    levelplot(angleDif@data[,1] ~
                  round(angleDif@coords[,1], round.digits)*
                  round(angleDif@coords[,2], round.digits),
              xlab=list(xlab, cex=cex), ylab=list(ylab, cex=cex),
              scales=list(x=list(cex=cex.axis),
                          y=list(cex=cex.axis, rot=90), cex=cex.axis,
                          tck = c(1,0)),
              xlim=xlim,
              ylim=ylim,
              asp=asp,
              colorkey = list(labels=list(cex=cex.axis)),
              col.regions=col.regions,
              panel=function(...) {
                  if(useRaster)
                      panel.levelplot.raster(...)
                  else
                      panel.levelplot(...)
                  plotBorder(slave, panel = T)
                  plotBorder(object, panel = T)
              }, ...
    )
}