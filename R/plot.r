#' Plot SAR record
#' 
#' Plot one SAR or two SAR records together.
#' 
#' @param object object of the \code{\link{SAR-class}} or a subclass 
#' (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}) (or 
#' an object of the \code{\link{SARSet-class}}, then slave is ignored). 
#' @param slave object of the \code{\link{SAR-class}} or a subclass 
#' (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param xlab title of the x axis.
#' @param ylab title of the y axis.
#' @param maxpixels Maximum number of cells to use for the plot (see \code{\link{plot}} for RasterLayer object).
#' @param max.val maximal amplitude to plot, higher amplitudes are ploted in white. 
#' To use even the highest amplitudes type 'object@data@max'.
#' @param plotIntersect logical. Plot the intersection area?
#' @param border color of the border.
#' @param grid logical. Add a grid to the plot?
#' @param col color ramp (see \code{\link{colorRampPalette}}).
#' @param north.arrow logical. Plot a north arrow?
#' @param north.arrow.pos vector c(x,y) that contains the position of the north arrow.
#' @param north.arrow.length length of the north arrow.
#' @param scalebar logical. Plot a scalebar?
#' @param scalebar.pos vector c(x,y) that contains the position of the scalebar.
#' @param scalebar.col color of the scalebar.
#' @param scalebar.d distance in kilometers.
#' @param ... plotting parameters like xlim, ylim and asp.
#' @export
#' @examples
#' data(kili)
#' plotSAR(master)
#' plotSAR(slave) 
setGeneric('plotSAR',
           function(object, slave=NULL, 
                    xlab='Longitude', ylab='Latitude', 
                    maxpixels=50000, max.val=500, cex.axis=1,
                    cex.lab=1, legend.by=30, 
                    plotIntersect=T, border='red', grid=T,
                    plot.legend=T, legend.lab='Amplitude',
                    legend.line=2.2,
                    breaks=seq(0, max.val, by = 1),
                    legend.breaks=seq(0, max.val, by = legend.by),
                    zlim=c(0, max.val),
                    cols=colorRampPalette(c('grey0', 'grey100'))(length(seq(0, max.val, by = 1))-1),
                    north.arrow=F, north.arrow.pos=NULL, north.arrow.length=0.05,
                    scalebar=F, scalebar.pos=NULL, scalebar.col='black',
                    scalebar.d=100, 
                    ...) {
               standardGeneric('plotSAR')
               xlim <- par('xaxp')[1:2]
               ylim <- par('yaxp')[1:2]
               if(north.arrow) {
                   if(is.null(north.arrow.pos)) {
                       north.arrow.pos <- c(xlim[2], ylim[1]+0.03*diff(ylim))
                   }
                   north.arrow(xb=north.arrow.pos[1], 
                               yb=north.arrow.pos[2], 
                               len=north.arrow.length,
                               lab='N', cex.lab=cex.lab)
               }
               if(scalebar) {
                   if(is.null(scalebar.pos)) {
                       scalebar.pos <- c(xlim[2]-0.27*diff(xlim),
                                         ylim[1]+0.02*diff(ylim))
                   }
                   scalebar(scalebar.d, xy=scalebar.pos, 
                            'bar', lonlat = T, col=scalebar.col,
                            below='km', divs=3, cex=cex.lab, adj=c(0.5, -1.3))
               }
               if(plot.legend) {
                   plotLegend(legend.lab=legend.lab,
                              col=cols,
                              zlim=zlim, cex=cex.lab,
                              cex.axis=cex.axis, 
                              legend.line=legend.line, 
                              legend.by=legend.by, ...)
               }
           })

#' @rdname plotSAR
#' @export
setMethod('plotSAR', 'SAR',
          function(object, slave=NULL, 
                   xlab, ylab, maxpixels, max.val, cex.axis,
                   cex.lab, legend.by,
                   plotIntersect, border, grid,
                   plot.legend, legend.lab, legend.line,
                   breaks, legend.breaks, zlim, cols,
                   xlim=c(object@extent@xmin, object@extent@xmax),
                   ylim=c(object@extent@ymin, object@extent@ymax), 
                   asp=NULL, ...) {
              if(is.null(asp)) {
                  if(object@extent[1]<180) {
                      asp <- 1/cos((mean(range(ylim))*pi)/180)
                  } else {
                      asp <- 1
                  }
              }
              plot(xlim, ylim, type='n', cex.axis=cex.axis, cex.lab=cex.lab,
                   xlab=xlab, ylab=ylab, asp=asp, ...)
              if(grid)
                  grid()
              image(object, col=cols, breaks=breaks, add=T, maxpixels=maxpixels, 
                    legend=F, ...)
          })

#' @rdname plotSAR
#' @export
setMethod('plotSAR', 'RasterLayer',
          function(object, ...) {
              sar <- new('SAR', raster=object)
              plotSAR(sar, slave=NULL, 
                      xlab, ylab, maxpixels, max.val, cex.axis,
                      cex.lab, legend.by,
                      plotIntersect, border, grid,
                      plot.legend=F, legend.lab, legend.line,
                      breaks, legend.breaks, zlim, cols,
                      ...)
          })

#' @rdname plotSAR
#' @export
setMethod('plotSAR', c('SAR', 'SAR'),
          function(object, slave, xlab, ylab, maxpixels, max.val, cex.axis,
                   cex.lab, legend.by,
                   plotIntersect, border, grid,
                   plot.legend, legend.lab, legend.line,
                   breaks, legend.breaks, zlim, cols,
                   xlim=c(min(object@extent@xmin, slave@extent@xmin),
                          max(object@extent@xmax, slave@extent@xmax)),
                   ylim=c(min(object@extent@ymin, slave@extent@ymin),
                          max(object@extent@ymax, slave@extent@ymax)),
                   asp=NULL,
                   ...) {
              if(is.null(asp)) {
                  if(object@extent[1]<180) {
                      asp <- 1/cos((mean(range(ylim))*pi)/180)
                  } else {
                      asp <- 1
                  }
              }
              plot(xlim, ylim, type = 'n', cex.axis=cex.axis, cex.lab=cex.lab,
                   xlab=xlab, ylab=ylab, asp=asp, ...)
              if(grid)
                  grid()
              image(object, col=cols, breaks=breaks, add=T, maxpixels=maxpixels, 
                    legend=F, ...)
              image(slave, col=cols, breaks=breaks, add=T, maxpixels=maxpixels, 
                    legend=F, ...)
              if(plotIntersect)
                  plot(intersection(object, slave), add=T, border=border)
              
          })

#' @rdname plotSAR
#' @export
setMethod('plotSAR', 'SARSet',
          function(object, slave=NULL, ...) {
              for(i in 1:length(object)) {
                  master <- object[[i]]
                  for(j in i:length(object)) {
                      slave <- object[[j]]
                      if(!identical(master, slave)) {
                          plotSAR(master, slave, ...)
                      }
                  }
              }
          })

plotSARLegend <- function(object, max.val=250, legend.by=30,
                          cex.axis=1, cex.lab=1,
                          breaks=seq(0, max.val, by = 1),
                          legend.breaks=seq(0, max.val, by = legend.by),
                          cols=colorRampPalette(c('grey0', 'grey100'))(length(seq(0, max.val, by = 1))-1), 
                          side=4, line=2.4, legend.label='Amplitude', 
                          new=F, mar=c(1, 0, 0, 0)) {
    par(mar=mar, new=new)
    if (object@rotated) {
        object@rotated <- F # otherwise the plot function does not work
    }
    plot(object, legend.only=T,
         breaks = breaks, col=cols, maxpixels=1,
         axis.args=list(at=legend.breaks, labels=legend.breaks, cex.axis=cex.axis),
         legend.args=list(text=legend.label, side=side, line=line, cex=cex.lab))
}

removeFlipping <- function(object, ...) {
    #sar.array = as.array(A)
    #sar.flipped = apply(sar.array, 2, function(x) x[dim(x)[1]:1])
    gP <- object@geolocationPoints
    diff(gP@data$row[c(1,length(gP))])
    diff(gP@data$col[1:2])
    diff(gP@coords[c(1,length(gP)),'lat'])  # compare to row
    diff(gP@coords[1:2,'lon'])              # compare to col
    
    if(diff(gP@data$col[1:2])*diff(gP@coords[1:2,'lon'])<0) {
        print('flipped horizontally')
        object <- flip(object, 'x')
    }
    
    if(diff(gP@data$row[c(1,length(gP))])*diff(gP@coords[c(1,length(gP)),'lat'])<0) {
        print('flipped vertically')
        object <- flip(object, 'y')
    }
    return(object)
}

#' @rdname plotSAR
#' @export
setMethod('plot', c('SARSet', 'ANY'),
          function(x, y=NULL, ...) {
              plotSAR(x, ...)
          })
