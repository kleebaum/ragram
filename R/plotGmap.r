#' Plot google earth map
#' 
#' Plots a google earth map that shows the location of SAR records. 
#' Uses the \code{\link[dismo]{gmap}} method of the dismo package.
#'  
#' @param object object of the \code{\link{SARSet-class}} 
#' or object of the \code{\link{SAR-class}} (or a subclass e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param xlim the x limits (x1, x2) of the plot. 
#' @param ylim the y limits (y1, y2) of the plot.
#' @param xlab title of the x axis.
#' @param ylab title of the y axis.
#' @param col color within the recorded area.
#' @param sar TRUE if location should be plotted.
#' @param asp aspect, default is 1/cos((mean(range(ylim)) * pi)/180).
#' @param border color of the \code{\link{border}} of the SAR location.
#' @param ... graphical parameters of \code{\link[dismo]{gmap}} method.
#' @export
#' @seealso  \code{\link{plotMap}}, \code{\link[dismo]{gmap}}, \code{\link{border}}, \code{\link{plotBorder}}
#' @examples
#' data(kili)
#'
#' plotGmap(kili)
setGeneric('plotGmap',
           function(object,
                    xlim=c(-5,5)+mean(object@extent[1:2]),
                    ylim=c(-5,5)+mean(object@extent[3:4]),
                    xlab='Longitude', ylab='Latitude', sar=T, 
                    col=rgb(1,0,0,0.5), scale=1, border='black', ...) {
             if(!is.element('maps', installed.packages()[,1])) {
               stop('Please install the `maps` package first.')
             }
             if(!is.element('dismo', installed.packages()[,1])) {
               stop('Please install the `dismo` package first.')
             }
             require(dismo)
             require(maps)
             plotMap(object, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, 
                     map.default.text = F, sar=F, orbit=F, ...)
             e <- extent(xlim[1], xlim[2],
                         ylim[1], ylim[2])
             tryCatch(plot(gmap(e, lonlat = T, type='satellite', scale=scale), 
                           inter=T, 
                           xlim=xlim, ylim=ylim, add=T, ...),
                      error=function(error) {
                        print(error)
                        print('Google maps does not answer :-(.')
                        print('Try without projection... ')
                        plot(gmap(e))
                      })
             if(sar)
               plotBorder(object, col=col, add=T, border=border, ...)
             standardGeneric('plotGmap')
           })

#' @export
setMethod('plotGmap', signature('SAR'), function(object, ...) {
  print('GMap for single SAR object.')
})

#' @export
setMethod('plotGmap', signature('SARSet'), function(object, ...) {
  print('GMap for set of SAR objects.')
})