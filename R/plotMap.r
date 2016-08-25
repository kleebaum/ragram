#' Plot map
#' 
#' Plots a map that shows the location of SAR records.
#' Uses the \code{\link[maps]{map}} method of the maps package.
#'  
#' @param object object of the \code{\link{SARSet-class}} 
#' or object of the \code{\link{SAR-class}} (or a subclass e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param database character string naming a geographical database (see \code{\link[maps]{map}}).
#' @param regions character vector that names the polygons to draw (see \code{\link[maps]{map}}).
#' @param xlim the x limits (x1, x2) of the plot. 
#' @param ylim the y limits (y1, y2) of the plot.
#' @param xlab title of the x axis.
#' @param ylab title of the y axis.
#' @param map.default.text TRUE if default text is plotted using the \code{\link[maps]{map.text}} method.
#' @param labels vector of text that should be plotted on the map.
#' @param labels.x x coordinates of labels.
#' @param labels.y y coordinates of labels.
#' @param col color within the recorded area.
#' @param sar TRUE if location should be plotted.
#' @param orbit TRUE if orbit number is plotted
#' @param border color of the \code{\link{border}} of the SAR location.
#' @param ... graphical parameters of \code{\link[maps]{map}} method.
#' @export
#' @seealso  \code{\link{plotGmap}}, \code{\link[maps]{map}}, \code{\link{border}}, \code{\link{plotBorder}}
#' @examples
#' data(kili)
#'
#' plotMap(kili)
#' plotMap(kili[[1]])
setGeneric('plotMap', 
           function(object, database='world', regions='.',
                    xlim=c(-8,8)+mean(object@extent[1:2]),
                    ylim=c(-8,8)+mean(object@extent[3:4]),
                    xlab='Longitude', ylab='Latitude',
                    main=NULL, add=F, map.default.text=T, sar=T,
                    labels.x=NULL, labels.y=NULL, labels=seq_along(labels.x), 
                    col=rgb(1,0,0,0.5), orbit=T, ...) {
               if(!is.element('maps', installed.packages()[,1])) {
                   stop('Please install the `maps` package first.')
               }
               standardGeneric('plotMap')
               if(map.default.text) {
                   xlimMapCorner <<- xlim
                   ylimMapCorner <<- ylim
                   map.text(database, regions, bg=rgb(1,1,1,0.3),
                            xlim=xlimMapCorner, ylim=ylimMapCorner, 
                            add=add, ...)
               } else {
                   map(database, regions, bg=rgb(1,1,1,0.3),
                       xlim=xlim, ylim=ylim, add=add, ...) 
               }
               if(!is.null(labels.x)) {
                   text(x=labels.x, y=labels.y, labels=labels, ...)
               }
               map.axes(...)
               title('', xlab=xlab, ylab=ylab, main=main, ...)
               if(sar) 
                   plotBorder(object, col=col, add=T, ...)
               if(orbit)
                   plotOrbitNumber(object, ...)
           })

setMethod('plotMap', 'SAR', function(object, ...) {
    cat('Map for single SAR object.\n')
})

setMethod('plotMap', 'SARSet', function(object, ...) {
    cat('Map for set of SAR objects.\n')
})