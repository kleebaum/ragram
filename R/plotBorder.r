#' Plot border
#' 
#' Plots border provided by metadata as a \code{\link{SpatialPolygons}} object.
#'  
#' @param object object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param col color within the recorded area.
#  @param panel TRUE if the border should be plotted on a lattice plot (e.g. levelplot).
#' @param ... Further arguments to \code{\link[graphics]{plot}}.
#' @export
#' @seealso \code{\link{border}}
#' @examples
#' data(kili)
#' 
#' plotBorder(master)
#' 
#' plotMap(kili, sar=FALSE, orbit=FALSE)
#' 
#' plotBorder(kili[[1]], add=TRUE)
#' plotOrbitNumber(kili[[1]])
#' plotBorder(kili[[2]], add=TRUE)
#' plotOrbitNumber(kili[[2]])
setGeneric('plotBorder', 
           function(object, col=rgb(1,0,0,0.5), #panel=F, 
                    ...) {
               standardGeneric('plotBorder')
           })

#' @rdname plotBorder
#' @export
setMethod('plotBorder', 'SAR',
          function(object, col=col, ...) {
              # if(panel) {
              #     cornerCoords <- data.frame(object@cornerLon, object@cornerLat)
              #     cornerCoords <- cornerCoords[chull(cornerCoords), ]
              #     panel.polygon(x=cornerCoords[,1], y=cornerCoords[,2], ...)
              # } else { 
                  plot(border(object), col=col, ...)
              # }
          })

#' @rdname plotBorder
#' @export
setMethod('plotBorder', 'SARSet',
          function(object, col=col, ...) {
              # if(panel) {
              #     lapply(object, plotBorder, panel=panel, ...)
              # } else { 
                  borders <- border(object)
                  for (i in 1:length(borders)) {
                      plot(borders[i], col=col, ...)
              # }
              }
          })