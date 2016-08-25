#' Plot orbit number
#' 
#' Adds orbit numbers of SAR records.
#'  
#' @param object object of the \code{\link{SARSet-class}} 
#' or object of the \code{\link{SAR-class}} (or a subclass e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param labels character that is ploted (default: object@orbit).
#' @param x x coordinates of labels.
#' @param y y coordinates of labels.
#' @param cex Text size (see \code{[graphics]\link{par}}).
#' @param col Plotting color (see \code{[graphics]\link{par}}).
#  @param panel TRUE if text is added to a levelplot.
#' @param ... parameters of \code{\link{text}} method.
#' @export
#' @seealso  \code{\link{plotMap}}, \code{\link{plotGmap}}
#' @examples
#' data(kili)
#' 
#' plotMap(kili[[1]], orbit=FALSE)
#' plotOrbitNumber(kili[[1]])
#' 
#' plotMap(kili, orbit=FALSE)
#' plotOrbitNumber(kili)
setGeneric('plotOrbitNumber', 
           function(object, cex=1, col='black', #panel=F, 
                    ...) {
               standardGeneric('plotOrbitNumber')
})

#' @rdname plotOrbitNumber
#' @export
setMethod('plotOrbitNumber', 'SAR',
          function(object, cex=cex, col=col, #panel,
                   x=object@centerLon,
                   y=object@centerLat, 
                   labels=object@orbit, ...) {
              # if(panel) {
              #     trellis.focus("panel", 1, 1)
              #     ltext(labels=labels, x=x, y=y, cex=cex, ...)
              #     trellis.unfocus()
              # } else {
                  text(x=x, y=y, labels=labels, cex=cex,
                       col=col, ...)
              # }
})

#' @rdname plotOrbitNumber
#' @export
setMethod('plotOrbitNumber', 'SARSet',
          function(object, ...) {
              lapply(object, function(sar, ...) {
                  plotOrbitNumber(sar, cex, col, #panel, 
                                  ...)
              })
              cat('Added orbit numbers for a set of SAR objects.\n')
})