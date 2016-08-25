#' Zero normalized cross correlation (ZNCC)
#' 
#' Calculates the zero normalized cross correlation (ZNCC) coefficient between two \code{\link{SAR-class}} objects.
#' This method also works for \code{\link{RasterLayer-class}} objects. The maximum ZNCC value determines disparity.
#' 
#' @param master \code{\link{SAR-class}} object (or a subclass e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param slave \code{\link{SAR-class}} object (or a subclass e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param u1 Integer. Column, i.e. y coordinate of master pixel.
#' @param v1 Integer. Row, i.e. x coordinate of master pixel.
#' @param u2 Integer. Column, i.e. y coordinate of slave pixel.
#' @param v2 Integer. Row, i.e. x coordinate of slave pixel.
#' @param n Integer. (Window size-1)/2 in x direction.
#' @param m Integer. (Window size-1)/2 in y direction.
#' @export
setGeneric('zncc', function(master, slave, u1, v1, u2, v2, n=3, m=n) {
    standardGeneric('zncc')
})

#' @rdname zncc
#' @export
setMethod('zncc', c('RasterLayer', 'RasterLayer'), 
          function(master, slave, u1, v1, u2, v2, n=3, m=n) {
              cor(master[(u1-n):(u1+n), (v1-m):(v1+m)], slave[(u2-n):(u2+n), (v2-m):(v2+m)])
          })

#' @rdname zncc
#' @export
setMethod('zncc', c('matrix', 'matrix'), 
          function(master, slave, u1, v1, u2, v2, n=3, m=n) {
              master <- raster(master)
              slave <- raster(slave)
              cor(master[(u1-n):(u1+n), (v1-m):(v1+m)], slave[(u2-n):(u2+n), (v2-m):(v2+m)])
          })

#' Sum of squared differences (SSD)
#'
#' Calculates the sum of squared differences (SSD) between two \code{\link{SAR-class}} objects.
#' This method also works for \code{\link{RasterLayer-class}} objects. The minimum SSD value determines disparity.
#' 
#' @param master \code{\link{SAR-class}} object (or a subclass e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param slave \code{\link{SAR-class}} object (or a subclass e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param u1 Integer. Column, i.e. y coordinate of master pixel.
#' @param v1 Integer. Row, i.e. x coordinate of master pixel.
#' @param u2 Integer. Column, i.e. y coordinate of slave pixel.
#' @param v2 Integer. Row, i.e. x coordinate of slave pixel.
#' @param n Integer. (Window size-1)/2 in x and y direction.
#' @export
ssd <- function(master, slave, u1, v1, u2, v2, n=3) {
    sum((master[(u1-n):(u1+n), (v1-n):(v1+n)] - slave[(u2-n):(u2+n), (v2-n):(v2+n)])^2)
}

#' Sum of absolute differences (SAD)
#'
#' Calculates the sum of absolute differences (SAD) between two \code{\link{SAR-class}} objects.
#' This method also works for \code{\link{RasterLayer-class}} objects. The minimum SAD value determines disparity.
#' 
#' @param master \code{\link{SAR-class}} object (or a subclass e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param slave \code{\link{SAR-class}} object (or a subclass e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param u1 Integer. Column, i.e. y coordinate of master pixel.
#' @param v1 Integer. Row, i.e. x coordinate of master pixel.
#' @param u2 Integer. Column, i.e. y coordinate of slave pixel.
#' @param v2 Integer. Row, i.e. x coordinate of slave pixel.
#' @param n Integer. (Window size-1)/2 in x and y direction.
#' @export
sad <- function(master, slave, u1, v1, u2, v2, n) {
    sum(abs(master[(u1-n):(u1+n), (v1-n):(v1+n)] - slave[(u2-n):(u2+n), (v2-n):(v2+n)]))
}

#' Window mean
#' 
#' Calculates window mean for a given \code{\link{SAR-class}} object.
#' The window is a square.
#' 
#' @param object \code{\link{SAR-class}} object (or a subclass e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param u Integer. Center of window (column, i.e. y coordinate).
#' @param v Integer. Center of window (row, i.e. x coordinate).
#' @param n Integer. (Window size-1)/2 in x and y direction.
#' @export
getAverage <- function(object, u, v, n) {
    s <- 0
    for (i in seq(-n, n, 1))
        for (j in seq(-n, n, 1)) {
            s = s + as.numeric(object[u+i,v+j])
        }
    return(s/(2*n+1)^2)
}

# getAvgAndSD <- function(img, u, v, n) {
#     avg <- getAverage(img, u, v, n)
#     sd <- 0
#     for (i in seq(-n, n, 1))
#         for (j in seq(-n, n, 1))
#             sd = sd + (as.numeric(img[u+i,v+j]) - avg)^2
#     return(c(avg, sd^0.5))
# }
# 
# zncc <- function(img1, img2, u1, v1, u2, v2, n) {
#     avgAndSD1 <- getAvgAndSD(img1, u1, v1, n)
#     avgAndSD2 <- getAvgAndSD(img2, u2, v2, n)
#     
#     cov <- 0
#     for (i in seq(-n, n, 1))
#         for (j in seq(-n, n, 1))
#             cov <- cov + (as.numeric(img1[u1+i,v1+j]) - avgAndSD1[1]) * (as.numeric(img2[u2+i,v2+j]) - avgAndSD2[1])
#     return(cov/(avgAndSD1[2] * avgAndSD2[2]))
# }