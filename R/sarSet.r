#' Create a set of synthetic aperture radar (SAR) images
#' 
#' Creates a set of synthetic aperture radar (SAR) images. 
#' This is useful to identify stereo images.
#' 
#' @param elements list of SAR objects.
#' @export
#' @examples  
#' my.sar.set <- sarSet(c(my.sar.record1, my.sar.record2))
sarSet <- function(elements=c(), ...) {
    new('SARSet', elements, ...)
}