#' @useDynLib ragram
#'
#' @import sp
#' @import raster
#' @import gstat
#' @import XML
#' @import fields
#' @import gdalUtils
#' @import foreach
#' @import Rcpp
#' @import methods
#' @importFrom abind abind
#' @importFrom stats as.formula cor
#' @importFrom utils txtProgressBar setTxtProgressBar installed.packages
#' @importFrom grDevices colorRampPalette rgb chull
#  @importFrom lattice levelplot trellis.focus trellis.unfocus panel.levelplot panel.levelplot.raster
#  @importFrom dismo gmap
#' @importFrom graphics par points title

###---Settings---####
colors <- c("blue", "green", "yellow", "orange", "red", "purple")
