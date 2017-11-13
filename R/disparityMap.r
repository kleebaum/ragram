#' Calculated disparity map between two SAR records
#'
#' Calculates a disparity map between two SAR records using window based
#' zero normalized cross correlation (ZNCC).
#'
#' @param master Object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param slave Object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param window.size Integer. Edge Length of quadratic window(s) to calculate \code{\link{zncc}}.
#' @param search.area.size Integer. Edge Length of quadratic search area in slave image.
#' @param search.area.shift Numeric vector. Pixels in x and y direction that the search area should be shifted.
#' That is how a priori knowledge about disparity can be regarded.
#' @param resample.slave Logical. Should the slave image be resampled to the aggregated master image?
#' @param window.moving.step Integer. Step size to move windows in slave image. Default is 1.
#' @param run.parallel Logical. Run algorithm on more than one cores?
#' @param cores Integer. How many cores should be allocated?
#' @param log Logical. Log output to text file?
#' @param log.file Character. Log file address and name.
#' @return Four dimensional array. The first and second dimension give rows and columns, respectively.
#' The third and forth dimension gives disparities in x and y direction. The disparity is measured in pixel.
#' @export
#' @seealso \code{\link{zncc}}
#' @examples
#' data(kili)
#'
#' master <- aggregate(master, 5)
#'
#' disp.map <- disparityMap(master, slave, window.size=11, search.area.size=25)
#'
#' disp.map.lon <- raster(disp.map[,,1])
#' extent(disp.map.lon) <- extent(master)
#' plot(disp.map.lon)
#'
#' disp.map.lat <- raster(disp.map[,,2])
#' extent(disp.map.lat) <- extent(master)
#' plot(disp.map.lat)
#'
#' disp.map.diagonal <- disp.map.lon
#' values(disp.map.diagonal) <- sqrt(disp.map.lat[]^2 + disp.map.lon[]^2)
#' plot(disp.map.diagonal)
#'
#' # to run parallel register cores first, e.g.:
#' # library(doMC)
#' # registerDoMC(4)
disparityMapFaster <- function(master, slave, window.size = 3, search.area.size = 7,
  search.area.shift = c(0, 0), resample.slave = T, window.moving.step = 1, run.parallel = F,
  cores = 4, log = F, log.file = "dispMapLog.txt") {
  if (window.size%%2 == 0) {
    window.size <- window.size + 1
    cat("Window size needs to be uneven, it was increased to", window.size, "\n")
  }
  
  if (search.area.size%%2 == 0) {
    search.area.size <- search.area.size + 1
    cat("Search window size needs to be uneven, it was increased to", search.area.size,
      "\n")
  }
  
  if (resample.slave) {
    slave <- resample(slave, master)
  }
  
  indices <- as.array(round(seq((-1 * (search.area.size - max(window.size))/2),
    ((search.area.size - max(window.size))/2), by = window.moving.step)))  # shifts of window center relative to center of search area
  
  n <- (window.size - 1)/2  # n in formula of zncc
  
  cat("ZNCC is calculated for quadratic windows of edge length", window.size, "(measured in pixels).\n")
  cat("Search area size is", search.area.size, "pixels.\n")
  cat("Windows are centered for indices", indices, "in each search window (center is zero).\n")
  
  disp.map <- array(dim = c(max(dim(master)[1], dim(slave)[1]), max(dim(master)[2],
    dim(slave)[2]), 2), dimnames = list(NULL, NULL, c("dispY", "dispX")))
  
  zncc.matrix <- matrix(nrow = dim(indices), ncol = dim(indices), dimnames = list(indices,
    indices))
  dim.zncc.matrix <- dim(zncc.matrix)
  
  y.min <- abs(min(indices)) + max(n) + 1  # buffer to top of image
  x.min <- abs(min(indices)) + max(n) + 1  # buffer to left side of image
  y.max <- min(dim(master)[1], dim(slave)[1]) - abs(max(indices)) - max(n) - 1  # buffer to bottom of image
  x.max <- min(dim(master)[2], dim(slave)[2]) - abs(max(indices)) - max(n) - 1  # buffer to right side of image
  
  if (search.area.shift[2] < 0) {
    # shift in y direction
    y.min <- y.min - search.area.shift[2]
  } else {
    y.max <- y.max - search.area.shift[2]
  }
  if (search.area.shift[1] < 0) {
    # shift in x direction
    x.min <- x.min - search.area.shift[1]
  } else {
    x.max <- x.max - search.area.shift[1]
  }
  
  cat("Search windows are located starting at xmin", x.min, "to xmax", x.max,
    "\n")
  cat("Search windows are located starting at ymin", y.min, "to ymax", y.max,
    "\n")
  
  # convert rasters to matrices
  master <- matrix(master[], nrow = master@nrows, ncol = master@ncols, byrow = T)
  slave <- matrix(slave[], nrow = slave@nrows, ncol = slave@ncols, byrow = T)
  
  if (run.parallel) {
    cat("Started calculation of local disparity maps.\n")
    # registerDoMC(cores)
    split.points.y <- floor(seq(y.min, (y.max + 1), length.out = (cores + 1)))
    cat("Splitting points in vertical image direction are at (row numbers)",
      split.points.y, "\n")
    if (log)
      sink(log.file, append = F)
    
    disp.maps.local <- foreach(i = 1:cores, .combine = function(...) abind(..., along = 1)) %dopar% {
      y.min.local <- split.points.y[i]
      y.max.local <- split.points.y[i+1]-1
      
      if (log)
        sink(log.file, append = T)
      cat("Started calculation of local disparity map within ", y.min.local, ":",
        y.max.local, ".\n", sep = "")
      
      disp.map.local <- array(dim = c((y.max.local - y.min.local + 1), max(dim(master)[2],
        dim(slave)[2]), 2), dimnames = list(NULL, NULL, c("dispY", "dispX")))
      
      disp.map.local[1:(y.max.local-y.min.local+1), x.min:x.max, ] <- disparityMapCpp(master, slave, indices,
        n, y.min.local, y.max.local, x.min, x.max, search.area.shift)
      
      storage.mode(disp.map.local) <- "numeric"
      return(disp.map.local)
    }
    if (log)
      sink()
    disp.map[y.min:y.max, , ] <- disp.maps.local
  } else {
    cat("Started calculation of global disparity map (using one processor core).\n")
    disp.map[y.min:y.max, x.min:x.max, ] <- disparityMapCpp(master, slave, indices,
      n, y.min, y.max, x.min, x.max, search.area.shift)
  }
  disp.map[, , 1] <- disp.map[, , 1] + search.area.shift[1]
  disp.map[, , 2] <- disp.map[, , 2] + search.area.shift[2]
  return(disp.map)
}