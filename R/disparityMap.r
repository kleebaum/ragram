#' Calculated disparity map between two SAR records
#' 
#' Calculates a disparity map between two SAR records using window based 
#' zero normalized cross correlation (ZNCC).
#' 
#' @param master Object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param slave Object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param window.size Integer or vector of integers. Edge Length of quadratic window(s) to calculate \code{\link{zncc}}.
#' Correlation coefficients are multiplied if a vector is provided.
#' @param search.area.size Integer. Edge Length of quadratic search area in slave image.
#' @param search.area.shift Numeric vector. Pixels in x and y direction that the search area should be shifted.
#' That is how a priori knowledge about disparity can be regarded.
#' @param resample.slave Logical. Should the slave image be resampled to the aggregated master image?
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
disparityMap <- function(master, slave, 
                         window.size=3, search.area.size=7,
                         search.area.shift=c(0,0),
                         resample.slave=T,
                         window.moving.step=1,
                         run.parallel=F, cores=4, 
                         log=F, log.file='dispMapLog.txt', ...) {
    for(i in 1:length(window.size)) {
        if(window.size[i] %% 2 != 1) {
            window.size[i] <- window.size[i] + 1
            cat('Window size needs to be uneven. Window size nr., i,  was set on', window.size[i], '\n')
        }
    }
    if(search.area.size %% 2 != 1) {
        search.area.size <- search.area.size + 1
        cat('Search window size needs to be uneven, it was set on', search.area.size, '\n')
    }
    
    if(resample.slave) {
        slave <- resample(slave, master)
    }
    
    index <- as.array(round(seq((-1*(search.area.size-max(window.size))/2),
                                ((search.area.size-max(window.size))/2), 
                                by=window.moving.step)))
    
    if(length(window.size)>1) {
        cat('More than one window size is given. Correlation coefficients will be multiplied.\n')
    }
    
    n <- c((window.size-1)/2) # n in formula of zncc
    
    cat('ZNCC is calculated for a (maximal) window size of', max(window.size), 'pixels.\n')
    cat('Search area size is', search.area.size, 'pixels.\n')
    cat('Windows are centered for indices', index, 'in each search window (center is zero).\n')
    
    disp.map <- array(dim=c(max(dim(master)[1],dim(slave)[1]), max(dim(master)[2],dim(slave)[2]), 2),
                      dimnames = list(NULL, NULL, c('dispX', 'dispY')))
    
    zncc.matrix <- matrix(nrow = dim(index), ncol = dim(index), dimnames = list(index, index))
    dim.zncc.matrix <- dim(zncc.matrix)
    
    u.buffer.border <- abs(min(index))+max(n)+1 # u resembles y direction
    v.buffer.border <- abs(min(index))+max(n)+1 # v resembles x direction
    u.max <- min(dim(master)[1],dim(slave)[1])-abs(max(index))-max(n)-1
    v.max <- min(dim(master)[2],dim(slave)[2])-abs(max(index))-max(n)-1
    
    if(search.area.shift[2]< 0) { # shift in y direction
        u.buffer.border <- u.buffer.border - search.area.shift[2] 
    } else {
        u.max <- u.max - search.area.shift[2]
    }
    if(search.area.shift[1]< 0) { # shift in x direction
        v.buffer.border <- v.buffer.border - search.area.shift[1]
    } else {
        v.max <- v.max - search.area.shift[1]
    }
    
    master <- matrix(master[], nrow = master@nrows, ncol=master@ncols, byrow = T)
    slave <- matrix(slave[], nrow = slave@nrows, ncol=slave@ncols, byrow = T)
    
    if(run.parallel) {
        cat('Started calculation of local disparity maps.\n')
        #registerDoMC(cores)
        split.points.u <- floor(seq(u.buffer.border, (u.max+1), length.out=(cores+1)))
        cat('Splitting points in vertical image direction are at (row numbers)', split.points.u)
        if(log) sink(log.file, append=F)
        
        disp.maps.local <- foreach(i=1:cores, .combine=function(...) abind(..., along=1))  %dopar% {
            start.u <- split.points.u[i]
            end.u <- split.points.u[i+1]-1
            
            if(log) sink(log.file, append=T)
            cat('Started calculation of local disparity map within ', start.u, ':', end.u, '.\n', sep='')
            
            disp.map.local <- array(dim=c((end.u-start.u+1), max(dim(master)[2],dim(slave)[2]), 2),
                                    dimnames = list(NULL, NULL, c('dispX', 'dispY')))
            
            for (u in start.u:end.u) {
                for (v in v.buffer.border:v.max) {
                    zncc.matrix[,] <- znccMatrixCpp(master, slave, index,
                                                    u, v, n, search.area.shift)
                    
                    array.index <- arrayInd(which(zncc.matrix == max(zncc.matrix, na.rm=T)), dim.zncc.matrix)
                    disp.map.local[u-start.u+1,v,] <- rev(rownames(zncc.matrix)[array.index[,]])
                }
            }
            storage.mode(disp.map.local) <- 'numeric'
            return(disp.map.local)
        }
        if(log) sink()
        disp.map[u.buffer.border:u.max, ,] <- disp.maps.local
    } else {
        cat('Started calculation of global disparity map.\n')
        # disp.map[u.buffer.border:u.max, v.buffer.border:v.max, ] <- 
        #     disparityMapCpp(master, slave, index, n, u.buffer.border, u.max,
        #                 v.buffer.border,v.max,  search.area.shift)
        pb <- txtProgressBar(min=u.buffer.border, max=u.max, style=3)
        for (u in u.buffer.border:u.max) {
            setTxtProgressBar(pb, u)
            for (v in v.buffer.border:v.max) {
                zncc.matrix[,] <- znccMatrixCpp(master, slave, index,
                                                u, v, n, search.area.shift)
                
                array.index <- arrayInd(which(zncc.matrix == max(zncc.matrix, na.rm=T)),
                                        dim.zncc.matrix)
                disp.map[u,v,] <- rev(rownames(zncc.matrix)[array.index[,]])
            }
        }
        close(pb)
    }
    storage.mode(disp.map) <- 'numeric'
    disp.map[,,1] <- disp.map[,,1] + search.area.shift[1]
    disp.map[,,2] <- disp.map[,,2] + search.area.shift[2]
    return(disp.map)
}