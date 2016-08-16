#' Calculated disparity map between two SAR records
#' 
#' Calculates a disparity map between two SAR records using window based 
#' zero normalized cross correlation (ZNCC).
#' 
#' @param master object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param slave object of the \code{\link{SAR-class}} or a subclass (e.g. \code{\link{Sentinel-class}} or \code{\link{TSX-class}}).
#' @param window.size window size to calculate \code{\link{zncc}}.
#' @param search.area.size size of search area in slave image.
#' @param search.area.shift numeric vector. Pixels in x and y direction that the search window should be shifted.
#' That's a way you can put in a priori knowledge about disparity.
#' @param resample.slave logical. Should the slave image be resampled to the aggregated master image?
#' @param use.expected.disparity logical. Should the disparity should be estimated first?
#' @param index moving window indices.
#' @param run.parallel logical. Run algorithm on more than one cores?
#' @param cores integer. How many cores should be allocated?
#' @param log logical. Log output to text file?
#' @param log.file character. Log file name.
#' @export
#' @seealso \code{\link{zncc}}, \code{\link{plotDisparityMap}}
#' @examples
#' data(kili)
#' plotDisparityMap(master, slave)
disparityMap <- function(master, slave, window.size=3, search.area.size=7,
                         search.area.shift=c(0,0),
                         resample.slave=T,
                         window.moving.step=1,
                         index=as.array(round(seq((-1*(search.area.size-max(window.size))/2),
                                                  ((search.area.size-max(window.size))/2), 
                                                  by=window.moving.step))), 
                         run.parallel=F, cores=4, log=F, log.file='dispMapLog.txt', ...) {
    # if(window.size %% 2 != 1) {
    #     window.size <- window.size + 1
    #     cat('Window size needs to be unequal, it was set on', window.size, '\n')
    # }
    if(search.area.size %% 2 != 1) {
        search.area.size <- search.area.size + 1
        cat('Search window size needs to be unequal, it was set on', search.area.size, '\n')
    }
    
    if(resample.slave) {
        slave <- resample(slave, master)
    }
    
    n <- c((window.size-1)/2) # n in formula of zncc
    
    cat('ZNCC is calculated for a max window size of ', max(window.size), ' (pixels).\n', sep='')
    cat('Search area size is ', search.area.size, '.\n', sep='')
    cat('Windows are centered for indices', index, 'in each search window (center is zero).\n')
    
    disp.map <- array(dim=c(max(dim(master)[1],dim(slave)[1]), max(dim(master)[2],dim(slave)[2]), 2),
                      dimnames = list(NULL, NULL, c('disp_lat', 'disp_lon')))
    
    disp.temp <- matrix(nrow = dim(index), ncol = dim(index), dimnames = list(index, index))
    dim.disp.temp <- dim(disp.temp)
    
    u.buffer.border <- abs(min(index))+max(n)+1
    v.buffer.border <- abs(min(index))+max(n)+1
    u.max <- min(dim(master)[1],dim(slave)[1])-abs(max(index))-max(n)-1
    v.max <- min(dim(master)[2],dim(slave)[2])-abs(max(index))-max(n)-1
    
    if(search.area.shift[1]< 0) {
        u.buffer.border <- u.buffer.border - search.area.shift[1]
    } else {
        u.max <- u.max - search.area.shift[1]
    }
    if(search.area.shift[2]< 0) {
        v.buffer.border <- v.buffer.border - search.area.shift[2]
    } else {
        v.max <- v.max - search.area.shift[2]
    }
    
    master <- matrix(master[], nrow = master@nrows, 
                     ncol=master@ncols, byrow = T)

    slave <- matrix(slave[], nrow = slave@nrows, 
                     ncol=slave@ncols, byrow = T)
    
    if(run.parallel) {
        cat('Started calculation of local disparity maps.\n')
        registerDoMC(cores)
        split.points.u <- floor(seq(u.buffer.border, (u.max+1), length.out=(cores+1)))
        cat('Splitting points in horizontal image direction are at', split.points.u)
        if(log) sink(log.file, append=F)
        
        disp.maps.local <- foreach(i=1:cores, .combine=function(...) abind(..., along=1))  %dopar% {
            start.u <- split.points.u[i]
            end.u <- split.points.u[i+1]-1
            
            if(log) sink(log.file, append=T)
            cat('Started calculation of local disparity map within ', start.u, ':', end.u, '.\n', sep='')
            
            disp.map.local <- array(dim=c((end.u-start.u+1), max(dim(master)[2],dim(slave)[2]), 2),
                                    dimnames = list(NULL, NULL, c('disp_lat', 'disp_lon')))
            
            for (u in start.u:end.u) {
                for (v in v.buffer.border:v.max) {
                    disp.temp[,] <- .getPyramidalZnccsInSearchArea(master, slave, index,
                                                            u, v, n, search.area.shift)

                    array.index <- arrayInd(which(disp.temp == max(disp.temp, na.rm=T)), dim.disp.temp)
                    disp.map.local[u-start.u+1,v,] <- rownames(disp.temp)[array.index[,]]
                }
            }
            storage.mode(disp.map.local) <- 'numeric'
            return(disp.map.local)
        }
        if(log) sink()
        disp.map[u.buffer.border:u.max, ,] <- disp.maps.local
    } else {
        cat('Started calculation of global disparity map.\n')
        pb <- txtProgressBar(min=u.buffer.border, max=u.max, style=3)
        for (u in u.buffer.border:u.max) {
            setTxtProgressBar(pb, u)
            for (v in v.buffer.border:v.max) {
                disp.temp[,] <- .getPyramidalZnccsInSearchArea(master, slave, index,
                                                        u, v, n, search.area.shift)
                
                array.index <- arrayInd(which(disp.temp == max(disp.temp, na.rm=T)),
                                        dim.disp.temp)
                return(array.index)
                disp.map[u,v,] <- rownames(disp.temp)[array.index[,]]
            }
        }
        close(pb)
    }
    storage.mode(disp.map) <- 'numeric'
    return(disp.map)
}