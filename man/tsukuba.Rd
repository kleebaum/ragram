\name{tsukuba}
\docType{data}
\alias{tsukuba}
\title{Tsukuba stereo image pair}
\description{The Tsukuba stereo image pair \href{http://vision.middlebury.edu/stereo/data/scenes2001}{(University of Tsukuba)} is a common example in computer vision. In the ragram R package, we use this stereo image pair to demonstrate the calculation of the disparity map.
}
\examples{
    # data(tsukuba)
    # plot(tsukuba$layer.1)
    # plot(tsukuba$layer.2)
    
    # dispMap <- disparityMap(tsukuba$layer.1, tsukuba$layer.2, window.size = 11, search.area.size=31)

    # dispMapHorizontal <- raster(dispMap[,,1])
    # extent(dispMapHorizontal) <- extent(tsukuba$layer.1)
    # plot(dispMapHorizontal)

    # dispMapVertical <- raster(dispMap[,,2])
    # extent(dispMapVertical) <- extent(tsukuba$layer.1)
    # plot(dispMapVertical)

    # dispMapDiagonal <- dispMapLon
    # values(dispMapDiagonal) <- sqrt(dispMapHorizontal[]^2 + dispMapVertical[]^2)
    # plot(dispMapDiagonal)
}
\format{The tsukuba dataset includes a stack object with two layers (raster objects).}
\keyword{datasets}