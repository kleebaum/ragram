data("tsukuba")

plot(tsukuba$layer.1)
plot(tsukuba$layer.2)

cat('The disparity map is calculated. This can take some time.')
dispMap <- disparityMap(tsukuba$layer.1, tsukuba$layer.2, window.size = 11, search.area.size=31)

dispMapHorizontal <- raster(dispMap[,,1])
extent(dispMapHorizontal) <- extent(tsukuba$layer.1)
plot(dispMapHorizontal)

dispMapVertical <- raster(dispMap[,,2])
extent(dispMapVertical) <- extent(tsukuba$layer.1)
plot(dispMapVertical)

dispMapDiagonal <- dispMapHorizontal
values(dispMapDiagonal) <- sqrt(dispMapHorizontal[]^2 + dispMapVertical[]^2)
plot(dispMapDiagonal)