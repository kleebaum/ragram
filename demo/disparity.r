data("tsukuba")

plot(tsukuba.left)
plot(tsukuba.right)

dispMap <- disparityMap(tsukuba.left, tsukuba.right, window.size = 11, search.area.size=31,
  resample.slave = T, run.parallel = T)

dispMapHorizontal <- raster(dispMap[,,1])
extent(dispMapHorizontal) <- extent(tsukuba.left)
plot(dispMapHorizontal)

dispMapVertical <- raster(dispMap[,,2])
extent(dispMapVertical) <- extent(tsukuba.left)
plot(dispMapVertical)

dispMapDiagonal <- dispMapLon
values(dispMapDiagonal) <- sqrt(dispMapLon[]^2 + dispMapLat[]^2)
plot(dispMapDiagonal)