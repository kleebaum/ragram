# Ragram Radargrammetry package
# Tutorial

# load Sentinel-1 data of Kilimanjaro region
data(kili)

# master <- sarRecord('extdata/S1A_IW_GRDH_1SDV_20151220T155517.SAFE/',
#                     satellite = 'sentinel') 
# slave <- sarRecord('extdata/S1A_IW_GRDH_1SDV_20151215T154711.SAFE/',
#                     satellite = 'sentinel')

# plot (google) maps
plotMap(kili, orbit = F)
show(kili[[1]])
plotGmap(kili[[1]])

# plot incidence angles
plotAngles(kili[[1]])
plotAngles(kili[[5]])

plotAnglesDif(kili[[1]], kili[[2]])
plotOrbitNumber(kili[[1]])
plotOrbitNumber(kili[[2]])

plotAnglesDif(kili[[1]], kili[[5]])
plotOrbitNumber(kili[[1]], x=35.5)
plotOrbitNumber(kili[[5]], x=37.8)

plotAnglesDif(kili[[1]], kili[[5]], disparity = T,
              legend.lab = 'Disparity [m]', h=100)

# inspect cropped Sentinel-1 same side stereo pair
plotGmap(master, xlim=c(37,37.7), ylim=c(-3.6,-2.8), sar=F)
plot(spTransform(border(master), CRS('+init=epsg:4326')), 
     add=T, col='white')
plotSAR(master, maxpixels = 500000)
plotSAR(slave, maxpixels = 500000)

# plot incidence angles and expected disparity
plotAngles(master, interpolate = T, aggregate.fact = 10)
plotAngles(slave, interpolate = T, aggregate.fact = 10)
plotAnglesDif(master, slave, interpolate = T, aggregate.fact = 10)

# disparity map using zero normalied cross correlation (ZNCC)
master <- aggregate(master, 2)
dispMap <- disparityMap(master, slave, window.size = 25,
             search.area.size = 51)

dispMapLon <- raster(dispMap[,,1])
extent(dispMapLon) <- extent(master)
plot(dispMapLon)

dispMapLat <- raster(dispMap[,,2])
extent(dispMapLat) <- extent(master)
plot(dispMapLat)

dispMapDiagonal <- dispMapLon
values(dispMapDiagonal) <- sqrt(dispMapLon[]^2 + dispMapLat[]^2)
plot(dispMapDiagonal)
