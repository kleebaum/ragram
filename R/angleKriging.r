anglesKriging <- function(object, spatialPoints, z='thetaIn',
                         formula=as.formula(paste(z, '~ lon + lat')),
                         variogram.fit=T, plot.fit=F) {
    variogram <- variogram(formula, object)
    
    if(variogram.fit) {
        psill <- diff(range(variogram$gamma))
        range.var <- diff(range(variogram$dist))
        variogram.model <- vgm(psill, "Gau", range.var, 0)

        variogram.fit <- fit.variogram(variogram, variogram.model)
        if(plot.fit) {
            print(plot(variogram, model=variogram.fit, ...))
        }
        angleKriging <- krige(formula, object, spatialPoints, variogram.fit, maxdist=1000)
        if(is.na(angleKriging$var1.pred[1])) {
            cat('Fitting variogram was not successful, try without...\n')
            cat('variogram.fit is set to FALSE\n')
            angleKriging <- krige(formula, slave@geolocationPoints,
                                 object@geolocationPoints)
        }
    } else {
        angleKriging <- krige(formula, object, spatialPoints)
    }
    colnames(angleKriging@data) <- c(paste0(z, '.pred'), paste0(z, '.var'))
    return(angleKriging)
}