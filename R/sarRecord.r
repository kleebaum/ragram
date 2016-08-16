#' Create a synthetic aperture radar (SAR) object
#' 
#' Creates a synthetic aperture radar (SAR) object. 
#' The input is either an existing raster or the folder address of satellite data.
#' 
#' @param address character. Folder address of satellite data.
#' @param raster RasterLayer object.
#' @param satellite character. Either 'sentinel-1' or 'terrasar-x'.
#' @param warped logical. Should the SAR record be orientated using gdalwarp?
#' @param polarization. character. E.g.'vv' or 'hh'. 
#' If the polarization is not given it will be determined from metadata.
#' @return object of SAR class or subclass that is a well a RasterLayer object.
#' @export
#' @examples 
#' my.sar.record <- sarRecord(address='sentinel1/kili/S1A_IW_GRDH_1SDV_20141225T155516_20141225T155541_003877_004A5A_2263.SAFE/',
#' satellite='sentinel-1')
sarRecord <- function(address='', raster=NULL, satellite='', warped=F,
                      polarization=NULL, ...) {
    switch (satellite,
            'sentinel' = new('Sentinel', address=address, raster=raster, warped=warped,
                             polarization=polarization, ...),
            'sentinel-1' = new('Sentinel', address=address, raster=raster, warped=warped,
                               polarization=polarization, ...),
            # 'envisat' = new('Envisat', address=address, raster=raster, warped=warped,
            #                 polarization=polarization, ...),
            'terrasar-x' = new('TSX', address=address, raster=raster, warped=warped,
                               polarization=polarization, ...),
            'tsx' = new('TSX', address=address, raster=raster, warped=warped,
                               polarization=polarization, ...),
            new('SAR', address=address, raster=raster, warped=warped,
                polarization=polarization, ...)
    )
}