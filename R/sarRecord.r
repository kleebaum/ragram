#' Create a synthetic aperture radar (SAR) object
#' 
#' Creates a synthetic aperture radar (SAR) object. 
#' The input is either an existing raster or the folder address of satellite data.
#' 
#' @param object either a character given the folder address of satellite data or a RasterLayer object.
#' @param satellite character. Either 'sentinel-1' or 'terrasar-x'.
#' @param imgSubstring character. This substring is searched for when image data is read. 
#' @param polarization. character. E.g.'vv' or 'hh'. 
#' If the polarization is not given it will be determined from metadata.
#' @return object of SAR class or subclass that is a well a RasterLayer object.
#' @export
#' @examples 
#' master <- sarRecord('extdata/S1A_IW_GRDH_1SDV_20151220T155517.SAFE/',
#'                     satellite = 'sentinel') 
#' slave <- sarRecord('extdata/S1A_IW_GRDH_1SDV_20151215T154711.SAFE/',
#'                     satellite = 'sentinel')
setGeneric('sarRecord', function(object, satellite='', imgSubstring='',
                                 polarization=NULL, ...) {
    standardGeneric('sarRecord')
})

#' @rdname sarRecord
#' @export
setMethod('sarRecord', 'RasterLayer', function(object, ...) {
    sarRecordDecision(address='', raster=object, satellite=satellite, 
                      imgSubstring=imgSubstring, polarization=polarization, ...)
})

#' @rdname sarRecord
#' @export
setMethod('sarRecord', 'character', function(object, ...) {
        sarRecordDecision(address=object, raster=NULL, satellite=satellite, 
                          imgSubstring=imgSubstring, polarization=polarization, ...)
})

#' @rdname sarRecord
#' @export
setMethod('sarRecord', 'missing', function(...) {
    sarRecordDecision(address='', raster=NULL, satellite=satellite, 
                      imgSubstring=imgSubstring, polarization=polarization, ...)
})

sarRecordDecision <- function(address='', raster=NULL, satellite='', imgSubstring='',
                      polarization=NULL, ...) {
    switch (satellite,
            'sentinel' = new('Sentinel', address=address, raster=raster, imgSubstring=imgSubstring,
                             polarization=polarization, ...),
            'sentinel-1' = new('Sentinel', address=address, raster=raster, imgSubstring=imgSubstring,
                               polarization=polarization, ...),
            # 'envisat' = new('Envisat', address=address, raster=raster, imgSubstring=imgSubstring,
            #                 polarization=polarization, ...),
            'terrasar-x' = new('TSX', address=address, raster=raster, imgSubstring=imgSubstring,
                               polarization=polarization, ...),
            'tsx' = new('TSX', address=address, raster=raster, imgSubstring=imgSubstring,
                               polarization=polarization, ...),
            new('SAR', address=address, raster=raster, imgSubstring=imgSubstring,
                polarization=polarization, ...)
    )
}