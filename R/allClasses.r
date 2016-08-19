setClass('SARGeneric',
         representation = representation(
             satellite = 'character',
             cornerLon = 'numeric',
             cornerLat = 'numeric',
             centerLat = 'numeric',
             centerLon = 'numeric'),
         contains='VIRTUAL')

#' Geolocation points class
#' 
#' A class to handle geolocation control points (GCPs) and tie points (TPs).
#' This class inherits from the \code{\link{SpatialPointsDataFrame}} class of the sp package.
#'
#' @export
#' @seealso \code{\link{SpatialPointsDataFrame}}, \code{\link{SAR-class}}
setClass('GeolocationPoints',
         contains = 'SpatialPointsDataFrame')

#' Synthetic aperture radar (SAR) class
#' 
#' A class to handle synthetic aperture radar (SAR) data.
#' This class inherits from the \code{\link{RasterLayer}} class of the raster package.
#'
#' @slot address address of the SAR data main folder.
#' @slot polarization polarization of the SAR record (e.g. VV or HH).
#' @slot geolocationPoints \code{\link{GeolocationPoints-class}} object that contains GCPs/TPs.
#' @slot node direction of the satellite (ascending or descending).
#' @slot orbit orbit number (e.g. sentinel has 175 in total).
#' @slot imgSubstring character. This substring is searched for when image data is read. 
#' @export
#' @examples 
#' # orbit 130, dual polarization vv and vh
#' kili_2014_12_25_155516_vv <- new('Sentinel', warped=F, polarization='vv',
#'                              address='sentinel1/kili/S1A_IW_GRDH_1SDV_20141225T155516_20141225T155541_003877_004A5A_2263.SAFE/')
#'
#' # alternatively
#' sarRecord(address='sentinel1/kili/S1A_IW_GRDH_1SDV_20141225T155516_20141225T155541_003877_004A5A_2263.SAFE/',
#'           satellite='sentinel-1', warped=F, polarization='vv')
#' 
#' kili_2014_12_25_155516_vv@orbit
#' kili_2014_12_25_155516_vv@polarization
#' kili_2014_12_25_155516_vv@node
#' @seealso \code{\link{GeolocationPoints-class}}, \code{\link{SARSet-class}}
setClass('SAR',
         representation = representation(
             address = 'character',
             polarization = 'character', # either vh, vv, hh, hv, etc.
             geolocationPoints = 'GeolocationPoints',
             orbit = 'numeric', # orbit number, e.g. sentinel has 175
             node = 'character', # either ascending or descending
             imgSubstring = 'character', # search for a substring when data is read
             border = 'SpatialPolygons'
         ),
         prototype = prototype(
             satellite = 'generic satellite'),
         contains = c('RasterLayer', 'SARGeneric'))

#' Sentinel class
#' 
#' A class to handle Sentinel-1 SAR data. 
#' This class inherits from the \code{\link{SAR-class}}.
#'
#' @slot address address of the SAR data main folder.
#' @slot polarization polarization of the SAR record (e.g. VV or HH).
#' @slot geolocationPoints \code{\link{GeolocationPoints-class}} object that contains GCPs/TPs.
#' @slot node direction of the satellite (ascending or descending).
#' @slot orbit orbit number (e.g. sentinel has 175 in total).
#' @slot imgSubstring character. This substring is searched for when image data is read. 
#' @export
#' @examples 
#' # orbit 130, dual polarization vv and vh
#' kili_2014_12_25_155516_vv <- new('Sentinel', warped=F, polarization='vv',
#'                              address='sentinel1/kili/S1A_IW_GRDH_1SDV_20141225T155516_20141225T155541_003877_004A5A_2263.SAFE/')
#'
#' # alternatively
#' sarRecord(address='sentinel1/kili/S1A_IW_GRDH_1SDV_20141225T155516_20141225T155541_003877_004A5A_2263.SAFE/',
#'           satellite='sentinel-1', warped=F, polarization='vv')
#' 
#' kili_2014_12_25_155516_vv@orbit
#' kili_2014_12_25_155516_vv@polarization
#' kili_2014_12_25_155516_vv@node
#' @seealso \code{\link{SAR-class}}, \code{\link{GeolocationPoints-class}}, \code{\link{SARSet-class}}
setClass('Sentinel',
         prototype = prototype(
             satellite = 'sentinel-1'),
         contains = 'SAR')

setClass('Envisat',
         prototype = prototype(
             satellite = 'envisat'),
         contains = 'SAR')

#' TerraSAR-X class
#' 
#' A class to handle TerraSAR-X data. 
#' This class inherits from the \code{\link{SAR-class}}.
#'
#' @slot address address of the SAR data main folder.
#' @slot polarization polarization of the SAR record (e.g. VV or HH).
#' @slot geolocationPoints \code{\link{GeolocationPoints-class}} object that contains GCPs/TPs.
#' @slot node direction of the satellite (ascending or descending).
#' @slot orbit orbit number (e.g. sentinel has 175 in total).
#' @slot imgSubstring character. This substring is searched for when image data is read. 
#' @export
#' @examples 
#' tsx_kili_2016_07_03_154637_hh <- new('TSX', warped=F, 
#'                                  address='terrasarx/kili/TSX1_SAR__MGD_SE___HS_S_SRA_20160703T154637_20160703T154637')
#'
#' # alternatively
#' sarRecord(address='terrasarx/kili/TSX1_SAR__MGD_SE___HS_S_SRA_20160703T154637_20160703T154637',
#'           satellite='terrasar-x', warped=F)
#' 
#' @seealso \code{\link{SAR-class}}, \code{\link{GeolocationPoints-class}}, \code{\link{SARSet-class}}
setClass('TSX',
         prototype = prototype(
             satellite = 'terrasar-x'),
         contains = 'SAR')

#' SAR set class
#'
#' A class to handle a collection of synthetic aperture radar (SAR) records.
#' Objects of this class are lists.
#'
#' @slot crs \code{\link{CRS}} object
#' @export
#' @seealso \code{\link{SAR-class}}
#' @examples 
#' kiliSetAsc <- new('SARSet',
#'               c(kili_2016_06_05_155520_vv_warped,  # 130
#'               kili_2016_05_31_154724_vv_warped,    #  57
#'               kili_2015_04_19_154656_vv_warped))   #  57 
#' alternatively
#' sarSet(c(kili_2016_06_05_155520_vv_warped,         # 130
#'               kili_2016_05_31_154724_vv_warped,    #  57
#'               kili_2015_04_19_154656_vv_warped))   #  57
setClass('SARSet',
         representation = representation(
             crs = 'CRS',
             extent = 'Extent'),
         prototype = prototype(
             crs = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')),
         contains = c('list', 'SARGeneric'))