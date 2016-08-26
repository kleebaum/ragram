setClass('SARGeneric',
         representation = representation(
             centerLat = 'numeric',
             centerLon = 'numeric',
             cornerLat = 'numeric',
             cornerLon = 'numeric',
             satellite = 'character'),
         contains='VIRTUAL')

#' Geolocation points class
#' 
#' A class to handle geolocation control points (GCPs) and tie points (TPs).
#' This class inherits from the \code{\link[sp]{SpatialPointsDataFrame}} class of the sp package.
#'
#' @export
#' @seealso \code{\link{SpatialPointsDataFrame}}, \code{\link{SAR-class}}
setClass('GeolocationPoints',
         contains = 'SpatialPointsDataFrame')

#' Synthetic aperture radar (SAR) class
#' 
#' A class to handle synthetic aperture radar (SAR) data.
#' This class inherits from the \code{\link[raster]{RasterLayer-class}} class of the raster package.
#'
#' @slot address Character. Address of the SAR data main folder.
#' @slot polarization Character. Polarization of the SAR record (e.g. VV or HH).
#' @slot geolocationPoints \code{\link{GeolocationPoints-class}} object that contains GCPs/TPs.
#' @slot node Character. Direction of the satellite (ascending or descending).
#' @slot orbit Integer. Orbit number (e.g. sentinel has 175 in total).
#' @slot imgSubstring Character. This substring is searched for when image data is read. 
#' @examples 
#' # orbits 130 and 57
#' master <- new('Sentinel', address=system.file('extdata/S1A_IW_GRDH_1SDV_20151220T155517.SAFE', 
#'               package='ragram'))
#' slave <- new('Sentinel', address=system.file('extdata/S1A_IW_GRDH_1SDV_20151215T154711.SAFE', 
#'               package='ragram'))
#'
#' # alternatively
#' master <- sarRecord(system.file('extdata/S1A_IW_GRDH_1SDV_20151220T155517.SAFE', 
#'                     package='ragram'),
#'                     satellite = 'sentinel') 
#' slave <- sarRecord(system.file('extdata/S1A_IW_GRDH_1SDV_20151215T154711.SAFE', 
#'                    package='ragram'),
#'                    satellite = 'sentinel')
#'                     
#' master@orbit
#' master@polarization
#' master@node
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
#' @slot address Character. Address of the SAR data main folder.
#' @slot polarization Character. Polarization of the SAR record (e.g. VV or HH).
#' @slot geolocationPoints \code{\link{GeolocationPoints-class}} object that contains GCPs/TPs.
#' @slot node Character. Direction of the satellite (ascending or descending).
#' @slot orbit Integer. Orbit number (e.g. sentinel has 175 in total).
#' @slot imgSubstring Character. This substring is searched for when image data is read. 
#' @export
#' @examples 
#' # orbits 130 and 57
#' master <- new('Sentinel', address=system.file('extdata/S1A_IW_GRDH_1SDV_20151220T155517.SAFE', 
#'               package='ragram'))
#' slave <- new('Sentinel', address=system.file('extdata/S1A_IW_GRDH_1SDV_20151215T154711.SAFE', 
#'               package='ragram'))
#'
#' # alternatively
#' master <- sarRecord(system.file('extdata/S1A_IW_GRDH_1SDV_20151220T155517.SAFE', package='ragram'),
#'                     satellite = 'sentinel') 
#' slave <- sarRecord(system.file('extdata/S1A_IW_GRDH_1SDV_20151215T154711.SAFE', package='ragram'),
#'                    satellite = 'sentinel')
#'                     
#' master@orbit
#' master@polarization
#' master@node
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
#' @slot address Character. Address of the SAR data main folder.
#' @slot polarization Character. Polarization of the SAR record (e.g. VV or HH).
#' @slot geolocationPoints \code{\link{GeolocationPoints-class}} object that contains GCPs/TPs.
#' @slot node Character. Direction of the satellite (ascending or descending).
#' @slot orbit Integer. Orbit number (e.g. sentinel has 175 in total).
#' @slot imgSubstring Character. This substring is searched for when image data is read. 
#' @export
#' @examples 
#' # tsx_kili_2016_07_03_154637_hh <- new('TSX',
#' # address='terrasarx/kili/TSX1_SAR__MGD_SE___HS_S_SRA_20160703T154637_20160703T154637')
#' #
#' ## alternatively
#' # sarRecord('terrasarx/kili/TSX1_SAR__MGD_SE___HS_S_SRA_20160703T154637_20160703T154637',
#' #           satellite='terrasar-x')
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
#' @param elements List or vector of \code{\link{SAR-class}} or subclass objects.
#' @slot .Data List or vector of \code{\link{SAR-class}} or subclass objects.
#' @slot centerLat Numeric.
#' @slot centerLon Numeric.
#' @slot cornerLon Numeric vector.
#' @slot cornerLat Numeric vector.
#' @slot satellite Character.
#' @slot crs \code{\link[sp]{CRS-class}} object
#' @slot extent Object of \code{\link[raster]{Extent-class}}.
#' @export
#' @seealso \code{\link{SAR-class}}
#' @examples 
#' # orbits 130 and 57
#' master <- new('Sentinel', address=system.file('extdata/S1A_IW_GRDH_1SDV_20151220T155517.SAFE', 
#'               package='ragram'))
#' slave <- new('Sentinel', address=system.file('extdata/S1A_IW_GRDH_1SDV_20151215T154711.SAFE', 
#'               package='ragram'))
#'
#' # alternatively
#' master <- sarRecord(system.file('extdata/S1A_IW_GRDH_1SDV_20151220T155517.SAFE', package='ragram'),
#'                     satellite = 'sentinel') 
#' slave <- sarRecord(system.file('extdata/S1A_IW_GRDH_1SDV_20151215T154711.SAFE', package='ragram'),
#'                    satellite = 'sentinel')
#'                     
#' master@orbit
#' master@polarization
#' master@node
#' 
#' kiliSetAsc <- new('SARSet', c(master, slave))
#'               
#' # alternatively
#' kiliSetAsc <- sarSet(c(master, slave))
setClass('SARSet',
         representation = representation(
             centerLat = 'numeric',
             centerLon = 'numeric',
             cornerLat = 'numeric',
             cornerLon = 'numeric',
             satellite = 'character',
             crs = 'CRS',
             extent = 'Extent'),
         prototype = prototype(
             crs = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')),
         contains = c('list'))