#' dpath
#' 
#' Folder for the `sf` files
#'
#' @export
#' 
dpath <- file.path(Rfuns::datait_path, 'ispra')

#' cmn.lst
#'
#' Complete List of Italian Municipalities (`CMN`), with Zip Codes and Provinces Roads Codes.
#'
#' @import data.table
#'
#' @export
#'
cmn.lst <- {
    load('./data/geoCMN.rda')
    yc <- geoCMN[order(CMNd)]
    y <- yc$CMN
    names(y) <- paste0(yc$CMNd, ' (', yc$cap, ', ', yc$PRVs, ')')
    y
}

#' palette.lst
#' 
#' List of default colours to associate with the various Levels of Hazards and Risks
#'
#' @export
#' 
palette.lst <- list(
    'geo'  = list('P4' = '#6E0000', 'P3' = '#DC0000', 'P2' = '#DE6700', 'P1' = '#F1CA2E', 'AA' = '#FEFF68'),
    'hydro' = list('H' = '#0C2775', 'M' = '#2D72FF', 'L' = '#C3E8FF')
)

#' risks.lst
#' 
#' Description of the various Levels of Hazards and Risks
#'
#' @export
#' 
risks.lst <- list(
    'geo'  = list('P4' = 'Very High', 'P3' = 'High', 'P2' = 'Medium', 'P1' = 'Moderate', 'AA' = 'Attention'),
    'hydro' = list('H' = 'High', 'M' = 'Medium', 'L' = 'Low')
)

#' empty_poly
#' 
#' An empty polygon with CRS 3035 (`x = TRUE`, default) or 4326 (`x = FALSE`)
#'
#' @param x if `TRUE` the CRS of the polygon is the projected 3035 (ETRS89), otherwise the geodetic 4326 (WGS84) 
#'
#' @importFrom sf st_sf st_sfc st_polygon
#'
#' @export
#' 
empty_poly <- \(x = TRUE) st_sf(geometry = st_sfc(st_polygon()), crs = if(x) 3035 else 4326)

#' extract_poly
#' 
#' Extract polygons from a geometry collection, or leave it as it is if not.
#'
#' @param x an `sf` object
#'
#' @importFrom sf st_is st_collection_extract
#'
#' @export
#' 
extract_poly <- \(x) if(st_is(x, 'GEOMETRYCOLLECTION')) x <- st_collection_extract(x, 'POLYGON') else x

# .onAttach <- function(libname, pkgname) {
#     packageStartupMessage(
#     )
# }

# .onLoad <- function(libname, pkgname) {
#     oop <- options()
#     opts <- list(
#             pkgname.optname = " option value goes here "
#     )
#     toset <- !(names(opts) %in% names(oop))
#     if(any(toset)) options(opts[toset])
#   
#     invisible()
# }

# .onUnload <- function(libname, pkgname) {
#
# }

