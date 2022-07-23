#' dpath
#' 
#' Folder for the `sf` files
#'
#' @export
#' 
dpath <- file.path(Rfuns::datait_path, 'ispra')

#' #' cmn.lst
#' #' 
#' #' Complete List of Italian Municipalities (`CMN`), with Zip Codes and Provinces Roads Codes.
#' #'
#' #' @export
#' #' 
#' cmn.lst <- create_cmn_lst()

#' palette.lst
#' 
#' List of default colours to associate with the various Levels of Hazards and Risks
#'
#' @export
#' 
palette.lst <- list(
    'geo'  = list('P4' = '#6E0000', 'P3' = '#DC0000', 'P2' = '#DE6700', 'P1' = '#F1CA2E', 'AA' = '#FEFF68'),
    'idro' = list('H' = '#0C2775', 'M' = '#2D72FF', 'L' = '#C3E8FF')
)

#' risks.lst
#' 
#' Description of the various Levels of Hazards and Risks
#'
#' @export
#' 
risks.lst <- list(
    'geo'  = list('P4' = 'Very High', 'P3' = 'High', 'P2' = 'Medium', 'P1' = 'Moderate', 'AA' = 'Attention'),
    'idro' = list('H' = 'High', 'M' = 'Medium', 'L' = 'Low')
)


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

