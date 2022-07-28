#' map_risks_cmn
#'
#' Build a leaflet map for either the flood or landslide risk for one or more District, 
#' including "neighbours".
#'
#' @param x         The ISTAT code(s) of the required District(s)
#' @param risk      The Risk to draw on the map: `h`ydrological (floods) or `g`eological (landslides)
#' @param neigh     IF `TRUE`, also includes all neighbours of the specified District(s)
#' @param cmn       If `TRUE`, also draws the polygons of the District(s)
#' @param cmnv      If `TRUE` (and also `cmn`is `TRUE`), adds the polygons of the neighbours of the District(s) 
#' @param mps       If `TRUE`, returns a complete map; 
#'                  otherwise only the required polygons and the layers' menu 
#' @param maptile   If `NULL` keep the default maptile `CartoDB.Positron`, 
#'                  otherwise a 
#' @param lvl_nms   If `NULL` keep the default names for the levels of the specified risk, 
#'                  otherwise a character vector of the required length with the desired names
#' @param lvl_cls   If `NULL` keep the default colour palette for the levels of the specified risk, 
#'                  otherwise a character vector of the required length with the RGB values of the colours to be used
#' @param poly_tsp  A value between 0 and 1 for the transparency of the polygons
#' @param cpoly_col 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param add_tbl   Add a table as label with information about people 
#' @param verbose 
#' 
#' @return un oggetto `leaflet`
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import leaflet
#' @importFrom qs qread
#'
#' @export
#'
mappa_poli_comune <- function(x, risk,
                              neigh = FALSE, 
                              cmn = TRUE, 
                              cmnv = FALSE, 
                              mps = TRUE,
                              maptile = NULL,
                              lvl_nms = NULL,
                              lvl_cls = NULL,
                              poly_tsp = 0.2,
                              
                              
                              
                              
                              
                              add_tbl = TRUE,
                              verbose = FALSE
                              
    ){
    
        # LETTURA FILE POLIGONI+TABELLA
        y <- qread(file.path(dpath, risk, paste0(x, ifelse(neigh, 'v', ''))), nthreads = 6)
        
        if(is.na(y$bbx)){
            if(verbose) stop('The specified District has no hazard areas for the required risk.')
            return(NULL)
        }
        
        grps <- NULL
        yr <- risks.lst[[risk]]
        for(yrn in names(yr)){
            if(!is.null(y[[yrn]])){
                yrc <- palette.lst[[input$rdb_rsk]][[yrn]]
                grp <- risks.lst[[input$rdb_rsk]][[yrn]]
                grps <- c(grps, grp)
                y <- y |> 
                        addPolygons(
                            data = y[[yrn]], 
                            group = grp, 
                            stroke = FALSE, 
                            fillColor = yrc, 
                            fillOpacity = 1 - as.numeric(poly_tsp) / 10
                        )
            } 
        }
        
        # POLIGONO COMUNE
        if(cmn){
            y <- y |> 
                addPolygons(
                    data = y$CMN,
                    weight = 3,
                    color = 'red',
                    opacity = 1,
                    fillOpacity = 0,
                    label = if(add_tbl) y$dts else NULL,
                    highlightOptions = hlt.options
                )
        }
        
        # POLIGONI COMUNI LIMITROFI
        if(cmnv){
            y <- y |> 
            addPolygons(
                data = y$CMNv,
                weight = 2,
                color = 'black',
                opacity = 1,
                fillOpacity = 0,
                label = if(add_tbl) y$dtsv else NULL,
                highlightOptions = hlt.options
            )
        }
        
        # TITOLO
        if(ttl){
            # y <- y |> addControl()
        }    
        
        # MENU
        y |> 
            addLayersControl(
                overlayGroups = grps, 
                options = layersControlOptions(collapsed = FALSE)
            )

}
