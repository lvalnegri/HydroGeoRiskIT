#' build_table
#' 
#' Crete a tabnle to be used as label in leaflet map
#'
#' @param x the ISTAT code for the District
#' @param r the type of hazard: `hydro` or `geo`
#'
#' @return an htmlTable
#' 
#' @importFrom htmltools HTML
#' @importFrom htmlTable htmlTable
#' @importFrom Rfuns add_Kcomma
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#' 
#' @export
#'
build_table <- \(x, r){
    HTML(htmlTable(
        mdts[, .(var_id, description)][
            dts[CMN == x & var_id %in% mdts[is.na(risk), var_id], .(var_id, value = add_Kcomma(as.integer(value)))], on = 'var_id'
                ][,var_id := NULL],
        header = c(paste('Comune di', geoCMN[CMN == x, CMNd]), ''), 
        align.header = 'l',
        rnames = FALSE,
        align = 'lr'
    ))
}
