#' @importFrom data.table data.table
NULL

#' @import sf
NULL

#' @import leaflet
NULL

#' dataset
#'
#' Tabella dati territoriali di rischio idrogeologico
#'
#' @format A `data.table` object with the following fields:
#' \describe{
#'   \item{\code{CMN}}{ Codice ISTAT del Comune }
#'   \item{\code{var_id}}{ Identificativo della Variabile (chiave esterna per la tabella metadati `mdts`) }
#'   \item{\code{value}}{ Valore assunto dalla Variabile sul Comune }
#' }
#'
'dts'

#' metadata
#'
#' Tabella di spiegazione delle variabili contenute nella tabella dati `dts`
#'
#' @format A `data.table` object with the following fields:
#' \describe{
#'   \item{\code{var_id}}{ Identificativo della Variabile }
#'   \item{\code{id_orig}}{ Identificativo della Variabile nel file ISPRA }
#'   \item{\code{description}}{ Descrizione della Variabile }
#'   \item{\code{label}}{ Etichetta della Variabile }
#'   \item{\code{source}}{ Sorgente dati }
#'   \item{\code{year}}{ Anno di misurazione }
#'   \item{\code{risk}}{ Tipologia di Rischio }
#'   \item{\code{level}}{ Descrizione del Livello di Rischio }
#'   \item{\code{clevel}}{ Codice del Livello di Rischio }
#'   \item{\code{measure}}{ Dato Territoriale di riferimento }
#'   \item{\code{metric}}{ Specifica se Conteggio o Percentuale }
#' }
#'
'mdts'

#' cmn_lid
#'
#' Tabella di mappatura fra Codici Comuni ed Aree a Rischio
#'
#' @format A `data.table` object with the following fields:
#' \describe{
#'   \item{\code{risk}}{ Tipologia di Rischio: `G`eologico (Frana) o `I`drologico (Alluvione) }
#'   \item{\code{lid}}{ Identificativo unico formato come concatenazione fra `livello` e `id` }
#'   \item{\code{level}}{ Valori assunti dal Rischio (si veda la lista `livelli.lst`) }
#'   \item{\code{id}}{ Identificativo di Livello per ogni tipologia di Rischio }
#'   \item{\code{CMN}}{ Codice ISTAT del Comune }
#' }
#'
'cmn_lid'

#' geoCMN
#'
#' Dataset containing the complete list of Municipalities (as of 2022), and corresponding Province
#'
#' @return A `data.table` object with the following fields:
#' @format 
#' \describe{
#'   \item{\code{CMN}}{ ISTAT code for the Municipality }
#'   \item{\code{CMNd}}{ Name of the Municipality }
#'   \item{\code{PRV}}{ ISTAT code for the Province }
#'   \item{\code{PRVd}}{ Name of the Province }
#'   \item{\code{PRVs}}{ Road Code of the Province }
#' }
#'
'geoCMN'

#' bndCMN
#'
#' Digital Boundaries for Italian Municipalities
#'
#' @format An `sf` object with the following fields:
#' \describe{
#'   \item{\code{CMN}}{ ISTAT code for the District }
#'   \item{\code{CMNd}}{ Name of the District }
#' }
#'
'bndCMN'

#' bndPRV
#'
#' Digital Boundaries for Italian Provinces
#'
#' @format An `sf` object with the following fields:
#' \describe{
#'   \item{\code{PRV}}{ ISTAT code for the Province }
#'   \item{\code{PRVd}}{ Name of the Province }
#'   \item{\code{PRVs}}{ Road Code of the Province }
#' }
#'
'bndPRV'

#' neighbours
#'
#' For each Italian District, the list of all its bordering Districts
#'
#' @format A `data.table` object with the following fields:
#' \describe{
#'   \item{\code{CMN}}{ ISTAT code for the District }
#'   \item{\code{neigh}}{ ISTAT code for the bordering Districts }
#' }
#'
'neighbours'

#' mps
#'
#' @return A `Leaflet` object to be used as initial map for Shiny Web Apps
#'
'mps'
