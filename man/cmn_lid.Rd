% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{cmn_lid}
\alias{cmn_lid}
\title{cmn_lid}
\format{
A \code{data.table} object with the following fields:
\describe{
\item{\code{risk}}{ Tipologia di Rischio: \code{G}eologico (Frana) o \code{I}drologico (Alluvione) }
\item{\code{lid}}{ Identificativo unico formato come concatenazione fra \code{livello} e \code{id} }
\item{\code{level}}{ Valori assunti dal Rischio (si veda la lista \code{livelli.lst}) }
\item{\code{id}}{ Identificativo di Livello per ogni tipologia di Rischio }
\item{\code{CMN}}{ Codice ISTAT del Comune }
}
}
\usage{
cmn_lid
}
\description{
Tabella di mappatura fra Codici Comuni ed Aree a Rischio
}
\keyword{datasets}
