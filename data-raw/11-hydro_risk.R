###########################################
# ISPRA HYDROGEO RISK - Polygons Flooding #
###########################################

Rfuns::load_pkgs('data.table', 'dplyr', 'qs', 'sf')

in_path <- file.path(ext_path, 'it', 'ispra', 'hydro')
out_path <- file.path(datait_path, 'ispra', 'hydro')

# LETTURA e PULIZIA SHAPEFILES CON AGGIUNTA id
yb <- rbind(
        st_read(file.path(in_path, 'L.shp'), quiet = TRUE) |> select(-1) |> mutate(level = 'L', id = 1:n()),
        st_read(file.path(in_path, 'M.shp'), quiet = TRUE) |> select(-1) |> mutate(level = 'M', id = 1:n()),
        st_read(file.path(in_path, 'H.shp'), quiet = TRUE) |> select(-1) |> mutate(level = 'H', id = 1:n())
) |> dplyr::mutate(lid = paste0(id, level))
st_write(yb, file.path(in_path, 'idro.shp'))
qsave(yb, file.path(out_path, 'idro.qs'), nthreads = 10)

# CREA TABELLA LEGAMI id-CMN (intersezione con comuni)
ybc <- RbndIT::CMN |> st_transform(3035)
ye <- st_intersects(yb, ybc)
yt <- yb |> st_drop_geometry()
yc <- ybc |> st_drop_geometry()
ye <- rbindlist(lapply( 1:nrow(ye), \(x) data.table( yt[x,], yc[ye[[x]],] ) )) |> setnames('V2', 'CMN')
ye[, lid := paste0(id, livello)]
fwrite(ye, './data-raw/csv/idro_comuni.csv')
fst::write_fst(ye, file.path(out_path, 'idro.fst'))

# ESTRAI PROVINCIE
ybp <- RbndIT::PRV |> st_transform(3035)
yc <- RgeoIT::comuni[, .(CMN = as.integer(CMN), PRV)][order(PRV, CMN)]
y <- list()
for(p in ybp$PRV){
    message('Provincia ', p)
    ycp <- yc[PRV == p, CMN]
    y[[p]] <- st_intersection( yb |> subset(lid %in% ye[CMN %in% ycp, lid]), ybp |> subset(PRV == p) ) |> select(-PRV)
    qsave(y[[p]], file.path(out_path, p), nthreads = 10)
}
qsave(y, file.path(out_path, 'idro.prv'), nthreads = 10)

# ESTRAI COMUNI e CALCOLO DIFFERENZE
st_stuff <- \(x){
    if(is.na(sum(st_bbox(x)))) return(NULL)
    # if(!(st_is(x, 'MULTIPOLYGON') | st_is(x, 'POLYGON'))) x <- x |> st_collection_extract('POLYGON') 
    x |> st_collection_extract('POLYGON') |> st_union() |> st_transform(4326)
}
yy <- list()
for(p in ybp$PRV){
    message('Provincia ', p)
    for(pc in yc[PRV == p, CMN]){
        yt <- st_intersection(ybc |> subset(CMN == pc), y[[p]] ) |> select(livello)
        if(nrow(yt) > 0){
            yy[[as.character(pc)]] <- list(
                        'bbx' = st_bbox(yt |> st_transform(4326)),
                        'H' = yt |> subset(livello == 'H') |> st_stuff(),
                        'M' = st_difference(yt |> subset(livello == 'M') |> st_union(), yt |> subset(livello == 'H') |> st_union()) |> st_stuff(),
                        'L' = st_difference(yt |> subset(livello == 'L') |> st_union(), yt |> subset(livello %in% c('H', 'M')) |> st_union()) |> st_stuff(),
                        'CMN' = masteRconfini::CMN |> subset(CMN == pc)
            )
        } else { 
            yy[[as.character(pc)]] <- list( 'bbx' = NULL, 'CMN' = masteRconfini::CMN |> subset(CMN == pc) )
        }
        qsave(yy[[as.character(p)]], file.path(out_path, pc), nthreads = 10)
    }
}
qsave(yy, file.path(out_path, 'idro.cmn'), nthreads = 10)

# library(leaflet)
# leaflet() |>
#     aggiungi_tessera(tiles.lst[[19]]) |>
#     addPolygons(data = yt |> subset(livello == 'H'), group = 'H', fillColor = colori.lst$idro$H, opacity = 0, fillOpacity = 1) |> 
#     addPolygons(data = yt |> subset(livello == 'M'), group = 'M', fillColor = colori.lst$idro$M, opacity = 0, fillOpacity = 1) |> 
#     addPolygons(data = yt |> subset(livello == 'L'), group = 'L', fillColor = colori.lst$idro$L, opacity = 0, fillOpacity = 1) |> 
#     addLayersControl(overlayGroups = c('H', 'M', 'L'))

# ESCI
rm(list = ls())
gc()
