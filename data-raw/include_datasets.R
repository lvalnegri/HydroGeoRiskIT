##################################################
# Copia dati e mappa leaflet in PACKAGE DATA DIR #
##################################################

Rfuns::load_pkgs('RShinyUtils', 'data.table', 'leaflet', 'sf')
devtools::load_all()

dbn <- 'ispra'

fn <- 'mps'
y <- basemap(menu = FALSE, tiles = tiles.lst[[2]], add_pb_menu = FALSE, extras = NULL) |>
        fitBounds(bbox.it[1, 1], bbox.it[2, 1], bbox.it[1, 2], bbox.it[2, 2]) |>
        registerPlugin(spinPlugin) |>
        registerPlugin(leafletspinPlugin) |>
        clearShapes() |>
        end_spinmap_it()
assign(fn, y)
save( list = fn, file = file.path('data', paste0(fn, '.rda')), version = 3, compress = 'gzip' )

fn <- 'dts'
y <- fread('./data-raw/csv/dataset.csv')
y <- melt(y, id.vars = 'CMN', variable.name = 'var_id', value.name = 'value')
dd_dbm_do(dbn, 'w', 'dataset', y) 
assign(fn, y)
save( list = fn, file = file.path('data', paste0(fn, '.rda')), version = 3, compress = 'gzip' )

fn <- 'mdts'
y <- fread('./data-raw/csv/metadata.csv', na.strings = '')
dd_dbm_do(dbn, 'w', 'metadata', y) 
assign(fn, y)
save( list = fn, file = file.path('data', paste0(fn, '.rda')), version = 3, compress = 'gzip' )

fn <- 'geoCMN'
system('iconv -f Latin1 -t utf-8 ./data-raw/csv/geoCMN_latin.csv > ./data-raw/csv/geoCMN.csv')
yc <- fread('./data-raw/csv/geoCMN.csv', keepLeadingZeros = TRUE)
dd_dbm_do(dbn, 'w', fn, yc) 
assign(fn, yc)
save( list = fn, file = file.path('data', paste0(fn, '.rda')), version = 3, compress = 'gzip' )

fn <- 'bndCMN'
y <- st_read('./data-raw/shp/bndCMN.shp') |> 
        dplyr::mutate(CMN = as.integer(CMN)) |> 
        merge(yc[, .(CMN, CMNd)])
assign(fn, y)
save( list = fn, file = file.path('data', paste0(fn, '.rda')), version = 3, compress = 'gzip' )

fn <- 'bndPRV'
y <- st_read('./data-raw/shp/bndPRV.shp') |> merge(unique(yc[, .(PRV, PRVd, PRVs)]))
assign(fn, y)
save( list = fn, file = file.path('data', paste0(fn, '.rda')), version = 3, compress = 'gzip' )

fn <- 'cmn_lid'
y1 <- fread('./data-raw/csv/cmn_lid_geo.csv')
y2 <- fread('./data-raw/csv/cmn_lid_hydro.csv')
y <- rbindlist(list( y1[, risk := 'H'], y2[, risk := 'G']), use.names = TRUE) |> setcolorder(c('risk', 'lid'))
dd_dbm_do(dbn, 'w', fn, y) 
assign(fn, y)
save( list = fn, file = file.path('data', paste0(fn, '.rda')), version = 3, compress = 'gzip' )

rm(list = ls())
gc()
