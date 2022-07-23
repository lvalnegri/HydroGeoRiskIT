##################################################
# Copia dati e mappa leaflet in PACKAGE DATA DIR #
##################################################

Rfuns::load_pkgs('data.table', 'leaflet')
devtools::load_all()

dbn <- 'ispra'

fn <- 'mps'
y <- Rfuns::basemap(menu = FALSE, tiles = tiles.lst[[2]], add_pb_menu = FALSE, extras = NULL) |>
        fitBounds(bbox.it[1, 1], bbox.it[2, 1], bbox.it[1, 2], bbox.it[2, 2]) |>
        registerPlugin(spinPlugin) |>
        registerPlugin(leafletspinPlugin) |>
        clearShapes() |>
        RShinyUtils::end_spinmap()
assign(fn, y)
save( list = fn, file = file.path('data', paste0(fn, '.rda')), version = 3, compress = 'gzip' )

fn <- 'dts'
y <- fread('./data-raw/csv/dataset.csv')
y <- melt(y, id.vars = 'CMN', variable.name = 'sigla', value.name = 'valore')
dbm_do(dbn, 'w', 'dataset', y) 
assign(fn, y)
save( list = fn, file = file.path('data', paste0(fn, '.rda')), version = 3, compress = 'gzip' )

fn <- 'mdts'
y <- fread('./data-raw/csv/metadata.csv', na.strings = '')[!is.na(sigla)]
dbm_do(dbn, 'w', 'metadati', y) 
assign(fn, y)
save( list = fn, file = file.path('data', paste0(fn, '.rda')), version = 3, compress = 'gzip' )

fn <- 'yc'
assign(fn, masteRgeo::comuni[, .SD, .SDcols = patterns('CMN|PRV')][order(CMNd)])
save( list = fn, file = file.path('data', paste0(fn, '.rda')), version = 3, compress = 'gzip' )

fn <- 'ybc'
assign(fn, readRDS(file.path(bnd_path, 'CMN', 's20', '0')))
save( list = fn, file = file.path('data', paste0(fn, '.rda')), version = 3, compress = 'gzip' )

fn <- 'yk'
y1 <- fst::read_fst(file.path(data_path, 'ispra', 'idro', 'idro.fst'), as.data.table = TRUE)
y2 <- fst::read_fst(file.path(data_path, 'ispra', 'geo', 'geo.fst'), as.data.table = TRUE)
y <- rbindlist(list( y1[, rischio := 'I'], y2[, rischio := 'G']), use.names = TRUE) |> setcolorder(c('rischio', 'lid'))
dbm_do(dbn, 'w', 'legami', y) 
assign(fn, y)
save( list = fn, file = file.path('data', paste0(fn, '.rda')), version = 3, compress = 'gzip' )

