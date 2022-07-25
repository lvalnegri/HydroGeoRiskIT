###########################################
# ISPRA HYDROGEO RISK - Polygons Flooding #
###########################################

# Preparation -----------------------------------
Rfuns::load_pkgs('HydroGeoRiskIT', 'data.table', 'dplyr', 'qs', 'rmapshaper', 'sf')

in_path <- file.path(ext_path, 'it', 'ispra', 'hydro')
out_path <- file.path(dpath, 'hydro')

build_table <- \(x, r){
    htmlTable::htmlTable(
        mdts[, .(var_id, description)][dts[CMN == x & var_id %in% mdts[is.na(risk), var_id], .(var_id, value = add_Kcomma(as.integer(value)))], on = 'var_id'][,var_id := NULL],
        rnames = FALSE,
        align = 'lr'
    )
}
empty_poly <- st_sf(geometry = st_sfc(st_polygon()), crs = 3035)
ybp <- bndPRV |> select(PRV) |> st_transform(3035)
ybc <- bndCMN |> select(CMN) |> st_transform(3035)

# Read and clean shapefiles, adding <id> --------
message('Reading and combining shapefiles...')
yb <- rbind(
        st_read(file.path(in_path, 'L.shp'), quiet = TRUE) |> select(-1) |> mutate(level = 'L', id = 1:n()),
        st_read(file.path(in_path, 'M.shp'), quiet = TRUE) |> select(-1) |> mutate(level = 'M', id = 1:n()),
        st_read(file.path(in_path, 'H.shp'), quiet = TRUE) |> select(-1) |> mutate(level = 'H', id = 1:n())
) |> dplyr::mutate(lid = paste0(id, level))
message(' - Saving big file...')
st_write(yb, file.path(in_path, 'hydro.shp'), append = FALSE)
qsave(yb, file.path(out_path, 'hydro.qs'), nthreads = 10)

# Build <lookups> table lid <-> CMN -------------
message('\nBuilding lookups between polygons and Districts...')
ye <- st_intersects(yb, ybc)
yt <- yb |> st_drop_geometry()
yc <- ybc |> st_drop_geometry()
ye <- rbindlist(lapply( 1:nrow(ye), \(x) data.table( yt[x,], yc[ye[[x]],] ) )) |> setnames('V2', 'CMN')
ye[, lid := paste0(id, level)]
fwrite(ye, './data-raw/csv/cmn_lid_hydro.csv')
fst::write_fst(ye, file.path(out_path, 'hydro.fst'))

# Cut Off by Province ---------------------------
message('\nCutting off Provinces:')
for(p in sort(ybp$PRV)){
    message('Province ', p)
    ycp <- geoCMN[PRV == p, CMN]
    y <- st_intersection( yb |> subset(lid %in% ye[CMN %in% ycp, lid]), ybp |> subset(PRV == p) ) |> select(-PRV)
    qsave(y, file.path(out_path, p), nthreads = 10)
}

# Cut Off by District ---------------------------
y.err <- NULL
message('\nCutting off Districts:')
for(p in sort(ybp$PRV)){
    message('\nProvince ', p)
    y <- qread(file.path(out_path, p), nthreads = 10)
    for(pc in sort(geoCMN[PRV == p, CMN])){
        message(' - District ', pc)
        tryCatch(
            expr = {
                yt <- st_intersection(ybc |> subset(CMN == pc), y) |> select(level)
                if(nrow(yt) > 0){
                    yth <- yt |> subset(level == 'H')
                    yth <- if(nrow(yth) == 0) { empty_poly } else { yth |> ms_dissolve(copy_fields = 'level') |> st_make_valid() }
                    if(nrow(yth) == 0){
                        yth <- empty_poly
                    } else {
                        if(st_is(yth, 'GEOMETRYCOLLECTION')) yth <- st_collection_extract(yth, 'POLYGON')
                    }
                    ytm <- yt |> subset(level == 'M')
                    ytm <- if(nrow(ytm) == 0) { empty_poly } else { ytm |> ms_dissolve(copy_fields = 'level') |> st_make_valid() }
                    ytl <- yt |> subset(level == 'L')
                    if(nrow(ytl) == 0){ 
                        empty_poly 
                    } else { 
                        ytl <- ytl |> ms_dissolve(copy_fields = 'level')  |> st_make_valid() |> st_difference(ytm) |> select(level) 
                        if(nrow(ytl) == 0){
                            ytl <- empty_poly
                        } else {
                            if(st_is(ytl, 'GEOMETRYCOLLECTION')) ytl <- st_collection_extract(ytl, 'POLYGON')
                        }
                    }
                    ytm <- ytm |> st_difference(yth)
                    if(nrow(ytm) == 0){
                        ytm <- empty_poly
                    } else {
                        if(st_is(ytm, 'GEOMETRYCOLLECTION')) ytm <- st_collection_extract(ytm, 'POLYGON')
                        ytm <- ytm |> select(level)
                    }
                    yt <- list(
                            'bbx' = st_bbox(yt |> st_transform(4326)),
                            'H' = yth |> st_transform(4326),
                            'M' = ytm |> st_transform(4326),
                            'L' = ytl |> st_transform(4326),
                            'CMN' = bndCMN |> subset(CMN == pc),
                            'dts' = build_table(pc)
                    )
                } else { 
                    yt <- list( 'bbx' = NULL, 'CMN' = bndCMN |> subset(CMN == pc) )
                }
                qsave(yt, file.path(out_path, pc), nthreads = 10)
            }, 
            error = \(e){ 
                message('   >>> ERROR!!! <<<')
                y.err <<- c(y.err, pc)
            }
        ) 
    }
}

message('\nTrying to build err Districts:')
for(pc in y.err){
    p <- cmn2prv(pc)
    y <- qread(file.path(out_path, p), nthreads = 10)
    message(' - District ', pc)
    yt <- st_intersection(ybc |> subset(CMN == pc), y) |> select(level) |> st_buffer(0.1)
    yth <- yt |> subset(level == 'H')
    yth <- if(nrow(yth) == 0) { empty_poly } else { yth |> ms_dissolve(copy_fields = 'level') |> st_make_valid() }
    ytm <- yt |> subset(level == 'M')
    ytm <- if(nrow(ytm) == 0) { empty_poly } else { ytm |> ms_dissolve(copy_fields = 'level') |> st_make_valid() }
    ytl <- yt |> subset(level == 'L')
    if(nrow(ytl) == 0) { empty_poly } else { ytl <- ytl |> ms_dissolve(copy_fields = 'level')  |> st_make_valid() |> st_difference(ytm) }
    ytm <- ytm |> st_difference(yth)
    yt <- list(
            'bbx' = st_bbox(yt |> st_transform(4326)),
            'H' = yth |> st_transform(4326),
            'M' = ytm |> st_transform(4326),
            'L' = ytl |> st_transform(4326),
            'CMN' = bndCMN |> subset(CMN == pc),
            'dts' = build_table(pc)
    )
    qsave(yt, file.path(out_path, pc), nthreads = 10)
}

# Done! -----------------------------------------
rm(list = ls())
gc()
