#############################################
# ISPRA HYDROGEO RISK - Polygons Landslides #
#############################################

# Preparation -----------------------------------
Rfuns::load_pkgs('HydroGeoRiskIT', 'data.table', 'dplyr', 'qs', 'sf')

in_path <- file.path(ext_path, 'it', 'ispra', 'geo')
out_path <- file.path(dpath, 'geo')

build_table <- \(x, r){
    htmlTable::htmlTable(
        mdts[, .(var_id, description)][dts[CMN == x & var_id %in% mdts[is.na(risk), var_id], .(var_id, value = add_Kcomma(as.integer(value)))], on = 'var_id'][,var_id := NULL],
        rnames = FALSE,
        align = 'lr'
    )
}
empty_pol <- st_sf(geometry = st_sfc(st_polygon()), crs = 3035)
ybp <- bndPRV |> select(PRV) |> st_transform(3035)
ybc <- bndCMN |> select(CMN) |> st_transform(3035)

# Read and clean shapefiles, adding <id> --------
y <- st_read(file.path(in_path, 'geo_ori.shp'), quiet = TRUE) |> 
        st_transform(3035) |> 
        mutate(livello = gsub('.* (.*$)', '\\1', per_fr_ita)) |>
        select(livello) |>
        arrange(livello) |> 
        group_by(livello) |> 
        mutate(id = 1:n()) |>
        ungroup() |> 
        mutate(lid = paste0(id, livello)) |> 
        select(lid, id, livello)
st_write(y, file.path(in_path, 'geo.shp'))
qsave(y, file.path(out_path, 'geo.qs'))

# Build <lookups> table lid <-> CMN -------------
yc <- bndCMN |> st_transform(3035)
ye <- st_intersects(y, yc)
yt <- y |> st_drop_geometry()
yc <- yc |> st_drop_geometry()
ye <- rbindlist(lapply( 1:nrow(ye), \(x) data.table( yt[x,], yc[ye[[x]],] ) )) |> setnames('V2', 'CMN')
ye[, lid := paste0(id, livello)]
fwrite(ye, './data-raw/csv/geo_cmn.csv')
fst::write_fst(ye, file.path(apath, 'geo.fst'))

# Cut Off by Province ---------------------------
ybp <- masteRconfini::PRV |> st_transform(3035)
yc <- masteRgeo::comuni[, .(CMN = as.integer(CMN), PRV)][order(PRV, CMN)]
y <- list()
for(p in ybp$PRV){
    message('Provincia ', p)
    ycp <- yc[PRV == p, CMN]
    y[[p]] <- st_intersection( yb |> subset(lid %in% ye[CMN %in% ycp, lid]), ybp |> subset(PRV == p) ) |> select(-PRV)
    qsave(y[[p]], file.path(out_path, p), nthreads = 10)
}

# Cut Off by District ---------------------------
for(p in ybp$PRV){
    message('Provincia ', p)
    for(pc in yc[PRV == p, CMN]){
        yt <- st_intersection(ybc |> subset(CMN == pc), y[[p]] ) |> select(livello) |> st_transform(4326)
        if(nrow(yt) > 0){
            y <- list(
                'bbx' = st_bbox(yt |> st_transform(4326)),
                'P1' = yt |> subset(livello == 'P1'),
                'P2' = yt |> subset(livello == 'P2'),
                'P3' = yt |> subset(livello == 'P3'),
                'P4' = yt |> subset(livello == 'P4'),
                'AA' = yt |> subset(livello == 'AA'),
                'CMN' = masteRconfini::CMN |> subset(CMN == pc)
            )
        } else { 
            y <- list( 'bbx' = NULL, 'CMN' = masteRconfini::CMN |> subset(CMN == pc) )
        }
        qsave(y, file.path(out_path, pc), nthreads = 10)
    }
}


# Done! -----------------------------------------
rm(list = ls())
gc()
