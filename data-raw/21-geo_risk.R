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
empty_poly <- st_sf(geometry = st_sfc(st_polygon()), crs = 4326)
extract_poly <- \(py, x) if(nrow(py |> subset(livello == x)) > 0) py |> subset(livello == x) else empty_poly
ybp <- bndPRV |> select(PRV) |> st_transform(3035)
ybc <- bndCMN |> select(CMN) |> st_transform(3035)

# Read and clean shapefiles, adding <id> --------
message('Reading and cleaning shapefile...')
yb <- st_read(file.path(in_path, 'geo_ori.shp'), quiet = TRUE) |> 
        st_transform(3035) |> 
        mutate(livello = gsub('.* (.*$)', '\\1', per_fr_ita)) |>
        select(livello) |>
        arrange(livello) |> 
        group_by(livello) |> 
        mutate(id = 1:n()) |>
        ungroup() |> 
        mutate(lid = paste0(id, livello)) |> 
        select(lid, id, livello)
st_write(yb, file.path(in_path, 'geo.shp'))
qsave(yb, file.path(out_path, 'geo.qs'), nthreads = 10)

# Build <lookups> table lid <-> CMN -------------
message('\nBuilding lookups between polygons and Districts...')
ye <- st_intersects(y, ybc)
yt <- yb |> st_drop_geometry()
yc <- ybc |> st_drop_geometry()
ye <- rbindlist(lapply( 1:nrow(ye), \(x) data.table( yt[x,], yc[ye[[x]],] ) )) |> setnames('V2', 'CMN')
ye[, lid := paste0(id, livello)]
fwrite(ye, './data-raw/csv/geo_cmn.csv')
fst::write_fst(ye, file.path(out_path, 'geo.fst'))

# Cut Off by Province ---------------------------
message('\nCutting off Provinces:')
for(p in ybp$PRV){
    message('Province ', p)
    ycp <- geoCMN[PRV == p, CMN]
    y <- st_intersection( yb |> subset(lid %in% ye[CMN %in% ycp, lid]), ybp |> subset(PRV == p) ) |> select(-PRV)
    qsave(y, file.path(out_path, p), nthreads = 10)
}

# Cut Off by District ---------------------------
message('\nCutting off Districts:')
for(p in ybp$PRV){
    message('\nProvince ', p)
    y <- qread(file.path(out_path, p), nthreads = 10)
    for(pc in geoCMN[PRV == p, CMN]){
        message(' - District ', pc)
        yt <- st_intersection(ybc |> subset(CMN == pc), y) |> select(livello) |> st_transform(4326)
        if(nrow(yt) > 0){
            yt <- list(
                'bbx' = st_bbox(yt),
                'P1' = extract_poly(yt, 'P1'),
                'P2' = extract_poly(yt, 'P2'),
                'P3' = extract_poly(yt, 'P3'),
                'P4' = extract_poly(yt, 'P4'),
                'AA' = extract_poly(yt, 'AA'),
                'CMN' = bndCMN |> subset(CMN == pc),
                'dts' = build_table(pc)
            )
        } else { 
            yt <- list( 'bbx' = NULL, 'CMN' = bndCMN |> subset(CMN == pc) )
        }
        qsave(yt, file.path(out_path, pc), nthreads = 10)
    }
}

# Done! -----------------------------------------
rm(list = ls())
gc()
