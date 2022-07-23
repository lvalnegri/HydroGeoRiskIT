#############################################
# ISPRA HYDROGEO RISK - Polygons Landslides #
#############################################

Rfuns::load_pkgs('data.table', 'dplyr', 'qs', 'sf')

in_path <- file.path(ext_path, 'it', 'ispra', 'geo')
out_path <- file.path(datait_path, 'ispra', 'geo')

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

yc <- RbndIT::CMN |> st_transform(3035)
ye <- st_intersects(y, yc)
yt <- y |> st_drop_geometry()
yc <- yc |> st_drop_geometry()
ye <- rbindlist(lapply( 1:nrow(ye), \(x) data.table( yt[x,], yc[ye[[x]],] ) )) |> setnames('V2', 'CMN')
ye[, lid := paste0(id, livello)]
fwrite(ye, './data-raw/csv/geo_cmn.csv')
fst::write_fst(ye, file.path(apath, 'geo.fst'))

rm(list = ls())
gc()
