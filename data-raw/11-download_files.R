#######################################################
# ISPRA HYDROGEO RISK - download and decompress files #
#######################################################

Rfuns::load_pkgs()

opath <- file.path(ext_path, 'it', 'ispra')
tmpf <- tempfile()

# FLOODING (hydro risk)
download.file('https://idrogeo.isprambiente.it/opendata/wms/Mosaicatura_ISPRA_2020_aree_pericolosita_idraulica.zip', tmpf)
out_path <- file.path(opath, 'hydro')
unzip(tmpf, exdir = out_path)
file.remove(grep('pdf$', list.files(out_path, full.names = TRUE), value = TRUE))
file.rename(list.files(out_path, full.names = TRUE), file.path(out_path, gsub('(.{1}).*(\\..*$)', '\\1\\2', list.files(out_path))))

# LANDSLIDES (geo risk)
download.file('https://idrogeo.isprambiente.it/opendata/wms/Mosaicatura_ISPRA_2020_2021_aree_pericolosita_frana_PAI.zip', tmpf)
out_path <- file.path(opath, 'geo')
unzip(tmpf, exdir = out_path)
file.remove(grep('pdf$', list.files(out_path, full.names = TRUE), value = TRUE))
file.rename(list.files(out_path, full.names = TRUE), file.path(out_path, gsub('(.{1}).*(\\..*$)', 'geo_ori\\2', list.files(out_path))))

unlink(tmpf)
rm(list = ls())
gc()
