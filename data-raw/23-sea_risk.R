##########################################
# ISPRA HYDROGEO RISK - Polygons Tsunami #
##########################################

# Preparation -----------------------------------
Rfuns::load_pkgs('HydroGeoRiskIT', 'data.table', 'dplyr', 'rmapshaper', 'qs', 'sf')

in_path <- file.path(ext_path, 'it', 'ispra', 'sea')
out_path <- file.path(dpath, 'sea')

ybp <- bndPRV |> select(PRV) |> st_transform(3035)
ybc <- bndCMN |> select(CMN) |> st_transform(3035)

