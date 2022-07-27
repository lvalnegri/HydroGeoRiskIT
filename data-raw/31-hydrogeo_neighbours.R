####################################################
# ISPRA HYDROGEO RISK - Create Neighbours Polygons #
####################################################

Rfuns::load_pkgs('HydroGeoRiskIT', 'data.table', 'htmltools', 'qs', 'rmapshaper', 'sf')

for(r in c('hydro', 'geo')){
    yn <- names(risks.lst[[r]])
    for(p in bndPRV$PRV){
        message('\n(', toupper(substr(r, 1, 1)), ') Province ', p)
        for(pc in geoCMN[PRV == p, CMN]){
            if(!file.exists(file.path(dpath, r, paste0(pc, 'v')))){
                message(' - District ', pc)
                yvc <- neighbours[CMN == pc, neigh]
                y <- lapply(yvc, \(z) qread(file.path(dpath, r, z), nthreads = 10))
                y <- y[!sapply(y, \(x) is.null(x$bbx))]
                y <- lapply(
                    yn, 
                    \(x){ 
                        yt <- do.call(
                            'rbind', 
                            lapply(y, \(z) if(st_is_empty(z[[x]])) NULL else z[[x]] |> subset(select = 'level'))
                        )
                        yt <- yt[!sapply(yt, is.null)]
                        if(is.null(yt)) empty_poly(FALSE) else yt |> ms_dissolve(copy_fields = 'level')
                    }
                ) |> setNames(yn)
                ybx <- do.call('rbind', lapply(y, \(x) if(st_is_empty(x)) NULL else x))
                if(!is.null(ybx)){
                    y$bbx <- st_bbox(ybx)
                    y$CMN <- bndCMN |> subset(CMN == pc)
                    y$dts <- build_table(pc)
                    y$CMNv <- bndCMN |> subset(CMN %in% yvc & CMN != pc)
                    yvc <- yvc[which(yvc != pc)]
                    y$dtsv <- lapply(yvc, build_table)
                    qsave(y, file.path(dpath, r, paste0(pc, 'v')), nthreads = 10)
                }
            }
        }
    }
}

# rm(list = ls())
# gc()
