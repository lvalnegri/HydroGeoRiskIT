Rfuns::load_pkgs('RShinyUtils', 'HydroGeoRiskIT', 'data.table', 'DT', 'htmltools', 'leaflet', 'qs', 'sf', 'shiny', 'shinyjs')

ui <- fluidPage(

    useShinyjs(),
    faPlugin,
    tags$head(
        tags$title('HydroGeological Risk in Italy'),
        tags$style("@import url('https://datamaps.uk/assets/datamaps/icons/fontawesome/all.css;')"),
        tags$style(HTML("
            #out_map { height: calc(100vh - 80px) !important; }
            .well { 
                padding: 10px;
                height: calc(100vh - 80px);
                overflow-y: auto; 
                border: 10px;
                background-color: #EAF0F4; 
            }
            ::-webkit-scrollbar {
                width: 8px;
            }
            ::-webkit-scrollbar-track {
                background: #f1f1f1;
            }
            ::-webkit-scrollbar-thumb {
                background: #888;
            }
            ::-webkit-scrollbar-thumb:hover {
                background: #555;
            }
            .col-sm-3 { padding-right: 0; }
            #titolo_menu_mappa{ 
                margin-bottom: 10px; 
                font-weight: 700;
                font-size: 120%;
            }
        "))
    ),
    # includeCSS('./styles.css'),
    
    titlePanel('HydroGeological Risk'),

    fluidRow(
        column(3,
            wellPanel(
                shinyWidgets::virtualSelectInput(
                    'cbo_cmn', 'DISTRICT:', cmn.lst, character(0), search = TRUE, 
                    placeholder = 'Select a District', 
                    searchPlaceholderText = 'Search...', 
                    noSearchResultsText = 'No District found!'
                ),
                radioButtons('rdb_rsk', 'RISK', c('Flooding' = 'hydro', 'Landslide' = 'geo')),
                checkboxInput('chk_vcn', 'INCLUDE NEIGHBOURS', FALSE),
                br(),
                selectInput('cbo_tls', 'MAPTILES:', tiles.lst, tiles.lst[[19]]),
                sliderInput('sld_tsp', 'POLYGON OPACITY:', 1, 10, 8, 1)
            )
        ),
        column(9, leafletOutput('out_map', width = '100%') )
    )

)

server <- function(input, output) {

    # INIZIALIZZAZIONE MAPPA
    output$out_map <- renderLeaflet({ leaflet() })

    # ESTRAZIONE POLIGONI
    dts <- reactive({
        req(input$cbo_cmn)
        qread(file.path(dpath, input$rdb_rsk, paste0(input$cbo_cmn, ifelse(input$chk_vcn, 'v', ''))), nthreads = 4)
    })
    
    # AGGIORNAMENTO MAPPA E TABELLA LATERALE
    observeEvent(
        {
            input$cbo_cmn
            input$rdb_rsk
            input$chk_vcn 
        }, 
        {
            
            bbx <- as.numeric(dts()$bbx)
            if(length(bbx) > 0){ 
                
                y <- leafletProxy('out_map') |>
                        removeShape(layerId = 'spinnerMarker') |>
                        clearShapes() |> clearControls() |> 
                        fitBounds(bbx[1], bbx[2], bbx[3], bbx[4])
                
                # POLIGONI AREE RISCHIO
                grps <- NULL
                yr <- risks.lst[[input$rdb_rsk]]
                for(yrn in names(yr)){
                    vai <- ifelse(input$chk_vcn, !is.null(dts()[[yrn]]), !st_is_empty(dts()[[yrn]]))
                    if(vai){
                        yrc <- palette.lst[[input$rdb_rsk]][[yrn]]
                        grp <- risks.lst[[input$rdb_rsk]][[yrn]]
                        grps <- c(grps, grp)
                        y <- y |> 
                                addPolygons(
                                    data = dts()[[yrn]], 
                                    group = grp, 
                                    stroke = FALSE, 
                                    fillColor = yrc, 
                                    fillOpacity = as.numeric(input$sld_tsp) / 10
                                )
                    } 
                }
                
                # POLIGONI COMUNI
                if(input$chk_vcn)
                    y <- y |> 
                    addPolygons(
                        data = dts()$CMNv,
                        weight = 2,
                        color = 'black',
                        opacity = 1,
                        fillOpacity = 0,
                        label = dts()$dtsv,
                        highlightOptions = hlt.options
                    )
                y <- y |> 
                        addPolygons(
                            data = dts()$CMN,
                            weight = 3,
                            color = 'red',
                            opacity = 1,
                            fillOpacity = 0,
                            label = dts()$dts,
                            highlightOptions = hlt.options
                        )
                
                # TITOLO? 
                # y <- y |> addControl()
                
                # RILASCIO MAPPA FINALE
                y |> 
                    addLayersControl(overlayGroups = grps, options = layersControlOptions(collapsed = FALSE)) |> 
                    end_spinmap_it()
                
            } else {

            }

        }
    )
    
    # AGGIORNAMENTO TESSERE MAPPA
    observe({ leafletProxy('out_map') |> clearTiles() |> add_maptile(input$cbo_tls) })
    
    # TABELLA DETTAGLIO
    observeEvent(input$btn_tbl, {
        
    })
    
}

shinyApp(ui = ui, server = server)
