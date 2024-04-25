## Kiri Daust
## App for Question 1
library(shiny)
library(data.table)
library(sf)
library(RPostgres)
library(pool)
library(DT)
library(leaflet)
library(scales)

##this file contains the javascript functions for running the map
source("leaflet_js.R")

##connect to database
dbCon <- dbPool(
    drv = RPostgres::Postgres(),
    dbname = "kiri_assign",
    host = Sys.getenv("DB_HOST"),
    port = 5432,
    user = "postgres",
    password = Sys.getenv("DB_PWD")
)

onStop(function() {
    poolClose(dbCon)
})

bgc_options <- sort(dbGetQuery(dbCon,"select distinct plant_bec from cbst_pl")[,1])

# Define UI for application 
ui <- fluidPage(
    titlePanel("CBST Seedlot Selection Tool"),
    sidebarLayout(
        sidebarPanel(
            h2("I have a cutblock"),
            selectInput("spp_choose","Select Species", choices = "Pl - Lodgepole Pine", selected = "Pl - Lodgepole Pine"),
            selectInput("bgc_choose",label = "Select BGC(s)", choices = bgc_options, multiple = TRUE),
            sliderInput("matchprop","Select HTP Threshold",
                        min = 0.9, max = 1, value = 0.975,step = 0.001),
            actionButton("calculate","Let's Go!!!"),
            actionButton("clearmap","Clear Map"),
            h2("Results"),
            h3("Matching BGCs"),
            dataTableOutput("bgc_tab"),
            h3("Possible Seedlots"),
            dataTableOutput("sl_tab"),
            downloadButton("download_seedlot")
        ),
        
        mainPanel(
            h2("BGC Map"),
            p("Click on the map to select a BGC.
              The selected BGC will show in yellow; BGCs with matching CBST options will show in pink,
              with the opacity proportial to the HGt value."),
            leafletOutput("bgc_map",height = "90vh")
        )
    )
)

server <- function(input, output, session) {

    output$bgc_map <- renderLeaflet({
        leaflet() %>%
            setView(lng = -122.77222, lat = 51.2665, zoom = 5) %>%
            addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "Positron",
                             options = leaflet::pathOptions(pane = "mapPane")) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite",
                                      options = leaflet::pathOptions(pane = "mapPane")) %>%
            leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap",
                                      options = leaflet::pathOptions(pane = "mapPane")) %>%
            add_bgc_map() %>% ##calling the javascript tileserver
            leaflet::addLayersControl(
                baseGroups = c("Positron","Satellite", "OpenStreetMap"),
                overlayGroups = c("BEC"),
                position = "topright")
    })
    
    ##update bgc_choose when map is clicked
    observeEvent(input$becselect_click,{
        updateSelectInput(session,"bgc_choose", selected = input$becselect_click)
    })
    
    ##tell JS to highlight map
    observeEvent(input$bgc_choose,{
        session$sendCustomMessage("highlightBEC",input$bgc_choose)
    })
    
    ##remove hightlight
    observeEvent(input$clearmap,{
        session$sendCustomMessage("clearBEC","xxx")
    })
    
    ##main workhose - queries database
    observeEvent(input$calculate,{
        output$sl_tab <- DT::renderDataTable({ ##seedlot table
            bgc_sel <- input$bgc_choose
            sl_query <- paste0("select seedlot, genetic_class, orchard_no, bec
                        from cbst_seedlot
                        join cbst_pl on cbst_seedlot.bec = cbst_pl.seed_bec
                        where plant_bec in ('",paste(bgc_sel, collapse = "','"),
                               "') and htp >= ",input$matchprop," and species like 'PL%'")
            #cat(sl_query)
            seedlot_out <- dbGetQuery(dbCon, sl_query)
            seedlot_out
        })
        output$bgc_tab <- DT::renderDataTable({ ##bgc table
            bgc_sel <- input$bgc_choose
            bgc_query <- paste0("select plant_bec, seed_bec, htp
                            from cbst_pl
                            where plant_bec in ('",paste(bgc_sel, collapse = "','"),
                                "') and htp >= ",input$matchprop)

            bgc_out <- dbGetQuery(dbCon, bgc_query)
            new_bgcs <- bgc_out$seed_bec
            opa_match <- rescale(bgc_out$htp, to = c(0.35,1),
                                 from = c(input$matchprop,1))
            colmap <- data.table(BGC = c(bgc_sel,new_bgcs),
                                 Col = c(rep("#FDF506",length(bgc_sel)),
                                         rep("#FF0A7D",length(new_bgcs))),
                                 Opacity = c(rep(1,length(bgc_sel)),opa_match))
            session$sendCustomMessage("colour_bec",colmap) ##and colour the map, with specified BGCs and opacity
            bgc_out
        })

        output$download_seedlot <- downloadHandler(
            filename = "Seedlot_Options.csv",
            content = function(file){
                bgc_sel <- input$bgc_choose
                sl_query <- paste0("select seedlot, genetic_class, orchard_no, bec
                        from cbst_seedlot
                        join cbst_pl on cbst_seedlot.bec = cbst_pl.seed_bec
                        where plant_bec in ('",paste(bgc_sel, collapse = "','"),
                                   "') and htp >= ",input$matchprop," and species like 'PL%'")
                seedlot_out <- dbGetQuery(dbCon, sl_query)
                fwrite(seedlot_out,file)
            }
        )

    })
}

shinyApp(ui = ui, server = server)
