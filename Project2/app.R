library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(RColorBrewer)
library(devtools)
library(usethis)
library(rsconnect)
library(plotly)
library(ggplot2)
library(DT)
library(tidyr)
library(ECharts2Shiny)
library(shinyalert)
library(shinycssloaders)
library(shinyWidgets)
library(rgeos)
library(rgdal)
library(sp)
library(leaflet.extras)



bikes <- readOGR("http://data.phl.opendata.arcgis.com/datasets/b5f660b9f0f44ced915995b6d49f6385_0.geojson")
#bikepalette <- colorFactor(palette() , levels = levels(bikes$TYPE))

facilities <- readOGR("http://data.phl.opendata.arcgis.com/datasets/b3c133c3b15d4c96bcd4d5cc09f19f4e_0.geojson")


ui <-dashboardPage(
    #fluidPage(useShinyalert())
    dashboardHeader(title = "Leaflet Crawler")
    ## Sidebar content
    #arealist <- append(arealist, "All", after =  0)
    ,dashboardSidebar(
        # Create sidebar menu with menu items, Filtering options, legend display, and submission button
        sidebarMenu(
            selectInput(inputId = "lanetype",label =  "Bike Lane Type", choices = levels(bikes$TYPE), selected = "Conventional" ,multiple = T)
            , selectInput(inputId = "assettype",label =  "Facility Asset Type", choices = levels(facilities$ASSET_TYPE), selected = "Building" ,multiple = T)
            #color palette building options
            #,selectInput("colors", "Color Scheme", rownames(subset(brewer.pal.info, category %in% c("seq", "div"))))
            ,actionButton("submit", label = "Submit")
            # Download buttons for entire filtered data sets
            ,downloadButton("downloadBikes", "Download Bikes Data")
            ,br()
            ,downloadButton("downloadFacilities", "Download Facilites Data")
            ,checkboxInput("legend", "Show legend", TRUE)
            )
        )
    
    ,dashboardBody( #width = 400, height = 400, 
        mainPanel(
            tabsetPanel(
                tabPanel("Map of Philadelphia Public Resources"
                         ,leafletOutput("map"
                                        ,width="100%"
                                        ) %>% 
                             withSpinner(color="#0dc5c1")
                         )
                ,tabPanel("Bikes"
                          ,fluidRow(plotlyOutput("plot1") %>% 
                                        withSpinner(color="#0dc5c1"))
                          ,fluidRow(checkboxInput(inputId = "show_data1"
                                                   ,label = "Show data table"
                                                   ,value = TRUE))
                          ,fluidRow(
                              #div(style = 'overflow-x: scroll'
                                        # Show data table ---------------------------------------------
                                        box(width = 12, DT::dataTableOutput(outputId = "dataTable1") %>% 
                                                withSpinner(color="#0dc5c1"))
                                        #)
                                    )
                          )
                ,tabPanel("Facilities"
                          , fluidRow(plotlyOutput("plot2") %>% 
                                         withSpinner(color="#0dc5c1"))
                          , fluidRow(checkboxInput(inputId = "show_data2"
                                                   ,label = "Show data table"
                                                   ,value = TRUE))
                          ,fluidRow(#div(style = 'overflow-x: scroll'
                                        # Show data table ---------------------------------------------
                                        box(width = 12, DT::dataTableOutput(outputId = "dataTable2") %>% 
                                                withSpinner(color="#0dc5c1"))
                          #)
                          )
                )
                )
            )
        )
    )

server <- function(input, output, session) {
    
    
    # bike lanes filtered data
    filteredBikes <- reactive({
        filtlane <- bikes
        
        req(input$lanetype)
        
        if (length(input$lanetype) > 0) {
            filtlane <- subset(filtlane, TYPE %in% input$lanetype)
        }
        
        return(filtlane)
    })
    
    
    # Reactive expression for filtered facilities data
    filteredFacilities <- reactive({
        facilities_data <- facilities
        
        req(input$assettype)
        # Boros
        filtfac <- subset(facilities_data, ASSET_TYPE %in% input$assettype)
        
        # Sewer type
        if (length(input$assettype) > 0) {
            filtfac <- subset(filtfac, ASSET_TYPE %in% input$assettype)
        }

        return(filtfac)
    })
    
    View(class(filteredFacilities()))
    
    
    output$map <- renderLeaflet({
# Use leaflet() here, and only include aspects of the map that won't need to change dynamically (at least, not unless the entire map is being torn down and recreated).
        leaflet() %>% 
            setView(-75.1652, 39.9526, 9) %>%
            addProviderTiles("Esri.WorldStreetMap", group = "Street") %>%
            addProviderTiles(providers$Stamen.Terrain, group = 'Terrain') %>%
            addLayersControl(baseGroups = c("Street", "Terrain") 
                              ,overlayGroups = c("Bike Lanes", "Facilities")
                              ,options = layersControlOptions(collapsed = FALSE))
    })
    
    # Incremental changes to the map (in this case, replacing the lines when a bike lane type is chosen).
    observeEvent(input$submit,{
        bikesfiltered <- filteredBikes()
        bikepalette <- colorFactor(palette() , levels = levels(bikesfiltered$TYPE))

        leafletProxy("map", data = bikesfiltered) %>%
            clearGroup(group = "Bike Lanes") %>%
            addPolylines(data = bikesfiltered,  color = ~bikepalette(TYPE), group = "Bike Lanes") 
            #clearControls() %>%
            #addLegend(position = "bottomright", pal = bikepalette, values = ~TYPE, title = "Bike Lane Types", group = "Bike Lanes")
    })
    
    # Add in dynamic legeneds
    observe({        
        bikesfiltered <- filteredBikes()
        bikepalette <- colorFactor(palette() , levels = levels(bikesfiltered$TYPE))
        
        if(input$legend){
    leafletProxy("map", data = bikesfiltered) %>%
        clearControls() %>%
        addLegend(position = "bottomright", pal = bikepalette, values = ~TYPE, title = "Bike Lane Types", group = "Bike Lanes")
        }
        else{leafletProxy("map", data = bikesfiltered) %>%
                clearControls()}
    })
    
    # # Replace layer with filtered philly publuc facilities
    observeEvent(input$submit,{
        facilitiesFiltered <- filteredFacilities()
        # Data is filtered facilities
        leafletProxy("map", data = facilitiesFiltered@data) %>%
            clearGroup(group = "Facilities") %>%
            addMarkers(clusterOptions = markerClusterOptions(), data = facilitiesFiltered@data, group = "Facilities", label = facilities$ASSET_NAME)
    })
    
    # Create histogram object for Bike lane type count 
    output$plot1 <- renderPlotly({
        ggplotly(
            ggplot(data = filteredBikes()@data, aes(TYPE))+
                geom_histogram(stat = "count") +
                theme(axis.text.x = element_text(angle = 90))
        )
    })
    
    # Create histogram object for facilities asset grouping 
    output$plot2 <- renderPlotly({
        ggplotly(
            ggplot(data = filteredFacilities()@data, aes(ASSET_GROUP1))+
                geom_histogram(stat = "count") +
                theme(axis.text.x = element_text(angle = 90))
        )
    })
    
    # Print bikes data table if checked -------------------------------------
    output$dataTable1 <- DT::renderDataTable(
        if(input$show_data1){
            DT::datatable(data = bikes@data
                          # Enable Buttons --------------------------------
                          ,extensions = 'Buttons'
                          ,options = list(pageLength = 10,
                                          # Turn off search ----------------
                                          dom = "Btp",
                                          # Buttons available --------------
                                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                          scrollX = TRUE 
                                              #'400px'
                          )
                          ,rownames = FALSE
            )  %>% 
                
                # Format text example ---------------------------------------
            formatStyle(
                columns = 5, 
                valueColumns = 5, 
                color = styleEqual(c("R","G", "PG", "PG-13"), c("red", "green", "blue", "yellow"))
            ) %>%
                
                # Format background example ---------------------------------
            formatStyle(
                columns = 8,
                #background = styleColorBar(range(bikes@data), '#cab2d6'),
                backgroundSize = '90% 85%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center')
        }
    )
    
    # Print Facilities data table if checked -------------------------------------
    output$dataTable2 <- DT::renderDataTable(
        if(input$show_data2){
            DT::datatable(data = facilities@data
                          # Enable Buttons --------------------------------
                          ,extensions = 'Buttons'
                          ,options = list(pageLength = 10,
                                          # Turn off search ----------------
                                          dom = "Btp",
                                          # Buttons available --------------
                                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                          scrollX = TRUE
                                          #scrollY = "400px"
                          )
                          ,rownames = FALSE
            )  %>% 
                
                # Format text example ---------------------------------------
            formatStyle(
                columns = 5, 
                valueColumns = 5, 
                color = styleEqual(c("R","G", "PG", "PG-13"), c("red", "green", "blue", "yellow"))
            ) %>%
                
                # Format background example ---------------------------------
            formatStyle(
                columns = 8,
                #background = styleColorBar(range(facilities@data), '#cab2d6'),
                backgroundSize = '90% 85%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center')
        }
    )
    
    # Downloadable csv of selected bike dataset ----
    output$downloadBikes <- downloadHandler(
        filename = paste("Bikes_", as.character(input$selected_type), ".csv", sep = ""),
        # modify to only pull for selected options
        content = function(filename) {
            write.csv(filteredBikes(), filename, row.names = FALSE)
        }
    )
    
    # Downloadable csv of selected facilities dataset ----
    output$downloadFacilities <- downloadHandler(
        filename = paste("Bikes_", as.character(input$selected_type), ".csv", sep = ""),
        # modify to only pull for selected options
        content = function(filename) {
            write.csv(filteredFacilites(), filename, row.names = FALSE)
        }
    )
}

shinyApp(ui, server)