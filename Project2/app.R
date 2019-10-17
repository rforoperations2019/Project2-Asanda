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


ui <-dashboardPage(
    #fluidPage(useShinyalert())
    dashboardHeader(title = "Leaflet Crawler")
    ## Sidebar content
    #arealist <- append(arealist, "All", after =  0)
    ,dashboardSidebar(
        # Create sidebar menu with menu items, Filtering options, and submission button
        sidebarMenu(
            selectInput(inputId = "lanetype",label =  "Bike Lane Type", choices = append(levels(bikes$TYPE), "All", after = 0),selected = "All" ,multiple = T)
            , selectInput(inputId = "assettype",label =  "Facility Asset Type", choices = append(levels(facilities$ASSET_TYPE), "All", after = 0),selected = "All" ,multiple = T)
            ,selectInput("colors", "Color Scheme", rownames(subset(brewer.pal.info, category %in% c("seq", "div"))))
            ,actionButton("submit", label = "Submit")
            ,checkboxInput("legend", "Show legend", TRUE)
            )
        )
    ,dashboardBody(
        #fluidPage(leafletOutput("map"))
        mainPanel(
            tabsetPanel(
                tabPanel("Map of Philadelphia Public Resources"
                         ,leafletOutput("map",width="200%",height="400px")
                         )
                ,tabPanel("Bikes"
                          , fluidRow(plotlyOutput("plot1") %>% withSpinner(color="#0dc5c1"))
                          , fluidRow(checkboxInput(inputId = "show_data1"
                                                   ,label = "Show data table"
                                                   ,value = TRUE))
                          ,fluidRow(div(style = 'overflow-x: scroll'
                                        # Show data table ---------------------------------------------
                                        ,DT::dataTableOutput(outputId = "dataTable1") %>% withSpinner(color="#0dc5c1")
                                        )
                                    )
                          )
                ,tabPanel("Facilities"
                          , fluidRow(plotlyOutput("plot2") %>% withSpinner(color="#0dc5c1"))
                          , fluidRow(checkboxInput(inputId = "show_data2"
                                                   ,label = "Show data table"
                                                   ,value = TRUE))
                          ,fluidRow(div(style = 'overflow-x: scroll'
                                        # Show data table ---------------------------------------------
                                        ,DT::dataTableOutput(outputId = "dataTable2") %>% withSpinner(color="#0dc5c1")
                          )
                          )
                )
                )
            )
        )
    )

server <- function(input, output, session) {
    
    bikes <- readOGR("http://data.phl.opendata.arcgis.com/datasets/b5f660b9f0f44ced915995b6d49f6385_0.geojson")
    #bikepalette <- colorFactor(palette() , levels = levels(bikes$TYPE))
    
    facilities <- readOGR("http://data.phl.opendata.arcgis.com/datasets/b3c133c3b15d4c96bcd4d5cc09f19f4e_0.geojson")
    
    
    # bike lanes filtered data
    filteredBikes <- reactive({
        bikes_data <- bikes
        
        req(input$assettype)
        # Boros
        filtlane <- subset(bikes_data, ASSET_TYPE == input$lanetype)
        
        # Sewer type
        if (length(input$lanetype) > 0) {
            filtlane <- subset(filtlane, bikes_data %in% input$lanetype)
        }
        
        return(filtlane)
    })
    
    # This reactive expression represents the palette function,
    # which changes as the user makes selections in UI.
    colorpal <- reactive({
        colorNumeric(input$colors, bikes$TYPE)
    })
    
    # Reactive expression for the data subsetted to what the user selected
    filteredBikes <- reactive({
        
        req(input$lanetype)
        
        if(input$lanetype == "All") {
            lanefilt <- quote(TYPE != "@?><")
        } else {
            lanefilt <- quote(TYPE == input$lanetype) 
        }
        
        bikes@data %>%
            filter_(lanefilt)
        
        #quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]   
    })
    
    
    # Facilities Filtered data
    filteredFacilities <- reactive({
        facilities_data <- facilities
        
        req(input$assettype)
        # Boros
        filtfac <- subset(facilities_data, ASSET_TYPE == input$assettype)
        
        # Sewer type
        if (length(input$assettype) > 0) {
            filtfac <- subset(filtfac, ASSET_TYPE %in% input$assettype)
        }

        return(filtfac)
    })
    
    # This reactive expression represents the palette function,
    # which changes as the user makes selections in UI.
    # colorpal <- reactive({
    #     colorNumeric(input$colors, bikes$TYPE)
    # })
    
    
    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        leaflet() %>% 
            addProviderTiles("Esri.WorldStreetMap", group = "Street") %>%
            addProviderTiles(providers$Stamen.Terrain, group = 'Terrain') %>%
            addLayersControl(baseGroups = c("Street", "Terrain") 
                              ,overlayGroups = c("Bike Lanes", "Districts", "Facilities")
                              ,options = layersControlOptions(collapsed = FALSE)) #%>%
            # addPolylines(data = filteredBikes(),color = ~colorpal()(TYPE), group = "Bike Lanes", weight = 3) %>%
            # addLegend(position = "topright" , pal = colorpal(), values = bikes$TYPE, title = "Lane Type", group = "Bike Lanes)
            #fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    })
    
    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    # observeEvent(input$submit,{
    #     bikesfiltered <- filteredBikes()
    #     bikepalette <- colorFactor(palette() , levels = levels(bikesfiltered$TYPE))
    # 
    #     leafletProxy("map", data = bikesfiltered) %>%
    #         clearGroup(group = "Bike Lanes") %>%
    #         addPolylines(data = bikesfiltered, color = ~bikepalette(TYPE), group = "Bike Lanes")
    # })
    
    # Replace layer with filtered greenInfrastructure
    observeEvent(input$submit,{
        facilitiesFiltered <- filteredFacilities()
        # Data is filtered facilities
        leafletProxy("map", data = facilitiesFiltered@data) %>%
            clearGroup(group = "Facilities") %>%
            addMarkers(clusterOptions = markerClusterOptions(), data = facilitiesFiltered@data, group = "Facilities", label = facilities$ASSET_NAME)
    })
    # 
    
    # # Use a separate observer to recreate the legend as needed.
    # observe({
    #     proxy <- leafletProxy("map", data = data)
    #     
    #     # Remove any existing legend, and only if the legend is
    #     # enabled, create a new one.
    #     proxy %>% clearControls()
    #     if (input$legend) {
    #         pal <- colorpal()
    #         proxy %>% addLegend(position = "bottomright",
    #                             pal = pal, values = ~mag
    #         )
    #     }
    # })
    
    # Create scatterplot object for energy usage 
    output$plot1 <- renderPlotly({
        ggplotly(
            ggplot(data = filteredBikes(), aes(TYPE))+
                geom_histogram(stat = "count") +
                theme(axis.text.x = element_text(angle = 90))
            # ggplot(data = bikes, aes_string(x = mtcars$wt, y = mtcars$mpg, color = mtcars$carb))
            # +geom_point()
            # +theme(axis.text.x = element_text(angle = 90))
        )
    })
    
    # Create scatterplot object for energy usage 
    output$plot2 <- renderPlotly({
        ggplotly(
            ggplot(data = filteredFacilities()@data, aes(ASSET_GROUP1))+
                geom_histogram(stat = "count") +
                theme(axis.text.x = element_text(angle = 90))
            # ggplot(data = mtcars, aes_string(x = mtcars$wt, y = mtcars$mpg, color = mtcars$carb))
            # +geom_point()
            # +theme(axis.text.x = element_text(angle = 90))
        )
    })
    
    # Print data table 1 if checked -------------------------------------
    output$dataTable1 <- DT::renderDataTable(
        if(input$show_data1){
            DT::datatable(data = bikes@data
                          # Enable Buttons --------------------------------
                          ,extensions = 'Buttons'
                          ,options = list(pageLength = 10,
                                          # Turn off search ----------------
                                          dom = "Btp",
                                          # Buttons available --------------
                                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
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
                #background = styleColorBar(range(CEnergy), '#cab2d6'),
                backgroundSize = '90% 85%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center')
        }
    )
    
    # Print data table 2 if checked -------------------------------------
    output$dataTable2 <- DT::renderDataTable(
        if(input$show_data2){
            DT::datatable(data = facilities@data
                          # Enable Buttons --------------------------------
                          ,extensions = 'Buttons'
                          ,options = list(pageLength = 10,
                                          # Turn off search ----------------
                                          dom = "Btp",
                                          # Buttons available --------------
                                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
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
                #background = styleColorBar(range(CEnergy), '#cab2d6'),
                backgroundSize = '90% 85%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center')
        }
    )
}

shinyApp(ui, server)