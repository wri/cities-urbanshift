library(shiny)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(plyr)
library(dplyr)
library(rgdal)
library(shinyWidgets)
library(rnaturalearth)
library(tidyverse)
library(sf)
library(rgeos)
library(httr)
library(jsonlite)
library(raster)
library(data.table)
library(DT)
library(leafem)
library(RColorBrewer)
library(shinydisconnect)

# define aws s3 path

aws_s3_path = "https://cities-urbanshift.s3.eu-west-3.amazonaws.com/"

############### Load data

# read indicator definition ------------

indicators_definitions = read.csv(paste(aws_s3_path,
                                        "indicators/indicators_definition.csv",
                                        sep = ""))

# get list of themes
indicators_themes = unique(indicators_definitions$theme)

# get list of indicators

indicators_list = unique(indicators_definitions$indicator_label)


# read boundaries georef -----

boundary_georef = read.csv(paste(aws_s3_path,
                                 "data/boundaries/v_0/boundary_georef.csv",
                                 sep = ""),
                           fileEncoding="UTF-8-BOM")

cities = unique(boundary_georef$geo_name)


# read indicator ------------


indicators = read.csv(paste(aws_s3_path,
                            "indicators/cities_indicators.csv",
                            sep = ""),
                      encoding="UTF-8")

indicators_test = read.csv(paste(aws_s3_path,
                                 "indicators/cities_indicators_v2_test-ted.csv",
                                 sep = ""),
                           encoding="UTF-8")

indicators = indicators %>% 
  left_join(indicators_test[,c("geo_id",
                               "SICB_2_habitat_connectivity")],
            by = "geo_id") 

indicators = indicators %>% 
  mutate(SICB_1_percent_of_natural_areas = 100 * SICB_1_percent_of_natural_areas,
         SICB_2_habitat_connectivity = 100 * SICB_2_habitat_connectivity) 


############### App

ui = navbarPage("UrbanShift-Dashboard",
                id = "active_tab",
                
                ### Indicators tab ----
                tabPanel("Indicators",
                         
                         ### Filters ----
                         fluidRow(
                           
                           
                           
                           column(3,
                                  
                                  ### Select city  ----
                                  selectInput(inputId = "city",
                                              label = "Select your city",
                                              choices = cities,
                                              selected = "CRI-San_Jose",
                                              width = '100%'),
                                  
                                  # select theme ----
                                  selectizeInput(inputId = "theme",
                                                 label = "Theme",
                                                 choices = indicators_themes,
                                                 selected = indicators_themes[1],
                                                 multiple = FALSE,
                                                 width = '100%'),
                                  
                                  # select indicator ----
                                  selectizeInput(inputId = "indicator",
                                                 label = "Select indicator",
                                                 choices = indicators_list,
                                                 selected = "Percent of Tree cover",
                                                 multiple = FALSE,
                                                 width = '100%'),
                                  
                                  # Main indicators
                                  
                                  h4("City wide level: "),
                                  htmlOutput("city_wide_indicator"),
                                  
                           ),
                           ### Specify plots ----
                           column(8,
                                  div(style = "background-color: red; width: 100%; height: 100%;"),
                                  tabsetPanel(type = "tabs",
                                              ### Map plot
                                              tabPanel("Map", 
                                                       leafletOutput("indicator_map", 
                                                                     height = 500),
                                                       # disconnect message
                                                       disconnectMessage(
                                                         text = "An error occurred due to the data volumetry. Please refresh the page and try again with another city.",
                                                         refresh = "Refresh",
                                                         background = "#FFFFFF",
                                                         colour = "#077D29",
                                                         refreshColour = "#337AB7",
                                                         overlayColour = "#000000",
                                                         overlayOpacity = 0.6,
                                                         width = 450,
                                                         top = 50,
                                                         size = 22),
                                                       actionButton("disconnect", 
                                                                    "Disconnect the dashboard",
                                                                    width = '30%',
                                                                    # class = "btn-warning",
                                                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                       )),
                                              ### Table plot
                                              tabPanel("Table", DT::dataTableOutput("indicator_table"),
                                                       downloadButton(outputId = "downloadData",
                                                                      label = "Download data")),
                                              ### timeseirs plot
                                              tabPanel("Chart", 
                                                       plotlyOutput("indicator_chart",
                                                                    height = 500)),
                                              ### Data description
                                              tabPanel("Definitions", htmlOutput("indicator_definition", 
                                                                                 height = 500))
                                  )
                           )
                         )
                ),
                
                ## Second tab panel ----
                tabPanel("Layers"
                )
                
                
                
)

# Define server
server <- function(input, output, session) {
  
  # disconnect message
  observeEvent(input$disconnect, {
    session$close()
  })
  
  # Update indicators based on selected theme
  observeEvent(input$theme,{
    updateSelectInput(session,
                      'indicator',
                      choices=unique(indicators_definitions[indicators_definitions$theme==input$theme, "indicator_label"]),
                      selected = unique(indicators_definitions[indicators_definitions$theme==input$theme, "indicator_label"])[1],
    )
  })
  
  
  
  
  observe({
    
    # update panel data when the panel is selected
    input$active_tab
    
    geo_name = input$city
    print(geo_name)
    
    # read boundaries -----
    aoi_boundary_name = boundary_georef[boundary_georef$geo_name == geo_name, "aoi_boundary_name"]
    units_boundary_name = boundary_georef[boundary_georef$geo_name == geo_name, "units_boundary_name"]
    
    boundary_aoi = st_read(paste(aws_s3_path,
                                 "data/boundaries/v_0/boundary-",
                                 geo_name,
                                 "-",
                                 aoi_boundary_name,
                                 ".geojson",
                                 sep = "")
    )
    
    boundary_unit = st_read(paste(aws_s3_path,
                                  "data/boundaries/v_0/boundary-",
                                  geo_name,
                                  "-",
                                  units_boundary_name,
                                  ".geojson",
                                  sep = "")
    )
    
    # join ----------------
    
    aoi_indicators = boundary_aoi %>%
      dplyr::select(geo_id) %>%
      left_join(indicators, by = "geo_id")
    
    unit_indicators = boundary_unit %>%
      dplyr::select(geo_id) %>%
      left_join(indicators, by = "geo_id")
    
    # get selected indicator
    
    selected_indicator_label = input$indicator
    
    selected_indicator_name = indicators_definitions %>% 
      filter(indicator_label %in% selected_indicator_label) %>% 
      pull(indicator_name)
    
    # get indicator values  -----
    
    selected_indicator_values = unit_indicators %>% 
      as.data.frame() %>% 
      pull(selected_indicator_name)
    
    # indicator color values ----
    
    pal_indicator<- colorNumeric(palette = "Greens", 
                                 domain = selected_indicator_values,
                                 na.color = "gray",
                                 revers = FALSE)
    
    # indicator labels for map ----
    
    labels_indicator <- sprintf("<strong>%s</strong><br/>%s: %s",
                                unit_indicators$geo_name,
                                selected_indicator_label,
                                round(selected_indicator_values, 2)) %>% 
      lapply(htmltools::HTML)
    
    
    # # read tml raster ----
    # 
    # tml_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/data/tree_cover/tree_mosaic_land/v_0/",
    #                       geo_name,
    #                       "-",
    #                       aoi_boundary_name,
    #                       "-TML-tree_cover-2000.tif",
    #                       sep = "")
    # 
    # # collect raster data
    # city_tml = raster(tml_data_path)
    # 
    # city_tml_boundary = raster::mask(city_tml,
    #                                  boundary_aoi)
    # city_tml_boundary[city_tml_boundary<11] = NA
    # 
    # # define color for tree cover
    # pal_tml <- colorNumeric(palette = "Greens",
    #                         domain = values(city_tml_boundary), 
    #                         na.color = "transparent")
    
    
    ########################
    # map indicator ----
    ########################
    
    # center map  
    output$indicator_map <- renderLeaflet({
      leaflet(boundary_aoi) %>%
        addTiles() %>%
        fitBounds(~as.numeric(st_bbox(boundary_aoi)[1]),
                  ~as.numeric(st_bbox(boundary_aoi)[2]),
                  ~as.numeric(st_bbox(boundary_aoi)[3]),
                  ~as.numeric(st_bbox(boundary_aoi)[4]))
    })
    
    
    
    # plot map ----
    leafletProxy(mapId = "indicator_map")  %>%
      clearControls() %>%
      clearShapes() %>%
      # boundaries ----
    addPolygons(data = boundary_aoi,
                group = "Administrative boundaries",
                stroke = TRUE, color = "black", weight = 3,dashArray = "3",
                smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.3,
                  bringToFront = TRUE),
                label = boundary_aoi$geo_name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
      # # Add TML raster
      # addRasterImage(city_tml_boundary, 
      #                colors = pal_tml,
      #                opacity = 0.9,
      #                maxBytes = 20 * 1024 * 1024,
      #                project=FALSE,
      #                group = "Tree cover") %>%
      # addLegend(pal = pal_tml,
      #           values = values(city_tml_boundary),
      #           title = "Tree cover",
      #           group = "Tree cover",
    #           position = "bottomleft") %>%
    # Percent of tree cover
    addPolygons(data = boundary_aoi,
                group = "Administrative boundaries",
                stroke = TRUE, color = "black", weight = 3,dashArray = "3",
                smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.3,
                  bringToFront = TRUE),
                label = boundary_aoi$geo_name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
      # Percent of tree cover
      addPolygons(data = unit_indicators,
                  group = selected_indicator_label,
                  fillColor = ~pal_indicator(selected_indicator_values),
                  weight = 1,
                  opacity = 1,
                  color = "grey",
                  fillOpacity = 0.8,
                  label = labels_indicator,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = FALSE),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 6px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal_indicator,
                values = selected_indicator_values,
                opacity = 0.9,
                title = selected_indicator_label,
                group = selected_indicator_label,
                position = "topright",
                labFormat = labelFormat(suffix = "")) %>%
      # Layers control
      addLayersControl(
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_label),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      # hideGroup(c("Tree cover")) %>% 
      addFullscreenControl()
    
    #########################################
    ### Main indicators ----
    
    # city wide  ----
    city_wide_indicator_value = aoi_indicators %>%
      as.data.frame() %>%
      pull(selected_indicator_name) %>% 
      round(2)
    
    output$city_wide_indicator <- renderText({
      paste("<center>","<font size=5px; weight=500; color=\"#168A06\"><b>", city_wide_indicator_value, "%")
    })
    
    #########################################
    ### Table ----
    
    # Table plot
    
    table_plot = unit_indicators %>% 
      drop_na(selected_indicator_name, geo_name) %>% 
      as.data.frame() %>%
      dplyr::select(-geometry) %>% 
      dplyr::select(geo_name,selected_indicator_name) %>% 
      mutate_if(is.numeric, round, 2) %>% 
      distinct(geo_name, .keep_all = T) %>% 
      arrange(desc(selected_indicator_name)) 
    
    
    names(table_plot) = c("City name",selected_indicator_label)
    
    output$indicator_table <- DT::renderDataTable(
      DT::datatable(table_plot, 
                    options = list(pageLength = 25)) %>% formatStyle(
                      selected_indicator_label,
                      backgroundColor = styleInterval(seq(from = min(table_plot[,selected_indicator_label]),
                                                          to = max(table_plot[,selected_indicator_label]),
                                                          length.out = 8), 
                                                      brewer.pal(9, "Greens")
                      ),
                      fontWeight = 'bold')
    )
    
    
    # output data to download
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$city,"-", input$indicator,"-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(table_plot, file)
      }
    )
    
    #########################################
    ### Chart ----
    
    output$indicator_chart <- renderPlotly({
      fig = plot_ly(x = table_plot$`City name`,
                    y = table_plot[[colnames(table_plot)[2]]],
                    type = "bar",
                    orientation = "v",
                    name = names(table_plot)[2],
                    color = I("green4")) %>% 
        layout(yaxis = list(title = names(table_plot)[2]),
               xaxis = list(title = 'Cities',categoryorder = "total descending"))
      
      fig
      
    })
    
    #########################################
    ### Indicator definition text  ----
    
    indicator_def_text = indicators_definitions %>% 
      filter(indicator_label == selected_indicator_label) %>% 
      pull(indicator_definition)
    
    indicator_data_sources = indicators_definitions %>% 
      filter(indicator_label == selected_indicator_label) %>%  
      pull(data_sources)
    
    indicator_importance = indicators_definitions %>% 
      filter(indicator_label == selected_indicator_label) %>%  
      pull(importance)
    
    indicator_methods = indicators_definitions %>% 
      filter(indicator_label == selected_indicator_label) %>%  
      pull(methods)
    
    # plot text 
    output$indicator_definition <- renderText({
      paste("<right>","<font size=3px; weight=100; color=\"#168A06\"><b>",
            "<font color=\"#168A06\"><b>", " ", "<br>",
            "<font color=\"#168A06\"><b>","Definition: ",
            "<font color=\"#454545\"><b>", indicator_def_text,
            "<br/>",
            "<font color=\"#168A06\"><b>", " ", "<br>",
            "<font color=\"#168A06\"><b>","Data sources: ",
            "<font color=\"#454545\"><b>", indicator_data_sources,
            "<br/>",
            "<font color=\"#168A06\"><b>", " ", "<br>",
            "<font color=\"#168A06\"><b>","Importance: ",
            "<font color=\"#454545\"><b>", indicator_importance,
            "<br/>",
            "<font color=\"#168A06\"><b>", " ", "<br>",
            "<font color=\"#168A06\"><b>","Methods: ",
            "<font weight=50; color=\"#454545\"><b>", indicator_methods
      )
    })
    
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)