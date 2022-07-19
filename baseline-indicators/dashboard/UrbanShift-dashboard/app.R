library(shiny)
library(plotly)
library(leaflet)
library(leaflet.extras) 
library(plyr)
library(dplyr)
library(rgdal)
library(shinyWidgets)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(sf)
library(rgeos)
library(httr)
library(jsonlite)
library(raster)
library(data.table)
library(DT)
library(leafem)

############### Load data

indicators_themes = c("Biodiversity", "Greenspace", "GHG emissions")


indicators_list = c("SICB-1: Percent of natural areas")

# read boundaries -----

boundary_georef = read.csv("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/v_0/boundary_georef.csv",
                           fileEncoding="UTF-8-BOM")

boundary_georef = boundary_georef %>%
    # recode empty strings "" by NAs
    na_if("") %>%
    # remove NAs
    drop_na(units_boundary_name)

cities = unique(boundary_georef$geo_name)




# read indicator ------------


indicators = read.csv("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/indicators/cities_indicators_df.csv",
                      encoding="UTF-8")

indicators_df = indicators %>% 
    mutate(`SICB-1-value` = round(`SICB.1_percent_natural_areas`,2)) %>% 
    mutate(`SICB-1-score`=
               case_when(`SICB-1-value` < 1 ~ "0",
                         `SICB-1-value` <7 ~ "1",
                         `SICB-1-value` <14 ~ "2",
                         `SICB-1-value` <20 ~ "3",
                         `SICB-1-value` >= 20 ~ "4")) %>% 
    dplyr::select(geo_id,`SICB-1-value`,`SICB-1-score`)


# define color palette for WOrld cover -----
Trees_10_green = "#006400"
Shrubland_20_orange = "#ffbb22"
Grassland_30_yellow = "#ffff4c" 
Cropland_40_mauve = "#f096ff"
Built_up_50_red = "#fa0000"
Barren_sparse_vegetation_60_gray = "#b4b4b4"
Snow_ice_70_white = "#f0f0f0"
Open_Water_80_blue = "#0064c8"
Herbaceous_wetland_90_blue2 = "#0096a0"
Mangroves_95_green2 = "#00cf75"
Moss_lichen_100_beige = "#fae6a0"

worldcover_col = c(Trees_10_green,
                   Shrubland_20_orange,
                   Grassland_30_yellow,
                   Cropland_40_mauve,
                   Built_up_50_red,
                   Barren_sparse_vegetation_60_gray,
                   Snow_ice_70_white,
                   Open_Water_80_blue,
                   Herbaceous_wetland_90_blue2,
                   Mangroves_95_green2,
                   Moss_lichen_100_beige)

worldcover_labels = c('Trees','Shrubland','Grassland','Cropland','Built-up',
                      'Barren / sparse vegetation','Snow/ice','Open water','Herbaceous wetland',
                      'Mangroves','Moss/lichen')


# define color palette for scores -----
pal_score <- colorFactor(palette = c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"), 
                         levels = c("0","1","2","3","4"),
                         na.color = "transparent",
                         revers = TRUE)


############### App

ui = navbarPage("UrbanShift",
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
                                                selected = cities[9],
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
                                                   selected = indicators_list[1],
                                                   multiple = FALSE,
                                                   width = '100%'),
                                    
                                    # Main indicators
                                    
                                    h5("Cty wide level: "),
                                    htmlOutput("city_wide_indicator"),
                                    ),
                             ### Specify plots ----
                             column(8, 
                                    div(style = "background-color: red; width: 100%; height: 100%;"),
                                    tabsetPanel(type = "tabs",
                                                ### Map plot 
                                                tabPanel("Map", leafletOutput("indicator_map", height = 600)),
                                                ### Table plot 
                                                tabPanel("Table", DT::dataTableOutput("indicator_table"),
                                                         downloadButton(outputId = "downloadData", 
                                                                        label = "Download data")),
                                                ### Data description
                                                tabPanel("Definitions", plotlyOutput("inidcator_definition", height = 600))
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
    
    # # Update indicators based on selected theme
    # observeEvent(input$theme,{
    #     updateSelectInput(session,
    #                       'indicator',
    #                       choices=unique(indicators_definitions[indicators_definitions$theme==input$theme, "indicator_name"]),
    #                       selected = unique(indicators_definitions[indicators_definitions$theme==input$theme, "indicator_name"])[1],
    #     )
    # })
    
    
    observe({
        
        # update panel data when the panel is selected
        input$active_tab
        
        geo_name = input$city
        print(geo_name)
        
        # read boundaries -----
        aoi_boundary_name = boundary_georef[boundary_georef$geo_name == geo_name, "aoi_boundary_name"]
        units_boundary_name = boundary_georef[boundary_georef$geo_name == geo_name, "units_boundary_name"]
        
        aoi_boundary_name = aoi_boundary_name[1]
        units_boundary_name = units_boundary_name[1]
        
        boundary_aoi = st_read(paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/v_0/boundary-",
                                     geo_name,
                                     "-",
                                     aoi_boundary_name,
                                     ".geojson",
                                     sep = "")
        )
        
        boundary_unit = st_read(paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/v_0/boundary-",
                                      geo_name,
                                      "-",
                                      units_boundary_name,
                                      ".geojson",
                                      sep = "")
        )
        
        
        # join ----------------
        
        aoi_indicators = boundary_aoi %>% 
            # dplyr::select(geo_id) %>% 
            left_join(indicators_df, by = "geo_id")
        
        unit_indicators = boundary_unit %>% 
            # dplyr::select(geo_id) %>% 
            left_join(indicators_df, by = "geo_id")
        
        
        # read esa land cover ----
        worldcoverc_data_path = paste("/vsicurl/https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/esa_world_cover/v_0/",
                                      geo_name,
                                      "-",
                                      aoi_boundary_name,
                                      "-ESA-world_cover-2000.tif",
                                      sep = "")
        
        # raster natural-area-layer ----
        worldcover_natural_data_path = paste("/vsicurl/https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/esa_world_cover/v_0/",
                                             geo_name,
                                             "-",
                                             aoi_boundary_name,
                                             "-ESA-world_cover-2000-natural_areas.tif",
                                             sep = "")
        
        
        # map indicator ----
        
        
        output$indicator_map <- renderLeaflet({
            leaflet(boundary_aoi) %>% 
                addTiles() %>% 
                # setView(lat = city_boundary_centroid_lat, lng = city_boundary_centroid_lng, zoom = 5)
                fitBounds(~as.numeric(st_bbox(boundary_aoi)[1]),
                          ~as.numeric(st_bbox(boundary_aoi)[2]),
                          ~as.numeric(st_bbox(boundary_aoi)[3]),
                          ~as.numeric(st_bbox(boundary_aoi)[4]))
        })
        
        # define color palette for I1 levels
        pal_SCIB_1 <- colorNumeric(palette = "Greens", 
                                   domain = unit_indicators$`SICB-1-value`,
                                   na.color = "transparent",
                                   revers = FALSE)
        
        
        
        # define labels
        labels_SCIB_1 <- sprintf("<strong>%s</strong><br/>%s: %s %s<br/>%s: %s",
                                 unit_indicators$geo_name,
                                 "Proportion of natural areas",
                                 round(unit_indicators$`SICB-1-value`, 2), "",
                                 "Score",unit_indicators$`SICB-1-score`) %>% 
            lapply(htmltools::HTML)
        
        # plot map ----
        leafletProxy(mapId = "indicator_map")  %>%
            clearControls() %>%
            clearShapes() %>%
            # boundaries ----
        addPolygons(data = boundary_unit,
                    group = "Administrative boundaries",
                    stroke = TRUE, color = "black", weight = 2,dashArray = "3",
                    smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
                    highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.3,
                        bringToFront = TRUE),
                    label = boundary_unit$geo_name,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>% 
            # `SICB-1-value`
            addPolygons(data = unit_indicators,
                        group = "Porportion of natural areas - value",
                        fillColor = ~pal_SCIB_1(`SICB-1-value`),
                        weight = 1,
                        opacity = 1,
                        color = "grey",
                        fillOpacity = 0.7,
                        label = labels_SCIB_1,
                        highlightOptions = highlightOptions(color = "black", weight = 2,
                                                            bringToFront = FALSE),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 6px"),
                            textsize = "15px",
                            direction = "auto")) %>%
            addLegend(pal = pal_SCIB_1,
                      values = unit_indicators$`SICB-1-value`,
                      opacity = 0.9,
                      title = "Percent of natural areas (%)",
                      group = "Porportion of natural areas - value",
                      position = "topright",
                      labFormat = labelFormat(suffix = "")) %>%
            # SICB-1-score
            addPolygons(data = unit_indicators,
                        group = "Porportion of natural areas - score",
                        fillColor = ~pal_score(`SICB-1-score`),
                        weight = 1,
                        opacity = 1,
                        color = "grey",
                        fillOpacity = 0.7,
                        label = labels_SCIB_1,
                        highlightOptions = highlightOptions(color = "black", weight = 2,
                                                            bringToFront = FALSE),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 6px"),
                            textsize = "15px",
                            direction = "auto")) %>%
            # I1 score legend
            addLegend(colors =  c("#145A32","#2ECC71","#F4D03F","#E67E22","#C0392B"),
                      labels = c("4 (> 20.0%)",
                                 "3 (14.0% – 20.0%)",
                                 "2 (7.0% – 13.9%)",
                                 "1 (0% – 6.9%)",
                                 "0 (< 1.0%)"),
                      opacity = 1,
                      title = "Natural areas scores (%)",
                      group = "Porportion of natural areas - score",
                      position = "bottomright",
                      labFormat = labelFormat(suffix = "")) %>%
            # addGeotiff(file = worldcover_natural_data_path,
            #            opacity = 0.7,
            #            colorOptions = colorOptions(palette = c("Green","white"), na.color = "transparent"),
            #            group = "Natural land cover") %>% 
            addGeotiff(file = worldcoverc_data_path,
                       opacity = 0.9,
                       colorOptions = colorOptions(palette = worldcover_col, na.color = "transparent"),
                       group = "Land cover types") %>% 
            addLegend(colors = worldcover_col,
                      labels = worldcover_labels,
                      title = "World Cover",
                      group = "Land cover types",
                      position = "bottomleft",
                      opacity = 1) %>%
            # Layers control
            addLayersControl(
                overlayGroups = c("Porportion of natural areas - value",
                                  "Porportion of natural areas - score",
                                  "Natural land cover",
                                  "Land cover types",
                                  "Administrative boundaries"),
                options = layersControlOptions(collapsed = FALSE)
            ) %>% 
            hideGroup(c("Porportion of natural areas - score",
                        "Natural land cover",
                        "Administrative boundaries")) %>%
            addFullscreenControl()
        
        #########################################
        ### Main indicators ----
        
        # city wide  ----
        city_wide_indicator_value = aoi_indicators %>% 
            as.data.frame() %>% 
            pull("SICB-1-value")
        output$city_wide_indicator <- renderText({ 
            paste("<center>","<font size=5px; weight=500; color=\"#1E90FF\"><b>", city_wide_indicator_value, "%")
        })
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
