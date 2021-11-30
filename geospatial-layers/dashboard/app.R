library(shiny)
library(plotly)
library(leaflet)
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


######################################
# Get city administrative boundaries
######################################


aws.get.city.boundary = function(city_id){
    
    # define path
    city_boundary_path = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/",city_id,"-boundary.geojson",
                               sep = "")
    
    # read the data#
    city_boundary <- st_read(city_boundary_path,
                             quiet = TRUE)
    return(city_boundary)
    
}


#################### Get Dynamic World Land Cover data

aws.get.dw.by.year = function(year, city_id, data_source){
    
    
    # get city boundary
    city_boundary = aws.get.city.boundary(city_id = city_id)
    
    # get local path
    # local_path = paste(getwd(),"/data/land_use/sentinel2_ndvi/",sep ="")
    local_path = "C:/Users/saifs/Documents/urbanshift/data/land_use/dynamic_world/"
    aws_path = "https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/dynamic_world/"
    
    if(data_source == "local"){
        path = local_path}
    
    else if(data_source == "aws"){
        path = aws_path
    }
    
    # define path to collect dw land cover
    # city_landcover_path = paste("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/land_use/dynamic_world/", city_id, "-landcover", year,".tif",
    #                             sep = "")
    
    # define path
    city_landcover_path = paste(path, city_id,"-landcover",year,".tif",
                                sep = "")
    
    # collect raster data
    city_landcover_raster= raster(city_landcover_path)
    
    # mask raster based on administrative boundaries
    city_landcover_mask  = raster::mask(city_landcover_raster,city_boundary)
    
    return(city_landcover_mask)
}


# define param for plot
# define colors for each class

water_blue_0 = "#419BDF"
trees_green_dark_1 = "#397D49"
grass_green_light_2 = "#88B053" 
flooded_veg_mauve_3 = "#7A87C6" 
crops_orange_4 = "#E49635" 
scrub_yellow_5 = "#DFC35A" 
built_area_red_6 = "#C4281B" 
bare_gray_7 = "#A59B8F" 
snow_ice_mauve_light_8 = "#B39FE1" 

# define color vector
landcover_col = c(water_blue_0,
                  trees_green_dark_1,
                  grass_green_light_2,
                  flooded_veg_mauve_3,
                  crops_orange_4,
                  scrub_yellow_5,
                  built_area_red_6,
                  bare_gray_7,
                  snow_ice_mauve_light_8)



# define labels
labels_landcover = c('Water','Trees','Grass','Flooded vegetation','Crops',
                     'Scrub/shrub','Built Area','Bare ground','Snow/Ice')


############### App

ui = navbarPage("UrbanShift-Dashboard",
                id = "active_tab",
                
                ### Amenity exposure panel ----
                tabPanel("Geospatial Layers",
                         
                         ### First row ----
                         fluidRow(
                             ### Specify filters ----
                             column(3,
                                    
                                    
                                    ### Specify city 
                                    selectInput(inputId = "layer_city", 
                                                label = "Select your city", 
                                                choices = c("RWA-Kigali","CRI-San_Jose"),
                                                selected = "RWA-Kigali",
                                                width = '100%'
                                    ),
                                    
                                    ### Specify Theme 
                                    selectInput(inputId = "layer_theme", 
                                                label = "Select a theme", 
                                                choices = "Land use/Land cover",
                                                selected = "Land use/Land cover",
                                                multiple = FALSE,
                                                width = '100%'
                                    ),
                                    
                                    ### Specify layers 
                                    selectInput(inputId = "layer_data", 
                                                label = "Select layer", 
                                                choices = "Dynamic World land cover",
                                                selected = "Dynamic World land cover",
                                                multiple = FALSE,
                                                width = '100%'
                                    ),
                                    
                                    ### Specify Period 
                                    selectInput(inputId = "layer_year", 
                                                label = "Select year of interest", 
                                                choices = "2020",
                                                selected = "2020",
                                                multiple = FALSE,
                                                width = '100%'
                                    )
  
                                    
                             ),
                             ### Specify plots ----
                             column(8, 
                                    div(style = "background-color: red; width: 100%; height: 100%;"),
                                    tabsetPanel(type = "tabs",
                                                ### Map plot 
                                                tabPanel("Map", leafletOutput("layer_map_plot", height = 600)),
                                                ### timeseirs plot 
                                                tabPanel("Time serie", plotlyOutput("ts_plot", height = 600))
                                    )
                             )
                         ),
                         
                         
                         ## Second row ----
                         fluidRow(
                             column(12, offset = 0, h4("Observations"), DT::dataTableOutput("table_layer"))
                         )
                ),
                ## Baseline indicators panel ----
                tabPanel("Baseline indicators",
                         
                         ### First row ----
                         fluidRow(
                             ### Specify filters ----
                             column(3,
                                    
                                    
                                    ### Specify city 
                                    selectInput(inputId = "indicators_city", 
                                                label = "Select your city", 
                                                choices = "CRI-San_Jose",
                                                selected = "CRI-San_Jose",
                                                width = '100%'
                                    ),
                                    
                                    ### Specify Theme 
                                    selectInput(inputId = "indicators_theme", 
                                                label = "Select a theme", 
                                                choices = "Greenspace",
                                                selected = "Greenspace",
                                                multiple = FALSE,
                                                width = '100%'
                                    ),
                                    
                                    ### Specify indicator 
                                    selectInput(inputId = "indicators_kpi", 
                                                label = "Select indicator", 
                                                choices = "Vegetation land percent (DW)",
                                                selected = "Vegetation land percent (DW)",
                                                multiple = FALSE,
                                                width = '100%'
                                    ),
                                    
                                    ### Specify Period 
                                    selectInput(inputId = "indicators_year", 
                                                label = "Select year of interest", 
                                                choices = "2020",
                                                selected = "2020",
                                                multiple = FALSE,
                                                width = '100%'
                                    )
                                    
                                    
                                    
                             ),
                             ### Specify plots ----
                             column(8,
                                    div(style = "background-color: red; width: 100%; height: 100%;"),
                                    tabsetPanel(type = "tabs",
                                                ### Map plot
                                                tabPanel("City indicator", leafletOutput("indicators_city", height = 600))
                                                ### timeseirs plot
                                                # tabPanel("Vulnerability", plotlyOutput("Sector_plot_pop", height = 600))
                                    )
                             )
                         ),
                         
                         
                         # ## Second row ----
                         # fluidRow(
                         #     column(12, offset = 0, h4("Observations"), DT::dataTableOutput("table_population"))
                         # )
                )
)


server <- function(input, output, session) {
    
    
    ### leaflet map definition ----
    ### Country
    
    # get city boundary
    # city_boundary = aws.get.city.boundary(city_id = input$layer_city)
    # 
    # city_boundary_centroid <- st_centroid(filtereData)
    # city_boundary_centroid_lat = city_boundary_centroid$geometry[[1]][1]
    # city_boundary_centroid_lng = city_boundary_centroid$geometry[[1]][2]
    # 
    # output$layer_map_plot <- renderLeaflet({
    #     leaflet() %>%
    #         setView(lng = city_boundary_centroid_lng, lat = city_boundary_centroid_lat, zoom = 10)
    # })
    
    # selected_city = input$City
    # selected_sector = input$Sector
    # 
    # amenity_city_selected = amenity_exposure_lst %>% 
    #     filter(city_name == selected_city,
    #            sector_name %in% selected_sector)
    
    
    
    # Reactive expression for the data subsetted to what the user selected
    # filtereData <- reactive({
    #     city_boundary[city_boundary$id == input$layer_city, ]
    # })
    # 
    # 
    # 
    # 
    # output$layer_map_plot <- renderLeaflet({
    #     leaflet(filtereData()) %>%
    #         addTiles() %>%
    #         # setView(lat = -1.937144, lng = 30.12304, zoom = 10)
    #         setView(lat = city_boundary_centroid_lat, lng = city_boundary_centroid_lng, zoom = 10)
    # })

    
    
    #update sector list depending on selected city
    # observeEvent(
    #     input$City,
    #     updateSelectInput(session, "Sector", "Sector",
    #                       choices = unique(amenity_exposure_lst[amenity_exposure_lst$city_name == input$City, "sector_name"]) 
    #                       # choices = amenity_exposure_lst %>%
    #                       #     filter(city_name == input$City) %>%
    #                       #     distinct(sector_name) %>%
    #                       #     pull(sector_name)
    #     )
    # )
    
    ### reactive plots definition ----
    observe({
        
        ### Layers tab ----
        
        
        # filter data
        layer_selected_city = input$layer_city
        layer_selected_theme = input$layer_theme
        layer_selected_layer = input$layer_data
        layer_selected_year = input$layer_year
        
        # get city boundary
        city_boundary = aws.get.city.boundary(city_id = layer_selected_city)
        
        # get DW landcover
        city_landcover_dw = aws.get.dw.by.year(year = layer_selected_year, 
                                                    city_id = layer_selected_city, 
                                                    data_source = "aws")
        
        city_boundary_centroid <- st_centroid(city_boundary)
        city_boundary_centroid_lat = city_boundary_centroid$geometry[[1]][1]
        city_boundary_centroid_lng = city_boundary_centroid$geometry[[1]][2]
        
        output$layer_map_plot <- renderLeaflet({
            leaflet() %>%
                setView(lat = city_boundary_centroid_lng, lng = city_boundary_centroid_lat, zoom = 10)
        })
        
        # define a color palette
        pal_landcover <- colorFactor(landcover_col, 
                                     values(city_landcover_dw),
                                     na.color = "transparent")
        
        
        # plot map
        leafletProxy(mapId = "layer_map_plot", data= city_boundary)  %>%
            # Base groups
            addTiles(group = "OSM (default)") %>%
            addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB") %>%
            addTiles() %>%
            clearControls() %>%
            clearShapes() %>%
            # plot boundary
            addPolygons(data = city_boundary,
                        group = "Administrative boundaries",
                        stroke = TRUE, color = "black", weight = 3,dashArray = "3",
                        smoothFactor = 0.3, fill = FALSE, fillOpacity = 0.5,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.3,
                            bringToFront = TRUE),
                        label = city_boundary$city_name,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")) %>%
            addRasterImage(city_landcover_dw, 
                           colors = pal_landcover, 
                           opacity = 0.7,
                           group = "Land Cover") %>%
            addLegend(colors = landcover_col,
                      labels = labels_landcover,
                      title = "Land Cover") %>%  
            addLayersControl(
                baseGroups = c("OSM (default)", "CartoDB"),
                overlayGroups = c("Land Cover",
                                  "Amenity exposure class"),
                options = layersControlOptions(collapsed = FALSE)
            )
        
        ### Baseline indicators panel ----
        
        # # filter data
        # selected_city = input$City_pop
        # selected_sector = input$Sector_pop
        # 
        # # filter heat threshold
        # city_lst_mask_threshold = city_lst_mask
        # 
        # # get heat threshold
        # heat_threshold_value = input$heat_threshold_pop
        # 
        # values(city_lst_mask_threshold)[values(city_lst_mask_threshold) <= heat_threshold_value] = 0
        # values(city_lst_mask_threshold)[values(city_lst_mask_threshold) > heat_threshold_value] = 1
        # 
        # 
        # # filter population category
        # selected_population = input$Category_pop
        # 
        # # get filtered population category
        # city_pop_mask = read.pop.category(pop_category = selected_population)
        # 
        # # create exposed population raster
        # city_pop_heat_exposure = city_pop_mask * city_lst_mask_threshold
        # 
        # # filter boundary data
        # city_boundary = boundary %>% 
        #     filter(city_id == selected_city_id)
        # 
        # pal_Grid_pop <- colorNumeric("RdYlBu", 
        #                              values(city_pop_mask),
        #                              na.color = "transparent",
        #                              reverse = TRUE)
        # 
        # pal_Grid_pop_exposure <- colorNumeric("RdYlBu", 
        #                                       values(city_pop_heat_exposure),
        #                                       na.color = "transparent",
        #                                       reverse = TRUE)
        # 
        # # plot map
        # leafletProxy(mapId = "Map_plot_pop", data = filtereData())  %>%
        #     # Base groups
        #     addTiles(group = "OSM (default)") %>%
        #     addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB") %>%
        #     addTiles() %>%
        #     clearControls() %>%
        #     clearShapes() %>%
        #     # plot boundary
        #     addPolygons(data = city_boundary,
        #                 group = "Administrative boundaries",
        #                 stroke = TRUE, color = "black", weight = 3,dashArray = "3",
        #                 smoothFactor = 0.3, fill = FALSE, fillOpacity = 0.5,
        #                 highlight = highlightOptions(
        #                     weight = 5,
        #                     color = "#666",
        #                     dashArray = "",
        #                     fillOpacity = 0.3,
        #                     bringToFront = TRUE),
        #                 label = city_boundary$city_name,
        #                 labelOptions = labelOptions(
        #                     style = list("font-weight" = "normal", padding = "3px 8px"),
        #                     textsize = "15px",
        #                     direction = "auto")) %>%
        #     # plot Population distribution raster
        #     addRasterImage(city_pop_mask, 
        #                    colors = pal_Grid_pop, 
        #                    opacity = 0.7,
        #                    group = "Population",
        #                    maxBytes = 8 * 1024 * 1024) %>%
        #     # Legend for population count
        #     addLegend(pal = pal_Grid_pop, 
        #               values = ~values(city_pop_mask), 
        #               opacity = 0.9,
        #               title = "Population",
        #               position = "bottomright") %>% 
        #     # plot Population distribution raster
        #     addRasterImage(city_pop_heat_exposure, 
        #                    colors = pal_Grid_pop_exposure, 
        #                    opacity = 0.7,
        #                    group = "Population exposure",
        #                    maxBytes = 8 * 1024 * 1024) %>%
        #     # Legend for population count
        #     addLegend(pal = pal_Grid_pop_exposure, 
        #               values = ~values(city_pop_heat_exposure), 
        #               opacity = 0.9,
        #               title = "Population exposure",
        #               position = "topright") %>% 
        #     addLayersControl(
        #         baseGroups = c("OSM (default)", "CartoDB"),
        #         overlayGroups = c("Administrative boundaries",
        #                           "Population",
        #                           "Population exposure"),
        #         options = layersControlOptions(collapsed = FALSE)
        #     )
        
        ### Sector plot ----
        
        # city_amenity_sector_exposure = city_amenity %>% 
        #     group_by(gcom_sector_name) %>% 
        #     summarise(nb_amenities = n(),
        #               lst_min = min(exposure_lst_mean),
        #               lst_mean = mean(exposure_lst_mean),
        #               lst_max =  max(exposure_lst_mean),
        #               nb_exposed_amenities_amenity_threshold = length(exposure_lst_mean[exposure_lst_mean>heat_threshold_value]),
        #               deviation_amenity_threshold = mean(deviation_from_threshold)) %>% 
        #     mutate(exposure_class =
        #                case_when(deviation_amenity_threshold <= 0 ~ "Low", 
        #                          deviation_amenity_threshold <= 5 ~ "Moderate",
        #                          deviation_amenity_threshold > 5 ~ "High")
        #     ) %>% 
        #     mutate(exposure_color =
        #                case_when(exposure_class == "Low" ~ "green", 
        #                          exposure_class == "Moderate" ~ "orange",
        #                          exposure_class == "High" ~ "red")
        #     ) %>% 
        #     arrange(desc(deviation_amenity_threshold))
        # 
        # city_amenity_sector_exposure$gcom_sector_name <- factor(city_amenity_sector_exposure$gcom_sector_name, 
        #                                                         levels = unique(city_amenity_sector_exposure$gcom_sector_name)[order(city_amenity_sector_exposure$deviation_amenity_threshold, decreasing = TRUE)])
        # 
        # exposure_color = city_amenity_sector_exposure$exposure_color
        # 
        # output$Sector_plot <- renderPlotly({
        #     
        #     fig = city_amenity_sector_exposure %>% 
        #         arrange(desc(deviation_amenity_threshold)) %>% 
        #         plot_ly() %>% 
        #         add_trace(x = ~gcom_sector_name, 
        #                   y = ~deviation_amenity_threshold, 
        #                   type = "bar",
        #                   orientation = "v",
        #                   marker = list(color = exposure_color),
        #                   text = ~paste("Deviation ratio: ", deviation_amenity_threshold, '<br>Sector name:', gcom_sector_name)) %>% 
        #         layout(yaxis = list(title = 'Heat deviation ratio from all amenities (%)'), 
        #                xaxis = list(title = ''),
        #                barmode = 'stack',
        #                legend = list(orientation = 'h', x = 0.2, y = -0.5))
        #     
        #     fig
        # })
        # 
        # output$Sector_plot_pop <- renderPlotly({
        #     
        #     fig = city_amenity_sector_exposure %>% 
        #         arrange(desc(deviation_amenity_threshold)) %>% 
        #         plot_ly() %>% 
        #         add_trace(x = ~gcom_sector_name, 
        #                   y = ~deviation_amenity_threshold, 
        #                   type = "bar",
        #                   orientation = "v",
        #                   marker = list(color = exposure_color),
        #                   text = ~paste("Deviation ratio: ", deviation_amenity_threshold, '<br>Sector name:', gcom_sector_name)) %>% 
        #         layout(yaxis = list(title = 'Heat deviation ratio from all amenities (%)'), 
        #                xaxis = list(title = ''),
        #                barmode = 'stack',
        #                legend = list(orientation = 'h', x = 0.2, y = -0.5))
        #     
        #     fig
        # })
        
        ### Main indicators ----
        
        # selected_city_lst_value = round(unique(city_amenity$city_lst_avg),2)
        
        # # City average heat value
        # output$selected_city_heat_value <- renderText({ 
        #     paste("<font size=3px>", "City average heat value: ","<font size=5px; weight=500; color=\"#0000FF\"><b>", selected_city_lst_value, "°C")
        # })
        
        # # Amenity average heat value
        # selected_amenities_avg_heat = round(mean(city_amenity$exposure_lst_mean),2)
        # output$selected_amenities_avg_heat <- renderText({ 
        #     paste("<center>","<font size=5px; weight=500; color=\"#1E90FF\"><b>", selected_amenities_avg_heat, "°C")
        # })
        # 
        # # Selected amenities deviation heat
        # selected_amenities_deviation_heat_value = round(mean(city_amenity$heat_dev_from_amenities),2) 
        # output$selected_amenities_deviation_heat_value <- renderText({ 
        #     paste("<center>","<font size=5px; weight=500; color=\"#1E90FF\"><b>", selected_amenities_deviation_heat_value, "°C")
        # })
        # 
        # # Selected amenities deviation ratio
        # selected_amenities_deviation_heat_ratio = round((selected_amenities_avg_heat/city_amenity_avg_heat * 100)-100,2)
        # output$selected_amenities_deviation_heat_ratio <- renderText({ 
        #     paste("<center>","<font size=5px; weight=500; color=\"#1E90FF\"><b>", selected_amenities_deviation_heat_ratio, "%")
        # })
        
        # output$selected_heat_threshold <- renderText({ 
        #     paste("<font size=3px>", "Selected heat threshold: ","<font size=5px; weight=500; color=\"#0000FF\"><b>", heat_threshold_value, "%")
        # })
        
        
        ### plot table ----
        
        # city_amenity_sector_exposure_plot = city_amenity_sector_exposure %>% 
        #     mutate(lst_min = round(lst_min,2),
        #            lst_mean = round(lst_mean,2),
        #            lst_max = round(lst_max,2),
        #            deviation_amenity_threshold = round(deviation_amenity_threshold,2)) %>% 
        #     dplyr::select("Sector name" = gcom_sector_name,
        #                   "Number of amenities" = nb_amenities,
        #                   "Min heat value" = lst_min,
        #                   "Average heat value" = lst_mean,
        #                   "Max heat value" = lst_max,
        #                   "Number of exposed amenities" = nb_exposed_amenities_amenity_threshold,
        #                   "average deviation ratio" = deviation_amenity_threshold,
        #                   "Exposure category" = exposure_class)
        # 
        # 
        # output$table_amenity <- DT::renderDataTable(
        #     DT::datatable(city_amenity_sector_exposure_plot, 
        #                   options = list(pageLength = 25)) %>% formatStyle(
        #                       "average deviation ratio", target = "row", 
        #                       backgroundColor = styleInterval(c(0, 5,20), c("lightgreen", "yellow", "orange", "red")),
        #                       fontWeight = 'bold')
        #     
        # )
        # 
        # output$table_population <- DT::renderDataTable(
        #     DT::datatable(city_amenity_sector_exposure_plot, 
        #                   options = list(pageLength = 25)) %>% formatStyle(
        #                       "average deviation ratio", target = "row", 
        #                       backgroundColor = styleInterval(c(0, 5,20), c("lightgreen", "yellow", "orange", "red")),
        #                       fontWeight = 'bold')
        #     
        # )
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)