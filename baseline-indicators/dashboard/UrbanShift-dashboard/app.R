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
library(shinyjs)

# define aws s3 path

aws_s3_path = "https://cities-urbanshift.s3.eu-west-3.amazonaws.com/"

############### Load data

# read indicator definition ------------

indicators_definitions = read.csv(paste(aws_s3_path,
                                        "indicators/indicators_definition_v4.csv",
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

boundary_georef = boundary_georef %>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  drop_na(units_boundary_name)

cities = unique(boundary_georef$geo_name)


# read indicator ------------


indicators_v4 = read.csv(paste(aws_s3_path,
                               "indicators/urbanshift_indicators_v4.csv",
                               sep = ""),
                         encoding="UTF-8")

indicators = indicators_v4 %>%
  mutate(BIO.1 = 100 * BIO.1) %>% 
  mutate(BIO.2 = 100 * BIO.2) %>% 
  mutate(BIO.3 = na_if(BIO.3, -9999)) %>% 
  mutate(BIO.3 = 100 * BIO.3) %>% 
  mutate(BIO.4 = na_if(BIO.4, -9999)) %>% 
  mutate(BIO.5 = na_if(BIO.5, -9999)) %>% 
  mutate(BIO.6 = na_if(BIO.6, -9999)) %>% 
  mutate(LND.1 = 100 * LND.1) %>% 
  mutate(LND.2 = 100 * LND.2) %>% 
  mutate(LND.4 = 100 * LND.4) %>% 
  mutate(LND.5 = 100 * LND.5) %>% 
  mutate(LND.6 = 100 * LND.6) %>% 
  mutate(LND.7 = na_if(LND.7, -9999)) %>%
  mutate(LND.7 = 100 * LND.7) %>% 
  mutate(LND.8 = na_if(LND.8, -9999)) %>%
  mutate(LND.8 = 100 * LND.8) %>% 
  mutate(GRE.2 = 100 * GRE.2) %>% 
  mutate(GRE.3 = 100 * GRE.3) %>% 
  mutate(GRE.4 = 100 * GRE.4)


# read indicator GHG-1  ------------
indicators_GHG_1 = read.csv(paste(aws_s3_path,
                                  "indicators/urbanshift_GHG-1.csv",
                                  sep = ""),
                            encoding="UTF-8")

indicators_GHG_1_aoi = indicators_GHG_1 %>% 
  filter(!X %in% c("ARG-Mar_del_Plata_ADM-2_1",
                   "ARG-Ushuaia_ADM-3_1",
                   "BRA-Teresina_ADM-2-union_1",
                   "BRA-Florianopolis_ADM-2-union_1",
                   "BRA-Belem_ADM-2-union_1",
                   "IND-Chennai_ADM-6-union_1")) %>% 
  add_column(geo_parent_name = c("ARG-Mendoza",
                                 "ARG-Mar_del_Plata",
                                 "ARG-Ushuaia",
                                 "ARG-Salta",
                                 "ARG-Buenos_Aires",
                                 "BRA-Teresina",
                                 "BRA-Florianopolis",
                                 "BRA-Belem",
                                 "CRI-San_Jose",
                                 "RWA-Kigali",
                                 "SLE-Freetown_city",
                                 "SLE-Freetown_region",
                                 "MAR-Marrakech",
                                 "IND-Chennai",
                                 "IND-Pune",
                                 "IND-Surat",
                                 "CHN-Chengdu",
                                 "CHN-Chongqing",
                                 "CHN-Ningbo",
                                 "IDN-Jakarta",
                                 "IDN-Bitung",
                                 "IDN-Semarang",
                                 "IDN-Balikpapan",
                                 "IDN-Palembang")) %>%
  mutate(GHG.1 = 100 * total_change_co2e) %>% 
  dplyr::select(geo_parent_name,GHG.1) %>%
  mutate_if(is.numeric, round, 0)

indicators = indicators %>% 
  right_join(indicators_GHG_1_aoi,
             by = "geo_parent_name") 

indicators_GHG_1_unit = indicators_GHG_1 %>% 
  dplyr::select(1,
                c(205:268),
                c(329:418)) %>% 
  dplyr::select(-c("co_2000_co2e",
                   "ch4_2000_co2e",
                   "co2_2000_co2e",
                   "nox_2000_co2e",
                   "ch4_2020_co2e",
                   "co_2020_co2e" ,
                   "oc_2020_co2e" , 
                   "nmvoc_2020_co2e",
                   "co2_2020_co2e" ,
                   "nox_2020_co2e")) %>% 
  rename(geo_id = X)


indicators = indicators %>% 
  left_join(indicators_GHG_1_unit,
            by = "geo_id")


# label indicator map function -----

pal.indicator.fun = function(selected_indicator_values){
  if(sum(is.na(selected_indicator_values)) == length(selected_indicator_values))
  {
    print("NOT available")
    selected_indicator_values = 0
    
    pal_indicator<- colorNumeric(palette = "gray",
                                 domain = selected_indicator_values,
                                 na.color = "gray",
                                 revers = FALSE)
  } else {
    pal_indicator<- colorNumeric(palette = "Greens",
                                 domain = selected_indicator_values,
                                 na.color = "gray",
                                 revers = FALSE)
  }
  return(pal_indicator)
}



############### App

ui = tagList(
  useShinyjs(),
  navbarPage("UrbanShift-Dashboard",
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
                                                 selected = "Natural Areas",
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
                                              id = "tabs",
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
                         )
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
  
  # hide tab based on selected indicator
  observeEvent(input$indicator, {
    if(input$indicator %in% c("Change in greenhouse gas emissions")){
      showTab(inputId = "tabs", target = "Chart")
      showTab(inputId = "tabs", target = "Definitions")
      showTab(inputId = "tabs", target = "Table")
      hideTab(inputId = "tabs", target = "Map")
      show("city_wide_indicator") 
    } 
  })
  
  
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
    
    print(aoi_boundary_name)
    print(units_boundary_name)
    
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
    
    print(selected_indicator_name)
    
    # get indicator legend  -----
    selected_indicator_legend = indicators_definitions %>% 
      filter(indicator_label %in% selected_indicator_label) %>% 
      pull(indicator_legend)
    
    # get indicator values  -----
    
    selected_indicator_values = unit_indicators %>% 
      as.data.frame() %>% 
      pull(selected_indicator_name)
    
    print(selected_indicator_values)
    
    # indicator color values ----
    
    
    
    # pal_indicator<- colorNumeric(palette = "Greens", 
    #                              domain = selected_indicator_values,
    #                              na.color = "gray",
    #                              revers = FALSE)
    
    # pal.indicator.fun = function(selected_indicator_values){
    #   if(is.na(sum(is.na(selected_indicator_values)) == length(selected_indicator_values)))
    #   {
    #     print("NOT available")
    #     selected_indicator_values = 0
    #     
    #     pal_indicator<- colorNumeric(palette = "gray",
    #                                  domain = selected_indicator_values,
    #                                  na.color = "gray",
    #                                  revers = FALSE)
    #   } else {
    #     pal_indicator<- colorNumeric(palette = "Greens",
    #                                  domain = selected_indicator_values,
    #                                  na.color = "gray",
    #                                  revers = FALSE)
    #   }
    #   return(pal_indicator)
    # }
    
    pal_indicator = pal.indicator.fun(selected_indicator_values) 
    
    
    # indicator labels for map ----
    
    labels_indicator <- sprintf("<strong>%s</strong><br/>%s: %s",
                                unit_indicators$geo_name,
                                selected_indicator_label,
                                round(selected_indicator_values, 2)) %>% 
      lapply(htmltools::HTML)
    
    
    ########################
    # collect layers ----
    ########################
    
    # layers: esa world cover
    if(input$indicator %in% c("Percent of Natural Areas")){
      
      esa_worldcover_data_path = paste("/vsicurl/https://cities-urbanshift.s3.eu-west-3.amazonaws.com/",
                                       "data/land_use/esa_world_cover/v_0/",
                                       geo_name,
                                       "-",
                                       aoi_boundary_name,
                                       "-ESA-world_cover-2000.tif",
                                       sep = "")
      
      
      # collect raster data
      esa_worldcover_data = raster(esa_worldcover_data_path)
      
      city_esa_worldcover = raster::mask(esa_worldcover_data,
                                         boundary_aoi)
      
      
      # define color palette for WOrld cover
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
      
      # define a color palette
      pal_worldcover <- colorFactor(palette = worldcover_col, 
                                    levels = c("10","20","30","40","50","60",
                                               "70","80","90","95","100"),
                                    na.color = "transparent")
      
    }
    
    
    ########################
    # map indicator ----
    ########################
    
    
    m = leaflet(boundary_aoi) %>%
      addTiles() %>%
      fitBounds(~as.numeric(st_bbox(boundary_aoi)[1]),
                ~as.numeric(st_bbox(boundary_aoi)[2]),
                ~as.numeric(st_bbox(boundary_aoi)[3]),
                ~as.numeric(st_bbox(boundary_aoi)[4])) %>% 
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
      # indicator layer - value
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
      addFullscreenControl()
    
    
    # Esa world cover ----
    if(input$indicator  %in% c("Percent of Natural Areas")){
      m = m %>% 
        # plot layer: ESA world cover ----
      addRasterImage(city_esa_worldcover,
                     colors = pal_worldcover,
                     opacity = 1,
                     maxBytes = 100 * 1024 * 1024,
                     project=FALSE,
                     group = "Land cover types") %>%
        addLegend(colors = worldcover_col,
                  labels = worldcover_labels,
                  title = "World Cover",
                  group = "Land cover types",
                  position = "bottomleft",
                  opacity = 1) %>%
        # Layers control
        addLayersControl(
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_label,
                            "Land cover types"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        hideGroup(c("Land cover types")) 
    }
    
    
    
    # center map  
    output$indicator_map <- renderLeaflet({
      m
    })
    
    #########################################
    ### Main indicators ----
    
    # city wide  ----
    city_wide_indicator_value = aoi_indicators %>%
      as.data.frame() %>%
      pull(selected_indicator_name) %>% 
      round(2)
    
    # unit
    
    if(input$indicator %in% c("Natural Areas",
                              "Connectivity of ecological networks",
                              "Biodiversity in built-up areas (birds)",
                              "Permeable areas",
                              "Tree cover",
                              "Proportion of natural areas restored",
                              "Number of habitat types restored",
                              "Protected areas",
                              "Protection of Key Biodiversity Areas",
                              "Built-up Key Biodiversity Areas",
                              "Urban open space for public use",
                              "Urban open space for public use",
                              "Proximity to public open space",
                              "Proximity to tree cover")){
      city_wide_indicator_value_unit = "%"
    } else if(input$indicator %in% c("Vascular plant species",
                                     "Bird species",
                                     "Arthropod species")){
      city_wide_indicator_value_unit = "#"

    } else if(input$indicator %in% c("Recreational space per capita")){
      city_wide_indicator_value_unit = "hectares"
      
    } else if(input$indicator %in% c("Change in greenhouse gas emissions")){
      city_wide_indicator_value_unit = "% change"
    }
      else if(input$indicator %in% c("Climate change impact of trees")){
      city_wide_indicator_value_unit = "Mg CO2 eq/hectare"
    }
    

    
    # output plot
    output$city_wide_indicator <- renderText({
      paste("<center>","<font size=5px; weight=500; color=\"#168A06\"><b>", 
            city_wide_indicator_value, 
            city_wide_indicator_value_unit,"<br>",
            "<font size=2px; weight=500; color=\"#168A06\"><b>",
            selected_indicator_legend)
    })
    
    #########################################
    ### Table ----
    
    # Table plot
    
    if(input$indicator %in% c("Change in greenhouse gas emissions")){
      table_plot_years = aoi_indicators %>% 
        as.data.frame() %>%
        dplyr::select(-geometry) %>% 
        pivot_longer(
          cols = bc_agl_2000_co2e:nmvoc_tro_2020_co2e,
          names_to = c("gas", "sector","year","unit"),
          names_pattern = "(.*)_(.*)_(.*)_(.*)",
          values_to = "value") %>% 
        dplyr::select(geo_name,
                      gas,
                      sector, 
                      year,
                      value) %>% 
        arrange(desc(value)) %>% 
        mutate_at("gas", 
                  ~recode(.,
                          "bc"='Black carbon', 
                          'ch4'='Methane',
                          'co' = 'Carbon monoxide',
                          'co2' = 'Carbon dioxide',
                          'nox' = 'Nitrogen oxides',
                          'so2' = 'Sulfur dioxide',
                          'oc' = 'Organic carbon',
                          'nh3' = 'Ammonia',
                          'nmvoc' = 'Non-methane volatile organic compounds')) %>% 
        mutate_at("sector", 
                  ~recode(.,
                          "agl"='Agriculture livestock', 
                          'ags'='Agriculture soils',
                          'awb' = 'Agriculture waste burning',
                          'ene' = 'Power generation',
                          'fef' = 'Fugitives',
                          'ind' = 'Industry',
                          'res' = 'Residential, commercial, and other combustion',
                          'shp' = 'Ships',
                          'slv' = 'Solvents',
                          'sum' = 'All sources',
                          'swd' = 'Solid waste and wastewater',
                          'tnr' = 'Off-road transportation',
                          'tro' = 'Road transportation'))
      
      table_plot = table_plot_years %>% 
        dplyr::select("Sector" = "sector",
                      "Gas" = "gas",
                      "Year" = "year",
                      "Emissions (tonnes of CO2 equivalent)" = "value")
    } else {
      table_plot = unit_indicators %>% 
        drop_na(selected_indicator_name, geo_name) %>% 
        as.data.frame() %>%
        dplyr::select(-geometry) %>% 
        dplyr::select(geo_name,selected_indicator_name) %>% 
        mutate_if(is.numeric, round, 2) %>% 
        distinct(geo_name, .keep_all = T) %>% 
        arrange(desc(selected_indicator_name)) 
      
      selected_indicator_legend_table = str_remove(selected_indicator_legend, "<br> ")
      names(table_plot) = c("City name",selected_indicator_legend_table)
    }
    
    
    
    # plot table
    
    
    output$indicator_table <- DT::renderDataTable(
      if(input$indicator %in% c("Change in greenhouse gas emissions")){
        DT::datatable(table_plot,
                      options = list(pageLength = 10,order = list(list(2, 'desc')))) 
      } else {
        DT::datatable(table_plot, 
                    options = list(pageLength = 10,
                                   order = list(list(2, 'desc')))) %>% formatStyle(
                                     selected_indicator_legend_table,
                                     backgroundColor = styleInterval(seq(from = min(table_plot[,selected_indicator_legend_table]),
                                                                         to = max(table_plot[,selected_indicator_legend_table]),
                                                                         length.out = 8), 
                                                                     brewer.pal(9, "Greens")
                                     ),
                                     fontWeight = 'bold')
      }
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
    
    if(input$indicator %in% c("Change in greenhouse gas emissions")){
      
      table_plot_2000 = table_plot_years %>% 
        filter(year == 2000)
      
      table_plot_2020 = table_plot_years %>% 
        filter(year == 2020)
      
      fig_2000 <- plot_ly(table_plot_2000, 
                          x = ~sector, 
                          y = ~value, 
                          type = 'bar',
                          name = ~gas,
                          color = ~gas,
                          legendgroup= ~year) %>% 
        layout(yaxis = list(title = 'Greenhouse gas emissions <br> (tonnes of CO2 equivalent)'), 
               xaxis = list(title = 'sectors',categoryorder = "total descending"),
               barmode = 'stack')
      
      
      fig_2020 <- plot_ly(table_plot_2020, 
                          x = ~sector, 
                          y = ~value, 
                          type = 'bar',
                          name = ~gas,
                          color = ~gas,
                          legendgroup= ~year,
                          showlegend = FALSE) %>% 
        layout(yaxis = list(title = 'Greenhouse gas emissions <br> (tonnes of CO2 equivalent)'), 
               xaxis = list(title = 'sectors',categoryorder = "total descending"),
               barmode = 'stack')
      
      annotations = list( 
        list( 
          x = 0.2,  
          y = 1.0,  
          text = "2000",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE 
        ),  
        list( 
          x = 0.8,  
          y = 1,  
          text = "2020",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE 
        ))
      
      fig <- subplot(fig_2000, fig_2020, shareY = TRUE, margin = 0.05) %>% 
        layout(legend=list(title=list(text='<b> Gases </b>')),
               annotations = annotations)
    } else {
      fig = plot_ly(x = table_plot$`City name`,
                    y = table_plot[[colnames(table_plot)[2]]],
                    type = "bar",
                    orientation = "v",
                    name = names(table_plot)[2],
                    color = I("green4")) %>% 
        layout(yaxis = list(title = selected_indicator_legend),
               xaxis = list(title = 'Cities',categoryorder = "total descending"))
      
      fig
    }
    
    output$indicator_chart <- renderPlotly({
      fig
      
    })
    
    # output$indicator_chart <- renderPlotly({
    #   fig = plot_ly(x = table_plot$`City name`,
    #                 y = table_plot[[colnames(table_plot)[2]]],
    #                 type = "bar",
    #                 orientation = "v",
    #                 name = names(table_plot)[2],
    #                 color = I("green4")) %>% 
    #     layout(yaxis = list(title = selected_indicator_legend),
    #            xaxis = list(title = 'Cities',categoryorder = "total descending"))
    #   
    #   fig
    #   
    # })
    
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