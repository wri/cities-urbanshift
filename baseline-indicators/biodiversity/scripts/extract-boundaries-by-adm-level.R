



# read San Jose mteropolitan Boundary

boundary = st_read("https://cities-urbanshift.s3.eu-west-3.amazonaws.com/data/boundaries/CRI-San_Jose-boundary.geojson",
                   quiet = TRUE)

# read costa rica ADMIN2 boundaries

geoBoundaries_CRI_ADM2 = st_read("./github/cities-urbanshift/baseline-indicators/biodiversity/data/geoBoundaries-CRI-ADM2_simplified.geojson",
                    quiet = TRUE)

boundary_ADM2 = st_intersection(boundary, geoBoundaries_CRI_ADM2)


# plot map

leaflet(data = boundary, height = 500, width = "100%") %>% 
  addTiles() %>%
  addProviderTiles("OpenStreetMap.France", group = "OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter , group = "CartoDB") %>% 
  # addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  setView(lat = boundary_centroid_lat,
          lng = boundary_centroid_lon,
          zoom = 9) %>% 
  # Add boundaries: metropolitan
  addPolygons(data = boundary,
              group = "Administrative boundaries",
              stroke = TRUE, color = col_Black, weight = 3,dashArray = "3",
              smoothFactor = 0.3, fill = TRUE, fillOpacity = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = FALSE)) %>%
  # Add boundaries: Municipality
  addPolygons(data = intersection,
              group = "Municipality boundaries",
              stroke = TRUE, color = col_Black, weight = 3,dashArray = "3",
              smoothFactor = 0.3, fill = TRUE, fillOpacity = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = FALSE)) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("OSM","CartoDB"),
    overlayGroups = c("Municipality boundaries"),
    options = layersControlOptions(collapsed = FALSE)
  ) 


# store boundaries adm2 geojson file

st_write(boundary_ADM2,
         "./github/cities-urbanshift/baseline-indicators/biodiversity/data/boundaries-CRI-San_Jose-ADM2.geojson")
