# Objectives

WRI and the UrbanShift regional coordinators engaged with all UrbanShift cities to understand their data-related needs and to identify which data layers are most relevant to each city. 

A data needs survey has been implemented highlighting and allowed the identification of the main data topics to address: `Land cover (e.g., built-up/impervious, vegetation, trees, water)`, `Topography`, `Population density`, `Built-up density`, `Land use (e.g., residential, commercial, industrial, vacant, agriculture)`, `Administrative boundaries`, `Amenities/service locations (e.g., jobs, health care, recreation)`, `Roads`, `Transit stops and transportation network`, `Buildings and other infrastructure`, `Emissions of air pollutants`, and `Natural hazards`.

Three main outputs will be delivered as part of this project:

- **Data Catalog**: A list referencing the available geospatial datasets regarding the identified themes and collected with respect to the boundaries of UrbanShift cities.
- **Geospatial Data Portal**: A data portal providing access to the collected and processed layers with the respective metadata and information describing these layers.
- **Data explorer**: A web application for visualizing the different geospatial layers for each UrbanShift city.

# Methodology

We have identified the available datasets that can be used for addressing the previously listed topics. Each dataset is considered as a **feature** that may be characterized by 5 development status:

- `collected`: The dataset is collected from the data source with respect to the city administrative boundaries and stored in AWS S3 bucket.
- `explored`: Providing a raw data exploration notebook for collecting needed information about the metadata.
- `documented`: Filling the necessary metadata that will be exposed through the data catalog.
- `integrated`: Integrating the city scale data into the data hub with the needed information
- `exposed`: Visualize the geospatial layer through a data exploration web application.

The table below lists the different datasets we are considering to process for this project with their respective status.

| Theme | Layer | Data source | Status | 
| ---- |---- |---- | ---- |
| Boundaries | Administrative boundaries | geoBoundaries | `collected` `explored` |
| Land Use/Land cover | Dynamic World land use | Dynamic World | `collected` `explored` |
| Land Use/Land cover | NDVI | | `collected` `explored` |
| Land Use/Land cover | Inra-Urban Land Use | | `collected` `explored` |
| Land Use/Land cover | Tree cover | Tree Outside of Forst (WRI) | `collected` `explored` |
| Infrastructure | Physical amenities | Open Street Map | `collected` `explored` |
| Biodiversity | Protected areas | World Database on Protected Areas | `collected` `explored` |
| Biodiversity | Species locations and counts | WGlobal Biodiversity Information Facility | `collected` `explored` |
| Demographic | Population count | WorldPop |  |
| Infrastructure | Road network | Open Street Map |  |
| Land Use/Land cover | Global Human Settlement Layers, Built-up Grid | GHS-BUILT (ESA) |  |
| Land Use/Land cover | Global Human Settlement Layers, Settlement Grid  | GHS-SMOD (ESA) |  |
| Land Use/Land cover | World Settlement Footprint  | WSF2019 (DLR) |  |
| Air quality | Nitrogen Dioxide (NO₂) Satellite Measurements  | Sentinel-5P OFFL NO2 (ESA) |  |
| Air quality | Carbon Monoxide (CO)   | Sentinel-5P OFFL CO (ESA) |  |
| Air quality | Aerosol Index Satellite Measurements  | Sentinel-5P OFFL AER AI (ESA) |  |
| Emissions | GHG & local pollutant emissions  | CAMS global emission inventories |  |
| Climate | Land Surface Temperature estimation | Landsat |  |
| Climate | Precipitation | NEX-GDDP (NASA) |  |
| Climate | Temperature | NEX-GDDP  (NASA) |  |
| Climate | Flooding | Aqueduct Floods  (WRI) |  |
| Climate | Water risk | Aqueduct Water Risk Atlas  (WRI) |  |


