# Objectives

The baseline indicators attempt to provide insightful metrics at the city scale by addressing three main themes: green space and land restoration, biodiversity, and greenhouse gas emissions. Those indicators are computed based on available open global datasets and hence replicable for all UrbanShift cities.

# Methodology

## Land degradation & greenspace

| Indicator name | Description | Used datasets | Years |
| ------- | ------- | ------- | ------- | 
| Percent of vegetation land based on Dynamic World land cover classes | Percent of land that is trees/water/grass/Scrub/flooded vegetation land cover | Dynamic World Land cover | [2016,2020] |
| Percent of vegetation land based on NDVI threshold | Percent of land that is vegetation (NDVI threshold > 0.4) | Sentinel-2 | [2016,2020] |
| Percent of land with tree cover | Percent of land that has tree cover | Tree Outside of Forests (TOF) | [2020] |
| Percent of built area with tree cover | Percent of land that has tree cover | Dynamic World Land cover, Tree Outside of Forests (TOF) | [2020] |
| Percent of built area with vegetation | Percent of built area with vegetation based on NDVI threshold | Dynamic World Land cover, Sentinel-2 | [2016,2020] |
| Percent of Intra-Urban land classes | Percent of land based on Urban Land Use classification: `Open space`,`Residential`,`Atomistic`,`Informal subdivision`, `Formal subdivision`,`Housing projects`. | Intra-Urban Land Use | [2020] |
| Percent of tree cover by urban land classes | Percent of tree cover level (as expressed in Tree Outside of Forests dataset) by intra-urbal land use classes | Intra-Urban Land Use, Tree Outside of Forests (TOF) | [2020] |
| Percent of vegetation by urban land classes | Percent of vegetation (based on NDVI threshold) by intra-urbal land use classes | Intra-Urban Land Use, Sentinel-2  | [2020] |

## Biodiversity

| Indicator name | Definition | Data sources |
| ------- | ------- | ------- | 
| I-1. Proportion of natural areas | (Total area of natural, restored and naturalised areas) ÷ (Area of city) × 100% | `ESA WorldCover (natural areas as all values except crop, built-up, bare)` | 
| I-2. Connectivity measures or ecological networks to counter fragmentation |  |` ESA WorldCover (natural areas as all values except crop, built-up, bare)` | 
| I-3. Native biodiversity in built-up areas (birds) | (Number of native bird species found in built-up areas) ÷ (Total number of native bird species in the city) × 100% | `ESA WorldCover`, `iNaturalist 2020 research-grade observations` | 
| I-3. Native biodiversity in built-up areas (birds) | (Number of native bird species found in built-up areas) ÷ (Total number of native bird species in the city) × 100% | `ESA WorldCover`, `iNaturalist 2020 research-grade observations` | 
| I-4. Change in number of native species (vascular plants) | Total increase in number of vascular plant species (as a result of re-introduction, rediscovery, new species found due to more intensive and comprehensive surveys, etc.) | `iNaturalist 2020 research-grade observations` | 
|I-5. Change in number of native species (birds) | Total increase in number of native bird species (as a result of re-introduction, rediscovery, new species found due to more intensive and comprehensive surveys, etc.) | `iNaturalist 2020 research-grade observations` | 
|I-6. Change in number of native species (arthropods)| Total increase in number of native arthropod species (as a result of re-introduction, rediscovery, new species found due to more intensive and comprehensive surveys, etc.) | `iNaturalist 2020 research-grade observations` | 
|I-7. Habitat restoration| (Area of habitat restored*) ÷ (Area of original habitat that 
is degraded**) × 100% |  | 
|I-8. Proportion of protected natural areas | (Area of protected or secured natural areas) ÷ (Total area of the city) × 100% |   `World Database of Protected Areas`| 
|I-9. Proportion of invasive alien species | To ensure that the comparison of invasive alien specie with that of native species is meaningful, it would have to be a comparison of identical taxonomic groups.(Number of invasive alien species in a taxonomic group) ÷ (Total number of native species of the same taxonomic group + number of invasive alien species) × 100% | `Global Invasive Species Database`| 
|I-10. Regulation of quantity of water | (Total permeable area) ÷ (Total terrestrial area of the city) × 100% | `GAIA 2018 30m impervious area`| 
|I-11. Climate regulation: carbon storage and cooling effect of vegetation | (Tree canopy cover) ÷ (Total terrestrial area of the city) × 100% | | 
|I-12. Recreational services | (Area of parks, nature conservation areas and other green spaces with natural areas and protected or secured accessible natural areas) /1000 persons | `OpenStreetMap`, `WorldPop` | |
|I-13 Proximity to parks | (Population of city living within 400m from a park/green space) ÷ (Total population of city) × 100% | `OpenStreetMap`, `WorldPop` | 

## GHG emissions


# Key results

| Theme | City | Report |
| --- | --- |--- |
| Green space | San-Jose | [San-Jose Greenspace basline idicators report](https://cities-urbanshift.s3.eu-west-3.amazonaws.com/reports/UrbanShift-Greenspace-Indicators.html) |
| Green space | Kigali | [Kigali Greenspace basline idicators report](https://cities-urbanshift.s3.eu-west-3.amazonaws.com/reports/UrbanShift-Greenspace-Indicators-Kigali.html) |
| Green space | Freetown | [Freetown Greenspace basline idicators report](https://cities-urbanshift.s3.eu-west-3.amazonaws.com/reports/UrbanShift-Greenspace-Indicators-Freetown.html) |
| Biodiversity | San-Jose | [San-Jose Biodiversity basline idicators report](https://cities-urbanshift.s3.eu-west-3.amazonaws.com/baseline-indicators/biodiversity/reports/UrbanShift-Biodiversity-SanJose.html) |
