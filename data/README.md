# Raw data folder

## STOC

Cleaned French Bird Survey database extraction (extracted by Benoît Fontaine on 23/01/2025)

 - `fbbs_200m.csv`  bird abundances in distances <= 200m 
 - `sampling.csv` sampling information (number of sessions, locations and habitat for each listening point each year)
 
## CLC

[Corine Land Cover](https://land.copernicus.eu/en/products/corine-land-cover) data. Extracted via the Copernicus web interface on 10/06/2024.

- `CLC_2018` shapefiles of 2018 land cover data for continental France
- `clc_legend.csv` correspondence between CLC land cover codes and land cover description

## HII

[Human Impact Index](https://wcshumanfootprint.org/data-access) data. .tif files of yearly hii around continental France, extracted on 08/04/2025. See scripts/download_humain_impact_data.R for the download script.

## maps

Shapefiles of the French administrative regions, accessed at [data.gouv.fr](https://www.data.gouv.fr/fr/datasets/contours-des-regions-francaises-sur-openstreetmap), © the contributors of OpenStreetMap under the [Open Database License (ODbL)](https://opendatacommons.org/licenses/odbl/1-0/)
