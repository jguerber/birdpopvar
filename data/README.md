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

# Processed data folder

These files are not to be provided manually: they are outputs of analyses within the all_steps pipeline. Combined with the raw data files, they constitute all the necessary information to run the analyses in the shortcut pipeline. If one of the project pipelines has already been run, these files' contents are also accessible with `tar_read`.

- **aggregate_survey.csv** : yearly bird abundance by community (groups of listening points in the same `HABITAT_GROUP`). AB_SUM is the sum of bird abundance across listening points, AB_MAX is their maximum
- **community_coordinates** : geographical coordinates (WGS84 lon/lat and UTM31 X/Y) by community
- **all_local_trends_outputs.csv** : for each species time series, several summary statistics for the local trend model (trend, se, p) and for the variability of residuals around the trend (sd, mse)
- **bird_population_variability.csv** : community-level aggregated population variability metrics. In particular, sd_r_average and abs_trend_average are the mean detrended population variability and the mean absolute population trend (see Methods in the manuscript text). Also includes anthropogenic pressure indices: Landscape complexity H_fine for the smallest available Corine Land Cover categories, mean Human Impact Index values over 2001-2014, for focal points and for several buffer sizes, mean species richness and mean number of points
