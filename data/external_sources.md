# Instructions for retrieving additional raw data from external sources

`.` and `$PROJECT_DIR` represent the location of the project directory on your computer. Most likely the place where you extracted the `code` directory of the archive, where the project file `birdpopvar.Rproj` is located.

## Countour maps of French regions from [data.gouv.fr](https://www.data.gouv.fr/fr/datasets/contours-des-regions-francaises-sur-openstreetmap)

Interactively: navigate to the portal, in the "Fichiers" (files) tab, go to the "Téléchargements" tab of the "Export 2018" row and click on the link. Extract the contents of the downloaded archive to `./data/raw/maps/regions`

From an UNIX-like shell:

```
wget -O /path/to/download/regions.zip https://www.data.gouv.fr/api/1/datasets/r/aacf9338-8944-4513-a7b9-4cd7c2db2fa9
cd $PROJECT_DIR
mkdir -p ./data/raw/maps/regions
unzip /path/to/download/regions.zip -d ./data/raw/maps/regions
```

## [Corine Land Cover](https://land.copernicus.eu/en/products/corine-land-cover) land use data

Data used in the study was extracted via the Copernicus web interface on 10/06/2024.

Interactively: Navigate to the [2018 release download page](https://land.copernicus.eu/en/products/corine-land-cover/clc2018#download). Click "Go to download area". You will be prompted to create a free account or login to the EU. After logging in, in the data viewer, click "Area selection" on the right-hand side of the map, enter "FR" in country code and click on download on the left-hand panel. Click on the cart icon at the top of the screen, check that you're downloading the "CORINE Land Cover 2018 (vector/raster 100 m), Europe, 6-yearly" with "Area: France" selected. Select "Shapefile (SHP)" in the "Format" drop-down, tick the box at the left and click "Process download request". Extract the contents of the .zip in `./data/raw/CLC/CLC_2018`. Files should all have "U2018_CLC2018_V2020_20u1" as file names (with various file extensions), rename if necessary.

Place the `clc_legend.csv` file from the archive in `./data/raw/CLC`.

## [Human Footprint Database](https://wcshumanfootprint.org/data-access) human impact index data

Data used in the study was extracted on 08/04/2025.

We provide an R script for downloading HII data for continental France. After having restored the required packages for the project (via `renv::restore()`), run `source(here::here('scripts/download_human_impact_data.R'))` in an R shell. Check that `./data/raw/HII/HII_France` contains 20 .tif files (around 35.3MB per file before 2015).
