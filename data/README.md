# Data directory for reproducing the project

This directory contains two sub-directories, `raw` and `processed`. Column-by-column file metadata is provided within each sub-directory, this document is a summary.

## `processed`: Processed data

These files are not to be altered manually: they are outputs of analyses within the pipeline. Combined with the raw data files, they constitute all the necessary information to run the analyses in the shortcut pipeline. If one of the project pipelines has already been run, these files' contents are also accessible with `tar_read`.

- **aggregate_survey.csv** : yearly bird abundance by community (groups of listening points in the same `HABITAT_GROUP`). AB_SUM is the sum of bird abundance across listening points, AB_MAX is their maximum
- **community_coordinates** : geographical coordinates (WGS84 lon/lat and UTM31 X/Y) by community
- **all_local_trends_outputs.csv** : for each species time series, several summary statistics for the local trend model (trend, se, p) and for the variability of residuals around the trend (sd, mse)
- **bird_population_variability.csv** : community-level aggregated population variability metrics. In particular, sd_r_average and abs_trend_average are the mean detrended population variability and the mean absolute population trend (see Methods in the manuscript text). Also includes anthropogenic pressure indices: Landscape complexity H_fine for the smallest available Corine Land Cover categories, mean Human Impact Index values over 2001-2014, for focal points and for several buffer sizes, mean species richness and mean number of points

## `raw`: Raw data from the [French Breeding Bird survey](https://www.vigienature.fr/fr/suivi-temporel-des-oiseaux-communs-stoc)

Located in the `data/raw/STOC` directory (STOC stands for FBBS in French).

Cleaned French Bird Survey database extraction (extracted by Benoît Fontaine on 23/01/2025). If you want to re-use the data for your own projects, please inform Benoît Fontaine at benoit.fontaine@mnhn.fr .

 - `fbbs_200m.csv`  bird abundances in distances <= 200m by year and sampling site. Bird counts from survey sites that were not used for our statistical analyses are not included. Contact benoit.fontaine@mnhn.fr for access to the whole survey.
 - `sampling.csv` sampling information (number of sessions, locations and habitat for each listening point each year)
