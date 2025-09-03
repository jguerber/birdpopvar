# Metadata for processed data

## `bird_population_variability.csv`: stability components with habitat type and land use intensity variables for each community

18 columns, with header, comma-separated

 - COMMUNITY_ID: unique identifier of community
 - sd_r_average: mean detrended population variability
 - abs_trend_average: mean absolute population trend
 - N_series: number of time series that were used to compute the stability components
 - lon: wgs84 longitude of the center of the community's points
 - lat: wgs84 latitude of the center of the community's points
 - X: UTM 31 easting (in meters) of the center of the community's points
 - Y: UTM 31 northing (in meters) of the center of the community's points
 - area_buffer: area of the union of 250m radius circular buffers around the community's points
 - H: Shannon entropy of the land use type proportions, using "broad" categories as defined in `clean_clc_legend` (`R/functions_corine_land_cover.R`)
 - H_fine: Shannon entropy of the land use type proportions, using the finest CORINE Land Cover classification available
 - HII_10km; HII_25km and HII_5km: averaged HII values across 2001-2014 for a circular radius around the community's lon/lat coordinates, with radius specified in the column name
 - HII_focal: average HII value across 2001-2014 at the community's lon/lat coordinates
 - mu_SR: mean species richness in the community across the sampling years
 - N_POINTS_avg: mean number of points in the community across the sampling years
 - N_YEARS: number of years

## `aggregate_survey.csv`: bird counts by community

9 columns, with header, comma-separated

 - unnamed first columns: row number
 - COMMUNITY_ID: unique identifier of community. Composed of the unique identifier of the square plot and the habitat category of the points for this community, pasted by an underscore.
 - SAMPLING: sampling type. "2_passages" if the two survey sessions were done, "passage_1" when only the first was done, "passage_2" when only the second was done
 - YEAR: observation year
 - SPECIES: FBBS species code
 - N_POINTS: number of points in the community (may vary across years)
 - AB_SUM: sum of bird abundances across all points in the community
 - AB_MAX: maximum of bird abundances across all points in the community
 - AB_REL: AB_SUM / N_POINTS
 
## `all_local_trends_outputs.csv`: species temporal trends outputs by community by species (i.e. by time series)

20 columns, with header, comma-separated

 - trend: temporal trend maximum likelihood estimate, on the log scale
 - se: standard error around trend
 - p: temporal trend p-value
 - model_name: distribution used for fitting the temporal trend
 - dAIC: AIC difference of this model with the best tested distribution, 0 if it's the best
 - convProblems: number of errors and warnings raised during model fitting
 - threshold_warnings: number of expected warnings in the case of no convergence issue (see `single_series_trends ` in `R/functions_local_trends.R`)
 - best: logical, is this fit the best model
 - N: number of years across the time series
 - corrected: logical, whether or not SAMPLING varied in time and had to be included as a covariate
 - N_ab: number of years with zero abundance
 - series_id: unique time series identifier (`paste0(COMMUNITY_ID, '_', SPECIES )`)
 - residual_type: "pearson" for Pearson residuals
 - sd_r: standard deviation of residuals
 - mse: mean squared error of the temporal trend model
 - mean_ab: mean abundance of the species across the time series
 - raw_cv: standard deviation of the abundance of the species divided by mean_ab
 - npars: number of parameters of the temporal trend model
 - theta: dispersion estimator
 - cv_eq: sd_r / mean_ab
 
## `community_coordinates.csv`: geographical coordinates of each community

6 columns, with header, comma-separated

 - unnamed column: row index
 - COMMUNITY_ID: unique identifier of community
 - lon: wgs84 longitude of the center of the community's points
 - lat: wgs84 latitude of the center of the community's points
 - X: UTM 31 easting (in meters) of the center of the community's points
 - Y: UTM 31 northing (in meters) of the center of the community's points
