# Metadata for raw French Breeding Bird Survey data

See Methods in the manuscript for details about the sampling protocol. Both files were subsetted to include only square plots relevant to the corresponding manuscript.

## STOC/fbbs_200m.csv

10 unnamed columns, no header, semi-colon separated. Column names used here are defined in `clean_fbbs` in `R/functions_data_cleaning.R`

 - id_carre: unique identifier of the square plot
 - num_point: identifier of the listening point (*not* unique)
 - annee: year of observation
 - code_sp: FBBS species code
 - passage: number of survey sessions done that year
 - maxabd: bird abundance (maximum of abundance across the survey sessions). May be integer or float
 - habitat_p: complete habitat code of the main habitat, as entered by the observer
 - habitat_s: complete habitat code of the secundary habitat, as entered by the observer (not used in the study)
 - lon: WGS84 longitude of the listening point, as entered by the observer
 - lat: WGS84 latitude of the listening point, as entered by the observer

## STOC/sampling.csv

10 names columns, with header, comma-separated.

 - point: unique listening point identifier
 - carre: unique square plot identifier
 - annee: year of observation
 - passages: "2_passages" if the two survey sessions were done, "passage_1" when only the first was done, "passage_2" when only the second was done
 - "p_milieu": short habitat code of the main habitat
 - "s_milieux": habitat codes for the secondary habitat (not used in the study)
 - "longitude_wgs84": WGS84 longitude of the listening point
 - "latitude_wgs84": WGS84 latitude of the listening point
 - "longitude_grid_wgs84": approximate WGS84 longitude of the center of the square plot
 - "latitude_grid_wgs84": approximate WGS84 latitude of the center of the square plot

Square and point geographical coordinates may mismatch because of data input errors, this is handled in the analysis pipeline by keeping only square plots with coordinates reasonably close to point coordinates.
