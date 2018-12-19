### Load packages
library(tidyverse)
library(sf)

### Reef sediment layer
rf_sed <- st_read("rf_sed_poly_area.shp")

### Caribbean EEZs
carib <- st_read("carib_eez.shp")

### Project carib to match rf_sed
carib_rpr <- carib %>%
  st_transform(crs = "+proj=cea +lon_0=-160 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

### Intersect reef layer and Carib EEZs
rf_sed_carib <- st_intersection(rf_sed, carib_rpr)
### possible this drops some reefs if slight misalignment between reef polygons (which are gridded by WRI) and EEZ outline?

### Assign values to low/medium/high (can change these numbers-- just a placeholder for now)
rf_sed_carib$threat_value[rf_sed_carib$THREAT_TXT == "High"] <- 1
rf_sed_carib$threat_value[rf_sed_carib$THREAT_TXT == "Medium"] <- 0.5
rf_sed_carib$threat_value[rf_sed_carib$THREAT_TXT == "Low"] <- 0

### Calculate weighted means from this
weighted_mean_threat <- rf_sed_carib %>%
  group_by(Country) %>%
  summarise(w_mean_threat = weighted.mean(threat_value, poly_area))


st_write(rf_sed_carib, "rf_sed_carib.shp")