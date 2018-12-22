### Load packages
library(tidyverse)
library(sf)
library(rgdal)
library(sp)
library(rgeos)
library(lwgeom)

### Reef sediment layer
rf_sed <- st_read("rf_sed_poly_area.shp")

### Caribbean EEZs
eez <- st_read("carib_eez.shp")

### Project eez to match rf_sed
eez_rpr <- eez %>%
  st_transform(crs = "+proj=cea +lon_0=-160 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

### Carib island shp - to deal with reef polygons that are "on land"
load("carib_polygons.Rdata")

### The polygons are very complex: use gSimplify to simplify their topology for faster processing
carib_simple <- gSimplify(carib, tol = 0.05, topologyPreserve = TRUE) %>%
  st_as_sf() %>%
  st_transform(crs = "+proj=cea +lon_0=-160 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") %>%
  st_make_valid()

# ### Convert to sf and reproject
# carib_sf <- st_as_sf(carib) %>%
#   st_transform(crs = "+proj=cea +lon_0=-160 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# ### Combine eezs and islands
# car_all <- st_union(eez_rpr, carib_sf)
car_all_simple <- st_union(eez_rpr, carib_simple)
# 
# st_write(car_all, "carib_eez_islands.shp")
# 
# car_all <- st_read("carib_eez_islands.shp")

### Crop reef layer (it's huge)
car_reefs <- st_crop(rf_sed, 
                     c(xmin = 8133228, xmax = 11577240, 
                       ymin = 1081973, ymax = 3205316))

# st_write(car_reefs, "carib_reefs_sed.shp")

# ### Intersect reef layer and carib islands + eezs
# rf_sed_carib <- st_intersection(car_reefs, car_all)
# rf_sed_carib_simple <- st_intersection(car_reefs, car_all_simple)

### Intersect reef sedimentation layer and EEZs
rf_eez <- st_intersection(car_reefs, eez_rpr)
st_write(rf_eez, "eez_intersection_rfsed.shp")

### Intersect reef sedimentation layer and islands
rf_islands <- st_intersection(car_reefs, carib_simple)

### Union the intersections
rf_union <- st_union(rf_eez, rf_islands)
st_write(rf_union, "reefsed_union_eez_islands.shp")

### Opened this in ArcGIS and Dissolved to get rid of overlapping polygons, but this dropped data from attribute table

### Now trying this method: export attribute table with column called "new_id" that is equal to FID of original shapefile. Then Dissolve, then merge new shapefile with exported attribute table.



### Still need to figure out which polygons should be St Barthelemy










### Assign values to low/medium/high (can change these numbers-- just a placeholder for now)
rf_sed_carib$threat_value[rf_sed_carib$THREAT_TXT == "High"] <- 1.5
rf_sed_carib$threat_value[rf_sed_carib$THREAT_TXT == "Medium"] <- 1.25
rf_sed_carib$threat_value[rf_sed_carib$THREAT_TXT == "Low"] <- 1

### Calculate weighted means from this
weighted_mean_threat <- rf_sed_carib %>%
  group_by(Country) %>%
  summarise(w_mean_threat = weighted.mean(threat_value, poly_area))


st_write(rf_sed_carib, "rf_sed_carib.shp")