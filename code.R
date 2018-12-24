### Load packages
library(tidyverse)
library(sf)
library(rgdal)
library(sp)
library(rgeos)
library(lwgeom)

### Reef sediment layer
rf_sed <- st_read("rf_sed_poly_area.shp")

### Open joint EEZ and island layer
eez_isl <- st_read("EEZ_land_v2_201410.shp")

###### Drop Chad and Libya because they are messed up
eez_isl_fix <- subset(eez_isl, !Country == "Chad")
eez_isl_fix <- subset(eez_isl_fix, !Country == "Libya")

###### Reproject to match rf_sed
eez_isl_fix <- eez_isl_fix %>%
  st_transform(crs = "+proj=cea +lon_0=-160 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
eez_isl_crop <- st_crop(eez_isl_fix, 
                        c(xmin = 8133228, xmax = 11577240, 
                          ymin = 1081973, ymax = 3205316))

###### Check for validity
st_is_valid(eez_isl_crop)
eez_isl_crop <- st_make_valid(eez_isl_crop)

### Crop reef layer (it's huge)
car_reefs <- st_crop(rf_sed, 
                     c(xmin = 8133228, xmax = 11577240, 
                       ymin = 1081973, ymax = 3205316))

###### Check for validity
st_is_valid(car_reefs)
car_reefs <- st_make_valid(car_reefs)

### Intersect reef layers and carib eez_isl
rf_carib <- st_intersection(car_reefs, eez_isl_crop)
st_write(rf_carib, "carib_reefs_12_22.shp")

### Open in ArcGIS to code reefs in BART, SABA, BON, EUST
###### result: carib_reefs_12_22_fixed.shp

carib <- st_read("carib_reefs_12_22_fixed.shp")

### Assign values to low/medium/high (can change these numbers-- just a placeholder for now)
carib$THREAT[carib$THREAT_TXT == "High"] <- 1.4
carib$THREAT[carib$THREAT_TXT == "Medium"] <- 1.2
carib$THREAT[carib$THREAT_TXT == "Low"] <- 1

### Calculate weighted means from this
weighted_mean_threat <- carib %>%
  group_by(Country) %>%
  summarise(w_mean_threat = weighted.mean(THREAT, poly_area))

### Rename islands to match other files
weighted_mean_threat$Country <- as.character(weighted_mean_threat$Country)
weighted_mean_threat$Country[weighted_mean_threat$Country == "Cayman Is."] <- "Cayman Islands"
weighted_mean_threat$Country[weighted_mean_threat$Country == "Curaçao"] <- "Curacao"
weighted_mean_threat$Country[weighted_mean_threat$Country == "Saint Vincent and the Grenadines"] <- "St. Vincent and the Grenadines"
weighted_mean_threat$Country[weighted_mean_threat$Country == "The Bahamas"] <- "Bahamas"
weighted_mean_threat$Country[weighted_mean_threat$Country == "Turks & Caicos Is."] <- "Turks & Caicos"
weighted_mean_threat$Country[weighted_mean_threat$Country == "United States Virgin Islands"] <- "USVI"
weighted_mean_threat$Country[weighted_mean_threat$Country == "Virgin Islands, British"] <- "BVI"

### Convert to df
wmt_df <- weighted_mean_threat %>% st_set_geometry(NULL)

### Write csv
write.csv(wmt_df, "weighted_mean_threat.csv", row.names = FALSE)

### Write csv to target_species project
write.csv(wmt_df, "/Users/Katherine Siegel/Documents/target_sp/watershed_exp.csv")


#################################################
### Old code: EEZs and islands
### Caribbean EEZs
eez <- st_read("carib_eez.shp")

# ### Project eez to match rf_sed
# eez_rpr <- eez %>%
#   st_transform(crs = "+proj=cea +lon_0=-160 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
# 
# ### Carib island shp - to deal with reef polygons that are "on land"
# load("carib_polygons.Rdata")
# 
# ### The polygons are very complex: use gSimplify to simplify their topology for faster processing
# carib_simple <- gSimplify(carib, tol = 0.05, topologyPreserve = TRUE) %>%
#   st_as_sf() %>%
#   st_transform(crs = "+proj=cea +lon_0=-160 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") %>%
#   st_make_valid()

# ### Convert to sf and reproject
# carib_sf <- st_as_sf(carib) %>%
#   st_transform(crs = "+proj=cea +lon_0=-160 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# # ### Combine eezs and islands
# # car_all <- st_union(eez_rpr, carib_sf)
# car_all_simple <- st_union(eez_rpr, carib_simple)
# 
# st_write(car_all, "carib_eez_islands.shp")
# 
# car_all <- st_read("carib_eez_islands.shp")


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