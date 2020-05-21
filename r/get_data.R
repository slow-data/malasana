#===========================================
# @Project: Malasana
# @Name: get_data2
# @author: jprimav
# @date: 2020/05
#===========================================

rm(list=ls())

## --- Libraries
library(here)
require(sf)
sessionInfo()
# R version 3.5.3 (2019-03-11)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)


# Malasana is not an official neighborhood, with formal administrative boundaries.
# Its unofficial boundaries are Calle Fuencarral on the East, Calle San Bernardo on the West, Gran V??a on the South and Calle Carranza on the North,
# which makes it all included in the official barrio "Universidad" of district "Centro"
# Let's use Universidad and sorrounding barrios polygon to limit our analysis to Malasana data then...

# Read Madrid Barrios
barrios <- st_read(dsn = here("shapes", "BARRIOS_ETRS89", "BARRIOS.shp"))
plot(barrios[3])
# Reproject barrios to epsg:4326
barrios4326 <- st_transform(barrios, 4326)

# Spain OSM excerpt downloaded from https://download.geofabrik.de/europe/spain.html and stored in the /shapes subfolder of this project.

# Read buildings (slow)
gis_osm_buildings_a_free_1 <- st_read(dsn = here("shapes", "spain-latest-free.shp", "gis_osm_buildings_a_free_1.shp"))
gis_osm_buildings_a_free_1$geometry # OSM comes with epsg:4346 projection

# Crop to Madrid bounding box
bldg_mad <- st_crop(gis_osm_buildings_a_free_1, barrios4326)
bldg_mad
plot(bldg_mad[1])
# Write object
st_write(obj = bldg_mad, dsn = here("shapes", "bldg_mad", "bldg_mad.shp"), delete_layer = TRUE)

# Read roads (3708286 features! very slow)
gis_osm_roads_free_1 <- st_read(dsn = here("shapes", "spain-latest-free.shp", "gis_osm_roads_free_1.shp"))
# crop to Madrid bounding box
roads_mad <- st_crop(gis_osm_roads_free_1, barrios4326)
# write madrid roads shape file
st_write(roads_mad, here("shapes", "roads_mad", "roads_mad.shp"), delete_layer = TRUE)

# Read landuse (big)
gis_osm_landuse_a_free_1 <- st_read(dsn = here("shapes", "spain-latest-free.shp", "gis_osm_landuse_a_free_1.shp"))
gis_osm_landuse_a_free_1$geometry # OSM comes with epsg:4346 projection

# Crop to Madrid bounding box
landuse_mad <- st_crop(gis_osm_landuse_a_free_1, barrios4326)
landuse_mad
landuse_mad[grep("Dos de Mayo", landuse_mad$name),]
# Write object
st_write(obj = landuse_mad, dsn = here("shapes", "landuse_mad", "landuse_mad.shp"), delete_layer = TRUE)

# Read bodies of water
gis_osm_water_a_free_1 <- st_read(dsn = here("shapes", "spain-latest-free.shp", "gis_osm_water_a_free_1.shp"))
gis_osm_water_a_free_1$geometry # OSM comes with epsg:4346 projection

# Crop to Madrid bounding box
waterbody_mad <- st_crop(gis_osm_water_a_free_1, barrios4326)
waterbody_mad
# Write object
st_write(obj = waterbody_mad, dsn = here("shapes", "waterbody_mad", "waterbody_mad.shp"), delete_layer = TRUE)

# Read waterways
gis_osm_waterways_free_1 <- st_read(dsn = here("shapes", "spain-latest-free.shp", "gis_osm_waterways_free_1.shp"))
gis_osm_waterways_free_1$geometry # OSM comes with epsg:4346 projection

# Crop to Madrid bounding box
waterway_mad <- st_crop(gis_osm_waterways_free_1, barrios4326)
waterway_mad
# Write object
st_write(obj = waterway_mad, dsn = here("shapes", "waterway_mad", "waterway_mad.shp"), delete_layer = TRUE)