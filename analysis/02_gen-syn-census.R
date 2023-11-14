################################################################################
############ TEST TO GENERATE SYNTHETIC CENSUS USING GEOSPATIAL DATA ###########
################################################################################

### below is the javascript code that generates original built raster from GEE

# // Define the collection and bands
# var esri_lulc_ts = ee.ImageCollection("JRC/GHSL/P2023A/GHS_BUILT_C");
#
# // Define the date range
# var gee_datestart = "2018-01-01";
# var gee_dateend = "2018-12-31";
#
# // Define the path to your bdi_sf geometry on Earth Engine
# var eth_shapefile = ee.FeatureCollection("projects/ee-iedochie/assets/burundi");
#
# var eth_box = eth_shapefile.geometry().bounds()
#
# // Query the Earth Engine data
# var data = esri_lulc_ts
# .filterDate(ee.Date(gee_datestart), ee.Date(gee_dateend))
# .filterBounds(eth_box);
#
# // Mosaic the images within the date range
# var mosaic = data.mosaic();
#
# // Clip the mosaic to the specified region
# //var clippedImage = mosaic.clip(eth_box.geometry());
#
# // Define the export parameters
# var export_params = {
#   image: mosaic,
#   description: "BDI_GHSL_builtup_chrs",
#   folder: "rgee", // Specify the destination folder in your Google Drive
#   scale: 10,
#   region: eth_box,
#   maxPixels: 500000000000//
# };
#
# // Export the clipped mosaic image as a GeoTIFF to Google Drive
# Export.image.toDrive(export_params);

pacman::p_load(sf, data.table, raster,
               reticulate, terra, ggplot2,
               dplyr, crsuggest, osmdata,
               foreach, doParallel, future,
               furrr)

### read in the stored data from file
builtup_dir <- "//esapov/esapov/BDI/GEO/BuildingFootprints/ghsl_builtup"

### dissolve the geometries with overlap or intersection
ghslbuilt_raster <- raster(paste0(builtup_dir, "/BDI_GHSL_builtup_chrs.tif"))

# rcl_list <- c(0, 6, NA, 10, 16, 1, 20, 26, NA)
#
# rcl_matrix <- matrix(rcl_list, ncol = 3, byrow = TRUE)
#
# ghslbuilt_raster <- raster::reclassify(ghslbuilt_raster,
#                                        rcl_matrix)
#
# # raster::writeRaster(ghslbuilt_raster,
# #                     "data-raw/ghslbuilt_raster_mod.tif")
#
# # ghslbuilt_raster <- raster("data-raw/ghslbuilt_raster_mod.tif")
#
# ghslpolygon_dt <- as.polygons(rast(ghslbuilt_raster))
#
# ghslpolygon_dt <- sf::st_as_sf(ghslpolygon_dt)


# ### RUN ONLY FOR REPLICATION PURPOSES
# ghslresidence_dt <- raster::as.data.frame(ghslbuilt_raster,
#                                           xy = TRUE)
#
# ghslresidence_dt <- ghslresidence_dt[ghslresidence_dt$built_characteristics == 1,]
#
# ghslresidence_dt <- ghslresidence_dt[is.na(ghslresidence_dt$built_characteristics) == FALSE,]
#
# saveRDS(ghslresidence_dt,
#         "data-raw/ghslresidence.RDS")

## bring in the shapefile
ghslresidence_dt <- readRDS("data-raw/ghslresidence.RDS")

ghslresidence_dt <- sf::st_as_sf(ghslresidence_dt,
                                 crs = 4326,
                                 coords = c("x", "y"))

grid_dt <- readRDS("data-raw/ind_list.RDS")

grid_dt <- grid_dt$admin3_grid

grid_dt <- grid_dt[, c("admin0Pcod", "admin1Pcod", "admin2Pcod",
                       "admin3Pcode", "admin2Name", "geometry")] %>%
  sf::st_as_sf(crs = "+init=EPSG:32735",
               agr = "constant") %>%
  sf::st_transform(crs = 4326)

### read in the settlement footprint data
stl_dir <- "//esapov/esapov/BDI/GEO/Population"

stlmt_dt <- sf::st_read(paste0(stl_dir,
                               "/GRID3_Burundi_Settlement_Extents2C_Version_01.01..shp"))


### read in the hotosm data
bldosm_dt <- sf::st_read(paste0("//esapov/esapov/BDI/GEO/BuildingFootprints/osm_bldpoly",
                                "/hotosm_bdi_buildings_polygons.shp"))

#### create residential area shapefile by combining the OSM data with the GHSL residential
#### centroids
# residential_dt <- st_parallel_join(x = bldosm_dt[, c("osm_id")],
#                                    y = ghslresidence_dt[, c("admin3Pcode",
#                                                             "admin2Pcod")],
#                                    numCores = 30,
#                                    splitvar = "admin2Pcod")

residential_dt <- st_join(x = bldosm_dt[,"osm_id"],
                          y = ghslresidence_dt[, c("admin3Pcode", "admin2Pcod")])

saveRDS(residential_dt, "data-raw/residential_combine.RDS")




















