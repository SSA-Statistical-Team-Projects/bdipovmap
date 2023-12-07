################################################################################
############ TEST TO GENERATE SYNTHETIC CENSUS USING GEOSPATIAL DATA ###########
################################################################################

sf::sf_use_s2(FALSE)

pacman::p_load(sf, data.table, raster,
               reticulate, terra, ggplot2,
               dplyr, crsuggest, osmdata,
               foreach, doParallel, future,
               furrr)

### read in the stored data from file
builtup_dir <- "//esapov/esapov/BDI/GEO/BuildingFootprints/ghsl_builtup"

### dissolve the geometries with overlap or intersection
ghslbuilt_raster <- raster(paste0(builtup_dir, "/BDI_GHSL_builtup_chrs.tif"))


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
pop_dir <- "//esapov/esapov/BDI/GEO/Population"
bld_dir <- "//esapov/esapov/BDI/GEO/BuildingFootprints"

##### ------------------------------------------------------------------------------- #####
##### CREATE A GEOSPATIAL DATASET OF BUILDINGS AND POPULATIONS AS THE GEOCENSUS FRAME #####
##### ------------------------------------------------------------------------------- #####
#### read in the WorldPop population data

pop_dt <- raster(paste0(pop_dir,
                        "/bdi_ppp_2020_UNadj_constrained.tif")) %>%
  rasterToPolygons() %>%
  st_as_sf()

pop_dt$bld_count <-
  raster(paste0(bld_dir,
                "/BDI_buildings_v2_0_count.tif")) %>%
  exactextractr::exact_extract(y = pop_dt,
                               fun = "sum")

### expand pop_dt into a building dataset
pop_dt$popgrid_id <- 1:nrow(pop_dt)

pop_dt <-
  pop_dt %>%
  mutate(area = st_area(pop_dt))

### expand the population dataset to be at the building level
bldcensus_dt <-
  pop_dt %>%
  st_drop_geometry() %>%
  as.data.table() %>%
  mutate(bld_count = as.integer(bld_count))

bldcensus_dt <- bldcensus_dt[rep(seq_len(.N), bld_count)]

bldcensus_dt[, estbld_pop := bdi_ppp_2020_UNadj_constrained / bld_count]

### merge in the geometry and create random point in each polygon
bldcensus_dt <-
  bldcensus_dt %>%
  merge(pop_dt[, c("popgrid_id")])

bldcensus_dt <-
  bldcensus_dt %>% st_as_sf(crs = 4326,
                            agr = "constant")

bldcensus_dt <-
  bldcensus_dt %>%
  st_centroid()


#### save the synthetic census

saveRDS(bldcensus_dt, "data-clean/synthetic_census.RDS")

























