################################################################################
############### ADD SOME ADDITIONAL VARIABLES INTO THE MODEL ###################
################################################################################

### load the library
pacman::p_load(dplyr, data.table, sf, raster, exactextractr, furrr, purrr)

dir <- "//esapov/esapov/BDI/GEO/Population/age_sex_structures"

agesex_rasterlist <- lapply(X = paste0(dir, "/",
                                       list.files(path = dir,
                                                  pattern = "_constrained_UNadj.tif")),
                            FUN = raster)

name_list <- unlist(lapply(agesex_rasterlist, names))

name_list <- gsub(pattern = "_2020_constrained_UNadj$",
                  replacement = "",
                  x = name_list)

#### read in the geographical census data

grid_dt <- readRDS("data-raw/ind_list.RDS")

grid_dt <- grid_dt$admin3_grid ### the geospatial grid data
grid_dt <- grid_dt[,c("admin1Pcod", "admin2Pcod", "admin2Name",
                      "admin3Pcode", "geometry")]

grid_dt <-
  grid_dt %>%
  mutate(admin3Pcod = paste0(admin2Pcod,
                             sprintf(paste0("%0", max(nchar(admin3Pcode)), "d"),
                                     admin3Pcode)))

grid_dt <-
  grid_dt %>%
  st_as_sf(crs = st_crs("+init=EPSG:32735"))

names(agesex_rasterlist) <- name_list

### extract age sex raster into the shapefile

grid_dt <- grid_dt[, c("admin3Pcod", "geometry")] %>%
  st_as_sf(crs = 4326)

### including the worldpop grids
pop_dir <- "//esapov/esapov/BDI/GEO/Population"
pop_dt <- raster(paste0(pop_dir,
                        "/bdi_ppp_2020_UNadj_constrained.tif")) %>%
  rasterToPolygons() %>%
  st_as_sf()




extract_list <-
  lapply(X = agesex_rasterlist,
         FUN = function(x){

           y <- exact_extract(x = x,
                              y = pop_dt,
                              fun = "sum")

           return(y)

         })

extract_dt <- as.data.table(extract_list)




















