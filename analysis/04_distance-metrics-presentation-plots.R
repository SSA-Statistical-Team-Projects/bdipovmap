################################################################################
##################### DISTANCE METRICS FOR DIAGNOSTICS #########################
################################################################################

pacman::p_load(dplyr, data.table, sf, raster)

#### read in the data

hh_dt <- readRDS("data-clean/08-osm_data.rds")

hh_dt <- hh_dt[, c("hhid", "closest_market", "closest_hospital", "closest_bank")]

add_dt <- readRDS("data-clean/access2.rds")

hh_dt <-
  hh_dt %>%
  merge(add_dt)

geosurvey_dt <- haven::read_dta("data-raw/hh_poverty.dta") ### household survey

geocodes_dt <-
  haven::read_spss("data-raw/Coordonnées GPS des ménages enquêtés.sav")

geocodes_dt <- as.data.table(geocodes_dt)
geosurvey_dt <- as.data.table(geosurvey_dt)

#### figure out where all the households with respect to the country
shp_dt <- sf::read_sf(dsn = "data-raw/adm1",
                      layer = "bdi_admbnda_adm1_igebu_ocha_20171103")


setnames(geocodes_dt, c("IDZD", "POIDS"), c("enum_area", "weight"))

geosurvey_dt[, hhno := as.integer(substr(hhid,
                                         nchar(as.character(hhid)) - 1,
                                         nchar(as.character(hhid))))]

## create a household ID from the geocode data
geocodes_dt[, hhid := paste0(enum_area,
                             sprintf("%02d", ID03))]

geocodes_dt[, hhid := as.integer(hhid)]

geosurvey_dt <- geocodes_dt[geosurvey_dt, on = "hhid"][, c("i.enum_area",
                                                           "i.weight") := NULL ]

add_dt <- geosurvey_dt[, c("hhid", "hh_size", "weight", "weight_adj",
                           "rpc_tot_cons", "pline_int_215", "urban")]

hh_dt <-
  hh_dt %>%
  merge(add_dt)

add_dt <- readRDS("data-clean/geosurvey.RDS")

raster_list <- lapply(paste0("//esapov/esapov/BDI/GEO/BuildingFootprints/osm_distance/",
                             list.files(path = "//esapov/esapov/BDI/GEO/BuildingFootprints/osm_distance",
                                        recursive = FALSE)),
                      raster::raster)

add_dt <- hh_dt

add_dt <- add_dt[,c("hhid")] %>%
  st_as_sf(crs = 4326, agr = "constant") %>%
  st_buffer(dist = 0.0008333)

add_dt <-
lapply(X = raster_list,
       FUN = function(x){

         add_dt[[names(x)]] <-
           exactextractr::exact_extract(x = x,
                                        y = add_dt,
                                        fun = "mean")
         add_dt <-
         add_dt %>%
           st_drop_geometry()

         return(add_dt)


       })

add_dt <- Reduce(f = "merge",
                 x = add_dt)

hh_dt <-
  hh_dt %>%
  merge(add_dt)


### create consumption deciles
hh_dt <-
  hh_dt %>%
  mutate(cons_decile = ntile(rpc_tot_cons, 10))

hh_dt <- as.data.table(hh_dt)

dist_cols <- c("closest_market", "closest_hospital", "closest_bank",
               "closest_capital_city", "closest_capital_city_linear",
               "bdi_osm_dst_road_100m_2016", "bdi_osm_dst_roadintersec_100m_2016",
               "bdi_osm_dst_waterway_100m_2016")


hh_dt[, (dist_cols) := lapply(.SD, function(x) ifelse(is.infinite(x), NA, x)), .SDcols = dist_cols]

hh_dt <- geosurvey_dt[,c("hhid", "province")][hh_dt, on = "hhid"]

hh_dt[, (dist_cols) := lapply(.SD, replace_na_with_avg), by = "province"]

urban_indicators <-
  hh_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = dist_cols, by = "urban"]

decile_indicators <-
  hh_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = dist_cols, by = "cons_decile"]





