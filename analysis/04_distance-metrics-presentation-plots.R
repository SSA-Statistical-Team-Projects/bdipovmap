################################################################################
##################### DISTANCE METRICS FOR DIAGNOSTICS #########################
################################################################################

pacman::p_load(dplyr, data.table, sf, raster, ggplot2, tidyverse)

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

add_dt <- hh_dt[, lapply(.SD, replace_na_with_avg), .SDcols = dist_cols, by = "province"]

hh_dt <- cbind(hh_dt[,colnames(hh_dt)[!colnames(hh_dt) %in% colnames(add_dt)], with = F],
               add_dt)

### remove all the i. variables
# hh_dt <- hh_dt[, colnames(hh_dt)[!grepl("^i.", colnames(hh_dt))], with = F]


closest_cols <- c("closest_market", "closest_hospital", "closest_bank",
                  "closest_capital_city", "closest_capital_city_linear")

hh_dt[, (closest_cols) := lapply(.SD, function(x) (x / 1000)), .SDcols = closest_cols]

hh_dt[, hhweight := hh_size * weight]

urban_indicators <-
  hh_dt[, lapply(.SD, weighted.mean, w = hhweight, na.rm = TRUE), .SDcols = dist_cols, by = "urban"]

decile_indicators <-
  hh_dt[, lapply(.SD, weighted.mean, w = hhweight, na.rm = TRUE), .SDcols = dist_cols, by = "cons_decile"]

custom_labels <- c("market", "hospital", "bank", "capital city", "capital city linear",
                   "major road", "road intersection", "major waterway")

###### put together the plots
urban_dt <-
urban_indicators %>%
  gather(key = "indicator", value = "distance_km", -urban) %>%
  mutate(sector = ifelse(urban == 1, "Urban", "Rural")) %>%
  mutate(sector = as.factor(sector))

urban_dt %>%
  ggplot(aes(x = indicator, y = distance_km, fill = sector)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(labels = setNames(custom_labels, unique(urban_dt$indicator))) +
  labs(title = "Distance to Nearest POI",
       x = "Indicator",
       y = "Distance (in km)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


decile_dt <-
  decile_indicators %>%
  gather(key = "indicator", value = "distance_km", -cons_decile)

ggplot(decile_dt, aes(x = indicator, y = distance_km, fill = factor(cons_decile))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(labels = setNames(custom_labels, unique(decile_dt$indicator))) +
  scale_fill_manual(values = scales::viridis_pal()(length(unique(decile_dt$cons_decile))), name = "Decile") +
  labs(title = "Distance to Nearest POI",
       x = "Indicator",
       y = "Distance (in km)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### geospatial plot to show regional variation

write.csv(hh_dt[, colnames(hh_dt)[!(grepl("geometry", colnames(hh_dt)))], with = F],
          "data-clean/distance_metrics.csv")































