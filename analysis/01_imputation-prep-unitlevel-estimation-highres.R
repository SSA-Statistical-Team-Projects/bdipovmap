################################################################################
################## POVERTY MAPPING FOR ADMIN 3 AND ADMIN 4 #####################
################################################################################
pacman::p_load("haven", "sf", "data.table", "dplyr", "ggplot2", "nlme", "RStata",
               "exactextractr", "raster", "povmap", "viridis", "glmmLasso",
               "lme4", "gridExtra", "MASS", "furrr", "purrr")

sf::sf_use_s2(FALSE)

### read in the datasets

grid_dt <- readRDS("data-raw/ind_list.RDS")

grid_dt <- grid_dt$admin4_poppoly ### the geospatial grid data

pop_dt <- readRDS("data-clean/worldpop.RDS")

grid_dt$admin4Pcode <- as.integer(grid_dt$admin4Pcode)
pop_dt$admin4Pcode <- as.integer(pop_dt$admin4Pcode)

grid_dt <- merge(grid_dt,
                 pop_dt[,c("admin4Pcode",
                           "bdi_ppp_2020_UNadj_constrained")] %>%
                   st_drop_geometry())

geosurvey_dt <- haven::read_dta("data-raw/hh_poverty.dta") ### household survey


geosurvey_dt <-
  geosurvey_dt %>%
  rename(welfare = "rae_tot_cons")

geosurvey_dt %>%
  mutate(poor = ifelse(welfare < pline_abs_lb_ae, 1, 0)) %>%
  summarise(weighted.mean(x = poor,
                          w = weight_adj * hh_size,
                          na.rm = TRUE))


geocodes_dt <-
  haven::read_spss("data-raw/Coordonnées GPS des ménages enquêtés.sav")

geocodes_dt <- as.data.table(geocodes_dt)
geosurvey_dt <- as.data.table(geosurvey_dt)

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


### read in the new admin shapefiles
shp_dt <- sf::st_read(dsn = "data-raw/diva-gis",
                      layer = "BDI_adm4")


### relabel shapefile area names
shp_dt <-
  shp_dt %>%
  rename(admin0Pcod = "ISO",
         admin0Name = "NAME_0",
         admin1Pcod = "ID_1",
         admin1Name = "NAME_1",
         admin2Pcod = "ID_2",
         admin2Name = "NAME_2",
         admin3Pcod = "ID_3",
         admin3Name = "NAME_3",
         admin4Pcod = "ID_4",
         admin4Name = "NAME_4")

### check the merge between survey, grid and new shapefile
cols_to_remove <- colnames(grid_dt)[grep("^admin.*(?:Name|Pcode|Pcod)$", colnames(grid_dt))]

grid_dt <- grid_dt[, -cols_to_remove, with = F] ##drop the old admin column names

gridpoly_dt <-
  grid_dt %>%
  mutate(grid_ID = 1:nrow(.))

gridpoly_dt <-
  gridpoly_dt[, c("grid_ID", "geometry")] %>%
  st_as_sf(crs = 32735) %>%
  st_transform(crs = 4326)

grid_dt <-
  grid_dt %>%
  mutate(grid_ID = 1:nrow(.)) %>%
  as.data.frame() %>%
  st_as_sf(crs = 32735) %>%
  st_transform(crs = 4326) %>%
  st_centroid() %>%
  st_join(shp_dt)

missing_admin <-
st_nearest_feature(x = grid_dt[is.na(grid_dt$admin4Pcod),],
                   y = shp_dt)

missing_dt <- shp_dt[missing_admin,]

add_dt <-
  grid_dt[is.na(grid_dt$admin4Pcod), colnames(grid_dt)[!colnames(grid_dt) %in% colnames(shp_dt)]] %>%
  cbind(missing_dt %>%
          st_drop_geometry())

### fix the variable names in add_dt to match grid_dt
colnames(add_dt) <- sub("^X(\\d{4})", "\\1", colnames(add_dt))


grid_dt <-
  grid_dt[!is.na(grid_dt$admin4Pcod),] %>%
  rbind(add_dt)

geosurvey_dt <-
  geosurvey_dt[, c("hhid", "hh_size", "weight", "weight_adj",
                   "welfare", "pline_abs_lb_ae",
                   "pline_int_215", "SI15", "SI16")] %>%
  st_as_sf(crs = 4326,
           coords = c("SI16", "SI15")) %>%
  st_join(shp_dt)

### create a geospatial pseudo-welfare aggregate variable at the grid level using NTL
hhgeo_plot <-
  ggplot() +
  geom_sf(data = shp_dt, fill = "white", color = "black") +
  geom_sf(data = geosurvey_dt %>%
            as.data.frame() %>%
            st_as_sf(),
          color = "red") +
  labs(title = "Household locations within Burundi",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

ggsave(plot = hhgeo_plot,
       "figures/hhlocations_plot.png")

### create some variables for growth in ntl
grid_dt <- as.data.table(grid_dt)

ntl_all_cols <- c("ntl_all_2015", "ntl_all_2016", "ntl_all_2017",
                  "ntl_all_2018", "ntl_all_2019", "ntl_all_2020")

grid_dt[, ntl_all_cols] <- grid_dt[, ntl_all_cols, with = F] + 1

ntl_built_up_cols <- c("ntl_built_up_2015", "ntl_built_up_2016", "ntl_built_up_2017",
                       "ntl_built_up_2018", "ntl_built_up_2019", "ntl_built_up_2020")

grid_dt[, ntl_built_up_cols] <- grid_dt[, ntl_built_up_cols, with = F] + 1

ntl_small_stl_cols <- c("ntl_small_stl_2015", "ntl_small_stl_2016", "ntl_small_stl_2017",
                        "ntl_small_stl_2018", "ntl_small_stl_2019", "ntl_small_stl_2020")

grid_dt[, ntl_small_stl_cols] <- grid_dt[, ntl_small_stl_cols, with = F] + 1

ntl_hamlet_cols <- c("ntl_hamlet_2015", "ntl_hamlet_2016", "ntl_hamlet_2017",
                     "ntl_hamlet_2018", "ntl_hamlet_2019", "ntl_hamlet_2020")

grid_dt[, ntl_hamlet_cols] <- grid_dt[, ntl_hamlet_cols, with = F] + 1

ntl_cols <- c(ntl_all_cols, ntl_built_up_cols, ntl_small_stl_cols, ntl_hamlet_cols)

grid_dt[, ntl_cols] <- log(grid_dt[, ntl_cols, with = F])

grid_dt[, ntl_all_growth := ntl_all_2020 - ntl_all_2015]
grid_dt[, ntl_built_up_growth := ntl_built_up_2020 - ntl_built_up_2015]
grid_dt[, ntl_small_stl_growth := ntl_small_stl_2020 - ntl_small_stl_2015]
grid_dt[, ntl_hamlet_growth := ntl_hamlet_2020 - ntl_hamlet_2015]


### growth in population density
grid_dt[, log_pop_density_growth := log(pop_density_2020 + 1) -
          log(pop_density_2015 + 1)]

### growth in each of the different landcover class types
grid_dt[, water_growth := `2020_Water` - `2017_Water`]
grid_dt[, trees_growth := `2020_Trees` - `2017_Trees`]
grid_dt[, floodedveg_growth := `2020_Flooded.vegetation` - `2017_Flooded.vegetation`]
grid_dt[, crops_growth := `2020_Crops` - `2017_Crops`]
grid_dt[, builtarea_growth := `2020_Built.Area` - `2017_Built.Area`]
grid_dt[, bareground_growth := `2020_Bare_ground` - `2017_Bare_ground`]
grid_dt[, snowice_growth := `2020_Snow.Ice` - `2017_Snow.Ice`]
grid_dt[, clouds_growth := `2020_Clouds` - `2017_Clouds`]
grid_dt[, rangeland_growth := `2020_Rangeland` - `2017_Rangeland`]



##### compute the regional welfare totals for each province
add_dt <-
geosurvey_dt %>%
  st_drop_geometry() %>%
  group_by(admin1Pcod) %>%
  summarise(welfare = sum(welfare * weight_adj * hh_size, na.rm = TRUE)) %>%
  merge(grid_dt[, c("grid_ID", "admin1Pcod", "ntl_all_2015",
                    "ntl_all_2016", "ntl_all_2017", "ntl_all_2018",
                    "ntl_all_2019", "ntl_all_2020",
                    "bdi_ppp_2020_UNadj_constrained")] %>%
          st_drop_geometry()) %>%
  group_by(admin1Pcod) %>%
  mutate(ntl2015_welf_int = log(1 + (((ntl_all_2015 / sum(ntl_all_2015)) * welfare))/bdi_ppp_2020_UNadj_constrained)) %>%
  mutate(ntl2016_welf_int = log(1 + (((ntl_all_2016 / sum(ntl_all_2016)) * welfare))/bdi_ppp_2020_UNadj_constrained)) %>%
  mutate(ntl2017_welf_int = log(1 + (((ntl_all_2017 / sum(ntl_all_2017)) * welfare))/bdi_ppp_2020_UNadj_constrained)) %>%
  mutate(ntl2018_welf_int = log(1 + (((ntl_all_2018 / sum(ntl_all_2018)) * welfare))/bdi_ppp_2020_UNadj_constrained)) %>%
  mutate(ntl2019_welf_int = log(1 + (((ntl_all_2019 / sum(ntl_all_2019)) * welfare))/bdi_ppp_2020_UNadj_constrained)) %>%
  mutate(ntl2020_welf_int = log(1 + (((ntl_all_2020 / sum(ntl_all_2020)) * welfare))/bdi_ppp_2020_UNadj_constrained))

add_dt$ntl2016_welf_int[is.na(add_dt$ntl2016_welf_int)] <- 0

grid_dt <-
  grid_dt %>%
  merge(add_dt[, c("grid_ID", "ntl2015_welf_int", "ntl2016_welf_int",
                   "ntl2017_welf_int", "ntl2018_welf_int",
                   "ntl2019_welf_int", "ntl2020_welf_int")])

##### target area variables at admin4, admin3 and admin2
### first make sure all variable names are properly created
colnames(grid_dt) <- gsub("^(\\d)", "y\\1", colnames(grid_dt))
colnames(grid_dt) <- gsub("\\.", "_", colnames(grid_dt))

### rename variable names that are over 32 characters long
invalid_names <- colnames(grid_dt)[nchar(colnames(grid_dt)) >= 32]

valid_names <- gsub(pattern = "_MS_MEAN_v01",
                    replacement = "",
                    invalid_names)

setnames(grid_dt, invalid_names, valid_names)

#### select the set of candidate variables
drop_vars <- c("grid_ID", "area", "poly_area", "ID_0", "admin0Pcod",
               "admin0Name", "admin1Pcod", "admin1Name", "admin2Pcod",
               "admin2Name", "admin3Name", "admin3Pcod", "admin4Name",
               "admin4Pcod", "VARNAME_4", "TYPE_4", "ENGTYPE_4",
               "geometry", "")
candidate_vars <- colnames(grid_dt)[!colnames(grid_dt) %in% drop_vars]

#### replace missing values with admin1 averages
grid_dt[, (candidate_vars) :=
          lapply(.SD, function(x){

            if (any(is.na(x))) {
              mean_val <- mean(x, na.rm = TRUE)
              replace(x, is.na(x), mean_val)
            } else {
              x
            }

          }),
        .SDcols = candidate_vars,
        by = "admin1Pcod"]

## some variables have super large values lets transform them!
columns_to_scale <- sapply(as.data.frame(grid_dt[,candidate_vars, with = F]), function(x) any(abs(x) > 100))

columns_to_scale <- columns_to_scale[columns_to_scale == TRUE & is.na(columns_to_scale) == FALSE]
columns_to_scale <- names(columns_to_scale)
#### log the building and population variables
log_vars <- columns_to_scale[grepl("density", columns_to_scale) | grepl("building", columns_to_scale)]

grid_dt[, (log_vars) := lapply(.SD,
                               function(x)(log(x + 1))),
        .SDcols = log_vars]

scale_vars <- columns_to_scale[!(columns_to_scale %in% log_vars)]

grid_dt[, (scale_vars) := lapply(.SD, scale, center = TRUE), .SDcols = scale_vars]

#### remove variables that are constant
constant_cols <- names(grid_dt)[sapply(grid_dt, function(x) length(unique(x)) == 1)]

grid_dt[, (constant_cols) := NULL]

candidate_vars <- candidate_vars[!(candidate_vars %in% constant_cols)]

### now finally compute the admin average variables
admin1_dt <-
  grid_dt[, lapply(.SD,
                   weighted.mean,
                   w = bdi_ppp_2020_UNadj_constrained,
                   na.rm = TRUE),
          .SDcols = candidate_vars,
          by = "admin1Pcod"]

colnames(admin1_dt)[colnames(admin1_dt) %in% candidate_vars] <-
  paste0("admin1_", colnames(admin1_dt)[colnames(admin1_dt) %in% candidate_vars])

admin2_dt <-
  grid_dt[, lapply(.SD, weighted.mean,
                   w = bdi_ppp_2020_UNadj_constrained,
                   na.rm = TRUE),
          .SDcols = candidate_vars,
          by = "admin2Pcod"]

colnames(admin2_dt)[colnames(admin2_dt) %in% candidate_vars] <-
  paste0("admin2_", colnames(admin2_dt)[colnames(admin2_dt) %in% candidate_vars])

admin3_dt <-
  grid_dt[, lapply(.SD,
                   weighted.mean,
                   w = bdi_ppp_2020_UNadj_constrained,
                   na.rm = TRUE),
          .SDcols = candidate_vars,
          by = "admin3Pcod"]

colnames(admin3_dt)[colnames(admin3_dt) %in% candidate_vars] <-
  paste0("admin3_", colnames(admin3_dt)[colnames(admin3_dt) %in% candidate_vars])

admin4_dt <-
  grid_dt[, lapply(.SD,
                   weighted.mean,
                   w = bdi_ppp_2020_UNadj_constrained,
                   na.rm = TRUE),
          .SDcols = candidate_vars,
          by = "admin4Pcod"]

colnames(admin4_dt)[colnames(admin4_dt) %in% candidate_vars] <-
  paste0("admin4_", colnames(admin4_dt)[colnames(admin4_dt) %in% candidate_vars])


grid_dt <- admin1_dt[grid_dt, on = "admin1Pcod"]
grid_dt <- admin2_dt[grid_dt, on = "admin2Pcod"]
grid_dt <- admin3_dt[grid_dt, on = "admin3Pcod"]
grid_dt <- admin4_dt[grid_dt, on = "admin4Pcod"]

cols_to_remove <- colnames(grid_dt)[grep("^admin.*(?:Name|Pcode|Pcod)$", colnames(grid_dt))]


#### merge in the geo household survey

grid_dt <-
  grid_dt %>%
  as.data.frame() %>%
  st_as_sf(crs = 4326) %>%
  st_drop_geometry() %>%
  merge(gridpoly_dt, by = "grid_ID")

drop_vars <- colnames(grid_dt)[colnames(grid_dt) %in% colnames(geosurvey_dt)]

geosurvey_dt <-
geosurvey_dt %>%
  st_join(grid_dt[,c(colnames(grid_dt)[!colnames(grid_dt) %in% colnames(geosurvey_dt)], "geometry")] %>%
            st_as_sf(crs = 4326))

geosurvey_dt <-
geosurvey_dt %>%
  mutate(lnwelfare = log(welfare + 1))

