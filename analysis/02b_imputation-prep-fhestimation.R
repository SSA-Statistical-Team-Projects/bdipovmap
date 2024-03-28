################################################################################
############### PREPARING THE SURVEY AND CENSUS DATA FOR SAE ###################
################################################################################

pacman::p_load("haven", "sf", "data.table", "dplyr", "ggplot2", "nlme", "RStata",
               "exactextractr", "raster", "povmap", "viridis", "glmmLasso",
               "lme4", "gridExtra", "MASS", "furrr", "purrr", "survey", "hdm",
               "sae", "moments")

sf::sf_use_s2(FALSE)

### read in the datasets

grid_dt <- readRDS("data-raw/ind_list.RDS")

grid_dt <- grid_dt$admin4_poppoly ### the geospatial grid data

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

#### include the grid area information with the geosurvey data
# grid_dt <-
# grid_dt %>%
#   mutate(admin4Pcod = paste0(admin3Pcod,
#                              sprintf(paste0("%0", max(nchar(admin4Pcode)), "d"),
#                                      admin4Pcode)))

admin_dt <- readRDS("data-clean/bdi_gridded_v02.rds")
admin_dt <- admin_dt$admin4_poppoly

grid_dt <- merge(x = grid_dt %>%
                   as.data.frame() %>%
                   st_as_sf(crs = 32735) %>%
                   st_drop_geometry(),
                 y = admin_dt[, c("admin4Pcode", "geometry")])

grid_dt <-
  grid_dt %>%
  as.data.frame() %>%
  st_as_sf(crs = 32735)

geosurvey_dt <-
  geosurvey_dt[, c("hhid", "hh_size", "weight", "weight_adj",
                   "rpc_tot_cons",
                   "pline_int_215", "SI15", "SI16")] %>%
  st_as_sf(crs = 4326,
           coords = c("SI16", "SI15")) %>%
  st_transform(crs = st_crs("+init=EPSG:32735")) %>%
  st_join(grid_dt)


#### check out the welfare distribution
welfare_plot <-
  geosurvey_dt %>%
  ggplot(aes(x = log(rpc_tot_cons))) +
  geom_histogram(binwidth = 0.1,
                 fill = "blue",
                 color = "black",
                 alpha = 0.7) +
  labs(title = "Welfare Distribution Histogram",
       x = "Log Welfare per capita",
       y = "Frequency") +
  theme_minimal()

ggsave(plot = welfare_plot,
       "figures/log_welfare_histogram.png")

saveRDS(geosurvey_dt, "data-clean/geosurvey.RDS")

#### we need to figure out why 624 obs dont merge
missing_dt <-
  geosurvey_dt[is.na(geosurvey_dt$admin2Pcod),] %>%
  as.data.frame() %>%
  merge(geocodes_dt[, c("hhid", "SI15", "SI16")]) %>%
  st_as_sf(crs = 4326,
           coords = c("SI16", "SI15")) %>%
  st_transform(crs = 32735)

test_dt <- readRDS("data-clean/bdi_gridded_v02.rds")

test_dt <- test_dt$admin2_Communes

# missing_dt <- st_join(missing_dt[,c("geometry", "hhid")],
#                       test_dt %>%
#                       st_transform(crs = 32735))

add_dt <- readRDS("data-raw/ind_list.RDS")

add_dt <- add_dt$admin2_Communes ### the geospatial grid data

test_dt <- merge(test_dt %>%
                   as.data.frame() %>%
                   st_as_sf(crs = 4326) %>%
                   st_drop_geometry(),
                 add_dt %>%
                   as.data.frame() %>%
                   st_as_sf(crs = 32735) %>%
                   st_transform(crs = 4326))

missing_dt <- st_join(missing_dt[, c("hhid", "hh_size", "weight",
                                     "weight_adj", "rpc_tot_cons",
                                     "pline_int_215")] %>%
                        st_transform(crs = 4326),
                      test_dt %>%
                        as.data.frame() %>%
                        st_as_sf(crs = 4326))
### remove area and poly_area variables from geosurvey_dt to match missing_dt
geosurvey_dt$poly_area <- NULL
geosurvey_dt$area <- NULL

missing_dt$admin3Pcode <- NA
missing_dt$admin4Pcode <- NA

geosurvey_dt <- rbind(geosurvey_dt[!is.na(geosurvey_dt$admin2Pcod),] %>%
                        st_transform(crs = 4326),
                      missing_dt[, colnames(geosurvey_dt)])

geosurvey_dt <- as.data.table(geosurvey_dt)

geosurvey_dt[, targetarea_codes := as.integer(substr(admin2Pcod, 4, nchar(admin2Pcod)))]
geosurvey_dt[, hhweight := weight * hh_size]

# ## drop variables that have too many NAs or NaNs
# geosurvey_dt[is.na(geosurvey_dt)] <- 0
# grid_dt[is.na(grid_dt)] <- 0


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


##### create the same variables within the geosurvey_dt
geosurvey_dt <- as.data.table(geosurvey_dt)

geosurvey_dt[, ntl_all_cols] <- geosurvey_dt[, ntl_all_cols, with = F] + 1


geosurvey_dt[, ntl_built_up_cols] <-
  geosurvey_dt[, ntl_built_up_cols, with = F] + 1

geosurvey_dt[, ntl_small_stl_cols] <-
  geosurvey_dt[, ntl_small_stl_cols, with = F] + 1

geosurvey_dt[, ntl_hamlet_cols] <- geosurvey_dt[, ntl_hamlet_cols, with = F] + 1

geosurvey_dt[, ntl_cols] <- log(geosurvey_dt[, ntl_cols, with = F])

geosurvey_dt[, ntl_all_growth := ntl_all_2020 - ntl_all_2015]
geosurvey_dt[, ntl_built_up_growth := ntl_built_up_2020 - ntl_built_up_2015]
geosurvey_dt[, ntl_small_stl_growth := ntl_small_stl_2020 - ntl_small_stl_2015]
geosurvey_dt[, ntl_hamlet_growth := ntl_hamlet_2020 - ntl_hamlet_2015]


### growth in population density
geosurvey_dt[, log_pop_density_growth := log(pop_density_2020 + 1) -
               log(pop_density_2015 + 1)]

### growth in each of the different landcover class types
geosurvey_dt[, water_growth := `2020_Water` - `2017_Water`]
geosurvey_dt[, trees_growth := `2020_Trees` - `2017_Trees`]
geosurvey_dt[, floodedveg_growth := `2020_Flooded.vegetation` -
               `2017_Flooded.vegetation`]
geosurvey_dt[, crops_growth := `2020_Crops` - `2017_Crops`]
geosurvey_dt[, builtarea_growth := `2020_Built.Area` - `2017_Built.Area`]
geosurvey_dt[, bareground_growth := `2020_Bare_ground` - `2017_Bare_ground`]
geosurvey_dt[, snowice_growth := `2020_Snow.Ice` - `2017_Snow.Ice`]
geosurvey_dt[, clouds_growth := `2020_Clouds` - `2017_Clouds`]
geosurvey_dt[, rangeland_growth := `2020_Rangeland` - `2017_Rangeland`]


# ### remove zero population grids
# grid_dt <-
#   grid_dt %>%
#   as.data.frame() %>%
#   st_as_sf(crs = 32735) %>%
#   st_transform(crs = 4326)


### read in the population raster
# pop_dir <- "//esapov/esapov/BDI/GEO/Population"
# pop_dt <- raster(paste0(pop_dir,
#                         "/bdi_ppp_2020_UNadj_constrained.tif")) %>%
#   rasterToPolygons() %>%
#   st_as_sf()
#
# pop_dt$admin4Pcode <- 1:nrow(pop_dt)
#
# pop_dt <-
#   pop_dt %>%
#   mutate(admin4Pcode = as.character(admin4Pcode))
#
# saveRDS(pop_dt, "data-clean/worldpop.RDS")

pop_dt <- readRDS("data-clean/worldpop.RDS")

grid_dt <- merge(grid_dt,
                 pop_dt[,c("admin4Pcode",
                           "bdi_ppp_2020_UNadj_constrained")] %>%
                   st_drop_geometry())

grid_dt <- as.data.table(grid_dt)

grid_dt <- grid_dt[bdi_ppp_2020_UNadj_constrained > 0,]


### include the admin2 variables as well
admin2_dt <- readRDS("data-raw/ind_list.RDS")

admin2_dt <- admin2_dt$admin2_Communes

admin2_dt <- as.data.table(admin2_dt)

rename_vars <-
  colnames(admin2_dt)[!grepl(pattern = "^admin|^area$",
                             x = colnames(admin2_dt))]

rename_vars <- paste0("targetave_", rename_vars)

setnames(admin2_dt,
         colnames(admin2_dt)[!grepl(pattern = "^admin|^area$",
                                    x = colnames(admin2_dt))],
         rename_vars)

grid_dt <- merge(grid_dt,
                 admin2_dt,
                 by = "admin2Pcod")

### add to the geosurvey
add_dt <-
  grid_dt[, c(rename_vars, "geometry"), with = F] %>%
  as.data.frame() %>%
  st_as_sf(crs = 32735) %>%
  st_transform(crs = 4326)


geosurvey_dt <-
  geosurvey_dt %>%
  as.data.frame() %>%
  st_as_sf(crs = 4326)

geosurvey_dt <- st_join(geosurvey_dt, add_dt)


### replace missing values with average value in admin1
target_cols <- grep("^targetave", names(geosurvey_dt), value = TRUE)

geosurvey_dt <- as.data.table(geosurvey_dt)

geosurvey_dt[, (target_cols) := lapply(.SD, function(x) {
  if (any(is.na(x))) {
    mean_val <- mean(x, na.rm = TRUE)
    replace(x, is.na(x), mean_val)
  } else {
    x
  }
}), .SDcols = target_cols,
by = "admin1Pcod"]

### replace variable names that start with digits
colnames(geosurvey_dt) <- gsub("^(\\d)", "y\\1", colnames(geosurvey_dt))
colnames(grid_dt) <- gsub("^(\\d)", "y\\1", colnames(grid_dt))

### quickly change all variable names that contain "."
colnames(geosurvey_dt) <- gsub("\\.", "_", colnames(geosurvey_dt))
colnames(grid_dt) <- gsub("\\.", "_", colnames(grid_dt))

### create area level dummies
geosurvey_dt <- cbind(geosurvey_dt,
                      as.data.table(dummify(geosurvey_dt$admin1Pcod)))

grid_dt <- cbind(grid_dt,
                 as.data.table(dummify(grid_dt$admin1Pcod)))


### include the admin2_dt variables into geosurvey_dt

### quickly compute poverty rate
geosurvey_dt %>%
  mutate(poor = ifelse(rpc_tot_cons < pline_int_215, 1, 0)) %>%
  summarise(intpovrate = weighted.mean(x = poor,
                                       w = weight * hh_size,
                                       na.rm = TRUE))

### rename variable names that are over 32 characters long
invalid_names <- colnames(geosurvey_dt)[nchar(colnames(geosurvey_dt)) >= 32]

### remove the ms_mean_v01 within the dhs data variable names
valid_names <- gsub(pattern = "_MS_MEAN_v01",
                    replacement = "",
                    invalid_names)

valid_names <- gsub(pattern = "_Flooded_vegetation",
                    replacement = "_fld_veg",
                    valid_names)

setnames(geosurvey_dt, invalid_names, valid_names)
setnames(grid_dt, invalid_names, valid_names)

################################################################################
################### MODEL SELECTION ANALYTICS PRE-IMPUTATION ###################
################################################################################
remove_vars <- c("hhid", "hh_size", "weight", "weight_adj", "hhweight",
                 "pline_int_215", "area", "poly_area", "rpc_tot_cons",
                 "geometry", "swe_sq_hist_dev", "targetave_swe_change_10",
                 "targetave_swe_sq_hist_dev", "targetave_geometry",
                 "targetarea_codes",
                 colnames(geosurvey_dt)[grepl("admin",
                                              colnames(geosurvey_dt))])

candidate_vars <- colnames(geosurvey_dt)[!colnames(geosurvey_dt) %in% remove_vars]

### turn grid and geospatial survey into data.tables
grid_dt <- as.data.table(grid_dt)
geosurvey_dt <- as.data.table(geosurvey_dt)

geosurvey_dt[, lnrpc_tot_cons := log(rpc_tot_cons + 1)]

## some variables have super large values lets transform them!
columns_to_scale <- sapply(as.data.frame(geosurvey_dt[,candidate_vars, with = F]), function(x) any(abs(x) > 100))

columns_to_scale <- columns_to_scale[columns_to_scale == TRUE & is.na(columns_to_scale) == FALSE]
columns_to_scale <- names(columns_to_scale)
#### log the building and population variables
log_vars <- columns_to_scale[grepl("density", columns_to_scale) | grepl("building", columns_to_scale)]

geosurvey_dt[, (log_vars) := lapply(.SD,
                                    function(x)(log(x + 1))),
             .SDcols = log_vars]

grid_dt[, (log_vars) := lapply(.SD,
                               function(x)(log(x + 1))),
        .SDcols = log_vars]

scale_vars <- columns_to_scale[!(columns_to_scale %in% log_vars)]

geosurvey_dt[, (scale_vars) := lapply(.SD, scale, center = TRUE), .SDcols = scale_vars]
grid_dt[, (scale_vars) := lapply(.SD, scale, center = TRUE), .SDcols = scale_vars]


model_dt <- geosurvey_dt[, c(candidate_vars,
                             "lnrpc_tot_cons",
                             "hhweight",
                             "targetarea_codes"),
                         with = FALSE]

model_dt[, names(model_dt) := lapply(.SD,
                                     function(x) replace(x, is.na(x), 0))]

model_dt <- as.data.frame(model_dt)

#### remove variables that are constant
constant_cols <- names(model_dt)[sapply(model_dt, function(x) length(unique(x)) == 1)]

# Remove constant columns
model_dt <- as.data.table(model_dt)
model_dt[, (constant_cols) := NULL]

candidate_vars <- candidate_vars[!(candidate_vars %in% constant_cols)]

model_dt <- as.data.frame(model_dt)

# haven::write_dta(geosurvey_dt[, c("lnrpc_tot_cons",
#                                   candidate_vars,
#                                   "weight",
#                                   "admin2Pcod"), with = FALSE],
#                  "data-raw/model.dta")

### first lets see how many households there are per grid
geosurveycheck_dt <-
  geosurvey_dt %>%
  dplyr::select(hhid, geometry) %>%
  st_as_sf(crs = 4326,
           agr = "constant")

geosurveycheck_dt <-
  geosurveycheck_dt %>%
  st_join(pop_dt)

geosurveycheck_dt <- as.data.table(geosurveycheck_dt)

grid_dist <- geosurveycheck_dt[is.na(admin4Pcode) == FALSE, length(hhid), by = admin4Pcode]
setnames(grid_dist, "V1", "grid_count")

grid_dist %>%
ggplot() +
  geom_histogram(aes(grid_count))

### histogram of number of households per grid


### prepare the grid level data for the area model estimation fay herriot style
candidate_vars <- colnames(grid_dt)[grepl("targetave",
                                          colnames(grid_dt))]

candidate_vars <- candidate_vars[!grepl("geometry", candidate_vars)]

commune_dt <- grid_dt[, lapply(.SD,
                               weighted.mean,
                               w = bdi_ppp_2020_UNadj_constrained),
                      .SDcols = candidate_vars,
                      by = admin2Pcod]

### compute variance and design effect
geosurvey_dt <-
geosurvey_dt %>%
  merge(geocodes_dt[, c("hhid", "enum_area")])

geosurvey_dt[, poor := ifelse(rpc_tot_cons < pline_int_215, 1, 0)]

geosvy_obj <- survey::svydesign(id = ~enum_area,
                                weights = ~hhweight,
                                survey.lonely.psu = "adjust",
                                data = geosurvey_dt)

areapop_dt <- svyby(~poor,
                    ~admin1Pcod,
                    geosvy_obj,
                    svymean,
                    deff = TRUE)

areapop_dt <- as.data.table(areapop_dt)

target_dt <- unique(geosurvey_dt[, c("admin1Pcod", "admin2Pcod", "targetarea_codes")])

areapop_dt <- target_dt[areapop_dt, on = "admin1Pcod"]

add_dt <- geosurvey_dt[, length(hhid), by = c("admin1Pcod", "admin2Pcod")] %>%
  setnames(old = "V1", new = "sample_size") ## compute size of sample in each target area

add_dt <- add_dt[!is.na(admin1Pcod),] ##drop missing observations (only 1)

areapop_dt <- add_dt[areapop_dt,
                     on = c("admin1Pcod",
                            "admin2Pcod")]

areapop_dt[, ess := sample_size / DEff.poor]

### re-estimate the actual poverty rates
pov_dt <-
  geosurvey_dt[, weighted.mean(x = poor,
                               w = hhweight,
                               na.rm = TRUE),
               by = c("admin1Pcod", "admin2Pcod")] %>%
  setnames(old = "V1", new = "Direct")

areapop_dt <- pov_dt[areapop_dt, on = c("admin1Pcod", "admin2Pcod")]

commune_dt <- areapop_dt[commune_dt, on = "admin2Pcod"]

## check poverty rate
geosurvey_dt %>%
  summarise(weighted.mean(x = poor,
                          w = hhweight,
                          na.rm = TRUE))

### add population to commune level data
communepop_dt <-
  grid_dt[, sum(bdi_ppp_2020_UNadj_constrained, na.rm = TRUE),
            by = admin2Pcod] %>%
  setnames(old = "V1", new = "population")


commune_dt <- communepop_dt[commune_dt, on = "admin2Pcod"]

### add province dummies
setnames(grid_dt, "admin1Pcod_x", "admin1Pcod")
grid_dt[, admin1Pcod_y := NULL]

add_dt <- as.data.table(grid_dt[, dummify(admin1Pcod)])
grid_dt <- cbind(grid_dt, add_dt)

add_dt <- unique(grid_dt[, c(colnames(grid_dt)[grepl("BDI",
                                                   colnames(grid_dt))],
                             "admin2Pcod"),
                         with = FALSE])

commune_dt <- add_dt[commune_dt, on = "admin2Pcod"]

candidate_vars <- c(candidate_vars,
                    colnames(add_dt)[grepl("BDI",
                                           colnames(add_dt))])

haven::write_dta(commune_dt[, c("Direct",
                                candidate_vars,
                                "population",
                                "admin1Pcod",
                                "admin2Pcod"), with = F],
                 "data-clean/commune_data.dta")

### next we perform model selection
selvars_list <- countrymodel_select(dt = commune_dt,
                                    xvars = candidate_vars,
                                    y = "Direct")

### ran the same thing in stata and the results seem better
stata_list <- c("targetave_prplit_conf90_sy_2019",
                "targetave_2018_Snow_Ice",
                "targetave_building_mean_area")


### identify spatial structures




### compute the fh model
domsize_dt <-
  geosurvey_dt[, sum(hhweight, na.rm = TRUE), by = admin2Pcod] %>%
  setnames(old = "V1", new = "domsize") %>%
  filter(!is.na(admin2Pcod))

saepop_dt <-
  sae::direct(y = poor,
              dom = admin2Pcod,
              sweight = hhweight,
              domsize = domsize_dt,
              data = geosurvey_dt) %>%
  mutate(var = SD ^ 2)

setnames(saepop_dt, "Domain", "admin2Pcod")

commune_dt$Direct <- NULL
saepop_dt <- as.data.table(saepop_dt)
commune_dt <- saepop_dt[commune_dt, on = "admin2Pcod"]

### combine data
### first find columns with all NAs

na_vars <- colnames(commune_dt)[apply(commune_dt, 2, function(x) any(is.na(x)))]

candidate_vars <- candidate_vars[!candidate_vars %in% na_vars]

combine_dt <- povmap::combine_data(pop_data = commune_dt[, c(selvars_list,
                                                             "targetarea_codes"),
                                                         with = FALSE],
                                   pop_domains = "targetarea_codes",
                                   smp_data = commune_dt[!is.na(Direct),
                                                         c("targetarea_codes",
                                                           "Direct",
                                                           "var",
                                                           "ess"),
                                                         with = FALSE],
                                   smp_domains = "targetarea_codes")

fhmodel_not <-
  povmap::fh(fixed = as.formula(paste("Direct ~ ", paste(selvars_list, collapse= "+"))),
             vardir = "var",
             combined_data = combine_dt,
             domains = "targetarea_codes",
             method = "ml",
             MSE = TRUE,
             mse_type = "analytical")

fhmodel_log <-
  povmap::fh(fixed = as.formula(paste("Direct ~ ", paste(selvars_list, collapse= "+"))),
             vardir = "var",
             combined_data = combine_dt,
             domains = "targetarea_codes",
             method = "ml",
             MSE = TRUE,
             mse_type = "analytical",
             transformation = "log",
             backtransformation = "bc_crude")

fhmodel_arcsin <-
  povmap::fh(fixed = as.formula(paste("Direct ~ ", paste(selvars_list, collapse= "+"))),
             vardir = "var",
             combined_data = combine_dt,
             domains = "targetarea_codes",
             method = "ml",
             MSE = TRUE,
             mse_type = "boot",
             transformation = "arcsin",
             backtransformation = "bc",
             eff_smpsize = "ess")




result_dt <- as.data.table(fhmodel_arcsin$ind)




#### compare province level rates
fhpov_dt <-
  result_dt %>%
  merge(unique(commune_dt[, c("population",
                              "targetarea_codes",
                              "admin1Pcod")]),
        by.y = "targetarea_codes",
        by.x = "Domain") %>%
  group_by(admin1Pcod) %>%
  summarise(provFH = weighted.mean(x = FH,
                                   w = population,
                                   na.rm = TRUE)) %>%
  merge(geosurvey_dt %>%
          group_by(admin1Pcod) %>%
          summarise(provDirect = weighted.mean(x = poor,
                                               w = hhweight,
                                               na.rm = TRUE)),
        by = "admin1Pcod")


### write the poverty rates to file
setnames(grid_dt, "admin2Name_y", "admin2Name")

grid_dt[, targetarea_codes := as.integer(substr(admin2Pcod, 4, nchar(admin2Pcod)))]

result_dt <-
  result_dt %>%
  merge(unique(grid_dt[, c("targetarea_codes", "admin2Name")]),
        by.x = "Domain",
        by.y = "targetarea_codes",
        all.x = TRUE)

write.csv(result_dt, "data-clean/ebp_results/fh_model_arcsinmodel_admin2_oldshapefile.csv")


### some post estimation statistics
coeftable_dt <- fh_reportcoef_table(model = fhmodel_arcsin)

write.csv(coeftable_dt, "data-clean/ebp_results/fharcsinmodel_coeftable_oldshapefile.csv")


### comparing Direct vs Small Area Estimates
plota <-
result_dt %>%
ggplot() +
  geom_point(aes(x = Direct,
                 y = FH)) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red") +
  ylab("FH Model Estimates") +
  xlab("Direct Estimates") +
  theme_bw()

#### include the names to the fhpov_dt for the table
provdomsize_dt <-
  geosurvey_dt[, sum(hhweight, na.rm = TRUE), by = admin1Pcod] %>%
  setnames(old = "V1", new = "domsize") %>%
  filter(!is.na(admin1Pcod))

directpov_dt <-
  sae::direct(y = poor,
              dom = admin1Pcod,
              sweight = hhweight,
              domsize = provdomsize_dt,
              data = geosurvey_dt[!is.na(admin1Pcod),]) %>%
  setnames(.,"Domain", "admin1Pcod") %>%
  mutate(var = SD ^ 2)

provpov_dt <-
  directpov_dt %>%
  merge(fhpov_dt[, c("admin1Pcod", "provFH")],
        by = "admin1Pcod")

provpov_dt <- as.data.table(provpov_dt)

provpov_dt[, DirectLB := Direct - (SD)*1.96]
provpov_dt[, DirectUB := Direct + (SD)*1.96]

provpov_dt <-
provpov_dt %>%
  merge(shp_dt[, c("admin1Name", "admin1Pcod")] %>% st_drop_geometry())

provpov_dt %>%
  ggplot(aes(x = admin1Name)) +
  geom_point(aes(y = provFH), color = "blue", size = 2) +  # Plotting provFH
  geom_errorbar(aes(ymin = DirectLB, ymax = DirectUB), width = 0.2, color = "red") +  # Error bars for DirectLB and DirectUB
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Adding a horizontal line at y = 0
  labs(x = "Province", y = "Poverty Rate") +  # Labeling axes
  theme_bw() +  # Setting a white background theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotating x-axis labels for better readability


write.csv(provpov_dt[, c("admin1Name","Direct", "DirectLB", "DirectUB")] %>% st_drop_geometry(),
          "data-clean/ebp_results/fharcsinmodel_directprovpovrates_oldshapefile.csv")


communeshp_dt <- readRDS("data-clean/bdi_gridded_v02.rds")

communeshp_dt <- communeshp_dt$admin2_Communes

communeshp_dt <-
  communeshp_dt %>%
  merge(result_dt, all = TRUE)

## includ population
communeshp_dt <-
  communeshp_dt %>%
  merge(communepop_dt, by = "admin2Pcod")


communeshp_dt %>%
  ggplot() +
  geom_sf(aes(fill = FH)) +
  scale_fill_viridis(option = "H") +
  theme_bw() +
  labs(fill = "Poverty Rate")

ggsave("figures/poverty_map_fhmodelarcsin_oldshapefile.png")

communeshp_dt %>%
  ggplot() +
  geom_sf(aes(fill = FH * population)) +
  scale_fill_viridis(option = "H") +
  theme_bw() +
  labs(fill = "Population \nof Poor")

ggsave("figures/poverty_map_count_fhmodelarcsin_oldshapefile.png")

#### get MSEs and compute province standard error for FH estimates








































