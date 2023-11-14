################################################################################
############### PREPARING THE SURVEY AND CENSUS DATA FOR SAE ###################
################################################################################

pacman::p_load("haven", "sf", "data.table", "dplyr", "ggplot2", "nlme", "RStata")

### read in the datasets

grid_dt <- readRDS("data-raw/ind_list.RDS")

grid_dt <- grid_dt$admin3_grid ### the geospatial grid data

geosurvey_dt <- haven::read_dta("data-raw/hh_poverty.dta") ### household survey

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

#### include the grid area information with the geosurvey data
grid_dt <-
grid_dt %>%
  mutate(admin3Pcod = paste0(admin2Pcod,
                             sprintf(paste0("%0", max(nchar(admin3Pcode)), "d"),
                                     admin3Pcode)))

grid_dt <-
  grid_dt %>%
  st_as_sf(crs = st_crs("+init=EPSG:32735"))

geosurvey_dt <-
  geosurvey_dt[, c("hhid", "hh_size", "weight", "weight_adj",
                   "pc_tot_cons", "pline_int_215", "SI15", "SI16")] %>%
  st_as_sf(crs = 4326,
           coords = c("SI16", "SI15")) %>%
  st_transform(crs = st_crs("+init=EPSG:32735")) %>%
  st_join(grid_dt)


saveRDS(geosurvey_dt, "data-clean/geosurvey.RDS")

#### figure out where all the households with respect to the country
shp_dt <- sf::read_sf(dsn = "data-raw/adm1",
                      layer = "bdi_admbnda_adm1_igebu_ocha_20171103")

hhgeo_plot <-
ggplot() +
  geom_sf(data = shp_dt, fill = "white", color = "black") +
  geom_sf(data = geosurvey_dt, color = "red") +
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


### remove zero population grids
grid_dt <- grid_dt[sum.bdi_ppp_2020 > 0,]


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

################################################################################
################### MODEL SELECTION ANALYTICS PRE-IMPUTATION ###################
################################################################################
remove_vars <- c("hhid", "hh_size", "weight", "weight_adj",
                 "pline_int_215", "area", "poly_area", "pc_tot_cons",
                 "geometry",
                 colnames(geosurvey_dt)[grepl("admin",
                                              colnames(geosurvey_dt))])

candidate_vars <- colnames(geosurvey_dt)[!colnames(geosurvey_dt) %in% remove_vars]

## drop variables that have too many NAs or NaNs
geosurvey_dt[is.na(geosurvey_dt)] <- 0
grid_dt[is.na(grid_dt)] <- 0



# fixed <- as.formula(paste("pc_tot_cons ~ ",
#                           paste(candidate_vars,
#                                 collapse = " + ")))
#
#
# mixed_model <- nlme::lme(
#   fixed = fixed,
#   data = geosurvey_dt,
#   random =
#   as.formula(paste0("~ 1 | as.factor(","admin2Pcod", ")")),
#   method = "REML",
#   control = nlme::lmeControl(maxIter = 1000,
#                              tolerance = 1e-6,
#                              opt = "nlminb",
#                              optimMethod = "REML",
#                              msMaxIter = 1000,
#                              msTol = 1e-7
#   ),
#   weights =
#     varComb(
#       varIdent(as.formula(
#         paste0("~ 1 | as.factor(", "admin2Pcod", ")")
#       )),
#       varFixed(as.formula(paste0("~1/", "weight")))
#     )
# )
### include the EA variable from geocodes_dt
geosurvey_dt <- geocodes_dt[, c("enum_area", "hhid")][geosurvey_dt, on = "hhid"]


### model selection using stata lasso regression model with cluster effect controls
model_vars <- countrymodel_select_stata(dt = geosurvey_dt,
                                        xvars = candidate_vars,
                                        y = "pc_tot_cons",
                                        weights = "weight",
                                        stata_path = "D:/Programs/Stata17/StataMP-64",
                                        stata_vnum = 17,
                                        cluster_id = "enum_area",
                                        area_tag = "BDI")

























































