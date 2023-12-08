################################################################################
############### PREPARING THE SURVEY AND CENSUS DATA FOR SAE ###################
################################################################################

pacman::p_load("haven", "sf", "data.table", "dplyr", "ggplot2", "nlme", "RStata",
               "exactextractr", "raster", "povmap", "viridis")

### read in the datasets

grid_dt <- readRDS("data-raw/ind_list.RDS")

grid_dt <- grid_dt$admin4_poppoly ### the geospatial grid data

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
# grid_dt <-
# grid_dt %>%
#   mutate(admin4Pcod = paste0(admin3Pcod,
#                              sprintf(paste0("%0", max(nchar(admin4Pcode)), "d"),
#                                      admin4Pcode)))

admin_dt <- readRDS("data-clean/bdi_gridded_v02.rds")
admin_dt <- admin_dt$admin4_poppoly

# grid_dt <- merge(x = grid_dt,
#                  y = admin_dt[, c("admin2Pcode", "geometry")])

grid_dt <-
  grid_dt %>%
  as.data.frame() %>%
  st_as_sf(crs = 32735)

geosurvey_dt <-
  geosurvey_dt[, c("hhid", "hh_size", "weight", "weight_adj",
                   "pc_tot_cons", "pline_int_215", "SI15", "SI16")] %>%
  st_as_sf(crs = 4326,
           coords = c("SI16", "SI15")) %>%
  st_transform(crs = st_crs("+init=EPSG:32735")) %>%
  st_join(grid_dt)


#### check out the welfare distribution
welfare_plot <-
geosurvey_dt %>%
  ggplot(aes(x = log(pc_tot_cons))) +
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
grid_dt <-
  grid_dt %>%
  as.data.frame() %>%
  st_as_sf(crs = 32735) %>%
  st_transform(crs = 4326)


### read in the population raster
pop_dir <- "//esapov/esapov/BDI/GEO/Population"
pop_dt <- raster(paste0(pop_dir,
                        "/bdi_ppp_2020_UNadj_constrained.tif")) %>%
  rasterToPolygons() %>%
  st_as_sf()

pop_dt$admin4Pcode <- 1:nrow(pop_dt)

pop_dt <-
  pop_dt %>%
  mutate(admin4Pcode = as.character(admin4Pcode))

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
                 admin2_dt)

### add to the geosurvey
add_dt <-
  grid_dt[, c(rename_vars, "geometry"), with = F] %>%
  st_as_sf(crs = 4326) %>%
  st_transform(crs = 32735)

geosurvey_dt <-
  geosurvey_dt %>%
  st_as_sf(crs = 32735)

geosurvey_dt <- st_join(geosurvey_dt, add_dt)

geosurvey_dt <- as.data.table(geosurvey_dt)


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

### check out the correlation matrices
geosurvey_dt$geometry <- NULL
geosurvey_dt$targetave_geometry <- NULL

geosurvey_dt[, lnpc_tot_cons := log(pc_tot_cons + 1)]

### create the admin2 variables as integers
geosurvey_dt[, targetarea_codes := as.integer(substr(admin2Pcod, 4, nchar(admin2Pcod)))]
grid_dt[, targetarea_codes := as.integer(substr(admin2Pcod, 4, nchar(admin2Pcod)))]


###
missing_dt <-
  geosurvey_dt[is.na(geosurvey_dt$admin2Pcod),] %>%
  as.data.frame() %>%
  merge(geocodes_dt[, c("hhid", "SI15", "SI16")]) %>%
  st_as_sf(crs = 4326,
           coords = c("SI16", "SI15")) %>%
  st_transform(crs = 32735)

test_dt <- readRDS("data-clean/bdi_gridded_v02.rds")

test_dt <- test_dt$admin2_Communes

missing_dt <- st_join(missing_dt[,c("geometry", "hhid")],
                      test_dt %>% st_transform(crs = 32735))

missing_dt$merger <- missing_dt$admin2Pcod

geosurvey_dt <- left_join(geosurvey_dt,
                          missing_dt[, c("hhid", "merger")] %>%
                            st_drop_geometry(),
                          by = "hhid")

geosurvey_dt[admin2Pcod == 0, admin2Pcod := merger]

geosurvey_dt[, targetarea_codes := as.integer(substr(admin2Pcod, 4, nchar(admin2Pcod)))]

geosurvey_dt$hhweight <- geosurvey_dt$weight * geosurvey_dt$hh_size

## drop variables that have too many NAs or NaNs
geosurvey_dt[is.na(geosurvey_dt)] <- 0
grid_dt[is.na(grid_dt)] <- 0

write.csv(cor(geosurvey_dt[, c(candidate_vars, "lnpc_tot_cons"), with = F]),
          "data-clean/corr_matrix2.csv")

### create some additional variables


### include the EA variable from geocodes_dt
geosurvey_dt <- geocodes_dt[, c("enum_area", "hhid")][geosurvey_dt, on = "hhid"]

geosurvey_dt[, lnpc_tot_cons := log(pc_tot_cons + 1)]
### model selection using stata lasso regression model with cluster effect controls

model_vars <- countrymodel_select_stata(dt = geosurvey_dt,
                                        xvars = candidate_vars,
                                        y = "lnpc_tot_cons",
                                        weights = "weight",
                                        stata_path = "D:/Programs/Stata17/StataMP-64",
                                        stata_vnum = 17,
                                        cluster_id = "enum_area",
                                        area_tag = "BDI")

model_vars2 <- countrymodel_select_stata(dt = geosurvey_dt,
                                         xvars = candidate_vars,
                                         y = "lnpc_tot_cons",
                                         weights = "weight",
                                         stata_path = "D:/Programs/Stata17/StataMP-64",
                                         stata_vnum = 17,
                                         cluster_id = "enum_area",
                                         area_tag = NULL)


#### estimate the model
unit_model <- povmap::ebp(fixed = as.formula(paste("pc_tot_cons ~ ", paste(model_vars, collapse= "+"))),
                          pop_data = as.data.frame(na.omit(grid_dt[,c(model_vars,
                                                                     "targetarea_codes",
                                                                     "bdi_ppp_2020_UNadj_constrained"),
                                                                     with = FALSE])),
                          pop_domains = "targetarea_codes",
                          smp_data = as.data.frame(na.omit(geosurvey_dt[!is.na(admin2Pcod),
                                                                        c("pc_tot_cons",
                                                                          model_vars,
                                                                          "targetarea_codes",
                                                                          "hhweight"),
                                                                          with = FALSE])),
                          smp_domains = "targetarea_codes",
                          L = 100,
                          B = 100,
                          transformation = "log",
                          threshold = 10381.14,
                          weights = "hhweight",
                          pop_weights = "bdi_ppp_2020_UNadj_constrained",
                          cpus = 30,
                          MSE = TRUE,
                          na.rm = TRUE)




#### plot poverty map

pov_dt <- readRDS("data-clean/bdi_gridded_v02.rds")

pov_dt <- pov_dt$admin2_Communes

pov_dt$targetarea_codes <- as.integer(substr(pov_dt$admin2Pcod, 4, nchar(pov_dt$admin2Pcod)))


pov_dt <- merge(x = unit_model$ind,
                y = pov_dt,
                by.x = "Domain",
                by.y = "targetarea_codes")

pov_dt %>%
  st_as_sf(crs = 4326) %>%
  ggplot() +
  geom_sf(aes(fill = Head_Count)) +
  scale_fill_viridis_c(option = "H") +
  theme_minimal()

ggsave("figures/bdi_poverty_map.png")

save.image(file = "data-raw/all_modelestimation.RData")

write.csv(pov_dt %>% st_as_sf(crs = 4326) %>% st_drop_geometry(),
          "data-clean/bdi_poverty_map.csv")

### include the population weights to compare poverty rates
weights_dt <- geosurvey_dt[,sum(hhweight, na.rm = TRUE), by = "targetarea_codes"]

setnames(weights_dt, "V1", "population")

pov_dt <- merge(pov_dt, weights_dt, by.x = "Domain", by.y = "targetarea_codes")

pov_dt %>%
  summarize(weighted.mean(x = Head_Count,
                          w = population))

geosurvey_dt[, poor := ifelse(pc_tot_cons < 10381.14, 1, 0)]


geosurvey_dt %>%
  summarize(weighted.mean(x = poor,
                          w = hhweight))






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




















