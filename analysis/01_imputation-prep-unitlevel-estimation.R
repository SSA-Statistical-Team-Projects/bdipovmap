################################################################################
############### PREPARING THE SURVEY AND CENSUS DATA FOR SAE ###################
################################################################################

pacman::p_load("haven", "sf", "data.table", "dplyr", "ggplot2", "nlme", "RStata",
               "exactextractr", "raster", "povmap", "viridis", "glmmLasso",
               "lme4", "gridExtra", "MASS", "furrr", "purrr")

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



################################################################################
############## PREPARE FOR MODEL SELECTION FOR MIXED MODELS ####################
################################################################################

### remove the multicollinear variables real quick

lmmodel_check <- lm(lnrpc_tot_cons ~ ., data = model_dt[, c(candidate_vars, "lnrpc_tot_cons")])

### remove the NAs
cand_vars <- names(lmmodel_check$coefficients[is.na(lmmodel_check$coefficients) == FALSE])
cand_vars <- cand_vars[!(cand_vars %in% "(Intercept)")]

# ### add all the BDI admin1 areas except 1
# cand_vars <- c(cand_vars, candidate_vars[grepl("BDI", candidate_vars)])
#
# cand_vars <- cand_vars[!(cand_vars %in% "BDI018")]

lmmodel_check <- lm(lnrpc_tot_cons ~ ., data = model_dt[, c(cand_vars,
                                                           "lnrpc_tot_cons")])

scale_model_dt <- as.data.frame(scale(geosurvey_dt[, cand_vars, with = F]))

scale_model_dt <- cbind(scale_model_dt, geosurvey_dt[, c("lnrpc_tot_cons", "targetarea_codes")])

dt <- cbind(scale_model_dt, geosurvey_dt[, colnames(geosurvey_dt)[grepl("BDI", colnames(geosurvey_dt))], with = F])

### model selection with lasso for mixed effects models
pql <- glmmPQL(lnrpc_tot_cons ~ 1,
               random = list(targetarea_codes = ~1),
               family = "gaussian",
               data = dt)

lambda <- seq(2000, 0, by = -50)

bic_vector <- rep(Inf, length(lambda))

delta_start <- c(as.numeric(pql$coefficients$fixed),
                 rep(0, length(cand_vars) + 18),
                 as.numeric(t(pql$coefficients$random$targetarea_codes)))

q_start <- as.numeric(VarCorr(pql)[1,1])


dt$targetarea_codes <- as.factor(dt$targetarea_codes)
# ### now lets find optimal lambda
# for (j in 1:10){
#
#   print(paste("Iteration", j, sep = ""))
#
#   glm1 <- try(
#
#     glmmLasso(as.formula(paste("lnpc_tot_cons ~ ", paste(cand_vars, collapse= "+"))),
#               rnd = list(targetarea_codes = ~1),
#               family = gaussian(link = "identity"),
#               data = na.omit(dt),
#               lambda = lambda[j],
#               switch.NR = TRUE,
#               final.re = TRUE,
#               control = list(start = delta_start,
#                              q_start = q_start)),
#     silent = TRUE
#   )
#
#   if(class(glm1) != "try-error"){
#
#     bic_vector[j] <- glm1$bic
#
#   }
#
#
# }


# future_map(lambda, ~parallel_find_optlambda(.x, dt, cand_vars, delta_start, q_start))
parallelMap::parallelStart(mode = "socket",
                           cpus = length(lambda),
                           show.info = FALSE)

parallel::clusterSetRNGStream()

parallelMap::parallelLibrary("nlme")
parallelMap::parallelLibrary("glmmLasso")


optlambda <- parallelMap::parallelLapply(xs = lambda,
                                         fun = find_optlambda,
                                         dt = dt,
                                         cand_vars = cand_vars,
                                         delta_start = delta_start,
                                         q_start = q_start)

parallelMap::parallelStop()


lasso_model <- glmmLasso(fix = as.formula(paste("lnrpc_tot_cons ~ ", paste(cand_vars, collapse= "+"))),
                         rnd = list(targetarea_codes = ~1),
                         family = gaussian(link = "identity"),
                         data = na.omit(dt),
                         lambda = lambda[which.min(optlambda)],
                         switch.NR = TRUE,
                         final.re = TRUE,
                         control=list(start = delta_start,
                                      q_start = q_start))

### select only indicators that are significant
lasso_output <- summary(lasso_model)

lasso_output <- as.data.frame(lasso_output$coefficients)

lasso_output$variable <- rownames(lasso_output)

selvars_dt <- lasso_output[abs(lasso_output$p.value) <= 0.1 & is.na(lasso_output$z.value) == FALSE,]

### ok lets estimate the poverty map
selvars_list <- selvars_dt$variable[!selvars_dt$variable %in% "(Intercept)"]

selvars_bdilist <- c(selvars_list,
                     candidate_vars[grepl("BDI", candidate_vars)])

### compute the unit context model
#### estimate the model

grid_dt <- cbind(grid_dt,
                 as.data.table(dummify(grid_dt$admin1Pcod_x)))

grid_dt[, targetarea_codes := as.integer(substr(admin2Pcod, 4, nchar(admin2Pcod)))]

selvars_bdilist <- selvars_bdilist[!grepl("BDI018", selvars_bdilist)]

unit_model <- povmap::ebp(fixed = as.formula(paste("rpc_tot_cons ~ ", paste(selvars_list, collapse= "+"))),
                          pop_data = as.data.frame(na.omit(grid_dt[,c(selvars_list,
                                                                      "targetarea_codes",
                                                                      "bdi_ppp_2020_UNadj_constrained"),
                                                                   with = FALSE])),
                          pop_domains = "targetarea_codes",
                          smp_data = as.data.frame(na.omit(geosurvey_dt[!is.na(admin2Pcod),
                                                                        c("rpc_tot_cons",
                                                                          selvars_list,
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

save.image("data-raw/all_modelestimation.RData")






















































































