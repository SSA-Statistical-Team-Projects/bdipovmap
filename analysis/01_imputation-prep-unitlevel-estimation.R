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

### read in the update to the welfare aggregates
add_dt <- haven::read_dta("data-raw/BDI_2020_EICVMB_v01_M_v01_A_SSAPOV_GMD.dta")

add_dt <- add_dt[, c("hhid", "welfare", "welfaredef", "cpi",
                     "ppp", "weight_p", "weight_h")]

geosurvey_dt <-
  geosurvey_dt %>%
  left_join(unique(add_dt), by = "hhid")

### check the international poverty rate (poverty rate matches at 62.1%)
add_dt %>%
  mutate(poor = ifelse(welfare / cpi / ppp / 365 < 2.15, 1, 0)) %>%
  summarise(weighted.mean(x = poor,
                          w = weight_p,
                          na.rm = TRUE))
### create the poverty line
geosurvey_dt <-
  geosurvey_dt %>%
  mutate(pline_int_215 = cpi * ppp * 365)


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
                   "welfare",
                   "pline_int_215", "SI15", "SI16")] %>%
  st_as_sf(crs = 4326,
           coords = c("SI16", "SI15")) %>%
  st_transform(crs = st_crs("+init=EPSG:32735")) %>%
  st_join(grid_dt)


#### check out the welfare distribution
welfare_plot <-
geosurvey_dt %>%
  ggplot(aes(x = log(welfare))) +
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
                                     "weight_adj", "welfare",
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
  mutate(poor = ifelse(welfare < pline_int_215, 1, 0)) %>%
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
                 "pline_int_215", "area", "poly_area", "welfare",
                 "geometry", "swe_sq_hist_dev", "targetave_swe_change_10",
                 "targetave_swe_sq_hist_dev", "targetave_geometry",
                 "targetarea_codes",
                 colnames(geosurvey_dt)[grepl("admin",
                                              colnames(geosurvey_dt))])

candidate_vars <- colnames(geosurvey_dt)[!colnames(geosurvey_dt) %in% remove_vars]

### turn grid and geospatial survey into data.tables
grid_dt <- as.data.table(grid_dt)
geosurvey_dt <- as.data.table(geosurvey_dt)

geosurvey_dt[, lnwelfare := log(welfare + 1)]

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
                             "lnwelfare",
                             "weight",
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

haven::write_dta(geosurvey_dt[, c("lnwelfare",
                                  candidate_vars,
                                  "weight",
                                  "admin2Pcod"), with = FALSE],
                 "data-raw/model.dta")


################################################################################
############## PREPARE FOR MODEL SELECTION FOR MIXED MODELS ####################
################################################################################
### perform the model selection with the lasso linear model
stata_vars <- countrymodel_select_stata(dt = geosurvey_dt[, c("lnwelfare",
                                                              candidate_vars,
                                                              "hhweight",
                                                              "admin2Pcod"), with = FALSE],
                                        xvars = candidate_vars,
                                        y = "lnwelfare",
                                        weights = "hhweight",
                                        selection = "BIC",
                                        stata_path = "D:/Programs/Stata18/StataMP-64",
                                        stata_vnum = 18,
                                        cluster_id = "admin2Pcod")



# ### remove the multicollinear variables real quick
#
# lmmodel_check <- lm(lnwelfare ~ ., data = model_dt[, c(candidate_vars, "lnwelfare")])
#
# ### remove the NAs
# cand_vars <- names(lmmodel_check$coefficients[is.na(lmmodel_check$coefficients) == FALSE])
# cand_vars <- cand_vars[!(cand_vars %in% "(Intercept)")]
#
# # ### add all the BDI admin1 areas except 1
# # cand_vars <- c(cand_vars, candidate_vars[grepl("BDI", candidate_vars)])
# #
# # cand_vars <- cand_vars[!(cand_vars %in% "BDI018")]
#
# lmmodel_check <- lm(lnwelfare ~ ., data = model_dt[, c(cand_vars,
#                                                            "lnwelfare")])
#
# scale_model_dt <- as.data.frame(scale(geosurvey_dt[, cand_vars, with = F]))
#
# scale_model_dt <- cbind(scale_model_dt, geosurvey_dt[, c("lnwelfare", "targetarea_codes")])
#
# dt <- cbind(scale_model_dt, geosurvey_dt[, colnames(geosurvey_dt)[grepl("BDI", colnames(geosurvey_dt))], with = F])
#
# ### model selection with lasso for mixed effects models
# pql <- glmmPQL(lnwelfare ~ 1,
#                random = list(targetarea_codes = ~1),
#                family = "gaussian",
#                data = dt)
#
# lambda <- seq(2000, 0, by = -50)
#
# bic_vector <- rep(Inf, length(lambda))
#
# delta_start <- c(as.numeric(pql$coefficients$fixed),
#                  rep(0, length(cand_vars) + 18),
#                  as.numeric(t(pql$coefficients$random$targetarea_codes)))
#
# q_start <- as.numeric(VarCorr(pql)[1,1])
#
#
# dt$targetarea_codes <- as.factor(dt$targetarea_codes)
# # ### now lets find optimal lambda
# # for (j in 1:10){
# #
# #   print(paste("Iteration", j, sep = ""))
# #
# #   glm1 <- try(
# #
# #     glmmLasso(as.formula(paste("lnpc_tot_cons ~ ", paste(cand_vars, collapse= "+"))),
# #               rnd = list(targetarea_codes = ~1),
# #               family = gaussian(link = "identity"),
# #               data = na.omit(dt),
# #               lambda = lambda[j],
# #               switch.NR = TRUE,
# #               final.re = TRUE,
# #               control = list(start = delta_start,
# #                              q_start = q_start)),
# #     silent = TRUE
# #   )
# #
# #   if(class(glm1) != "try-error"){
# #
# #     bic_vector[j] <- glm1$bic
# #
# #   }
# #
# #
# # }
#
#
# # future_map(lambda, ~parallel_find_optlambda(.x, dt, cand_vars, delta_start, q_start))
# parallelMap::parallelStart(mode = "socket",
#                            cpus = length(lambda),
#                            show.info = FALSE)
#
# parallel::clusterSetRNGStream()
#
# parallelMap::parallelLibrary("nlme")
# parallelMap::parallelLibrary("glmmLasso")
#
#
# optlambda <- parallelMap::parallelLapply(xs = lambda,
#                                          fun = find_optlambda,
#                                          dt = dt,
#                                          cand_vars = cand_vars,
#                                          delta_start = delta_start,
#                                          q_start = q_start)
#
# parallelMap::parallelStop()
#
#
# lasso_model <- glmmLasso(fix = as.formula(paste("lnwelfare ~ ", paste(cand_vars, collapse= "+"))),
#                          rnd = list(targetarea_codes = ~1),
#                          family = gaussian(link = "identity"),
#                          data = na.omit(dt),
#                          lambda = lambda[which.min(optlambda)],
#                          switch.NR = TRUE,
#                          final.re = TRUE,
#                          control=list(start = delta_start,
#                                       q_start = q_start))
#
# ### select only indicators that are significant
# lasso_output <- summary(lasso_model)
#
# lasso_output <- as.data.frame(lasso_output$coefficients)
#
# lasso_output$variable <- rownames(lasso_output)
#
# selvars_dt <- lasso_output[abs(lasso_output$p.value) <= 0.1 & is.na(lasso_output$z.value) == FALSE,]
#
# ### ok lets estimate the poverty map
# selvars_list <- selvars_dt$variable[!selvars_dt$variable %in% "(Intercept)"]
#
# selvars_bdilist <- c(selvars_list,
#                      candidate_vars[grepl("BDI", candidate_vars)])
#
# ### compute the unit context model
# #### estimate the model
#
grid_dt <- cbind(grid_dt,
                 as.data.table(dummify(grid_dt$admin1Pcod_x)))

grid_dt[, targetarea_codes := as.integer(substr(admin2Pcod, 4, nchar(admin2Pcod)))]
#
# selvars_bdilist <- selvars_bdilist[!grepl("BDI018", selvars_bdilist)]
#
# #### use the stata lasso model for the estimation
# stata_selvars_list <- readLines("model.txt")
#
# stata_selvars_list <- unlist(strsplit(stata_selvars_list, " "))
#
# setnames(grid_dt, invalid_names, valid_names)


geosurvey_dt$pline_int_215 <- 654.4216*1.0489*365*2.15
geosurvey_dt$hhweight <- geosurvey_dt$weight * geosurvey_dt$hh_size

geosurvey_dt %>%
  mutate(poor = ifelse(welfare < pline_int_215, 1, 0)) %>%
  summarise(weighted.mean(x = poor,
                          w = hhweight,
                          na.rm = TRUE))

#### there are some missing admin areas we need to give them target area codes
geosurvey_dt[is.na(targetarea_codes),
                         c(colnames(geosurvey_dt)[grepl("admin",
                                                        colnames(geosurvey_dt))],
                           "geometry",
                           "welfare"),
                         with = F] %>%
  st_as_sf(crs = 4326) %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = shp_dt) ###the three points appear to be outside the country






unit_model <- povmap::ebp(fixed = as.formula(paste("welfare ~ ", paste(stata_vars, collapse= "+"))),
                          pop_data = as.data.frame(na.omit(grid_dt[,c(stata_vars,
                                                                      "targetarea_codes",
                                                                      "bdi_ppp_2020_UNadj_constrained"),
                                                                   with = FALSE])),
                          pop_domains = "targetarea_codes",
                          smp_data = as.data.frame(na.omit(geosurvey_dt[!is.na(admin2Pcod),
                                                                        c("welfare",
                                                                          stata_vars,
                                                                          "targetarea_codes",
                                                                          "hhweight"),
                                                                        with = FALSE])),
                          smp_domains = "targetarea_codes",
                          L = 100,
                          B = 100,
                          transformation = "log",
                          threshold = 538670.3,
                          weights = "hhweight",
                          pop_weights = "bdi_ppp_2020_UNadj_constrained",
                          cpus = 30,
                          MSE = TRUE,
                          na.rm = TRUE)

### quickly create labels
setnames(grid_dt,
         old = c("admin0Pcod_x", "admin1Pcod_x", "admin2Name_x", "area_x"),
         new = c("admin0Pcod", "admin1Pcod", "admin2Name", "area"))


grid_dt <-
expss::apply_labels(grid_dt,
                    admin2Pcod = "Commune/Target Area Codes",
                    admin4Pcode = "WorldPop Grid idenitifer code",
                    admin3Pcode = "Grid-Level identifier",
                    admin0Pcod = "Country Code",
                    admin1Pcod = "Region Codes",
                    admin2Name = "Commune/Target Area Names",
                    area = "Anonymous Area",
                    poly_area = "Tesselated Grid Area",
                    ntl_all_2015 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2015",
                    ntl_all_2016 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2016",
                    ntl_all_2017 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2017",
                    ntl_all_2018 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2018",
                    ntl_all_2019 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2019",
                    ntl_all_2020 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2020",
                    ntl_built_up_2015 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2015",
                    ntl_built_up_2016 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2016",
                    ntl_built_up_2017 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2017",
                    ntl_built_up_2018 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2018",
                    ntl_built_up_2019 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2019",
                    ntl_built_up_2020 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2020",
                    ntl_small_stl_2015 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2015",
                    ntl_small_stl_2016 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2016",
                    ntl_small_stl_2017 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2017",
                    ntl_small_stl_2018 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2018",
                    ntl_small_stl_2019 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2019",
                    ntl_small_stl_2020 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2020",
                    ntl_hamlet_2015 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2015",
                    ntl_hamlet_2016 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2016",
                    ntl_hamlet_2017 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2017",
                    ntl_hamlet_2018 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2018",
                    ntl_hamlet_2019 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2019",
                    ntl_hamlet_2020 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2020",
                    pop_density_2015 = "2015 Population density",
                    pop_density_2016 = "2016 Population density",
                    pop_density_2017 = "2017 Population density",
                    pop_density_2018 = "2018 Population density",
                    pop_density_2019 = "2019 Population density",
                    pop_density_2020 = "2020 Population density",
                    lightscore_sy_2012 = "2012 Probability of electrification",
                    prplit_conf90_sy_2012 = "2012 Proportion of nights a settlement is brighter than uninhabited areas",
                    lightscore_sy_2019 = "2019 Probability of electrification",
                    prplit_conf90_sy_2019 = "2019 Proportion of nights a settlement is brighter than uninhabited areas",
                    y2017_Water = "Proportion area covered by water in 2017",
                    y2017_Trees = "Proportion area covered by trees in 2017",
                    y2017_Flooded_vegetation = "Proportion area covered by flooded vegetation in 2017",
                    y2017_Crops = "Proportion area covered by crops in 2017",
                    y2017_Built_Area = "Proportion area covered by built area in 2017",
                    y2017_Bare_ground = "Proportion area covered by bare ground in 2017",
                    y2017_Snow_Ice = "Proportion area covered by snow and ice in 2017",
                    y2017_Clouds = "Proportion area covered by clouds in 2017",
                    y2017_Rangeland = "Proportion area covered by rangeland in 2017",
                    y2018_Water = "Proportion area covered by water in 2018",
                    y2018_Trees = "Proportion area covered by trees in 2018",
                    y2018_Flooded_vegetation = "Proportion area covered by flooded vegetation in 2018",
                    y2018_Crops = "Proportion area covered by crops in 2018",
                    y2018_Built_Area = "Proportion area covered by built area in 2018",
                    y2018_Bare_ground = "Proportion area covered by bare ground in 2018",
                    y2018_Snow_Ice = "Proportion area covered by snow and ice in 2018",
                    y2018_Clouds = "Proportion area covered by clouds in 2018",
                    y2018_Rangeland = "Proportion area covered by rangeland in 2018",
                    y2019_Water = "Proportion area covered by water in 2019",
                    y2019_Trees = "Proportion area covered by trees in 2019",
                    y2019_Flooded_vegetation = "Proportion area covered by flooded vegetation in 2019",
                    y2019_Crops = "Proportion area covered by crops in 2019",
                    y2019_Built_Area = "Proportion area covered by built area in 2019",
                    y2019_Bare_ground = "Proportion area covered by bare ground in 2019",
                    y2019_Snow_Ice = "Proportion area covered by snow and ice in 2019",
                    y2019_Clouds = "Proportion area covered by clouds in 2019",
                    y2019_Rangeland = "Proportion area covered by rangeland in 2019",
                    y2020_Water = "Proportion area covered by water in 2020",
                    y2020_Trees = "Proportion area covered by trees in 2020",
                    y2020_Flooded_vegetation = "Proportion area covered by flooded vegetation in 2020",
                    y2020_Crops = "Proportion area covered by crops in 2020",
                    y2020_Built_Area = "Proportion area covered by built area in 2020",
                    y2020_Bare_ground = "Proportion area covered by bare ground in 2020",
                    y2020_Snow_Ice = "Proportion area covered by snow and ice in 2020",
                    y2020_Clouds = "Proportion area covered by clouds in 2020",
                    y2020_Rangeland = "Proportion area covered by rangeland in 2020",
                    building_sum = "Number of buildings within WorldPop grid containing Household",
                    building_density = "Number of buildings per square km lived area",
                    building_mean_area = "Mean building areas within WorldPop grid containing Household",
                    building_mean_length = "Mean building lengths within WorldPop grid containing Household",
                    building_mean_urban = "Urbanization rate within WorldPop grid containing Household",
                    pdsi_mean2018="Palmer Drought Severity Index mean 2018",
                    aet_mean2018="Actual evapotranspiration  mean 2018",
                    def_mean2018="Climate water deficit mean 2018",
                    pr_mean2018="Precipitation accumulation mean 2018",
                    ro_mean2018= "Runoff mean 2018",
                    soil_mean2018="Soil moisture 2018",
                    tmmn_mean2018= "Minimum temperature mean 2018",
                    tmmx_mean2018= "Maximum temperature mean 2018",
                    swe_mean2018= "Snow water equivalent mean 2018",
                    srad_mean2018= "Downward surface shortwave radiation mean 2018",
                    pdsi_mean2014="Palmer Drought Severity Index mean 2014",
                    pdsi_mean2014="Palmer Drought Severity Index mean 2014",
                    aet_mean2014="Actual evapotranspiration mean 2014",
                    def_mean2014="Climate water deficit mean 2014",
                    pr_mean2014="Precipitation accumulation mean 2014",
                    ro_mean2014= "Runoff mean 2014",
                    soil_mean2014="Soil moisture mean 2014",
                    tmmn_mean2014= "Minimum temperature mean 2014",
                    tmmx_mean2014= "Maximum temperature mean 2014",
                    swe_mean2014= "Snow water equivalent mean 2014",
                    srad_mean2014= "Downward surface shortwave radiation mean 2014",
                    pdsi_mean2009="Palmer Drought Severity Index mean 2009",
                    aet_mean2009="Actual evapotranspiration  mean 2009",
                    def_mean2009="Climate water deficit mean 2009",
                    pr_mean2009="Precipitation accumulation mean 2009",
                    ro_mean2009= "Runoff mean 2009",
                    soil_mean2009="Soil moisture  mean 2009",
                    tmmn_mean2009= "Minimum temperature mean 2009",
                    tmmx_mean2009= "Maximum temperature mean 2009",
                    swe_mean2009= "Snow water equivalent mean 2009",
                    srad_mean2009= "Downward surface shortwave radiation mean 2009",
                    pdsi_mean99_19="Palmer Drought Severity Index mean 1999-2019",
                    aet_mean99_19="Actual evapotranspiration  mean 1999-2019",
                    def_mean99_19="Climate water deficit mean 1999-2019",
                    pr_mean99_19="Precipitation accumulation mean 1999-2019",
                    ro_mean99_19= "Runoff  mean 1999-2019",
                    soil_mean99_19="Soil moisture mean 1999-2019",
                    tmmn_mean99_19= "Minimum temperature mean 1999-2019",
                    tmmx_mean99_19= "Maximum temperature mean 1999-2019",
                    swe_mean99_19= "Snow water equivalent mean 1999-2019",
                    srad_mean99_19= "Downward surface shortwave radiation 1999-2019",
                    pdsi_change_5="Five year change in Palmer Drought Severity Index mean",
                    aet_change_5="Five year change in Actual evapotranspiration mean",
                    def_change_5="Five year change in Climate water deficit mean",
                    pr_change_5="Five year change in Precipitation accumulation mean",
                    ro_change_5= "Five year change in Runoff mean",
                    soil_change_5="Five year change in Soil moisture mean",
                    tmmn_change_5= "Five year change in Minimum temperature mean",
                    tmmx_change_5= "Five year change in Maximum temperature mean",
                    swe_change_5= "Five year change in Snow water equivalent mean",
                    srad_change_5= "Five year change in Downward surface shortwave radiation mean",
                    pdsi_change_10="Ten year change in Palmer Drought Severity Index mean mean",
                    aet_change_10="Ten year change in Actual evapotranspiration mean",
                    def_change_10="Ten year change in Climate water deficit mean",
                    pr_change_10="Ten year change in Precipitation accumulation mean",
                    ro_change_10= "Ten year change in Runoff mean",
                    soil_change_10="Ten year change in Soil moisture mean",
                    tmmn_change_10= "Ten year change in Minimum temperature mean",
                    tmmx_change_10= "Ten year change in Maximum temperature mean",
                    swe_change_10= "Ten year change in Snow water equivalent mean",
                    srad_change_10= "Ten year change in Downward surface shortwave radiation mean",
                    pdsi_hist_dev="Historical deviation from the mean of Palmer Drought Severity Index",
                    aet_hist_dev="Historical deviation from the mean of Actual evapotranspiration",
                    def_hist_dev="Historical deviation from the mean of Climate water deficit",
                    pr_hist_dev="Historical deviation from the mean of Precipitation accumulation",
                    ro_hist_dev= "Historical deviation from the mean of Runoff",
                    soil_hist_dev="Historical deviation from the mean of Soil moisture",
                    tmmn_hist_dev= "Historical deviation from the mean of Minimum temperature",
                    tmmx_hist_dev= "Historical deviation from the mean of Maximum temperature",
                    swe_hist_dev= "Historical deviation from the mean of Snow water equivalent",
                    srad_hist_dev= "Historical deviation from the mean of Downward surface shortwave radiation",
                    pdsi_sq_hist_dev="Squared historical deviation from the mean of Palmer Drought Severity Index",
                    aet_sq_hist_dev="Squared historical deviation from the mean of Actual evapotranspiration",
                    def_sq_hist_dev="Squared historical deviation from the mean of Climate water deficit",
                    pr_sq_hist_dev="Squared historical deviation from the mean of Precipitation accumulation",
                    ro_sq_hist_dev= "Squared historical deviation from the mean of Runoff",
                    soil_sq_hist_dev="Squared historical deviation from the mean of Soil moisture",
                    tmmn_sq_hist_dev= "Squared historical deviation from the mean of Minimum temperature",
                    tmmx_sq_hist_dev= "Squared historical deviation from the mean of Maximum temperature",
                    swe_sq_hist_dev= "Squared historical deviation from the mean of Snow water equivalent",
                    srad_sq_hist_dev= "Squared historical deviation from the mean of Downward surface shortwave radiation",
                    meta_wealth_index = "Facebook Meta Wealth Index",
                    BU2016DHS_ANANEMWANY = "% women aged 15-49 with anemia (DHS 2016)",
                    BU2016DHS_CHVAC1CVCD = "% children 12-23 mths with vaccination card (DHS 2016)",
                    BU2016DHS_CHVACCCBAS = "% children 12-23 mths with 8 basic vaccinations (DHS 2016)",
                    BU2016DHS_CHVACCCDP1 = "% children 12-23 mths with DPT1 vaccination (DHS 2016)",
                    BU2016DHS_CHVACCCDP3 = "% children 12-23 mths with 3rd dose DPT (DHS 2016)",
                    BU2016DHS_CHVACSCMSL = "% children 12-23 mths received Measles vaccination (DHS 2016)",
                    BU2016DHS_CNNUTSCHA2 = "% children under age 5 stunted (DHS 2016)",
                    BU2016DHS_EDLITRMLIT = "% men aged 15-49 who are literate (DHS 2016)",
                    BU2016DHS_EDLITRWLIT = "% women aged 15-49 who are literate (DHS 2016)",
                    BU2016DHS_FPCUSMWMOD = "% currently married or in union women using modern contraception (DHS 2016)",
                    BU2016DHS_FPNADMWPDM = "ratio of currently married women using FP to FP demand from same group (DHS 2016)",
                    BU2016DHS_FPNADMWUNT = "% currently married or in union women with an unmet need for FP (DHS 2016)",
                    BU2016DHS_MLITNAPACC = "% of HH population sleeping under an ITN if ITN can be used by <=2 people (DHS 2016)",
                    BU2016DHS_RHANCNWN4P = "% women had live birth in last 5 years who had 4+ antenatal care visits (DHS 2016)",
                    BU2016DHS_RHDELPCDHF = "% live births in the 5 years preceding DHS 2016 delivered at a health facility (DHS 2016)",
                    BU2016DHS_WSSRCEPIMP = "% of population living in HHs with improved main source of drinking water (DHS 2016)",
                    BU2016DHS_WSTLETPNFC = "% of population living in HHs with main type of toilet is no facility (DHS 2016)",
                    normal_ntl_mean2015 = "2015 Average Night Luminosity (all areas unfiltered)",
                    normal_ntl_mean2016 = "2016 Average Night Luminosity (all areas unfiltered)",
                    normal_ntl_mean2017 = "2017 Average Night Luminosity (all areas unfiltered)",
                    normal_ntl_mean2018 = "2018 Average Night Luminosity (all areas unfiltered)",
                    normal_ntl_mean2019 = "2019 Average Night Luminosity (all areas unfiltered)",
                    normal_ntl_mean2020 = "2020 Average Night Luminosity (all areas unfiltered)",
                    ntl_all_growth = "Growth in Night Limonsity 2015 to 2020 (all areas unfiltered)",
                    ntl_built_up_growth = "Growth in Night Limonsity 2015 to 2020 (built up areas)",
                    ntl_small_stl_growth = "Growth in Night Limonsity 2015 to 2020 (small settlements)",
                    ntl_hamlet_growth = "Growth in Night Limonsity 2015 to 2020 (hamlets)",
                    log_pop_density_growth = "Growth in Population Density 2015 to 2020",
                    water_growth = "Change in water coverfraction 2017 to 2020",
                    trees_growth = "Change in tree coverfraction 2017 to 2020",
                    floodedveg_growth = "Change in flooded vegetation coverfraction 2017 to 2020",
                    crops_growth = "Change in crop coverfraction 2017 to 2020",
                    builtarea_growth = "Change in built up area 2017 to 2020",
                    bareground_growth = "Change in bareground coverfraction 2017 to 2020",
                    snowice_growth = "Change in snow ice coverfraction 2017 to 2020",
                    clouds_growth = "Change in cloud cover 2017 to 2020",
                    rangeland_growth = "Change in rangeland cover fraction 2017 to 2020",
                    bdi_ppp_2020_UNadj_constrained = "World Pop constrained population 2020",
                    targetave_ntl_all_2015 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2015 (target area average)",
                    targetave_ntl_all_2016 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2016 (target area average)",
                    targetave_ntl_all_2017 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2017 (target area average)",
                    targetave_ntl_all_2018 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2018 (target area average)",
                    targetave_ntl_all_2019 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2019 (target area average)",
                    targetave_ntl_all_2020 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2020 (target area average)",
                    targetave_ntl_built_up_2015 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2015 (target area average)",
                    targetave_ntl_built_up_2016 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2016 (target area average)",
                    targetave_ntl_built_up_2017 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2017 (target area average)",
                    targetave_ntl_built_up_2018 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2018 (target area average)",
                    targetave_ntl_built_up_2019 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2019 (target area average)",
                    targetave_ntl_built_up_2020 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2020 (target area average)",
                    targetave_ntl_small_stl_2015 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2015 (target area average)",
                    targetave_ntl_small_stl_2016 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2016 (target area average)",
                    targetave_ntl_small_stl_2017 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2017 (target area average)",
                    targetave_ntl_small_stl_2018 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2018 (target area average)",
                    targetave_ntl_small_stl_2019 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2019 (target area average)",
                    targetave_ntl_small_stl_2020 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2020 (target area average)",
                    targetave_ntl_hamlet_2015 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2015 (target area average)",
                    targetave_ntl_hamlet_2016 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2016 (target area average)",
                    targetave_ntl_hamlet_2017 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2017 (target area average)",
                    targetave_ntl_hamlet_2018 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2018 (target area average)",
                    targetave_ntl_hamlet_2019 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2019 (target area average)",
                    targetave_ntl_hamlet_2020 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2020 (target area average)",
                    targetave_pop_density_2015 = "2015 Population density (target area average)",
                    targetave_pop_density_2016 = "2016 Population density (target area average)",
                    targetave_pop_density_2017 = "2017 Population density (target area average)",
                    targetave_pop_density_2018 = "2018 Population density (target area average)",
                    targetave_pop_density_2019 = "2019 Population density (target area average)",
                    targetave_pop_density_2020 = "2020 Population density (target area average)",
                    targetave_lightscore_sy_2012 = "2012 Probability of electrification (target area average)",
                    targetave_prplit_conf90_sy_2012 = "2012 Proportion of nights a settlement is brighter than uninhabited areas (target area average)",
                    targetave_lightscore_sy_2019 = "2019 Probability of electrification (target area average)",
                    targetave_prplit_conf90_sy_2019 = "2019 Proportion of nights a settlement is brighter than uninhabited areas (target area average)",
                    targetave_2017_Water = "Proportion area covered by water in 2017 (target area average)",
                    targetave_2017_Trees = "Proportion area covered by trees in 2017 (target area average)",
                    targetave_2017_fld_veg = "Proportion area covered by flooded vegetation in 2017 (target area average)",
                    targetave_2017_Crops = "Proportion area covered by crops in 2017 (target area average)",
                    targetave_2017_Built_Area = "Proportion area covered by built area in 2017 (target area average)",
                    targetave_2017_Bare_ground = "Proportion area covered by bare ground in 2017 (target area average)",
                    targetave_2017_Snow_Ice = "Proportion area covered by snow and ice in 2017 (target area average)",
                    targetave_2017_Clouds = "Proportion area covered by clouds in 2017 (target area average)",
                    targetave_2017_Rangeland = "Proportion area covered by rangeland in 2017 (target area average)",
                    targetave_2018_Water = "Proportion area covered by water in 2018 (target area average)",
                    targetave_2018_Trees = "Proportion area covered by trees in 2018 (target area average)",
                    targetave_2018_fld_veg = "Proportion area covered by flooded vegetation in 2018 (target area average)",
                    targetave_2018_Crops = "Proportion area covered by crops in 2018 (target area average)",
                    targetave_2018_Built_Area = "Proportion area covered by built area in 2018 (target area average)",
                    targetave_2018_Bare_ground = "Proportion area covered by bare ground in 2018 (target area average)",
                    targetave_2018_Snow_Ice = "Proportion area covered by snow and ice in 2018 (target area average)",
                    targetave_2018_Clouds = "Proportion area covered by clouds in 2018 (target area average)",
                    targetave_2018_Rangeland = "Proportion area covered by rangeland in 2018 (target area average)",
                    targetave_2019_Water = "Proportion area covered by water in 2019 (target area average)",
                    targetave_2019_Trees = "Proportion area covered by trees in 2019 (target area average)",
                    targetave_2019_fld_veg = "Proportion area covered by flooded vegetation in 2019 (target area average)",
                    targetave_2019_Crops = "Proportion area covered by crops in 2019 (target area average)",
                    targetave_2019_Built_Area = "Proportion area covered by built area in 2019 (target area average)",
                    targetave_2019_Bare_ground = "Proportion area covered by bare ground in 2019 (target area average)",
                    targetave_2019_Snow_Ice = "Proportion area covered by snow and ice in 2019 (target area average)",
                    targetave_2019_Clouds = "Proportion area covered by clouds in 2019 (target area average)",
                    targetave_2019_Rangeland = "Proportion area covered by rangeland in 2019 (target area average)",
                    targetave_2020_Water = "Proportion area covered by water in 2020 (target area average)",
                    targetave_2020_Trees = "Proportion area covered by trees in 2020 (target area average)",
                    targetave_2020_fld_veg = "Proportion area covered by flooded vegetation in 2020 (target area average)",
                    targetave_2020_Crops = "Proportion area covered by crops in 2020 (target area average)",
                    targetave_2020_Built_Area = "Proportion area covered by built area in 2020 (target area average)",
                    targetave_2020_Bare_ground = "Proportion area covered by bare ground in 2020 (target area average)",
                    targetave_2020_Snow_Ice = "Proportion area covered by snow and ice in 2020 (target area average)",
                    targetave_2020_Clouds = "Proportion area covered by clouds in 2020 (target area average)",
                    targetave_2020_Rangeland = "Proportion area covered by rangeland in 2020 (target area average)",
                    targetave_building_sum = "Number of buildings within WorldPop grid containing Household (target area average)",
                    targetave_building_density = "Number of buildings per square km lived area (target area average)",
                    targetave_building_mean_area = "Mean building areas within WorldPop grid containing Household (target area average)",
                    targetave_building_mean_length = "Mean building lengths within WorldPop grid containing Household (target area average)",
                    targetave_building_mean_urban = "Urbanization rate within WorldPop grid containing Household (target area average)",
                    targetave_pdsi_mean2018="Palmer Drought Severity Index mean 2018 (target area average)",
                    targetave_aet_mean2018="Actual evapotranspiration mean 2018 (target area average)",
                    targetave_def_mean2018="Climate water deficit mean 2018 (target area average)",
                    targetave_pr_mean2018="Precipitation accumulation mean 2018 (target area average)",
                    targetave_ro_mean2018= "Runoff mean 2018 (target area average)",
                    targetave_soil_mean2018="Soil moisture 2018 (target area average)",
                    targetave_tmmn_mean2018= "Minimum temperature mean 2018 (target area average)",
                    targetave_tmmx_mean2018= "Maximum temperature mean 2018 (target area average)",
                    targetave_swe_mean2018= "Snow water equivalent mean 2018 (target area average)",
                    targetave_srad_mean2018= "Downward surface shortwave radiation mean 2018 (target area average)",
                    targetave_pdsi_mean2014="Palmer Drought Severity Index mean 2014 (target area average)",
                    targetave_pdsi_mean2014="Palmer Drought Severity Index mean 2014 (target area average)",
                    targetave_aet_mean2014="Actual evapotranspiration mean 2014 (target area average)",
                    targetave_def_mean2014="Climate water deficit mean 2014 (target area average)",
                    targetave_pr_mean2014="Precipitation accumulation mean 2014 (target area average)",
                    targetave_ro_mean2014= "Runoff mean 2014 (target area average)",
                    targetave_soil_mean2014="Soil moisture mean 2014 (target area average)",
                    targetave_tmmn_mean2014= "Minimum temperature mean 2014 (target area average)",
                    targetave_tmmx_mean2014= "Maximum temperature mean 2014 (target area average)",
                    targetave_swe_mean2014= "Snow water equivalent mean 2014 (target area average)",
                    targetave_srad_mean2014= "Downward surface shortwave radiation mean 2014 (target area average)",
                    targetave_pdsi_mean2009="Palmer Drought Severity Index mean 2009 (target area average)",
                    targetave_aet_mean2009="Actual evapotranspiration mean 2009 (target area average)",
                    targetave_def_mean2009="Climate water deficit mean 2009 (target area average)",
                    targetave_pr_mean2009="Precipitation accumulation mean 2009 (target area average)",
                    targetave_ro_mean2009= "Runoff mean 2009 (target area average)",
                    targetave_soil_mean2009="Soil moisture  mean 2009 (target area average)",
                    targetave_tmmn_mean2009= "Minimum temperature mean 2009 (target area average)",
                    targetave_tmmx_mean2009= "Maximum temperature mean 2009 (target area average)",
                    targetave_swe_mean2009= "Snow water equivalent mean 2009 (target area average)",
                    targetave_srad_mean2009= "Downward surface shortwave radiation mean 2009 (target area average)",
                    targetave_pdsi_mean99_19="Palmer Drought Severity Index mean 1999-2019 (target area average)",
                    targetave_aet_mean99_19="Actual evapotranspiration  mean 1999-2019 (target area average)",
                    targetave_def_mean99_19="Climate water deficit mean 1999-2019 (target area average)",
                    targetave_pr_mean99_19="Precipitation accumulation mean 1999-2019 (target area average)",
                    targetave_ro_mean99_19= "Runoff  mean 1999-2019 (target area average)",
                    targetave_soil_mean99_19="Soil moisture mean 1999-2019 (target area average)",
                    targetave_tmmn_mean99_19= "Minimum temperature mean 1999-2019 (target area average)",
                    targetave_tmmx_mean99_19= "Maximum temperature mean 1999-2019 (target area average)",
                    targetave_swe_mean99_19= "Snow water equivalent mean 1999-2019 (target area average)",
                    targetave_srad_mean99_19= "Downward surface shortwave radiation 1999-2019 (target area average)",
                    targetave_pdsi_change_5="Five year change in Palmer Drought Severity Index mean (target area average)",
                    targetave_aet_change_5="Five year change in Actual evapotranspiration mean (target area average)",
                    targetave_def_change_5="Five year change in Climate water deficit mean (target area average)",
                    targetave_pr_change_5="Five year change in Precipitation accumulation mean (target area average)",
                    targetave_ro_change_5= "Five year change in Runoff mean (target area average)",
                    targetave_soil_change_5="Five year change in Soil moisture mean (target area average)",
                    targetave_tmmn_change_5= "Five year change in Minimum temperature mean (target area average)",
                    targetave_tmmx_change_5= "Five year change in Maximum temperature mean (target area average)",
                    targetave_swe_change_5= "Five year change in Snow water equivalent mean (target area average)",
                    targetave_srad_change_5= "Five year change in Downward surface shortwave radiation mean (target area average)",
                    targetave_pdsi_change_10="Ten year change in Palmer Drought Severity Index mean mean (target area average)",
                    targetave_aet_change_10="Ten year change in Actual evapotranspiration mean (target area average)",
                    targetave_def_change_10="Ten year change in Climate water deficit mean (target area average)",
                    targetave_pr_change_10="Ten year change in Precipitation accumulation mean (target area average)",
                    targetave_ro_change_10= "Ten year change in Runoff mean (target area average)",
                    targetave_soil_change_10="Ten year change in Soil moisture mean (target area average)",
                    targetave_tmmn_change_10= "Ten year change in Minimum temperature mean (target area average)",
                    targetave_tmmx_change_10= "Ten year change in Maximum temperature mean (target area average)",
                    targetave_swe_change_10= "Ten year change in Snow water equivalent mean (target area average)",
                    targetave_srad_change_10= "Ten year change in Downward surface shortwave radiation mean (target area average)",
                    targetave_pdsi_hist_dev="Historical deviation from the mean of Palmer Drought Severity Index (target area average)",
                    targetave_aet_hist_dev="Historical deviation from the mean of Actual evapotranspiration (target area average)",
                    targetave_def_hist_dev="Historical deviation from the mean of Climate water deficit (target area average)",
                    targetave_pr_hist_dev="Historical deviation from the mean of Precipitation accumulation (target area average)",
                    targetave_ro_hist_dev= "Historical deviation from the mean of Runoff (target area average)",
                    targetave_soil_hist_dev="Historical deviation from the mean of Soil moisture (target area average)",
                    targetave_tmmn_hist_dev= "Historical deviation from the mean of Minimum temperature (target area average)",
                    targetave_tmmx_hist_dev= "Historical deviation from the mean of Maximum temperature (target area average)",
                    targetave_swe_hist_dev= "Historical deviation from the mean of Snow water equivalent (target area average)",
                    targetave_srad_hist_dev= "Historical deviation from the mean of Downward surface shortwave radiation (target area average)",
                    targetave_pdsi_sq_hist_dev="Squared historical deviation from the mean of Palmer Drought Severity Index (target area average)",
                    targetave_aet_sq_hist_dev="Squared historical deviation from the mean of Actual evapotranspiration (target area average)",
                    targetave_def_sq_hist_dev="Squared historical deviation from the mean of Climate water deficit (target area average)",
                    targetave_pr_sq_hist_dev="Squared historical deviation from the mean of Precipitation accumulation (target area average)",
                    targetave_ro_sq_hist_dev= "Squared historical deviation from the mean of Runoff (target area average)",
                    targetave_soil_sq_hist_dev="Squared historical deviation from the mean of Soil moisture (target area average)",
                    targetave_tmmn_sq_hist_dev= "Squared historical deviation from the mean of Minimum temperature (target area average)",
                    targetave_tmmx_sq_hist_dev= "Squared historical deviation from the mean of Maximum temperature (target area average)",
                    targetave_swe_sq_hist_dev= "Squared historical deviation from the mean of Snow water equivalent (target area average)",
                    targetave_srad_sq_hist_dev= "Squared historical deviation from the mean of Downward surface shortwave radiation (target area average)",
                    targetave_meta_wealth_index = "Facebook Meta Wealth Index (target area average)",
                    targetave_BU2016DHS_ANANEMWANY = "% women aged 15-49 with anemia (DHS 2016) (target area average)",
                    targetave_BU2016DHS_CHVAC1CVCD = "% children 12-23 mths with vaccination card (DHS 2016) (target area average)",
                    targetave_BU2016DHS_CHVACCCBAS = "% children 12-23 mths with 8 basic vaccinations (DHS 2016) (target area average)",
                    targetave_BU2016DHS_CHVACCCDP1 = "% children 12-23 mths with DPT1 vaccination (DHS 2016) (target area average)",
                    targetave_BU2016DHS_CHVACCCDP3 = "% children 12-23 mths with 3rd dose DPT (DHS 2016) (target area average)",
                    targetave_BU2016DHS_CHVACSCMSL = "% children 12-23 mths received Measles vaccination (DHS 2016) (target area average)",
                    targetave_BU2016DHS_CNNUTSCHA2 = "% children under age 5 stunted (DHS 2016) (target area average)",
                    targetave_BU2016DHS_EDLITRMLIT = "% men aged 15-49 who are literate (DHS 2016) (target area average)",
                    targetave_BU2016DHS_EDLITRWLIT = "% women aged 15-49 who are literate (DHS 2016) (target area average)",
                    targetave_BU2016DHS_FPCUSMWMOD = "% currently married or in union women using modern contraception (DHS 2016) (target area average)",
                    targetave_BU2016DHS_FPNADMWPDM = "ratio of currently married women using FP to FP demand from same group (DHS 2016) (target area average)",
                    targetave_BU2016DHS_FPNADMWUNT = "% currently married or in union women with an unmet need for FP (DHS 2016) (target area average)",
                    targetave_BU2016DHS_MLITNAPACC = "% of HH population sleeping under an ITN if ITN can be used by <=2 people (DHS 2016) (target area average)",
                    targetave_BU2016DHS_RHANCNWN4P = "% women had live birth in last 5 years who had 4+ antenatal care visits (DHS 2016) (target area average)",
                    targetave_BU2016DHS_RHDELPCDHF = "% live births in the 5 years preceding DHS 2016 delivered at a health facility (DHS 2016) (target area average)",
                    targetave_BU2016DHS_WSSRCEPIMP = "% of population living in HHs with improved main source of drinking water (DHS 2016) (target area average)",
                    targetave_BU2016DHS_WSTLETPNFC = "% of population living in HHs with main type of toilet is no facility (DHS 2016) (target area average)",
                    targetave_normal_ntl_mean2015 = "2015 Average Night Luminosity (all areas unfiltered) (target area average)",
                    targetave_normal_ntl_mean2016 = "2016 Average Night Luminosity (all areas unfiltered) (target area average)",
                    targetave_normal_ntl_mean2017 = "2017 Average Night Luminosity (all areas unfiltered) (target area average)",
                    targetave_normal_ntl_mean2018 = "2018 Average Night Luminosity (all areas unfiltered) (target area average)",
                    targetave_normal_ntl_mean2019 = "2019 Average Night Luminosity (all areas unfiltered) (target area average)",
                    targetave_normal_ntl_mean2020 = "2020 Average Night Luminosity (all areas unfiltered) (target area average)",
                    BDI001 = "Bubanza Dummy",
                    BDI002 = "Bujumbura Rural Dummy",
                    BDI003 = "Bururi Dummy",
                    BDI004 = "Cankuzo Dummy",
                    BDI005 = "Cibitoke Dummy",
                    BDI006 = "Gitega Dummy",
                    BDI007 = "Karuzi Dummy",
                    BDI008 = "Kayanza Dummy",
                    BDI009 = "Kirundo Dummy",
                    BDI010 = "Makamba Dummy",
                    BDI011 = "Muramvya Dummy",
                    BDI012 = "Muyinga Dummy",
                    BDI013 = "Mwaro Dummy",
                    BDI014 = "Ngozi Dummy",
                    BDI015 = "Rutana Dummy",
                    BDI016 = "Ruyigi Dummy",
                    BDI017 = "Bujumbura Mairie Dummy",
                    BDI018 = "Rumonge Dummy"
)


geosurvey_dt <-
  expss::apply_labels(geosurvey_dt,
                      admin2Pcod = "Commune/Target Area Codes",
                      admin4Pcode = "WorldPop Grid idenitifer code",
                      admin3Pcode = "Grid-Level identifier",
                      admin0Pcod = "Country Code",
                      admin1Pcod = "Region Codes",
                      admin2Name = "Commune/Target Area Names",
                      area = "Anonymous Area",
                      poly_area = "Tesselated Grid Area",
                      ntl_all_2015 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2015",
                      ntl_all_2016 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2016",
                      ntl_all_2017 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2017",
                      ntl_all_2018 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2018",
                      ntl_all_2019 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2019",
                      ntl_all_2020 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2020",
                      ntl_built_up_2015 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2015",
                      ntl_built_up_2016 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2016",
                      ntl_built_up_2017 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2017",
                      ntl_built_up_2018 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2018",
                      ntl_built_up_2019 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2019",
                      ntl_built_up_2020 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2020",
                      ntl_small_stl_2015 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2015",
                      ntl_small_stl_2016 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2016",
                      ntl_small_stl_2017 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2017",
                      ntl_small_stl_2018 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2018",
                      ntl_small_stl_2019 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2019",
                      ntl_small_stl_2020 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2020",
                      ntl_hamlet_2015 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2015",
                      ntl_hamlet_2016 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2016",
                      ntl_hamlet_2017 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2017",
                      ntl_hamlet_2018 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2018",
                      ntl_hamlet_2019 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2019",
                      ntl_hamlet_2020 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2020",
                      pop_density_2015 = "2015 Population density",
                      pop_density_2016 = "2016 Population density",
                      pop_density_2017 = "2017 Population density",
                      pop_density_2018 = "2018 Population density",
                      pop_density_2019 = "2019 Population density",
                      pop_density_2020 = "2020 Population density",
                      lightscore_sy_2012 = "2012 Probability of electrification",
                      prplit_conf90_sy_2012 = "2012 Proportion of nights a settlement is brighter than uninhabited areas",
                      lightscore_sy_2019 = "2019 Probability of electrification",
                      prplit_conf90_sy_2019 = "2019 Proportion of nights a settlement is brighter than uninhabited areas",
                      y2017_Water = "Proportion area covered by water in 2017",
                      y2017_Trees = "Proportion area covered by trees in 2017",
                      y2017_Flooded_vegetation = "Proportion area covered by flooded vegetation in 2017",
                      y2017_Crops = "Proportion area covered by crops in 2017",
                      y2017_Built_Area = "Proportion area covered by built area in 2017",
                      y2017_Bare_ground = "Proportion area covered by bare ground in 2017",
                      y2017_Snow_Ice = "Proportion area covered by snow and ice in 2017",
                      y2017_Clouds = "Proportion area covered by clouds in 2017",
                      y2017_Rangeland = "Proportion area covered by rangeland in 2017",
                      y2018_Water = "Proportion area covered by water in 2018",
                      y2018_Trees = "Proportion area covered by trees in 2018",
                      y2018_Flooded_vegetation = "Proportion area covered by flooded vegetation in 2018",
                      y2018_Crops = "Proportion area covered by crops in 2018",
                      y2018_Built_Area = "Proportion area covered by built area in 2018",
                      y2018_Bare_ground = "Proportion area covered by bare ground in 2018",
                      y2018_Snow_Ice = "Proportion area covered by snow and ice in 2018",
                      y2018_Clouds = "Proportion area covered by clouds in 2018",
                      y2018_Rangeland = "Proportion area covered by rangeland in 2018",
                      y2019_Water = "Proportion area covered by water in 2019",
                      y2019_Trees = "Proportion area covered by trees in 2019",
                      y2019_Flooded_vegetation = "Proportion area covered by flooded vegetation in 2019",
                      y2019_Crops = "Proportion area covered by crops in 2019",
                      y2019_Built_Area = "Proportion area covered by built area in 2019",
                      y2019_Bare_ground = "Proportion area covered by bare ground in 2019",
                      y2019_Snow_Ice = "Proportion area covered by snow and ice in 2019",
                      y2019_Clouds = "Proportion area covered by clouds in 2019",
                      y2019_Rangeland = "Proportion area covered by rangeland in 2019",
                      y2020_Water = "Proportion area covered by water in 2020",
                      y2020_Trees = "Proportion area covered by trees in 2020",
                      y2020_Flooded_vegetation = "Proportion area covered by flooded vegetation in 2020",
                      y2020_Crops = "Proportion area covered by crops in 2020",
                      y2020_Built_Area = "Proportion area covered by built area in 2020",
                      y2020_Bare_ground = "Proportion area covered by bare ground in 2020",
                      y2020_Snow_Ice = "Proportion area covered by snow and ice in 2020",
                      y2020_Clouds = "Proportion area covered by clouds in 2020",
                      y2020_Rangeland = "Proportion area covered by rangeland in 2020",
                      building_sum = "Number of buildings within WorldPop grid containing Household",
                      building_density = "Number of buildings per square km lived area",
                      building_mean_area = "Mean building areas within WorldPop grid containing Household",
                      building_mean_length = "Mean building lengths within WorldPop grid containing Household",
                      building_mean_urban = "Urbanization rate within WorldPop grid containing Household",
                      pdsi_mean2018="Palmer Drought Severity Index mean 2018",
                      aet_mean2018="Actual evapotranspiration  mean 2018",
                      def_mean2018="Climate water deficit mean 2018",
                      pr_mean2018="Precipitation accumulation mean 2018",
                      ro_mean2018= "Runoff mean 2018",
                      soil_mean2018="Soil moisture 2018",
                      tmmn_mean2018= "Minimum temperature mean 2018",
                      tmmx_mean2018= "Maximum temperature mean 2018",
                      swe_mean2018= "Snow water equivalent mean 2018",
                      srad_mean2018= "Downward surface shortwave radiation mean 2018",
                      pdsi_mean2014="Palmer Drought Severity Index mean 2014",
                      pdsi_mean2014="Palmer Drought Severity Index mean 2014",
                      aet_mean2014="Actual evapotranspiration mean 2014",
                      def_mean2014="Climate water deficit mean 2014",
                      pr_mean2014="Precipitation accumulation mean 2014",
                      ro_mean2014= "Runoff mean 2014",
                      soil_mean2014="Soil moisture mean 2014",
                      tmmn_mean2014= "Minimum temperature mean 2014",
                      tmmx_mean2014= "Maximum temperature mean 2014",
                      swe_mean2014= "Snow water equivalent mean 2014",
                      srad_mean2014= "Downward surface shortwave radiation mean 2014",
                      pdsi_mean2009="Palmer Drought Severity Index mean 2009",
                      aet_mean2009="Actual evapotranspiration  mean 2009",
                      def_mean2009="Climate water deficit mean 2009",
                      pr_mean2009="Precipitation accumulation mean 2009",
                      ro_mean2009= "Runoff mean 2009",
                      soil_mean2009="Soil moisture  mean 2009",
                      tmmn_mean2009= "Minimum temperature mean 2009",
                      tmmx_mean2009= "Maximum temperature mean 2009",
                      swe_mean2009= "Snow water equivalent mean 2009",
                      srad_mean2009= "Downward surface shortwave radiation mean 2009",
                      pdsi_mean99_19="Palmer Drought Severity Index mean 1999-2019",
                      aet_mean99_19="Actual evapotranspiration  mean 1999-2019",
                      def_mean99_19="Climate water deficit mean 1999-2019",
                      pr_mean99_19="Precipitation accumulation mean 1999-2019",
                      ro_mean99_19= "Runoff  mean 1999-2019",
                      soil_mean99_19="Soil moisture mean 1999-2019",
                      tmmn_mean99_19= "Minimum temperature mean 1999-2019",
                      tmmx_mean99_19= "Maximum temperature mean 1999-2019",
                      swe_mean99_19= "Snow water equivalent mean 1999-2019",
                      srad_mean99_19= "Downward surface shortwave radiation 1999-2019",
                      pdsi_change_5="Five year change in Palmer Drought Severity Index mean",
                      aet_change_5="Five year change in Actual evapotranspiration mean",
                      def_change_5="Five year change in Climate water deficit mean",
                      pr_change_5="Five year change in Precipitation accumulation mean",
                      ro_change_5= "Five year change in Runoff mean",
                      soil_change_5="Five year change in Soil moisture mean",
                      tmmn_change_5= "Five year change in Minimum temperature mean",
                      tmmx_change_5= "Five year change in Maximum temperature mean",
                      swe_change_5= "Five year change in Snow water equivalent mean",
                      srad_change_5= "Five year change in Downward surface shortwave radiation mean",
                      pdsi_change_10="Ten year change in Palmer Drought Severity Index mean mean",
                      aet_change_10="Ten year change in Actual evapotranspiration mean",
                      def_change_10="Ten year change in Climate water deficit mean",
                      pr_change_10="Ten year change in Precipitation accumulation mean",
                      ro_change_10= "Ten year change in Runoff mean",
                      soil_change_10="Ten year change in Soil moisture mean",
                      tmmn_change_10= "Ten year change in Minimum temperature mean",
                      tmmx_change_10= "Ten year change in Maximum temperature mean",
                      swe_change_10= "Ten year change in Snow water equivalent mean",
                      srad_change_10= "Ten year change in Downward surface shortwave radiation mean",
                      pdsi_hist_dev="Historical deviation from the mean of Palmer Drought Severity Index",
                      aet_hist_dev="Historical deviation from the mean of Actual evapotranspiration",
                      def_hist_dev="Historical deviation from the mean of Climate water deficit",
                      pr_hist_dev="Historical deviation from the mean of Precipitation accumulation",
                      ro_hist_dev= "Historical deviation from the mean of Runoff",
                      soil_hist_dev="Historical deviation from the mean of Soil moisture",
                      tmmn_hist_dev= "Historical deviation from the mean of Minimum temperature",
                      tmmx_hist_dev= "Historical deviation from the mean of Maximum temperature",
                      swe_hist_dev= "Historical deviation from the mean of Snow water equivalent",
                      srad_hist_dev= "Historical deviation from the mean of Downward surface shortwave radiation",
                      pdsi_sq_hist_dev="Squared historical deviation from the mean of Palmer Drought Severity Index",
                      aet_sq_hist_dev="Squared historical deviation from the mean of Actual evapotranspiration",
                      def_sq_hist_dev="Squared historical deviation from the mean of Climate water deficit",
                      pr_sq_hist_dev="Squared historical deviation from the mean of Precipitation accumulation",
                      ro_sq_hist_dev= "Squared historical deviation from the mean of Runoff",
                      soil_sq_hist_dev="Squared historical deviation from the mean of Soil moisture",
                      tmmn_sq_hist_dev= "Squared historical deviation from the mean of Minimum temperature",
                      tmmx_sq_hist_dev= "Squared historical deviation from the mean of Maximum temperature",
                      swe_sq_hist_dev= "Squared historical deviation from the mean of Snow water equivalent",
                      srad_sq_hist_dev= "Squared historical deviation from the mean of Downward surface shortwave radiation",
                      meta_wealth_index = "Facebook Meta Wealth Index",
                      BU2016DHS_ANANEMWANY = "% women aged 15-49 with anemia (DHS 2016)",
                      BU2016DHS_CHVAC1CVCD = "% children 12-23 mths with vaccination card (DHS 2016)",
                      BU2016DHS_CHVACCCBAS = "% children 12-23 mths with 8 basic vaccinations (DHS 2016)",
                      BU2016DHS_CHVACCCDP1 = "% children 12-23 mths with DPT1 vaccination (DHS 2016)",
                      BU2016DHS_CHVACCCDP3 = "% children 12-23 mths with 3rd dose DPT (DHS 2016)",
                      BU2016DHS_CHVACSCMSL = "% children 12-23 mths received Measles vaccination (DHS 2016)",
                      BU2016DHS_CNNUTSCHA2 = "% children under age 5 stunted (DHS 2016)",
                      BU2016DHS_EDLITRMLIT = "% men aged 15-49 who are literate (DHS 2016)",
                      BU2016DHS_EDLITRWLIT = "% women aged 15-49 who are literate (DHS 2016)",
                      BU2016DHS_FPCUSMWMOD = "% currently married or in union women using modern contraception (DHS 2016)",
                      BU2016DHS_FPNADMWPDM = "ratio of currently married women using FP to FP demand from same group (DHS 2016)",
                      BU2016DHS_FPNADMWUNT = "% currently married or in union women with an unmet need for FP (DHS 2016)",
                      BU2016DHS_MLITNAPACC = "% of HH population sleeping under an ITN if ITN can be used by <=2 people (DHS 2016)",
                      BU2016DHS_RHANCNWN4P = "% women had live birth in last 5 years who had 4+ antenatal care visits (DHS 2016)",
                      BU2016DHS_RHDELPCDHF = "% live births in the 5 years preceding DHS 2016 delivered at a health facility (DHS 2016)",
                      BU2016DHS_WSSRCEPIMP = "% of population living in HHs with improved main source of drinking water (DHS 2016)",
                      BU2016DHS_WSTLETPNFC = "% of population living in HHs with main type of toilet is no facility (DHS 2016)",
                      normal_ntl_mean2015 = "2015 Average Night Luminosity (all areas unfiltered)",
                      normal_ntl_mean2016 = "2016 Average Night Luminosity (all areas unfiltered)",
                      normal_ntl_mean2017 = "2017 Average Night Luminosity (all areas unfiltered)",
                      normal_ntl_mean2018 = "2018 Average Night Luminosity (all areas unfiltered)",
                      normal_ntl_mean2019 = "2019 Average Night Luminosity (all areas unfiltered)",
                      normal_ntl_mean2020 = "2020 Average Night Luminosity (all areas unfiltered)",
                      ntl_all_growth = "Growth in Night Limonsity 2015 to 2020 (all areas unfiltered)",
                      ntl_built_up_growth = "Growth in Night Limonsity 2015 to 2020 (built up areas)",
                      ntl_small_stl_growth = "Growth in Night Limonsity 2015 to 2020 (small settlements)",
                      ntl_hamlet_growth = "Growth in Night Limonsity 2015 to 2020 (hamlets)",
                      log_pop_density_growth = "Growth in Population Density 2015 to 2020",
                      water_growth = "Change in water coverfraction 2017 to 2020",
                      trees_growth = "Change in tree coverfraction 2017 to 2020",
                      floodedveg_growth = "Change in flooded vegetation coverfraction 2017 to 2020",
                      crops_growth = "Change in crop coverfraction 2017 to 2020",
                      builtarea_growth = "Change in built up area 2017 to 2020",
                      bareground_growth = "Change in bareground coverfraction 2017 to 2020",
                      snowice_growth = "Change in snow ice coverfraction 2017 to 2020",
                      clouds_growth = "Change in cloud cover 2017 to 2020",
                      rangeland_growth = "Change in rangeland cover fraction 2017 to 2020",
                      bdi_ppp_2020_UNadj_constrained = "World Pop constrained population 2020",
                      targetave_ntl_all_2015 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2015 (target area average)",
                      targetave_ntl_all_2016 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2016 (target area average)",
                      targetave_ntl_all_2017 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2017 (target area average)",
                      targetave_ntl_all_2018 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2018 (target area average)",
                      targetave_ntl_all_2019 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2019 (target area average)",
                      targetave_ntl_all_2020 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Human Settlement Area 2020 (target area average)",
                      targetave_ntl_built_up_2015 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2015 (target area average)",
                      targetave_ntl_built_up_2016 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2016 (target area average)",
                      targetave_ntl_built_up_2017 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2017 (target area average)",
                      targetave_ntl_built_up_2018 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2018 (target area average)",
                      targetave_ntl_built_up_2019 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2019 (target area average)",
                      targetave_ntl_built_up_2020 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Built Up areas 2020 (target area average)",
                      targetave_ntl_small_stl_2015 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2015 (target area average)",
                      targetave_ntl_small_stl_2016 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2016 (target area average)",
                      targetave_ntl_small_stl_2017 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2017 (target area average)",
                      targetave_ntl_small_stl_2018 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2018 (target area average)",
                      targetave_ntl_small_stl_2019 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2019 (target area average)",
                      targetave_ntl_small_stl_2020 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Small Settlements Areas 2020 (target area average)",
                      targetave_ntl_hamlet_2015 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2015 (target area average)",
                      targetave_ntl_hamlet_2016 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2016 (target area average)",
                      targetave_ntl_hamlet_2017 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2017 (target area average)",
                      targetave_ntl_hamlet_2018 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2018 (target area average)",
                      targetave_ntl_hamlet_2019 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2019 (target area average)",
                      targetave_ntl_hamlet_2020 = "Average Night Luminosity (nanoWatts/sr/cm^2) in Hamlet Areas 2020 (target area average)",
                      targetave_pop_density_2015 = "2015 Population density (target area average)",
                      targetave_pop_density_2016 = "2016 Population density (target area average)",
                      targetave_pop_density_2017 = "2017 Population density (target area average)",
                      targetave_pop_density_2018 = "2018 Population density (target area average)",
                      targetave_pop_density_2019 = "2019 Population density (target area average)",
                      targetave_pop_density_2020 = "2020 Population density (target area average)",
                      targetave_lightscore_sy_2012 = "2012 Probability of electrification (target area average)",
                      targetave_prplit_conf90_sy_2012 = "2012 Proportion of nights a settlement is brighter than uninhabited areas (target area average)",
                      targetave_lightscore_sy_2019 = "2019 Probability of electrification (target area average)",
                      targetave_prplit_conf90_sy_2019 = "2019 Proportion of nights a settlement is brighter than uninhabited areas (target area average)",
                      targetave_2017_Water = "Proportion area covered by water in 2017 (target area average)",
                      targetave_2017_Trees = "Proportion area covered by trees in 2017 (target area average)",
                      targetave_2017_fld_veg = "Proportion area covered by flooded vegetation in 2017 (target area average)",
                      targetave_2017_Crops = "Proportion area covered by crops in 2017 (target area average)",
                      targetave_2017_Built_Area = "Proportion area covered by built area in 2017 (target area average)",
                      targetave_2017_Bare_ground = "Proportion area covered by bare ground in 2017 (target area average)",
                      targetave_2017_Snow_Ice = "Proportion area covered by snow and ice in 2017 (target area average)",
                      targetave_2017_Clouds = "Proportion area covered by clouds in 2017 (target area average)",
                      targetave_2017_Rangeland = "Proportion area covered by rangeland in 2017 (target area average)",
                      targetave_2018_Water = "Proportion area covered by water in 2018 (target area average)",
                      targetave_2018_Trees = "Proportion area covered by trees in 2018 (target area average)",
                      targetave_2018_fld_veg = "Proportion area covered by flooded vegetation in 2018 (target area average)",
                      targetave_2018_Crops = "Proportion area covered by crops in 2018 (target area average)",
                      targetave_2018_Built_Area = "Proportion area covered by built area in 2018 (target area average)",
                      targetave_2018_Bare_ground = "Proportion area covered by bare ground in 2018 (target area average)",
                      targetave_2018_Snow_Ice = "Proportion area covered by snow and ice in 2018 (target area average)",
                      targetave_2018_Clouds = "Proportion area covered by clouds in 2018 (target area average)",
                      targetave_2018_Rangeland = "Proportion area covered by rangeland in 2018 (target area average)",
                      targetave_2019_Water = "Proportion area covered by water in 2019 (target area average)",
                      targetave_2019_Trees = "Proportion area covered by trees in 2019 (target area average)",
                      targetave_2019_fld_veg = "Proportion area covered by flooded vegetation in 2019 (target area average)",
                      targetave_2019_Crops = "Proportion area covered by crops in 2019 (target area average)",
                      targetave_2019_Built_Area = "Proportion area covered by built area in 2019 (target area average)",
                      targetave_2019_Bare_ground = "Proportion area covered by bare ground in 2019 (target area average)",
                      targetave_2019_Snow_Ice = "Proportion area covered by snow and ice in 2019 (target area average)",
                      targetave_2019_Clouds = "Proportion area covered by clouds in 2019 (target area average)",
                      targetave_2019_Rangeland = "Proportion area covered by rangeland in 2019 (target area average)",
                      targetave_2020_Water = "Proportion area covered by water in 2020 (target area average)",
                      targetave_2020_Trees = "Proportion area covered by trees in 2020 (target area average)",
                      targetave_2020_fld_veg = "Proportion area covered by flooded vegetation in 2020 (target area average)",
                      targetave_2020_Crops = "Proportion area covered by crops in 2020 (target area average)",
                      targetave_2020_Built_Area = "Proportion area covered by built area in 2020 (target area average)",
                      targetave_2020_Bare_ground = "Proportion area covered by bare ground in 2020 (target area average)",
                      targetave_2020_Snow_Ice = "Proportion area covered by snow and ice in 2020 (target area average)",
                      targetave_2020_Clouds = "Proportion area covered by clouds in 2020 (target area average)",
                      targetave_2020_Rangeland = "Proportion area covered by rangeland in 2020 (target area average)",
                      targetave_building_sum = "Number of buildings within WorldPop grid containing Household (target area average)",
                      targetave_building_density = "Number of buildings per square km lived area (target area average)",
                      targetave_building_mean_area = "Mean building areas within WorldPop grid containing Household (target area average)",
                      targetave_building_mean_length = "Mean building lengths within WorldPop grid containing Household (target area average)",
                      targetave_building_mean_urban = "Urbanization rate within WorldPop grid containing Household (target area average)",
                      targetave_pdsi_mean2018="Palmer Drought Severity Index mean 2018 (target area average)",
                      targetave_aet_mean2018="Actual evapotranspiration mean 2018 (target area average)",
                      targetave_def_mean2018="Climate water deficit mean 2018 (target area average)",
                      targetave_pr_mean2018="Precipitation accumulation mean 2018 (target area average)",
                      targetave_ro_mean2018= "Runoff mean 2018 (target area average)",
                      targetave_soil_mean2018="Soil moisture 2018 (target area average)",
                      targetave_tmmn_mean2018= "Minimum temperature mean 2018 (target area average)",
                      targetave_tmmx_mean2018= "Maximum temperature mean 2018 (target area average)",
                      targetave_swe_mean2018= "Snow water equivalent mean 2018 (target area average)",
                      targetave_srad_mean2018= "Downward surface shortwave radiation mean 2018 (target area average)",
                      targetave_pdsi_mean2014="Palmer Drought Severity Index mean 2014 (target area average)",
                      targetave_pdsi_mean2014="Palmer Drought Severity Index mean 2014 (target area average)",
                      targetave_aet_mean2014="Actual evapotranspiration mean 2014 (target area average)",
                      targetave_def_mean2014="Climate water deficit mean 2014 (target area average)",
                      targetave_pr_mean2014="Precipitation accumulation mean 2014 (target area average)",
                      targetave_ro_mean2014= "Runoff mean 2014 (target area average)",
                      targetave_soil_mean2014="Soil moisture mean 2014 (target area average)",
                      targetave_tmmn_mean2014= "Minimum temperature mean 2014 (target area average)",
                      targetave_tmmx_mean2014= "Maximum temperature mean 2014 (target area average)",
                      targetave_swe_mean2014= "Snow water equivalent mean 2014 (target area average)",
                      targetave_srad_mean2014= "Downward surface shortwave radiation mean 2014 (target area average)",
                      targetave_pdsi_mean2009="Palmer Drought Severity Index mean 2009 (target area average)",
                      targetave_aet_mean2009="Actual evapotranspiration mean 2009 (target area average)",
                      targetave_def_mean2009="Climate water deficit mean 2009 (target area average)",
                      targetave_pr_mean2009="Precipitation accumulation mean 2009 (target area average)",
                      targetave_ro_mean2009= "Runoff mean 2009 (target area average)",
                      targetave_soil_mean2009="Soil moisture  mean 2009 (target area average)",
                      targetave_tmmn_mean2009= "Minimum temperature mean 2009 (target area average)",
                      targetave_tmmx_mean2009= "Maximum temperature mean 2009 (target area average)",
                      targetave_swe_mean2009= "Snow water equivalent mean 2009 (target area average)",
                      targetave_srad_mean2009= "Downward surface shortwave radiation mean 2009 (target area average)",
                      targetave_pdsi_mean99_19="Palmer Drought Severity Index mean 1999-2019 (target area average)",
                      targetave_aet_mean99_19="Actual evapotranspiration  mean 1999-2019 (target area average)",
                      targetave_def_mean99_19="Climate water deficit mean 1999-2019 (target area average)",
                      targetave_pr_mean99_19="Precipitation accumulation mean 1999-2019 (target area average)",
                      targetave_ro_mean99_19= "Runoff  mean 1999-2019 (target area average)",
                      targetave_soil_mean99_19="Soil moisture mean 1999-2019 (target area average)",
                      targetave_tmmn_mean99_19= "Minimum temperature mean 1999-2019 (target area average)",
                      targetave_tmmx_mean99_19= "Maximum temperature mean 1999-2019 (target area average)",
                      targetave_swe_mean99_19= "Snow water equivalent mean 1999-2019 (target area average)",
                      targetave_srad_mean99_19= "Downward surface shortwave radiation 1999-2019 (target area average)",
                      targetave_pdsi_change_5="Five year change in Palmer Drought Severity Index mean (target area average)",
                      targetave_aet_change_5="Five year change in Actual evapotranspiration mean (target area average)",
                      targetave_def_change_5="Five year change in Climate water deficit mean (target area average)",
                      targetave_pr_change_5="Five year change in Precipitation accumulation mean (target area average)",
                      targetave_ro_change_5= "Five year change in Runoff mean (target area average)",
                      targetave_soil_change_5="Five year change in Soil moisture mean (target area average)",
                      targetave_tmmn_change_5= "Five year change in Minimum temperature mean (target area average)",
                      targetave_tmmx_change_5= "Five year change in Maximum temperature mean (target area average)",
                      targetave_swe_change_5= "Five year change in Snow water equivalent mean (target area average)",
                      targetave_srad_change_5= "Five year change in Downward surface shortwave radiation mean (target area average)",
                      targetave_pdsi_change_10="Ten year change in Palmer Drought Severity Index mean mean (target area average)",
                      targetave_aet_change_10="Ten year change in Actual evapotranspiration mean (target area average)",
                      targetave_def_change_10="Ten year change in Climate water deficit mean (target area average)",
                      targetave_pr_change_10="Ten year change in Precipitation accumulation mean (target area average)",
                      targetave_ro_change_10= "Ten year change in Runoff mean (target area average)",
                      targetave_soil_change_10="Ten year change in Soil moisture mean (target area average)",
                      targetave_tmmn_change_10= "Ten year change in Minimum temperature mean (target area average)",
                      targetave_tmmx_change_10= "Ten year change in Maximum temperature mean (target area average)",
                      targetave_swe_change_10= "Ten year change in Snow water equivalent mean (target area average)",
                      targetave_srad_change_10= "Ten year change in Downward surface shortwave radiation mean (target area average)",
                      targetave_pdsi_hist_dev="Historical deviation from the mean of Palmer Drought Severity Index (target area average)",
                      targetave_aet_hist_dev="Historical deviation from the mean of Actual evapotranspiration (target area average)",
                      targetave_def_hist_dev="Historical deviation from the mean of Climate water deficit (target area average)",
                      targetave_pr_hist_dev="Historical deviation from the mean of Precipitation accumulation (target area average)",
                      targetave_ro_hist_dev= "Historical deviation from the mean of Runoff (target area average)",
                      targetave_soil_hist_dev="Historical deviation from the mean of Soil moisture (target area average)",
                      targetave_tmmn_hist_dev= "Historical deviation from the mean of Minimum temperature (target area average)",
                      targetave_tmmx_hist_dev= "Historical deviation from the mean of Maximum temperature (target area average)",
                      targetave_swe_hist_dev= "Historical deviation from the mean of Snow water equivalent (target area average)",
                      targetave_srad_hist_dev= "Historical deviation from the mean of Downward surface shortwave radiation (target area average)",
                      targetave_pdsi_sq_hist_dev="Squared historical deviation from the mean of Palmer Drought Severity Index (target area average)",
                      targetave_aet_sq_hist_dev="Squared historical deviation from the mean of Actual evapotranspiration (target area average)",
                      targetave_def_sq_hist_dev="Squared historical deviation from the mean of Climate water deficit (target area average)",
                      targetave_pr_sq_hist_dev="Squared historical deviation from the mean of Precipitation accumulation (target area average)",
                      targetave_ro_sq_hist_dev= "Squared historical deviation from the mean of Runoff (target area average)",
                      targetave_soil_sq_hist_dev="Squared historical deviation from the mean of Soil moisture (target area average)",
                      targetave_tmmn_sq_hist_dev= "Squared historical deviation from the mean of Minimum temperature (target area average)",
                      targetave_tmmx_sq_hist_dev= "Squared historical deviation from the mean of Maximum temperature (target area average)",
                      targetave_swe_sq_hist_dev= "Squared historical deviation from the mean of Snow water equivalent (target area average)",
                      targetave_srad_sq_hist_dev= "Squared historical deviation from the mean of Downward surface shortwave radiation (target area average)",
                      targetave_meta_wealth_index = "Facebook Meta Wealth Index (target area average)",
                      targetave_BU2016DHS_ANANEMWANY = "% women aged 15-49 with anemia (DHS 2016) (target area average)",
                      targetave_BU2016DHS_CHVAC1CVCD = "% children 12-23 mths with vaccination card (DHS 2016) (target area average)",
                      targetave_BU2016DHS_CHVACCCBAS = "% children 12-23 mths with 8 basic vaccinations (DHS 2016) (target area average)",
                      targetave_BU2016DHS_CHVACCCDP1 = "% children 12-23 mths with DPT1 vaccination (DHS 2016) (target area average)",
                      targetave_BU2016DHS_CHVACCCDP3 = "% children 12-23 mths with 3rd dose DPT (DHS 2016) (target area average)",
                      targetave_BU2016DHS_CHVACSCMSL = "% children 12-23 mths received Measles vaccination (DHS 2016) (target area average)",
                      targetave_BU2016DHS_CNNUTSCHA2 = "% children under age 5 stunted (DHS 2016) (target area average)",
                      targetave_BU2016DHS_EDLITRMLIT = "% men aged 15-49 who are literate (DHS 2016) (target area average)",
                      targetave_BU2016DHS_EDLITRWLIT = "% women aged 15-49 who are literate (DHS 2016) (target area average)",
                      targetave_BU2016DHS_FPCUSMWMOD = "% currently married or in union women using modern contraception (DHS 2016) (target area average)",
                      targetave_BU2016DHS_FPNADMWPDM = "ratio of currently married women using FP to FP demand from same group (DHS 2016) (target area average)",
                      targetave_BU2016DHS_FPNADMWUNT = "% currently married or in union women with an unmet need for FP (DHS 2016) (target area average)",
                      targetave_BU2016DHS_MLITNAPACC = "% of HH population sleeping under an ITN if ITN can be used by <=2 people (DHS 2016) (target area average)",
                      targetave_BU2016DHS_RHANCNWN4P = "% women had live birth in last 5 years who had 4+ antenatal care visits (DHS 2016) (target area average)",
                      targetave_BU2016DHS_RHDELPCDHF = "% live births in the 5 years preceding DHS 2016 delivered at a health facility (DHS 2016) (target area average)",
                      targetave_BU2016DHS_WSSRCEPIMP = "% of population living in HHs with improved main source of drinking water (DHS 2016) (target area average)",
                      targetave_BU2016DHS_WSTLETPNFC = "% of population living in HHs with main type of toilet is no facility (DHS 2016) (target area average)",
                      targetave_normal_ntl_mean2015 = "2015 Average Night Luminosity (all areas unfiltered) (target area average)",
                      targetave_normal_ntl_mean2016 = "2016 Average Night Luminosity (all areas unfiltered) (target area average)",
                      targetave_normal_ntl_mean2017 = "2017 Average Night Luminosity (all areas unfiltered) (target area average)",
                      targetave_normal_ntl_mean2018 = "2018 Average Night Luminosity (all areas unfiltered) (target area average)",
                      targetave_normal_ntl_mean2019 = "2019 Average Night Luminosity (all areas unfiltered) (target area average)",
                      targetave_normal_ntl_mean2020 = "2020 Average Night Luminosity (all areas unfiltered) (target area average)",
                      lnrpc_tot_cons = "Log Real Per Capita consumption",
                      rpc_tot_cons = "Real Per Capita Consumption",
                      BDI001 = "Bubanza Dummy",
                      BDI002 = "Bujumbura Rural Dummy",
                      BDI003 = "Bururi Dummy",
                      BDI004 = "Cankuzo Dummy",
                      BDI005 = "Cibitoke Dummy",
                      BDI006 = "Gitega Dummy",
                      BDI007 = "Karuzi Dummy",
                      BDI008 = "Kayanza Dummy",
                      BDI009 = "Kirundo Dummy",
                      BDI010 = "Makamba Dummy",
                      BDI011 = "Muramvya Dummy",
                      BDI012 = "Muyinga Dummy",
                      BDI013 = "Mwaro Dummy",
                      BDI014 = "Ngozi Dummy",
                      BDI015 = "Rutana Dummy",
                      BDI016 = "Ruyigi Dummy",
                      BDI017 = "Bujumbura Mairie Dummy",
                      BDI018 = "Rumonge Dummy")

save.image("data-raw/all_modelestimation.RData")






















































































