################################################################################
########### COMPUTING POST ESTIMATION RESULTS FOR THE UNIT LEVEL MODEL #########
################################################################################
pacman::p_load(povmap, data.table, dplyr, sf, ggplot2, viridis)

##### read in the data

load("data-raw/all_modelestimation.RData")

### compute overall poverty from model and estimates

geosurvey_dt %>%
  mutate(poor = ifelse(rpc_tot_cons < pline_int_215, 1, 0)) %>%
  summarise(weighted.mean(x = poor,
                          w = weight * hh_size,
                          na.rm = TRUE))

povest_dt <- as.data.table(unit_model$ind)

pop_dt <- grid_dt[,sum(bdi_ppp_2020_UNadj_constrained, na.rm = TRUE), by = targetarea_codes]

povest_dt <-
  povest_dt %>%
  mutate(Domain = as.integer(as.character(Domain))) %>%
  merge(pop_dt, by.x = "Domain", by.y = "targetarea_codes")


povest_dt %>%
  setnames(old = "V1", new = "population")

povest_dt %>%
  summarize(weighted.mean(x = Head_Count,
                          w = population))


### compute the basic statistics tables

###### include admin1Name in grid_dt and geosurvey_dt
geosurvey_dt <-
  geosurvey_dt %>%
  merge(shp_dt[, c("admin1Name", "admin1Pcod")] %>%
          st_drop_geometry())

# setnames(grid_dt,
#          old = c("admin0Pcod_x", "admin1Pcod_x", "admin2Name_x"),
#          new = c("admin0Pcod", "admin1Pcod", "admin2Name"))

grid_dt[,c("admin0Pcod_y", "admin1Pcod_y", "admin2Name_y") := NULL]

grid_dt <-
  grid_dt %>%
  merge(shp_dt[, c("admin1Name", "admin1Pcod")] %>%
          st_drop_geometry(),
        by = "admin1Pcod")


unit_model_dt <-
  na.omit(geosurvey_dt[,c("rpc_tot_cons", stata_vars,
                          "hhweight", "hhid", "admin1Name"), with = FALSE])


direct_dt <- povmap::direct(y = "rpc_tot_cons",
                            smp_data =   na.omit(geosurvey_dt[,c("rpc_tot_cons", stata_vars,
                                                                 "hhweight", "hhid", "admin1Name",
                                                                 "targetarea_codes"),
                                                              with = FALSE]),
                            smp_domains = "targetarea_codes",
                            weights = "hhweight",
                            var = TRUE,
                            threshold = 10381.14)

saveRDS(direct_dt, "figures/post-est-tables/direct_estimates.RDS")

descriptives_dt <-
  ebp_reportdescriptives(model = unit_model,
                         direct = direct_dt,
                         smp_data = na.omit(geosurvey_dt[,c("rpc_tot_cons", stata_vars,
                                                            "hhweight", "hhid", "admin1Name",
                                                            "targetarea_codes"),
                                                         with = FALSE]),
                         weights = "hhweight",
                         pop_weights = "targetarea_codes",
                         CV_level = "admin1Name",
                         pop_data = grid_dt,
                         pop_domains = "targetarea_codes",
                         threshold = 10381.14)


### just modify the survey poverty rate
descriptives_dt$poverty_df$survey[[1]] <-
  geosurvey_dt %>%
  mutate(poor = ifelse(rpc_tot_cons < pline_int_215, 1, 0)) %>%
  summarise(weighted.mean(x = poor,
                          w = weight * hh_size,
                          na.rm = TRUE))

saveRDS(descriptives_dt, "figures/post-est-tables/descriptives_tables.RDS")


##### compare the means between survey and census prior to model estimation
checkvariables_dt <-
ebp_test_means(varlist = stata_vars,
               pop_data = grid_dt,
               smp_data = geosurvey_dt,
               weights = "hhweight")

write.csv(checkvariables_dt,
          "figures/post-est-tables/compare_samplecensus_means.csv")

#### model estimations
ebp_modelresults_dt <-
  ebp_reportcoef_table(unit_model, 4)

write.csv(ebp_modelresults_dt,
          "figures/post-est-tables/ebp_model_estimates.csv")


cv_dt <- ebp_compute_cv(model = unit_model,
                        designvar = NULL,
                        threshold = 10381.14)

write.csv(cv_dt, "figures/post-est-tables/targetarea_cvtable.csv")

#### compute the post estimation diagnostics

plot(unit_model)



#### show and save the poverty maps
admin2_dt <- readRDS("data-clean/bdi_gridded_v02.rds")

admin2_dt <- admin2_dt$admin2_Communes

povest_dt <-
  admin2_dt %>%
  merge(unique(grid_dt[,c("admin2Pcod", "targetarea_codes")])) %>%
  merge(povest_dt, by.x = "targetarea_codes", by.y = "Domain")


povest_dt %>%
  ggplot() +
  geom_sf(aes(fill = Head_Count)) +
  scale_fill_viridis(option = "H") +
  theme_bw() +
  labs(fill = "Poverty Rate")

ggsave("figures/poverty_map.png")

povest_dt %>%
  ggplot() +
  geom_sf(aes(fill = Head_Count * population)) +
  scale_fill_viridis(option = "H") +
  theme_bw() +
  labs(fill = "Population \nof Poor")

ggsave("figures/poverty_map_count.png")



#### compare CV for model estimate vs direct estimates (HT)

cv_dt %>%
  ggplot() +
  geom_point(aes(x = HT_Head_Count_CV,
                 y = EBP_Head_Count_CV)) +
  theme_bw() +
  xlim(c(0, 0.6)) +
  ylim(c(0, 0.6)) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed",
              color = "red")

ggsave("figures/compare_direct_ebp_cv.png")


#### compare census based estimates to geopatial estimates at the admin1 level
povadmin1_dt <-
  povest_dt %>%
  st_drop_geometry() %>%
  group_by(admin1Pcod) %>%
  summarize(ebp_headcount = weighted.mean(x = Head_Count, w = population))


geosurvey_dt[, poor := ifelse(rpc_tot_cons < 10381.14, 1, 0)]

povadmin1_dt <-
  povadmin1_dt %>%
  merge(geosurvey_dt[, weighted.mean(x = poor, w = hhweight, na.rm = TRUE), by = "admin1Pcod"])

setnames(povadmin1_dt, "V1", "survey_headcount")

### merge in the area names
povadmin1_dt <-
  povadmin1_dt %>%
  merge(unique(geosurvey_dt[, c("admin1Pcod", "admin1Name")]))

### include sample size and population values
povadmin1_dt <-
  povadmin1_dt %>%
  merge(geosurvey_dt[, sum(hhweight, na.rm = TRUE), by = "admin1Pcod"]) %>%
  setnames(old = "V1", new = "survey_population")

povadmin1_dt <-
  povadmin1_dt %>%
  merge(grid_dt[, sum(bdi_ppp_2020_UNadj_constrained, na.rm = TRUE), by = "admin1Pcod"]) %>%
  setnames(old = "V1", new = "grid_population")

povadmin1_dt <-
  povadmin1_dt %>%
  merge(geosurvey_dt[,length(hhid), by = "admin1Pcod"]) %>%
  setnames(old = "V1", new = "sample_size")

povadmin1_dt <-
  povadmin1_dt %>%
  mutate(bmrate = ebp_headcount / survey_headcount)

## average benchmark rate
povadmin1_dt %>%
  summarise(weighted.mean(x = bmrate, w = survey_population))

### plot the comparison of census and survey estimates

povadmin1_dt %>%
  ggplot() +
  geom_point(aes(x = survey_headcount,
                 y = ebp_headcount)) +
  xlim(c(0, 0.9)) +
  ylim(c(0, 0.9)) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed",
              color = "red") +
  theme_bw() +
  xlab("EBP Model Estiamtes") +
  ylab("Direct Estimates")

ggsave("figures/compare_admin1_surveycensus_headcount.png")

write.csv(povadmin1_dt, "figures/post-est-tables/admin1_poverty_censusvssurvey.csv")



#### save a few other things for posterity

saveRDS(geosurvey_dt, "data-clean/model-data/fullgeosurvey.RDS")
saveRDS(grid_dt, "data-clean/model-data/fullgeocensus.RDS")
saveRDS(unit_model, "data-clean/model-data/ebp_unitmodel_results.RDS")


#### create the regression table

ebp_normalityfit(unit_model)



#### open cv_dt and add in the names of the areas
cv_dt <- read.csv("figures/post-est-tables/targetarea_cvtable.csv")

admin2_dt$targetarea_codes <- as.integer(substr(admin2_dt$admin2Pcod,
                                                4,
                                                nchar(admin2_dt$admin2Pcod)))

cv_dt <-
  cv_dt %>%
  merge(admin2_dt[, c("targetarea_codes", "admin2Name")],
        by.x = "Domain", by.y = "targetarea_codes")

write.csv(cv_dt, "figures/post-est-tables/targetarea_cvtable2.csv")



#### plot ntl tesselation for gitega and bujumbura
grid_dt %>%
  as.data.frame() %>%
  st_as_sf(crs = 32735) %>%
  filter(admin1Pcod == "BDI006") %>%
  ggplot() +
  geom_sf(aes(fill = pop_density_2015)) +
  scale_fill_viridis(option = "C") +
  theme_bw() +
  labs(fill = "2015 Population Density \n in Gitega")

ggsave("figures/2015populationdensity_gitega.png")

grid_dt %>%
  as.data.frame() %>%
  st_as_sf(crs = 32735) %>%
  filter(admin1Pcod == "BDI017") %>%
  ggplot() +
  geom_sf(aes(fill = prplit_conf90_sy_2019)) +
  scale_fill_viridis(option = "C") +
  theme_bw() +
  labs(fill = "2015 Population Density \n in Bujumbura Mairie")

ggsave("figures/2015populationdensity_bujumburamairie.png")

grid_dt %>%
  as.data.frame() %>%
  st_as_sf(crs = 32735) %>%
  filter(admin1Pcod == "BDI002") %>%
  ggplot() +
  geom_sf(aes(fill = prplit_conf90_sy_2019)) +
  scale_fill_viridis(option = "C") +
  theme_bw() +
  labs(fill = "2015 Population Density \n in Bujumbura Rural")

ggsave("figures/2015populationdensity_bujumburarural.png")

























