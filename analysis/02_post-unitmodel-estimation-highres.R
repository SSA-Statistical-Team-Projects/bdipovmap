################################################################################
########### COMPUTING POST ESTIMATION RESULTS FOR THE UNIT LEVEL MODEL #########
################################################################################
pacman::p_load(povmap, data.table, dplyr, sf, ggplot2, viridis)

##### read in the data
##load("//esapov/esapov/BDI/GEO/Population/unitmodel_images.RData")
load("data-raw/unitmodel_images.RData")


### check the distribution of log welfare within the model
geosurvey_dt %>%
  ggplot() +
  geom_histogram(aes(log(welfare)), binwidth = 0.1)


#### quick comparison of predict and household welfare
predictest_list <-
  lapply(X = list(admin4_model3, admin3_model3, admin2_model3,
               admin4_model3_boxcox, admin3_model3_boxcox, admin2_model3_boxcox,
               admin4_model3_ordernorm, admin3_model3_ordernorm, admin2_model3_ordernorm),
         FUN = function(x){

           results <-
             compute_ebpnat_pov(ebp_obj = x,
                                pop_dt = grid_dt[,c("wpop_population", "admin4Pcod")],
                                pop_weights = "wpop_population",
                                pop_domains = "admin4Pcod")

           return(results)

         })


### compute basic statistics tables
admin4_direct_dt <-
  povmap::direct(y = "welfare",
                 smp_data =   na.omit(geosurvey_dt[,c("welfare", admin3selvars_list,
                                                      "hhweight", "hhid", "admin1Name",
                                                      "admin4Pcod"),
                                                   with = FALSE]),
                 smp_domains = "admin4Pcod",
                 weights = "hhweight",
                 var = TRUE,
                 threshold = 11111.76)

admin3_direct_dt <-
  povmap::direct(y = "welfare",
                 smp_data =   na.omit(geosurvey_dt[,c("welfare", admin3selvars_list,
                                                      "hhweight", "hhid", "admin1Name",
                                                      "admin3Pcod"),
                                                   with = FALSE]),
                 smp_domains = "admin3Pcod",
                 weights = "hhweight",
                 var = TRUE,
                 threshold = 11111.76)

admin2_direct_dt <-
  povmap::direct(y = "welfare",
                 smp_data =   na.omit(geosurvey_dt[,c("welfare", admin3selvars_list,
                                                      "hhweight", "hhid", "admin1Name",
                                                      "admin2Pcod"),
                                                   with = FALSE]),
                 smp_domains = "admin2Pcod",
                 weights = "hhweight",
                 var = TRUE,
                 threshold = 11111.76)


### save direct objects
saveRDS(admin4_direct_dt, "data-clean/ebp_results/direct_objects/admin4_direct_estimtes.RDS")
saveRDS(admin3_direct_dt, "data-clean/ebp_results/direct_objects/admin3_direct_estimtes.RDS")
saveRDS(admin2_direct_dt, "data-clean/ebp_results/direct_objects/admin2_direct_estimtes.RDS")


admin4_descriptives_dt <-
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










