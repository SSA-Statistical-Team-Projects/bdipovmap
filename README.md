
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Poverty Mapping in Burundi : Reproducability Guidelines

<!-- badges: start -->
<!-- badges: end -->

The goal of bdipovmap is to create a poverty map for Burundi using the
Small Area Estimation approach of the Fay Herriot Model (Fay and
Herriot, 1979). This ReadMe shows how to reproduce all the tables and
results in the paper. You can find the script that generates this readme
file as “README.Rmd” in this github working directory.

## System Set-Up

1)  Please clone the repository and open up the project within RStudio.
    Make sure you are in the project environment. In the top right
    corner you will see the “bdipovmap” beside the R logo to show you
    are in the project’s environment. This is extremely important.
    Otherwise you cannot automatically load the user defined functions.

2)  Please install the R `devtools` package, run
    `install.packages("devtools")`

3)  Open `README.Rmd` and go ahead and knit the Markdown file. This file
    is basically running the script that produces the poverty map.

Please see code and outputs below:

    #> ℹ Loading bdipovmap
    #> Spherical geometry (s2) switched off
    #> 
    #> Saving 7 x 5 in image
    #> although coordinates are longitude/latitude, st_intersects assumes that they
    #> are planar
    #> 
    #> although coordinates are longitude/latitude, st_intersects assumes that they
    #> are planar
    #> 
    #> Saving 7 x 5 in image
    #> although coordinates are longitude/latitude, st_intersects assumes that they
    #> are planar
    #> 
    #> although coordinates are longitude/latitude, st_intersects assumes that they
    #> are planar
    #> 
    #> although coordinates are longitude/latitude, st_intersects assumes that they
    #> are planar
    #> 
    #> although coordinates are longitude/latitude, st_intersects assumes that they
    #> are planar
    #> 
    #> Non-sampled domains exist.
    #> 
    #> Please note that the model selection criteria are only computed based on the in-sample domains.
    #> 
    #> Please note that the model selection criteria are only computed based on the in-sample domains.
    #> 
    #> Please note that the model selection criteria are only computed based on the in-sample domains.
    #> 
    #> b =1
    #> 
    #> 
    #> b =2
    #> 
    #> 
    #> b =3
    #> 
    #> 
    #> b =4
    #> 
    #> 
    #> b =5
    #> 
    #> 
    #> b =6
    #> 
    #> 
    #> b =7
    #> 
    #> 
    #> b =8
    #> 
    #> 
    #> b =9
    #> 
    #> 
    #> b =10
    #> 
    #> 
    #> b =11
    #> 
    #> 
    #> b =12
    #> 
    #> 
    #> b =13
    #> 
    #> 
    #> b =14
    #> 
    #> 
    #> b =15
    #> 
    #> 
    #> b =16
    #> 
    #> 
    #> b =17
    #> 
    #> 
    #> b =18
    #> 
    #> 
    #> b =19
    #> 
    #> 
    #> b =20
    #> 
    #> 
    #> b =21
    #> 
    #> 
    #> b =22
    #> 
    #> 
    #> b =23
    #> 
    #> 
    #> b =24
    #> 
    #> 
    #> b =25
    #> 
    #> 
    #> b =26
    #> 
    #> 
    #> b =27
    #> 
    #> 
    #> b =28
    #> 
    #> 
    #> b =29
    #> 
    #> 
    #> b =30
    #> 
    #> 
    #> b =31
    #> 
    #> 
    #> b =32
    #> 
    #> 
    #> b =33
    #> 
    #> 
    #> b =34
    #> 
    #> 
    #> b =35
    #> 
    #> 
    #> b =36
    #> 
    #> 
    #> b =37
    #> 
    #> 
    #> b =38
    #> 
    #> 
    #> b =39
    #> 
    #> 
    #> b =40
    #> 
    #> 
    #> b =41
    #> 
    #> 
    #> b =42
    #> 
    #> 
    #> b =43
    #> 
    #> 
    #> b =44
    #> 
    #> 
    #> b =45
    #> 
    #> 
    #> b =46
    #> 
    #> 
    #> b =47
    #> 
    #> 
    #> b =48
    #> 
    #> 
    #> b =49
    #> 
    #> 
    #> b =50
    #> 
    #> 
    #> Saving 7 x 5 in image
    #> Saving 7 x 5 in image

4)  Please see the results in the paper below:

### Table 1: Basic Descriptive Statistics

``` r

descriptives_dt <- 
  data.table(tot_pop_million = round(sum(grid_dt$bdi_ppp_2020_UNadj_constrained, na.rm = TRUE)/1e6, 1),
             pop_hhs_million = round(sum(geosurvey_dt$weight, na.rm = TRUE)/1e6, 1),
             svy_hhs = nrow(geosurvey_dt),
             povrate_ipl = 
               geosurvey_dt %>%
               mutate(ifelse(rpc_tot_cons < pline_int_215, 1, 0)) %>%
               summarise(povrate_ipl = weighted.mean(x = poor,
                                                     w = weight * hh_size,
                                                     na.rm = TRUE)) %>%
               round(4),
             recent_census = 2008 %>% as.integer(),
             region_count_sample = length(unique(geosurvey_dt$admin1Pcod[!is.na(geosurvey_dt$admin1Pcod)])),
             median_cv_region = 
               directpov_dt %>%
               summarize(median = median(directpov_dt$CV/100, na.rm = TRUE)),
             mean_cv_region = 
               directpov_dt %>%
               summarize(mean = weighted.mean(x = directpov_dt$CV/100, 
                                              w = directpov_dt$SampSize,
                                              na.rm = TRUE)),
             num_targets_pop = length(unique(grid_dt$admin2Pcod[!is.na(grid_dt$admin2Pcod)])),
             num_targets_sample = length(unique(geosurvey_dt$admin2Pcod[!is.na(geosurvey_dt$admin2Pcod)]))) %>%
  t() %>%
  kable()


descriptives_dt
```

|                         |              |
|:------------------------|-------------:|
| tot_pop_million         |   11.9000000 |
| pop_hhs_million         |    2.6000000 |
| svy_hhs                 | 8358.0000000 |
| povrate_ipl.povrate_ipl |    0.6220000 |
| recent_census           | 2008.0000000 |
| region_count_sample     |   18.0000000 |
| median_cv_region.median |    0.0817298 |
| mean_cv_region.mean     |    0.0849985 |
| num_targets_pop         |  119.0000000 |
| num_targets_sample      |  118.0000000 |

### FH Model Results and Diagnostics

This contains the results seen in Tables 3 & 4

``` r

summary(fhmodel_arcsin)
#> Call:
#>  povmap::fh(fixed = as.formula(paste("Direct ~ ", paste(selvars_list, 
#>     collapse = "+"))), vardir = "var", combined_data = combine_dt, 
#>     domains = "targetarea_codes", method = "ml", transformation = "arcsin", 
#>     backtransformation = "bc", eff_smpsize = "ess", MSE = TRUE, 
#>     mse_type = "boot")
#> 
#> Out-of-sample domains:  1 
#> In-sample domains:  118 
#> 
#> Variance and MSE estimation:
#> Variance estimation method:  ml 
#> Estimated variance component(s):  0.006009158 
#> MSE method:  bootstrap 
#> 
#> Coefficients:
#>                                coefficients std.error t.value
#> (Intercept)                        1.681776  0.192613  8.7314
#> targetave_ntl_small_stl_2015      -0.060725  0.055422 -1.0957
#> targetave_2018_Snow_Ice           -0.519717  0.090189 -5.7625
#> targetave_BU2016DHS_EDLITRMLIT    -0.928747  0.248293 -3.7405
#> BDI015                             0.209452  0.047988  4.3647
#> BDI016                             0.187966  0.062067  3.0284
#>                                              p.value    
#> (Intercept)                    < 0.00000000000000022 ***
#> targetave_ntl_small_stl_2015               0.2732172    
#> targetave_2018_Snow_Ice               0.000000008286 ***
#> targetave_BU2016DHS_EDLITRMLIT             0.0001836 ***
#> BDI015                                0.000012729146 ***
#> BDI016                                     0.0024584 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Explanatory measures:
#>    loglike       AIC       BIC    AdjR2     FH_R2
#> 1 78.62755 -143.2551 -123.8603 0.490617 0.6859841
#> 
#> Residual diagnostics:
#>                         Skewness Kurtosis Shapiro_W Shapiro_p
#> Standardized_Residuals -0.243928 2.908918 0.9840939 0.1785206
#> Random_effects          0.245214 3.683454 0.9826130 0.1312978
#> 
#> Transformation:
#>  Transformation Back_transformation
#>          arcsin                  bc
```

### Residual Plots and Q-Q plots

This contains the figures 1 in the paper and figure 4 in the appendix

``` r

plot(fhmodel_arcsin)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

    #> Press [enter] to continue

<img src="man/figures/README-unnamed-chunk-5-2.png" width="100%" />

    #> Press [enter] to continue

<img src="man/figures/README-unnamed-chunk-5-3.png" width="100%" />

### Poverty Maps

``` r

communeshp_dt %>%
  ggplot() +
  geom_sf(aes(fill = FH)) +
  scale_fill_viridis(option = "H") +
  theme_bw() +
  labs(fill = "Poverty Rate")
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

``` r


communeshp_dt %>%
  ggplot() +
  geom_sf(aes(fill = FH * population)) +
  scale_fill_viridis(option = "H") +
  theme_bw() +
  labs(fill = "Population \nof Poor")
```

<img src="man/figures/README-unnamed-chunk-6-2.png" width="100%" />

### Comparing the Estimated and Direct Poverty Rates at Province Level

This is the figure 2 in the paper and the table 5 in the appendix

``` r

provpov_dt %>%
  ggplot(aes(x = admin1Name)) +
  geom_point(aes(y = provFH), color = "blue", size = 2) +  # Plotting provFH
  geom_errorbar(aes(ymin = DirectLB, ymax = DirectUB), width = 0.2, color = "red") +  # Error bars for DirectLB and DirectUB
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Adding a horizontal line at y = 0
  labs(x = "Province", y = "Poverty Rate") +  # Labeling axes
  theme_bw() +  # Setting a white background theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotating x-axis labels for better readability
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

``` r

provpov_dt[, c("admin1Name","Direct", "DirectLB", "DirectUB")] %>%
  kable()
```

| admin1Name       |    Direct |  DirectLB |  DirectUB |
|:-----------------|----------:|----------:|----------:|
| Bubanza          | 0.7120498 | 0.6146754 | 0.8094243 |
| Bujumbura Rural  | 0.5734744 | 0.4897737 | 0.6571752 |
| Bururi           | 0.5942636 | 0.4702965 | 0.7182306 |
| Cankuzo          | 0.6271910 | 0.5189706 | 0.7354113 |
| Cibitoke         | 0.5935719 | 0.5192082 | 0.6679357 |
| Gitega           | 0.5227880 | 0.4601838 | 0.5853921 |
| Karuzi           | 0.7532644 | 0.6474042 | 0.8591246 |
| Kayanza          | 0.5267290 | 0.4382899 | 0.6151681 |
| Kirundo          | 0.7357866 | 0.6309456 | 0.8406277 |
| Makamba          | 0.6189213 | 0.4864399 | 0.7514027 |
| Muramvya         | 0.7899189 | 0.6694732 | 0.9103645 |
| Muyinga          | 0.7092091 | 0.6015106 | 0.8169077 |
| Mwaro            | 0.6070329 | 0.5034811 | 0.7105847 |
| Ngozi            | 0.6431201 | 0.5546512 | 0.7315890 |
| Rutana           | 0.8419041 | 0.6961965 | 0.9876116 |
| Ruyigi           | 0.8205140 | 0.6808899 | 0.9601382 |
| Bujumbura Mairie | 0.1357966 | 0.1032138 | 0.1683795 |
| Rumonge          | 0.6133018 | 0.5088032 | 0.7178005 |
