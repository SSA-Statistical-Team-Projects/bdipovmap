##### some helper functions

create_vars <- function(dt){

  ntl_all_cols <- c("ntl_all_2015", "ntl_all_2016", "ntl_all_2017",
                    "ntl_all_2018", "ntl_all_2019", "ntl_all_2020")

  dt[, ntl_all_cols] <- dt[, ntl_all_cols, with = F] + 1

  ntl_built_up_cols <- c("ntl_built_up_2015", "ntl_built_up_2016", "ntl_built_up_2017",
                         "ntl_built_up_2018", "ntl_built_up_2019", "ntl_built_up_2020")

  dt[, ntl_built_up_cols] <- dt[, ntl_built_up_cols, with = F] + 1

  ntl_small_stl_cols <- c("ntl_small_stl_2015", "ntl_small_stl_2016", "ntl_small_stl_2017",
                          "ntl_small_stl_2018", "ntl_small_stl_2019", "ntl_small_stl_2020")

  dt[, ntl_small_stl_cols] <- dt[, ntl_small_stl_cols, with = F] + 1

  ntl_hamlet_cols <- c("ntl_hamlet_2015", "ntl_hamlet_2016", "ntl_hamlet_2017",
                       "ntl_hamlet_2018", "ntl_hamlet_2019", "ntl_hamlet_2020")

  dt[, ntl_hamlet_cols] <- dt[, ntl_hamlet_cols, with = F] + 1

  ntl_cols <- c(ntl_all_cols, ntl_built_up_cols, ntl_small_stl_cols, ntl_hamlet_cols)

  dt[, ntl_cols] <- log(dt[, ntl_cols, with = F])

  dt[, ntl_all_growth := ntl_all_2020 - ntl_all_2015]
  dt[, ntl_built_up_growth := ntl_built_up_2020 - ntl_built_up_2015]
  dt[, ntl_small_stl_growth := ntl_small_stl_2020 - ntl_small_stl_2015]
  dt[, ntl_hamlet_growth := ntl_hamlet_2020 - ntl_hamlet_2015]


  ### growth in population density
  dt[, log_pop_density_growth := log(pop_density_2020 + 1) - log(pop_density_2015 + 1)]

  ### growth in each of the different landcover class types
  dt[, water_growth := `2020_Water` - `2017_Water`]
  dt[, trees_growth := `2020_Trees` - `2017_Trees`]
  dt[, floodedveg_growth := `2020_Flooded.vegetation` - `2017_Flooded.vegetation`]
  dt[, crops_growth := `2020_Crops` - `2017_Crops`]
  dt[, builtarea_growth := `2020_Built.Area` - `2017_Built.Area`]
  dt[, bareground_growth := `2020_Bare_ground` - `2017_Bare_ground`]
  dt[, snowice_growth := `2020_Snow.Ice` - `2017_Snow.Ice`]
  dt[, clouds_growth := `2020_Clouds` - `2017_Clouds`]
  dt[, rangeland_growth := `2020_Rangeland` - `2017_Rangeland`]

  return(dt)

}


#### model selection functions

lm_model_select <- function(dt,
                            y,
                            xvars,
                            weights = NULL){

  ## clean the data real quick
  if(is.null(weights) == FALSE){

    dt <- na.omit(dt[,c(y, xvars, weights), with = F])

    weights <- dt[, weights, with = F]

    #weights <- scale(weights)

  } else {

    dt <- na.omit(dt[,c(y, xvars), with = F])

    weights <- 1

  }

  xset <- dt[, xvars, with = F]

  y <- dt[, y, with = F]

  model <- lm(formula = formula,
              data = cbind(y, xset),
              weights = weights)

  return(model)


}



#' Dissolve boundaries that share borders or polygons that share area in groups
#'


st_dissolve <- function(x, ...){

  ## check for intersection
  int_dt <- st_intersects(x = x, y = x, ...)

  group_list <- igraph::graph_from_adj_list(int_dt)

  x[["group"]] <- components(group_list)$membership

  union_dt <-
    x %>%
    group_by(group) %>%
    summarise(geometry = st_union(geometry))

  return(union_dt)


}

st_parallel_dissolve <- function(shp_list,
                                 numCores,
                                 ...){


  numCores <- min(numCores, parallel::detectCores())

  doParallel::registerDoParallel(cores = numCores)

  parallelMap::parallelLibrary("foreach")
  parallelMap::parallelLibrary("sf")
  parallelMap::parallelLibrary("igraph")
  parallelMap::parallelLibrary("tidyverse")


  shp_list <-
    foreach(i = 1:numCores) %dopar% {

      pacman::p_load(sf, igraph, dplyr)

      ## check for intersection
      int_dt <- sf::st_intersects(x = shp_list[[i]], y = shp_list[[i]], ...)

      group_list <- igraph::graph_from_adj_list(int_dt)

      shp_list[[i]][["group"]] <- igraph::components(group_list)$membership

      shp_list[[i]] <-
        shp_list[[i]] %>%
        group_by(group) %>%
        summarise(geometry = st_union(geometry))


    }

  return(shp_list)


}





countrymodel_select_stata <- function(dt,
                                      xvars,
                                      y,
                                      weights,
                                      selection = "BIC",
                                      stata_path,
                                      stata_vnum,
                                      cluster_id,
                                      area_tag = NULL){

  dt <- as.data.table(dt)

  if(is.null(weights) == FALSE){

    dt <- na.omit(dt[,c(y, xvars, weights, cluster_id), with = F])

    weights <- dt[, weights, with = F]

    #weights <- scale(weights)

  } else {

    dt <- na.omit(dt[,c(y, xvars, cluster_id), with = F])

    weights <- 1

  }

  xset <- dt[, xvars, with = F]

  y <- dt[, y, with = F]

  df <- as.data.frame(dt)
  names(df) <- abbreviate(names(df), minlength = 32)

  x <- colnames(xset)

  haven::write_dta(data = df, path = "./data.dta")

  options("RStata.StataPath" = stata_path)
  options("RStata.StataVersion" = stata_vnum)

  if (is.null(area_tag)) {

    stata_src <- c("use data.dta, replace",
                   paste0("lassowrapper ",
                          colnames(y),
                          " ",
                          x[1],
                          "-",
                          x[length(x)],
                          ", weights(",
                          weights,
                          ") select(bic, postsel) cluster(",
                          cluster_id,
                          ") input(data.dta) output(model.txt)"))

  } else {

    stata_src <- c("use data.dta, replace",
                   paste0("lassowrapper ",
                          colnames(y),
                          " ",
                          x[1],
                          "-",
                          x[length(x)],
                          ", weights(",
                          weights,
                          ") force(",
                          paste0(area_tag, "*"),
                          ") select(bic, postsel) cluster(",
                          cluster_id,
                          ") input(data.dta) output(model.txt)"))

  }

  RStata::stata(src = stata_src)

  var_list <- readLines("model.txt")

  var_list <- unlist(strsplit(var_list, " "))

  return(var_list)


}


#shells to Stata to estimate lasso
countryareamodel_select_stata <- function(dt,
                                          xvars,
                                          y,
                                          selection = "BIC",
                                          stata_path,
                                          stata_vnum){

  dt <- as.data.table(dt)

  dt <- na.omit(dt[,c(y, xvars), with = F])

  xset <- dt[, xvars, with = F]

  y <- dt[, y, with = F]

  df <- as.data.frame(dt)
  names(df) <- abbreviate(names(df), minlength = 32)

  x <- colnames(xset)

  haven::write_dta(data = df, path = "./data.dta")

  options("RStata.StataPath" = stata_path)
  options("RStata.StataVersion" = stata_vnum)

  stata_src <- c("use data.dta, replace",
                 paste0("lassoareawrapper ", colnames(y), " ", x[1], "-",
                        x[length(x)],", force(areadummy*) select(bic, postsel) input(data.dta) output(model.txt)"))

  RStata::stata(src = stata_src)

  var_list <- readLines("model.txt")

  ### separate the long string by space

  var_list <- strsplit(var_list, " +")[[1]]

  var_list <- var_list[!(grepl("o.areadummy", var_list))]

  var_list <- stringr::str_replace_all(var_list, "covrfrctn", "coverfraction")
  var_list <- stringr::str_replace_all(var_list, "coverfrctn", "coverfraction")

  return(var_list)


}




dummify <- function(x) {
  if(is.matrix(x) || is.data.frame(x)) {
    x <- as.data.frame(x)
    y <- do.call(data.frame, lapply(x, dummify))
    return(as.matrix(y))
  }
  # x is 1-dimensional
  if(is.complex(x))
    return(as.matrix(data.frame(Re=Re(x), Im=Im(x))))
  # convert factors etc
  if(is.character(x))
    x <- factor(x)
  if(is.logical(x))
    x <- factor(x, levels=c(FALSE,TRUE))
  if(is.factor(x)) {
    # convert to dummy variables
    nx <- length(x)
    lev <- levels(x)
    y <- matrix(0L, nrow=nx, ncol=length(lev))
    colnames(y) <- lev
    y[cbind(seq_len(nx), as.integer(x))] <- 1L
    return(y)
  }
  # convert to numeric
  y <- as.numeric(x)
  if(!is.matrix(y))
    y <- matrix(y, ncol=1)
  return(y)
}





### function to convert raster to as.data.table object
as.data.table.raster <- function(x,
                                 row.names = NULL,
                                 optional = FALSE,
                                 xy = FALSE,
                                 inmem = raster::canProcessInMemory(x, 2),
                                 ...) {

  stopifnot(require("data.table"))
  if(inmem) {
    v <- as.data.table(as.data.frame(x, row.names=row.names, optional=optional, xy=xy, ...))
  } else {
    tr <- blockSize(x, n=2)
    l <- lapply(1:tr$n, function(i)
      as.data.table(as.data.frame(getValues(x,
                                            row=tr$row[i],
                                            nrows=tr$nrows[i]),
                                  row.names=row.names, optional=optional, xy=xy, ...)))
    v <- rbindlist(l)
  }
  coln <- names(x)
  if(xy) coln <- c("x", "y", coln)
  setnames(v, coln)

  return(v)

}



#' A function to perform st_join operations in parallel
#'
#'
#' @inheritParams st_join
#'
#' @import sf dplyr foreach doParallel

st_parallel_join <- function(x, y, numCores, splitvar,...){

  ### set up the parallel processing system
  # num_cores <- detectCores()
  #
  # cl <- makeCluster(numCores)
  #
  # registerDoParallel(cl)
  plan(multisession,
       workers = numCores,
       .export = c("st_join", "x", "...", "splitvar"))

  options(future.globals.onReference = "error")

  y[["splitvar"]] <- y[[splitvar]]

  ## split the data into multiple datasets based on the number of cores
  df_list <-
    y %>%
    group_by(splitvar) %>%
    group_split()

  # clusterEvalQ(cl = cl, expr = library(sf))
  #
  # result_dtlist <- foreach(i = 1:length(df_list)) %dopar% {
  #
  #   library(sf)
  #
  #   result_dt <- st_join(x = x,
  #                        y = df_list[[i]],
  #                        ...)
  #
  #   gc()
  #
  # }

  result_dtlist <- future_map(df_list,
                              ~ st_join(x = x,
                                        y = .,
                                        ...))


  return(result_dtlist)


}



parallel_extract <- function(shp_dt,
                             raster_list,
                             fun_list,
                             numCores){

  ##initiating the parallelization process
  numCores <- min(numCores, parallel::detectCores()) ##use the minimum of the specified processors or the max

  doParallel::registerDoParallel(cores = numCores) ##initiate the number of cores to be used
  parallelMap::parallelLibrary("foreach") ##loading the parallel looping library
  parallelMap::parallelLibrary("exactextractr") ##loading the parallel looping library

  ##the parallelization process
  grid_list <-
    foreach (i = 1:length(raster_list), .combine = "cbind") %dopar% {

      exactextractr::exact_extract(x = raster_list[[i]],
                                   y = shp_dt,
                                   fun = fun_list[i])

    }


  return(grid_list)

}


##### a function to check the relationship between specific variables and outcome variable

plot_xy_relationship <- function(xvars, y, data) {

  data <- as.data.table(data)
  data <- na.omit(data[, c(xvars, y), with = FALSE])

  plotlist <- lapply(xvars, function(x) {

    p <- ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
      geom_point() +
      geom_smooth(method = "auto", se = FALSE)

    return(p)
  })

  print(arrangeGrob(grobs = plotlist, nrow = round(length(plotlist)^0.5)))
}

find_optlambda <- function(lambda,
                           dt,
                           cand_vars,
                           delta_start,
                           q_start) {

  tryCatch(
    {
      glm1 <- glmmLasso(
        as.formula(paste("lnrpc_tot_cons ~ ", paste(cand_vars, collapse = "+"))),
        rnd = list(targetarea_codes = ~1),
        family = gaussian(link = "identity"),
        data = na.omit(dt),
        lambda = lambda,
        switch.NR = TRUE,
        final.re = TRUE,
        control = list(start = delta_start, q_start = q_start)
      )
      return(glm1$bic)
    },
    error = function(e) Inf
  )

}




























