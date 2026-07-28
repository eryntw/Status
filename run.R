library(dplyr)
library(targets)

# packages --------
envFunc::check_packages(yaml::read_yaml("settings/packages.yaml") |> 
                          unlist() |> 
                          unname() |> 
                          unique()
                        , update_env = TRUE)

# tars --------

tars_local <- envTargets::make_tars(settings = envFunc::extract_scale()
                                    , save_yaml = FALSE)

tars_cleaned <- envTargets::make_tars(settings = envFunc::extract_scale("envCleaned")
                                      , project_base = fs::path("..", "envCleaned")
                                      , local = FALSE)

tars_sdm <- envTargets::make_tars(settings = envFunc::extract_scale("envSDMs")
                                  , project_base = fs::path("..", "envSDMs")
                                  , local = FALSE)

tars_range <- envTargets::make_tars(settings = envFunc::extract_scale("envRange")
                                    , project_base = fs::path("..", "envRange")
                                    , local = FALSE)

tars_eco <- envTargets::make_tars(settings = envFunc::extract_scale("envEco")
                                  , project_base = fs::path("..", "envEco")
                                  , list_names = c("extent", "grain", "aoi")
                                  , local = FALSE)

tars <- c(tars_local, tars_cleaned, tars_range, tars_sdm, tars_eco)
envTargets::write_tars(tars)


# run everything ----------
# in _targets.yaml
purrr::walk2(purrr::map(tars_local, "script")
             , purrr::map(tars_local, "store")
             , \(x, y) targets::tar_make(script = x, store = y)
)

# prune everything ----------
purrr::walk2(
  purrr::map(tars_local, "script")
             , purrr::map(tars_local, "store")
             , \(x, y) targets::tar_prune(script = x, store = y)
)

# tar_load everything ----------
stores <- purrr::map(tars_local, "store")
purrr::iwalk(stores, function(store_path, store_name) {
  
  message("Loading store: ", store_name)
  
  objs <- targets::tar_objects(store = store_path)
  
  targets::tar_load(
    names = tidyselect::all_of(objs),
    store = store_path,
    envir = .GlobalEnv
  )
  
})