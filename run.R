library(dplyr)
library(targets)
# packages --------
envFunc::check_packages(yaml::read_yaml("settings/packages.yaml") |> 
                          unlist() |> 
                          unname() |> 
                          unique(),
                        update_env = TRUE
)
# tars --------
## local ------
tars_local <- envTargets::make_tars(settings = envFunc::extract_scale("Status"),
                                    save_yaml = FALSE)

## Other projects ------
tars_cleaned <- envTargets::make_tars(settings = envFunc::extract_scale("envCleaned"),
                                      project_base = fs::path("..", "envCleaned"),
                                      local = FALSE)

tars_rec <- envTargets::make_tars(settings = envFunc::extract_scale("RecExtract"),
                                  project_base = fs::path("..", "RecExtract"),
                                  local = FALSE, 
                                  list_names = "store")

tars_sdm <- envTargets::make_tars(settings = envFunc::extract_scale("envSDMs"),
                                  project_base = fs::path("..", "envSDMs"),
                                  store_base = "/projects/projects_extra/out", # PROD
                                  local = FALSE)

tars_range <- envTargets::make_tars(settings = envFunc::extract_scale("envRange"),
                                    project_base = fs::path("..", "envRange"),
                                    local = FALSE)

tars <- c(tars_local, tars_cleaned, tars_rec, tars_sdm, tars_range)

envTargets::write_tars(tars)

# run everything ----------
purrr::walk2(purrr::map(tars_local, "script"),
             purrr::map(tars_local, "store"),
             \(x, y) targets::tar_make(script = x, store = y)
)
# prune everything ----------
purrr::walk2(purrr::map(tars_local, "script"),
             purrr::map(tars_local, "store"),
             \(x, y) targets::tar_prune(script = x, store = y)
)