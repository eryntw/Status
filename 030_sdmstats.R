library(targets)
library(tarchetypes)
library(geotargets)
library(crew)

use_cores <- parallel::detectCores() - 2
tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages, 
               controller = crew::crew_controller_local(workers = use_cores))


# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

# targets -------

thresh_meta <- tar_read(thresh_meta, store = tars$setup$store)
capad2024_file <- tar_read(capad2024_file, store = tars$setup$store)

tar_plan(
  
  ## Protected area merge ----
  
  tar_target(
    capad_merged_path,
    {
      capad <- terra::vect(capad2024_file) %>% terra::aggregate(dissolve = TRUE)
      out_path <- "output/capad_merged.gpkg"
      fs::dir_create(fs::path_dir(out_path))
      terra::writeVector(capad, out_path, overwrite = TRUE)
      out_path
    },
    format = "file"
  ),
  
  tar_target(
    thresh_meta_grouped,
    thresh_meta |>
      dplyr::group_by(dplyr::row_number()) |>
      targets::tar_group(),
    iteration = "group"
  ),
  
  ## SDM stats (branch) ----
  
  # tar_target(
  #   sdm_calculation,
  #   {
  #     capad <- terra::vect(capad_merged_path)
  #     summarise_sdm(thresh_meta_grouped$path, capad_merged = capad)
  #   },
  #   pattern = map(thresh_meta_grouped),
  #   iteration = "list"
  # ),
  
  ## Merge stats ----
  
  # tar_target(
  #   sdm_calculation_all,
  #   dplyr::bind_rows(sdm_calculation)
  # ),

)