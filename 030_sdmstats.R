library(targets)
library(dplyr)
library(tarchetypes)
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
capad2024_path <- tar_read(capad2024_path, store = tars$setup$store)

# terra RAM safety -------
total_terra_ram_prop <- 0.6  # across all cores. Don't go above 0.6!
terra_memfrac <- total_terra_ram_prop / use_cores

tar_plan(
  
  ## Protected area merge ----
  
  tar_target(
    capad_merged_path,
    {
      capad <- terra::vect(capad2024_path) |> terra::aggregate(dissolve = TRUE)
      out_path <- "output/capad_merged.gpkg"
      fs::dir_create(fs::path_dir(out_path))
      terra::writeVector(capad, out_path, overwrite = TRUE)
      out_path
    },
    format = "file"
  ),
  
  tar_target(
    thresh_meta_grouped,
    thresh_meta |> head(3) |> ## test with three SDMs
      dplyr::group_by(dplyr::row_number()) |>
      targets::tar_group(),
    iteration = "group"
  ),
  
  ## Adaptive memfrac — accounts for fewer branches/species than workers ----
  
  tar_target(
    use_memfrac,
    if (dplyr::n_distinct(thresh_meta_grouped$tar_group) >= use_cores) {
      terra_memfrac
    } else {
      total_terra_ram_prop / dplyr::n_distinct(thresh_meta_grouped$tar_group)
    }
  ),
  
  ## SDM stats (branch) ----
  
  tar_target(
    sdm_calculation,
    {
      terra::terraOptions(memfrac = use_memfrac)
      capad <- terra::vect(capad_merged_path)
      summarise_sdm(thresh_meta_grouped$path, capad_merged = capad)
    },
    pattern = map(thresh_meta_grouped),
    iteration = "list",
    garbage_collection = TRUE
  ),
  
  ## Merge stats ----
  
  tar_target(
    sdm_summary,
    dplyr::bind_rows(sdm_calculation) |> 
      dplyr::inner_join(thresh_meta, by = "path")
  ),
)