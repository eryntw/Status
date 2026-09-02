library(targets)
library(dplyr)
library(tarchetypes)
library(crew)

use_cores <- 10 # Too many workers will crash edge calculation
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
  
  ## SDM test set ----
  tar_target(
    thresh_meta_test,
    thresh_meta_grouped |> dplyr::slice_sample(n = 10)
  ),
  
  ## Protected area merge ----
  
  tar_target(
    capad_merged_path,
    "/mnt/envshare/data/vector/raw/dcceew/capad_2024/capad_merged.gpkg",
    format = "file"
  ),
  
  tar_target(
    thresh_meta_grouped,
    thresh_meta |> 
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
  
  ## SDM in protected area (branch) ----
  
  tar_target(
    sdm_calculation,
    {
      terra::terraOptions(memfrac = use_memfrac)
      capad <- terra::vect(capad_merged_path)
      summarise_sdm(thresh_meta_grouped$path, 
                    capad_merged = capad)
    },
    pattern = map(thresh_meta_grouped),
    iteration = "list",
    garbage_collection = TRUE
  ),
  
  ## SDM edge (branch) ----
  tar_target(
    edge_calculation,
    {
      terra::terraOptions(memfrac = use_memfrac)
      summarise_edge(thresh_meta_grouped$path, 
                     target_crs = "EPSG:7853", # MGA zone 53
                     min_patch_ha = 36,
                     directions = 8)
    },
    pattern = map(thresh_meta_grouped),
    iteration = "list",
    garbage_collection = TRUE
  ),
  
  ## Merge stats ----
  
  tar_target(
    sdm_summary,
    thresh_meta |> 
      dplyr::inner_join(dplyr::bind_rows(sdm_calculation), 
                        by = "path") |> 
      dplyr::inner_join(dplyr::bind_rows(edge_calculation),
                        by = c("search_term" = "species"))
  ),
)