library(targets)
library(tarchetypes)
library(geotargets)
library(crew)

tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages, 
               controller = crew::crew_controller_local(workers = 100))


# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

# targets ------

tar_plan(
  
  ## Builtup, Agriculture, and Human Modification ----
  
  #### HCAS 2025 ----
  
  tar_target(
    hcas_path,
    "/mnt/envshare/data/raster/hcas/HCAS33_AHC_VAST_2015_2024.tif",
    format = "file"
  ),
  
  #### GHM 2022 ----
  tar_target(
    ghm_tile_files,
    fs::dir_ls(path = "/mnt/envshare/data/raster/GEE_GlobalHumanModification_90m",
               glob = "*.tif", 
               recurse = TRUE),
    cue = tar_cue(mode = "always")
  ),
  
  tar_files(
    ghm_tile_paths,
    ghm_tile_files
  ),
  
  tar_target(
    ghm_tile_meta,
    tibble::tibble(
      path = ghm_tile_paths,
      theme = fs::path_file(fs::path_dir(ghm_tile_paths))
    ) %>%
      dplyr::filter(theme %in% c("AA", "BU", "AG"))
  ),
  
  tar_target(
    ghm_mosaic_paths,
    mosaic_by_theme(ghm_tile_meta, out_dir = "output/ghm_mosaics"),
    format = "file"
  ),
  
  ## Future Developmen Vectors ------
  
  #### SARIG ----
  
  tar_target(
    sarig_files,
    fs::dir_ls(path = "/mnt/envshare/data/vector/raw/sarig", recurse = TRUE, glob = "*.shp"),
    cue = tar_cue(mode = "always")  # re-lists files every run, cheap check
  ),
  
  tar_files(
    sarig_paths,
    sarig_files
  ),
  
  tar_target(
    sarig_tbl,
    tibble::tibble(
      sarig_path = sarig_paths,
      layer_name = fs::path_file(fs::path_dir(sarig_paths))) %>% 
      dplyr::mutate(short_name = rename_sarig_layer(layer_name))
  ),
  
  geotargets::tar_terra_vect(
    combined_sarig,
    {
      spatvectors <- purrr::map(sarig_tbl$sarig_path, \(x) terra::vect(x)) |>
        purrr::set_names(sarig_tbl$short_name)
      
      combine_sarig_vectors(
        shp_list = spatvectors,
        date_overrides = list(RenewAssociated_app = "APPLICATIO")
      )
    }
  ),
  
  ## PDI ----

  tar_target(
    pdi_path,
    "/mnt/envshare/data/vector/raw/portal/pdi.gpkg",
    format = "file"
  ),

  tar_target(
    pdi_renewable,
    terra::vect(pdi_path) %>% 
      tidyterra::filter(!applicationstatus %in% c("Withdrawn", "Cancelled")) %>% 
      extract_pdi_by_theme(flag_type = "renewable")
  )
)