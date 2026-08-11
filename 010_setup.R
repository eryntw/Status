library(targets)
library(tarchetypes)
library(geotargets)
library(crew)

tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages, 
               controller = crew::crew_controller_local(workers = 50))


# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

# targets -------

tar_plan(
  
  #### Species SDM ----
  
  ## Get state SDM binary tif paths ------
  
  # Always re-list to catch new/removed species or dates
  tar_target(
    thresh_tif_paths,
    fs::dir_ls(
      path    = fs::path(tars$envSDMs$sdm$store, "sdm_fine"),
      regexp  = "__thresh__\\d{4}-\\d{2}-\\d{2}\\.tif$",
      recurse = TRUE
    ),
    cue = tar_cue(mode = "always")
  ),
  
  # Each path becomes its own hash-tracked sub-target
  tar_files(thresh_tif_files, thresh_tif_paths),
  
  # Metadata table — species/date parsed per file, branched dynamically
  tar_target(
    thresh_meta,
    tibble::tibble(
      path = thresh_tif_files,
      search_term = stringr::str_extract(thresh_tif_files, 
                                         "(?<=sdm_fine/)[^/]+"),
      date = stringr::str_extract(thresh_tif_files, 
                                  "\\d{4}-\\d{2}-\\d{2}(?=\\.tif$)")
    )
  ),
  
  tar_target(
    thresh_meta_classified,
    classify_species(thresh_meta, search_term_col = "search_term")
  ),
  
  #### GHM 2022 ----
  tar_target(
    ghm_mosaic_paths,
    fs::dir_ls(path = "/mnt/envshare/data/raster/GEE_GlobalHumanModification_90m/ghm_mosaics",
               glob = "*.tif", 
               recurse = TRUE),
    format = "files"
  ),

  ## Protected area ----
  
  tar_target(
    capad2024_path,
    "/mnt/envshare/data/vector/raw/dcceew/capad_2024/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp",
    format = "file"
  )

)