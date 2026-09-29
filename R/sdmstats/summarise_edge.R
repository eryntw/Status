#' Calculate edge/core habitat distances (GISfrag) for a single SDM raster
#'
#' @param path Path to a thresholded (binary 0/1) SDM raster.
#' @param target_crs Target CRS to reproject to (e.g. "EPSG:7853" for
#'   GDA2020 / MGA zone 53 -- adjust to match the zone your SA extent falls in).
#' @param min_patch_ha Minimum patch size to retain, in hectares. Patches
#'   smaller than this (using 8-cell adjacency) are set to 0 before edge
#'   detection.
#' @param directions Neighbourhood rule for patch and edge detection (4 or 8).
#'
#' @return A one-row tibble with per-species GISfrag summary stats.
summarise_edge <- function(path,
                           target_crs,
                           min_patch_ha = 36,
                           directions = 8,
                           buffer_cells = 5) {
  
  species_id <- path |>
    basename() |>
    tools::file_path_sans_ext() |>
    stringr::str_split_i("__", 1)
  
  r <- terra::rast(path)
  r <- terra::project(r, target_crs, method = "near")
  
  suitable_only <- terra::ifel(r == 1, 1, NA)
  n_suitable_raw <- terra::global(suitable_only, "sum", na.rm = TRUE)[1, 1]
  
  if (is.na(n_suitable_raw) || n_suitable_raw == 0) {
    return(
      tibble::tibble(
        species = species_id,
        n_suitable_cells = 0,
        n_edge_cells = NA_real_,
        suitable_area_ha = 0,
        gisfrag_mean_m = NA_real_
      )
    )
  }
  
  suitable_trimmed <- terra::trim(suitable_only)
  buffer_dist <- terra::res(r)[1] * buffer_cells
  e <- terra::ext(suitable_trimmed) + buffer_dist
  r <- terra::crop(r, e)
  
  cell_area_ha <- prod(terra::res(r)) / 10000
  min_cells <- ceiling(min_patch_ha / cell_area_ha)
  
  patches <- terra::patches(r, directions = directions, zeroAsNA = TRUE)
  patch_freq <- terra::freq(patches)
  small_ids <- patch_freq$value[patch_freq$count < min_cells]
  
  patches_na <- if (length(small_ids) > 0) {
    terra::classify(patches, cbind(small_ids, NA))
  } else {
    patches
  }
  
  r_clean <- terra::ifel(is.na(patches_na), 0, r)
  n_suitable <- terra::global(r_clean, "sum", na.rm = TRUE)[1, 1]
  
  if (is.na(n_suitable) || n_suitable == 0) {
    return(
      tibble::tibble(
        species = species_id,
        n_suitable_cells = 0,
        n_edge_cells = NA_real_,
        suitable_area_ha = 0,
        gisfrag_mean_m = NA_real_
      )
    )
  }
  
  # classes = TRUE is essential here: default (FALSE) only detects
  # NA/non-NA edges, which is meaningless for a 0/1 binary raster
  edge <- terra::boundaries(r_clean, classes = TRUE, inner = TRUE, directions = directions)
  edge_only <- terra::ifel(edge == 1 & r_clean == 1, 1, NA)
  
  dist_rast <- terra::distance(edge_only)
  suitable_dist <- terra::ifel(r_clean == 1, dist_rast, NA)
  
  n_edge <- terra::global(edge_only, "sum", na.rm = TRUE)[1, 1]
  gisfrag_mean <- terra::global(suitable_dist, "mean", na.rm = TRUE)[1, 1]
  
  tibble::tibble(
    species = species_id,
    n_suitable_cells = n_suitable,
    n_edge_cells = n_edge,
    suitable_area_ha = n_suitable * cell_area_ha,
    gisfrag_mean_m = gisfrag_mean
  )
}