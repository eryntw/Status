#' Summarise a single threshold raster: total presence pixels and how many
#' fall within protected areas
#'
#' @param path File path to a threshold raster (0/1 presence/absence)
#' @param capad_merged A single dissolved SpatVector representing all
#'   protected areas as one polygon (from merge_capad())
#'
#' @return A 1-row tibble: path, t1 (total presence pixels), t1_in_capad
#'   (coverage-weighted presence pixels inside protected area), pct_in_capad (%)
summarise_sdm <- \(path, capad_merged) {
  
  # step 1: read the threshold raster fresh from disk (0/1 presence/absence)
  r <- terra::rast(path)
  
  # step 2: reproject capad_merged to match the raster's CRS if needed —
  # reprojecting the vector (cheap) rather than the raster (expensive)
  if (!terra::same.crs(r, capad_merged)) {
    capad_merged <- terra::project(capad_merged, terra::crs(r))
  }
  
  # step 3: total presence pixel count across the whole raster
  t1 <- terra::global(r, fun = "sum", na.rm = TRUE)[[1]]
  
  # step 4: exact_extract needs an sf object, not a SpatVector —
  # convert here rather than upstream, to keep capad_merged as terra
  # throughout the rest of the pipeline
  capad_sf <- sf::st_as_sf(capad_merged)
  
  # step 5: coverage-weighted extraction — exact_extract computes the
  # fraction of each pixel covered by the polygon and sums presence
  # values weighted by that fraction, avoiding the cell-center bias of
  # terra::extract() on irregular protected-area boundaries
  t1_in_capad <- exactextractr::exact_extract(
    r, capad_sf,
    fun = "sum",
    progress = FALSE
  )
  
  # step 5b: exact_extract returns one value per feature in capad_sf;
  # since capad_merged is a single dissolved polygon, sum defensively
  # in case any multipart geometry produces >1 row
  t1_in_capad <- sum(t1_in_capad, na.rm = TRUE)
  
  # step 6: assemble the result — total presence pixels, coverage-weighted
  # presence pixels in protected areas, and the percentage this represents
  tibble::tibble(
    path         = path,
    t1           = t1,
    t1_in_capad  = t1_in_capad,
    pct_in_capad = ifelse(t1 > 0, t1_in_capad / t1 * 100, NA_real_)
  )
}