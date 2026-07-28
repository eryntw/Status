#' Summarise a single threshold raster: total presence pixels and how many
#' fall within protected areas
#'
#' @param path File path to a threshold raster (0/1 presence/absence)
#' @param capad_merged A single dissolved SpatVector representing all
#'   protected areas as one polygon (from merge_capad())
#'
#' @return A 1-row tibble: path, t1 (total presence pixels), t1_in_capad
#'   (presence pixels inside protected area), pct_in_capad (%)
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
  
  # step 4: extract raster values under the protected area polygon, and
  # sum them to get the count of presence pixels within protected areas
  ex <- terra::extract(r, capad_merged, ID = FALSE)
  t1_in_capad <- sum(ex[[1]], na.rm = TRUE)
  
  # step 5: assemble the result — total presence pixels, presence pixels
  # in protected areas, and the percentage this represents
  tibble::tibble(
    path         = path,
    t1           = t1,
    t1_in_capad  = t1_in_capad,
    pct_in_capad = ifelse(t1 > 0, t1_in_capad / t1 * 100, NA_real_)
  )
}