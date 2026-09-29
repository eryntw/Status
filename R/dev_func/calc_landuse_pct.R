#' Calculate percentage of a given landuse class (or classes) within each polygon
#'
#' Overlays polygons onto a categorical landuse raster and computes, per
#' polygon, the area-weighted percentage of area falling within the
#' specified class(es).
#'
#' @param vec SpatVector of polygons (e.g. renewable-flagged pdi polygons)
#' @param landuse SpatRaster of categorical landuse classes
#' @param classes Integer vector of class value(s) to extract — e.g. a
#'   single level (c(69)) or several combined (c(69, 70, 71))
#' @param col_name Name of the output column to store the result in
#'   (default "pct_class")
#'
#' @return The input SpatVector with a new column named col_name (0-100 scale)
#'
#' @examples
#' \dontrun{
#' # Single class
#' pdi_renewable <- calc_landuse_pct(
#'   vec      = pdi_renewable,
#'   landuse  = raster_e,
#'   classes  = c(69),
#'   col_name = "pct_builtup"
#' )
#'
#' # Chaining multiple themes onto the same SpatVector
#' pdi_renewable <- pdi_renewable |>
#'   calc_landuse_pct(landuse = raster_e, classes = c(69, 70, 71), col_name = "pct_builtup") |>
#'   calc_landuse_pct(landuse = raster_e, classes = c(10, 11, 12), col_name = "pct_agriculture")
#' }
#'
#' @export
calc_landuse_pct <- \(vec, landuse, classes, col_name = "pct_class") {
  
  # step 1: CRS check, reproject vec if needed
  if (!terra::same.crs(vec, landuse)) {
    vec <- terra::project(vec, terra::crs(landuse))
  }
  
  # step 2: extract raster values + area-weights under each polygon
  ex <- terra::extract(landuse, vec, weights = TRUE, ID = TRUE)
  names(ex)[2] <- "landuse_value"
  
  # step 3: per-polygon area-weighted percentage of area in the given class(es)
  landuse_summary <- ex |>
    dplyr::filter(!is.na(landuse_value)) |>
    dplyr::group_by(ID) |>
    dplyr::summarise(
      pct_class = sum(weight[landuse_value %in% classes]) / sum(weight) * 100,
      .groups = "drop"
    )
  
  # step 4: join back onto vec by row position (ID is 1-indexed, matches vec's row order)
  vec_df <- tibble::tibble(ID = seq_len(nrow(vec))) |>
    dplyr::left_join(landuse_summary, by = "ID") |>
    dplyr::mutate(pct_class = tidyr::replace_na(pct_class, 0))
  
  # step 5: assign to the user-specified column name
  vec[[col_name]] <- vec_df$pct_class
  
  vec
}