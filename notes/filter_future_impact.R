#' Identify polygons where at least `threshold` fraction of area falls in
#' hm_rast classes 1-3 (Residual/Modified/Transformed), i.e. still natural-ish
#' habitat where future development would represent a new impact
#'
#' @param vec SpatVector to evaluate (e.g. combined_vec_all)
#' @param hm_rast SpatRaster of hm_rast classes (1-6)
#' @param class_range Integer vector of hm_rast classes considered "less modified"
#'   (default 1:3 = Residual, Modified, Transformed)
#' @param threshold Minimum fraction (0-1) of polygon area required to fall
#'   within class_range for the polygon to be selected (default 0.3)
#'
#' @return A list with:
#'   - df: data.frame with all original fields from vec plus hm_rast_mean,
#'     hm_rast_majority, area_sqkm, frac_class_1_3, and a `selected` flag
#'   - selected_vec: SpatVector containing only polygons meeting the threshold
filter_future_impact <- \(vec, hm_rast, class_range = 1:3, threshold = 0.3) {
  
  # step 1: CRS check — reproject vec to match hm_rast if needed
  if (!terra::same.crs(vec, hm_rast)) {
    vec <- terra::project(vec, terra::crs(hm_rast))
  }
  
  # step 2: area in sqkm, computed on the vector as-is
  area_sqkm <- terra::expanse(vec, unit = "km")
  
  # step 3: extract raw pixel values + area-weights per polygon (long format:
  # one row per pixel per polygon, with an ID column linking back to polygon index)
  ex <- terra::extract(hm_rast, vec, weights = TRUE, ID = TRUE)
  names(ex)[2] <- "hm_rast_value"  # standardize column name regardless of raster's layer name
  
  # step 4: per-polygon summary stats, computed from the long-format extraction
  summary_by_id <- ex |>
    dplyr::filter(!is.na(hm_rast_value)) |>
    dplyr::group_by(ID) |>
    dplyr::summarise(
      hm_rast_mean = stats::weighted.mean(hm_rast_value, w = weight, na.rm = TRUE),
      # majority class = weight-summed mode (most area-weighted class)
      hm_rast_majority = {
        tab <- tapply(weight, hm_rast_value, sum)
        as.integer(names(tab)[which.max(tab)])
      },
      frac_class_1_3 = sum(weight[hm_rast_value %in% class_range]) / sum(weight),
      .groups = "drop"
    )
  
  # step 5: build the full data.frame — start from vec's attribute table,
  # add a row-index ID to join summary_by_id back correctly (extract's ID
  # corresponds to row order in vec, 1-indexed)
  df <- vec |>
    as.data.frame() |>
    dplyr::mutate(ID = dplyr::row_number(), area_sqkm = area_sqkm) |>
    dplyr::left_join(summary_by_id, by = "ID") |>
    dplyr::mutate(selected = !is.na(frac_class_1_3) & frac_class_1_3 >= threshold) |>
    dplyr::select(-ID)
  
  # step 6: build the filtered SpatVector — same row order as vec/df, so
  # boolean indexing is safe here
  selected_vec <- vec[df$selected, ]
  
  list(df = df, selected_vec = selected_vec)
}