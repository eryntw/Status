#' Convert a raster class (or classes) into a generalized landscape-scale vector
#'
#' @param raster SpatRaster to vectorize (e.g. Australia-wide HCAS)
#' @param mask_classes Class value(s) to extract
#' @param agg_fact Aggregation factor (coarsens resolution before vectorizing;
#'   higher = more generalized/landscape-scale, fewer small patches)
#' @param min_area_sqkm Minimum patch area to retain (drops small fragments)
#' @param crop_extent Optional SpatVector (or SpatExtent) to crop raster to
#'   before processing — e.g. a state boundary layer. If NULL (default),
#'   the full raster extent is used.
#' @param crop_buffer Buffer distance (map units) applied around crop_extent
#'   before cropping, to avoid edge artifacts (default 1000)
#'
#' @return SpatVector of generalized polygons for the given class(es)
raster_to_landscape_vec <- \(raster, mask_classes, agg_fact = 5, min_area_sqkm = 0.5,
                             crop_extent = NULL, crop_buffer = 1000) {
  
  # step 1: optional crop — only runs if crop_extent is supplied.
  if (!is.null(crop_extent)) {
    if (!terra::same.crs(raster, crop_extent)) {
      crop_extent <- terra::project(crop_extent, terra::crs(raster))
    }
    raster <- terra::crop(raster, terra::ext(crop_extent) + crop_buffer)
  }
  
  # step 2: coarsen resolution by aggregating agg_fact x agg_fact blocks of
  # cells into one. fun = "modal" takes the most frequent class in each block,
  # appropriate for categorical data — this generalizes the raster to
  # landscape scale and reduces the number of small fragments before vectorizing
  raster_coarse <- terra::aggregate(raster, fact = agg_fact, fun = "modal", na.rm = TRUE)
  
  # step 3: build a binary mask — any cell matching a value in mask_classes
  # (one or more classes) is reclassified to 1; everything else becomes NA.
  # Works for a single class or multiple classes (each mask_classes value
  # gets its own row in the reclassification table, all mapping to 1)
  mask <- terra::classify(
    raster_coarse,
    rcl = matrix(c(mask_classes, rep(1, length(mask_classes))), ncol = 2),
    others = NA
  )
  
  # step 4: convert the binary mask raster to polygons, dissolving adjacent
  # matching cells into merged polygons rather than one polygon per cell.
  # Keep only the polygons corresponding to value 1 (the masked class(es)),
  # dropping the NA/background polygon
  vec <- terra::as.polygons(mask, dissolve = TRUE)
  vec <- vec[vec[[1]] == 1, ]
  
  # step 5: compute area per polygon and drop small fragments below the
  # min_area_sqkm threshold, keeping only landscape-scale patches
  vec$area_sqkm <- terra::expanse(vec, unit = "km")
  vec[vec$area_sqkm >= min_area_sqkm, ]
}