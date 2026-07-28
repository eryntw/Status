#' Mosaic raster tiles by theme and write each mosaic to disk
#'
#' Groups tiles by theme, merges each theme's tiles into one mosaic,
#' and writes each mosaic to its own file — avoiding in-memory SpatRaster
#' objects being returned directly (which don't survive targets serialization).
#'
#' @param tile_meta A data frame with columns `path` (tile file paths) and
#'   `theme` (grouping variable)
#' @param out_dir Directory to write mosaicked rasters to
#'
#' @return A named character vector of output file paths, one per theme
mosaic_by_theme <- \(tile_meta, out_dir) {
  
  fs::dir_create(out_dir)
  themes <- unique(tile_meta$theme)
  
  purrr::map_chr(themes, \(th) {
    paths <- tile_meta |>
      dplyr::filter(theme == th) |>
      dplyr::pull(path)
    
    if (length(paths) == 0) {
      stop(glue::glue("No tiles found for theme '{th}'"))
    }
    
    tiles <- purrr::map(paths, terra::rast) |> unname()
    mosaic <- do.call(terra::merge, tiles)
    
    out_path <- fs::path(out_dir, glue::glue("ghm_{th}.tif"))
    terra::writeRaster(mosaic, out_path, overwrite = TRUE)
    as.character(out_path)
  }) |>
    purrr::set_names(themes)
}