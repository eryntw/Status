#' Combine multiple SpatVectors into one harmonized layer
#'
#' Auto-detects id/label/date columns per layer via regex (with manual
#' overrides for known exceptions), validates detected date columns against
#' an expected yyyy-mm-dd format, and tags each feature with its source
#' layer (short_name).
#'
#' @param shp_list Named list of SpatVectors, named by short_name
#'   (excludes pdi — handled separately due to differing schema)
#' @param id_pattern Regex to match the ID column
#' @param label_pattern Regex to match the label column
#' @param date_patterns Character vector of candidate date column patterns,
#'   checked in priority order — first match wins
#' @param date_overrides Named list: short_name -> column name, for layers
#'   where the date column doesn't follow date_patterns (e.g. mislabeled fields)
#' @param check_crs Logical; reproject all layers to match the first layer's CRS
#'
#' @return A single SpatVector with columns orig_id, orig_label, orig_date, short_name
combine_sarig_vectors <- \(shp_list,
                     id_pattern = "^FILE_REF",
                     label_pattern = "^LABEL$",
                     date_patterns = c("^GRANTED_DA$", "^GRANT_DATE$", "^APP_DATE$", "^DATE_TO$", "^DATE_RECEI$"),
                     date_overrides = list(),
                     source = "sarig",
                     check_crs = TRUE) {
  
  # fallback operator: use y if x is NA or empty
  `%||%` <- \(x, y) if (is.na(x) || length(x) == 0) y else x
  
  # checks whether a column's values look like yyyy-mm-dd (allows a few dirty rows)
  looks_like_date <- \(x) {
    x_valid <- x[!is.na(x) & x != ""]
    if (length(x_valid) == 0) return(FALSE)
    mean(stringr::str_detect(x_valid, "^\\d{4}-\\d{2}-\\d{2}")) > 0.9
  }
  
  # detects id/label/date columns for one layer, using overrides where supplied
  detect_fields <- \(v, short_name) {
    nm <- names(v)
    
    # step 1: id and label columns via name-pattern matching
    id_col <- nm[stringr::str_detect(nm, id_pattern)][1]
    label_col <- nm[stringr::str_detect(nm, label_pattern)][1]
    
    # step 2: date column — manual override takes priority over pattern matching
    if (!is.null(date_overrides[[short_name]])) {
      date_col <- date_overrides[[short_name]]
      if (!date_col %in% nm) {
        stop(glue::glue("Override date column '{date_col}' not found in layer '{short_name}'"))
      }
    } else {
      date_col <- NA_character_
      for (pat in date_patterns) {
        hit <- nm[stringr::str_detect(nm, pat)]
        if (length(hit) > 0) {
          date_col <- hit[1]
          break
        }
      }
    }
    
    # step 3: content check — warn if detected date column doesn't look like yyyy-mm-dd
    if (!is.na(date_col) && nrow(v) > 0) {
      vals <- as.character(v[[date_col]][[1]])
      if (!looks_like_date(vals)) {
        warning(glue::glue("Column '{date_col}' in layer '{short_name}' doesn't look like yyyy-mm-dd — check this mapping"))
      }
    }
    
    c(id = id_col %||% NA_character_,
      label = label_col %||% NA_character_,
      date = date_col %||% NA_character_)
  }
  
  # reference CRS taken from the first layer, used to harmonize all others
  ref_crs <- terra::crs(shp_list[[1]])
  
  combined <- purrr::imap(shp_list, \(v, short_name) {
    
    # detect this layer's field mapping
    fields <- detect_fields(v, short_name)
    id_col <- fields[["id"]]
    label_col <- fields[["label"]]
    date_col <- fields[["date"]]
    
    # id column is mandatory — stop if not found
    if (is.na(id_col)) {
      stop(glue::glue("No ID column detected in layer '{short_name}' (pattern: '{id_pattern}')"))
    }
    
    # flag empty layers (0 features) — not an error, just worth knowing
    if (nrow(v) == 0) {
      warning(glue::glue("Layer '{short_name}' has 0 features — skipping"))
    }
    
    # harmonize CRS across layers if needed
    if (check_crs && !terra::same.crs(v, ref_crs)) {
      v <- terra::project(v, ref_crs)
    }
    
    # pulls a column as character, or fills NA if the column wasn't found
    pull_or_na <- \(col) {
      if (is.na(col)) {
        if (nrow(v) > 0) {
          warning(glue::glue("No matching column detected for layer '{short_name}' — filling NA"))
        }
        rep(NA_character_, nrow(v))
      } else {
        as.character(v[[col]][[1]])
      }
    }
    
    # build the standardized attribute table: id, label, date, short_name
    v_sub <- v[, id_col]
    v_sub$orig_id    <- as.character(v[[id_col]][[1]])
    v_sub$orig_label <- pull_or_na(label_col)
    v_sub$orig_date  <- pull_or_na(date_col)
    v_sub$short_name <- rep(short_name, nrow(v_sub))
    v_sub$source     <- rep(source, nrow(v_sub))
    v_sub <- v_sub[, c("orig_id", "orig_label", "orig_date", "short_name", "source")]
    
    v_sub
  }) %>%
    # stack all standardized layers into one SpatVector
    purrr::reduce(rbind)
  
  combined
}