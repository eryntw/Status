#' Summarise IUCN Threat Scores by Level 1 (and optional Level 2)
#'
#' Filters a dataset by Level 1 threat category and optionally
#' Level 2 subcategory, then summarises threat impact scores per species.
#'
#' @param df A data frame containing threat data.
#' @param lv1_filter Numeric scalar. Level 1 threat category.
#' @param lv2_filter Optional numeric scalar. Level 2 subcategory.
#' @param agg_method Character. One of "max", "mean", or "sum".
#'
#' @return A tibble joined back to `df` with a new column:
#'         - `threat_<lv1>_score` (if Lv2 not provided)
#'         - `threat_<lv1>_<lv2>_score` (if Lv2 provided)
#'
#' @export

summarise_threat_L1 <- function(df,
                                lv1_filter,
                                lv2_filter = NULL,
                                agg_method = c("max", "mean", "sum")) {
  
  # ---- Validate aggregation method ----
  agg_method <- rlang::arg_match(agg_method)
  
  # ---- Choose aggregation function ----
  agg_fun <- switch(
    agg_method,
    max  = base::max,
    mean = base::mean,
    sum  = base::sum
  )
  
  # ---- Apply filtering ----
  df_processed <- df %>%
    dplyr::filter(.data$threat_lv1 == lv1_filter)
  
  if (!is.null(lv2_filter)) {
    df_processed <- df_processed %>%
      dplyr::filter(.data$threat_lv2 == lv2_filter)
  }
  
  # ---- Row-level score ----
  df_processed <- df_processed %>%
    dplyr::mutate(
      threat_row_score =
        .data$timing_score +
        .data$scope_score +
        .data$severity_score
    )
  
  # ---- Dynamic column name ----
  new_col_name <- if (is.null(lv2_filter)) {
    paste0("threat_", lv1_filter, "_score")
  } else {
    paste0("threat_", lv1_filter, "_", lv2_filter, "_score")
  }
  
  # ---- Aggregate per species ----
  df_summary <- df_processed %>%
    dplyr::group_by(.data$scientific_name) %>%
    dplyr::summarise(
      !!new_col_name :=
        agg_fun(.data$threat_row_score, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(df_summary)
}