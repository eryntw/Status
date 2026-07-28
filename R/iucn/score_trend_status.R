# score_redlist.R

#' Score population trend and IUCN conservation status
#'
#' Converts IUCN population trend and Red List category into numeric scores
#' suitable for threat or vulnerability analyses.
#'
#' @param data A data frame
#' @param trend_col Column containing population trend
#' @param status_col Column containing IUCN Red List status
#'
#' @return The input data frame with two new columns:
#'   - pop_trend_score
#'   - redlist_score
#'
#' @details
#' Population trend scoring:
#'   Decreasing = 2, Stable = 1, Increasing = 0
#'
#' IUCN status scoring:
#'   CR = 4, EN = 3, VU = 2, NT = 1, LC = 0
#'
#' Unknown or missing values are scored as 0.

score_trend_status <- function(data, trend_col, status_col){
  
  trend_col  <- rlang::ensym(trend_col)
  status_col <- rlang::ensym(status_col)
  
  # ---- lookup tables ----
  trend_lookup <- c(
    "decreasing" = 3,
    "stable"     = 2,
    "increasing" = 1,
    "unknown" = 2
  )
  
  status_lookup <- c(
    "CR" = 5,
    "EN" = 4,
    "VU" = 3,
    "NT" = 2,
    "LC" = 1
  )
  
  data %>%
    dplyr::mutate(
      
      # ---- population trend ----
      trend_score =
        trend_lookup[
          stringr::str_to_lower(as.character(!!trend_col))
        ] %>%
        unname() %>%
        tidyr::replace_na(0),
      
      # ---- red list status ----
      status_score =
        status_lookup[
          toupper(as.character(!!status_col))
        ] %>%
        unname() %>%
        tidyr::replace_na(0),
      
      # ---- combined ----
      TrendxStatus = trend_score*status_score
      
    )
}
