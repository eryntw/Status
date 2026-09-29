#' Summarise IUCN threat impacts
#'
#' Aggregates IUCN Level-2 threat scores into Level-1 categories and
#' computes species-level cumulative threat pressure.
#'
#' @param scored_threat A data.frame containing IUCN threat scoring output.
#' Must contain columns:
#' `scientific_name`, `common`, `threat_lv1`,
#' `severity_score`, `scope_score`, `timing_score`,
#' and `threat_impact_score`.
#'
#' @return A named list with two data.frames:
#' \describe{
#'   \item{Lv1_summary}{One row per species × Level-1 threat category}
#'   \item{species_summary}{One row per species}
#' }
#' @export
#'
#' @importFrom dplyr mutate group_by summarise n
#' @importFrom tidyr replace_na
summarise_iucn_threat <- function(scored_threat){
  
  required_cols <- c(
    "scientific_name",
    "common",
    "threat_lv1",
    "severity_score",
    "scope_score",
    "timing_score",
    "score_amean",
    "score_gmean"
  )
  
  if(!all(required_cols %in% names(scored_threat))){
    stop(
      "Input data must contain columns: ",
      paste(required_cols, collapse = ", ")
    )
  }
  
  # ---------- Level 1 threat summary ----------
  Lv1_summary <- scored_threat %>%
    dplyr::mutate(
      score_gmean = tidyr::replace_na(score_gmean, 0),
      score_amean = tidyr::replace_na(score_amean, 0),
    ) %>%
    dplyr::group_by(scientific_name, common, threat_lv1) %>%
    dplyr::summarise(
      score_sum_lv1 = sum(score_gmean, na.rm = TRUE),   # cumulative Lv1 pressure
      score_max_lv1 = max(score_gmean, na.rm = TRUE),   # strongest Lv1 threat
      n_lv2         = sum(score_gmean > 0, na.rm = TRUE), # how many specific processes
      .groups = "drop"
    )
  
  
  # ---------- Species summary ----------
  species_summary <- Lv1_summary %>%
    dplyr::group_by(scientific_name, common) %>%
    dplyr::summarise(
      score_sum_sp = sum(score_sum_lv1, na.rm = TRUE),   # total cumulative pressure
      score_max_sp = max(score_max_lv1, na.rm = TRUE),   # strongest single threat
      n_lv1        = dplyr::n(),                          # number of major threat types
      n_lv2_total  = sum(n_lv2, na.rm = TRUE),            # number of specific threats
      .groups = "drop"
    )
  
  
  # ---------- Return both ----------
  list(
    Lv1_summary = Lv1_summary,
    species_summary = species_summary
  )
}
