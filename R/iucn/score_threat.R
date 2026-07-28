# ============================================================
# threat_impact_score.R
#
# Compute IUCN Threat Impact Scores based on Ward et al. (2020)
# or a modified scoring system.
# ============================================================

#' Calculate IUCN Threat Impact Scores
#'
#' @param table A data.frame containing:
#'        - threat_timing
#'        - threat_scope
#'        - threat_severity
#'
#' @param score_system One of:
#'        - "ward" (original Ward et al. 2020)
#'        - "ward_modified" (your modified scores)
#'
#' @return The input table with added columns:
#'         timing_score, scope_score, severity_score, threat_impact_score
#'
#' @export
#' 
score_threat <- function(table, 
                                score_system = c("ward", "ward_modified", "binary"),
                                return_table = FALSE) {
  
  # ---- Validate inputs ----
  score_system <- match.arg(score_system)
  
  required_cols <- c("threat_timing", "threat_scope", "threat_severity")
  
  if (!all(required_cols %in% names(table))) {
    stop(
      "Input table must contain the following columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # ---- Scoring vectors: Ward et al. 2020 ----
  iucn_timing_ward <- c(
    "Past, Unlikely to Return" = 0,
    "Past, Likely to Return"   = 0,
    "Ongoing"                  = 3,
    "Future"                   = 1,
    "Unknown"                  = 0
  )
  
  iucn_scope_ward <- c(
    "Whole (>90%)"      = 3,
    "Majority (50-90%)" = 2,
    "Minority (<50%)"   = 1,
    "Unknown"           = 0
  )
  
  iucn_severity_ward <- c(
    "Rapid Declines"                   = 2,
    "Slow, Significant Declines"       = 1,
    "Causing/Could cause fluctuations" = 1,
    "Negligible declines"              = 0,
    "No decline"                       = 0,
    "Unknown"                          = 0
  )
  
  # ---- Modified scoring ----
  iucn_timing_mod <- c(
    "Past, Unlikely to Return" = 0,
    "Past, Likely to Return"   = 0,
    "Ongoing"                  = 3,
    "Future"                   = 1,
    "Unknown"                  = 1
  )
  
  iucn_scope_mod <- c(
    "Whole (>90%)"      = 3,
    "Majority (50-90%)" = 2,
    "Minority (<50%)"   = 1,
    "Unknown"           = 1
  )
  
  iucn_severity_mod <- c(
    "Very Rapid Declines"              = 3,
    "Rapid Declines"                   = 3,
    "Slow, Significant Declines"       = 2,
    "Causing/Could cause fluctuations" = 1,
    "Negligible declines"              = 0,
    "No decline"                       = 0,
    "Unknown"                          = 1
  )
  
  # ---- Binary scoring ----
  iucn_timing_binary <- c(
    "Past, Unlikely to Return" = 0,
    "Past, Likely to Return"   = 0,
    "Ongoing"                  = 1,
    "Future"                   = 0,
    "Unknown"                  = 0
  )
  
  iucn_scope_binary <- c(
    "Whole (>90%)"      = 1,
    "Majority (50-90%)" = 1,
    "Minority (<50%)"   = 0,
    "Unknown"           = 0
  )
  
  iucn_severity_binary <- c(
    "Very Rapid Declines"              = 1,
    "Rapid Declines"                   = 1,
    "Slow, Significant Declines"       = 1,
    "Causing/Could cause fluctuations" = 0,
    "Negligible declines"              = 0,
    "No decline"                       = 0,
    "Unknown"                          = 0
  )
  
  
  # ---- build scoring reference table ----
  make_score_table <- function(ward, mod, binary, category){
    
    tibble::tibble(
      Category = category,
      Description = unique(c(names(ward), names(mod), names(binary)))
    ) %>%
      dplyr::mutate(
        Ward_2020 = ward[Description] |> unname(),
        Modified  = mod[Description]  |> unname(),
        Binary    = binary[Description] |> unname()
      )
  }
  
  scoring_table <- dplyr::bind_rows(
    make_score_table(iucn_timing_ward, iucn_timing_mod, iucn_timing_binary,  "Timing"),
    make_score_table(iucn_scope_ward, iucn_scope_mod, iucn_scope_binary,  "Scope"),
    make_score_table(iucn_severity_ward, iucn_severity_mod, iucn_severity_binary, "Severity")
  )
  
  scoring_table <- scoring_table %>%
    dplyr::arrange(Category, dplyr::desc(Ward_2020))
  
  if (return_table) {
    return(scoring_table)
  }
  
  # ---- Choose scoring system based on user input ----
  if (score_system == "ward") {
    timing_vec   <- iucn_timing_ward
    scope_vec    <- iucn_scope_ward
    severity_vec <- iucn_severity_ward
    
  } else if (score_system == "ward_modified") {
    timing_vec   <- iucn_timing_mod
    scope_vec    <- iucn_scope_mod
    severity_vec <- iucn_severity_mod
    
  } else if (score_system == "binary") {
    timing_vec   <- iucn_timing_binary
    scope_vec    <- iucn_scope_binary
    severity_vec <- iucn_severity_binary
  }
  
  
  # ---- Apply scores ----
  out <- table %>%
    dplyr::mutate(
      threat_scope = dplyr::case_when(
        is.na(threat_scope) & code != "LC" ~ "Unknown",
        .default = threat_scope
      ),
      threat_severity = dplyr::case_when(
        is.na(threat_severity) & code != "LC" ~ "Unknown",
        .default = threat_severity
      )
    ) %>%
    dplyr::mutate(
      timing_score   = timing_vec[threat_timing],
      scope_score    = scope_vec[threat_scope],
      severity_score = severity_vec[threat_severity],
      score_amean = (timing_score + scope_score + severity_score) / 3,
      score_gmean = (timing_score*scope_score*severity_score) / 3,
    ) %>% 
    tidyr::separate(
      col  = threat_code,
      into = c("threat_lv1", "threat_lv2", "threat_lv3"),
      sep  = "_",
      fill = "right",   # if lv3 is missing → NA
      remove = FALSE    # keep original column
    )
  
  return(out)
}
