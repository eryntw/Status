#' Score Anthropogenic Habitat Use and Habitat Breadth
#'
#' Calculates species-level habitat scores based on IUCN habitat use,
#' including habitat suitability, seasonality, habitat breadth (Levels 1 & 2),
#' and anthropogenic habitat use. Outputs scaled scores suitable for
#' comparative analyses.
#'
#' @param df A data frame containing IUCN habitat information. Must include
#'   columns: Suitability, Season, MajorImportance, HabitatsLevel1,
#'   HabitatsLevel2, and species identifier columns.
#' @param use_major_importance Logical. If TRUE, habitat scores are weighted
#'   by MajorImportance.
#' @param species_cols Character vector of column names identifying species
#'   (default: c("ScientificName", "CommonName")).
#'
#' @return A data frame with one row per species, including:
#' \itemize{
#'   \item score_HB_L2 – summed habitat suitability score (Level 2)
#'   \item scaledHBscore_L2 – scaled log-transformed Level 2 habitat score
#'   \item HB_L1 – number of suitable Level 1 habitats
#'   \item scaledHB_L1 – scaled Level 1 habitat breadth
#'   \item anthro_habitat_sum – summed anthropogenic habitat score
#'   \item anthro_LogHabitat_scaled – scaled log-transformed anthropogenic score
#' }
#'
#' @details
#' Habitat suitability is weighted by seasonality and optionally by
#' MajorImportance. Anthropogenic habitats are scored using a predefined
#' lookup table and aggregated per species.
#'
#' @import dplyr stringr scales
#' @export
score_iucn_habitat <- function(
    df,
    use_major_importance = FALSE,
    species_cols = c("ScientificName", "CommonName")
) {
  
  # ===============================
  # Internal scoring helper functions
  # ===============================
  
  suitability_score <- function(x) {
    dplyr::case_when(
      stringr::str_to_lower(x) == "suitable" ~ 3,
      stringr::str_to_lower(x) == "marginal" ~ 1,
      TRUE ~ NA_real_
    )
  }
  
  season_score <- function(x) {
    dplyr::case_when(
      stringr::str_detect(stringr::str_to_lower(x), "resident") ~ 1,
      stringr::str_detect(stringr::str_to_lower(x), "breeding") ~ 0.5,
      stringr::str_detect(stringr::str_to_lower(x), "non")      ~ 0.5,
      TRUE ~ NA_real_
    )
  }
  
  major_score <- function(x) {
    dplyr::case_when(
      stringr::str_to_lower(x) == "yes" ~ 3,
      stringr::str_to_lower(x) == "no"  ~ 1,
      is.na(x)                          ~ 1,
      TRUE ~ 1
    )
  }
  
  # ===============================
  # Habitat suitability score – Level 2
  # ===============================
  
  habitat_L2 <- df %>%
    dplyr::mutate(
      suitability_score = suitability_score(Suitability),
      season_score      = season_score(Season),
      major_score       = major_score(MajorImportance),
      
      habitat_score_row = suitability_score * season_score,
      habitat_score_row = if (use_major_importance) {
        habitat_score_row * major_score
      } else habitat_score_row
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(species_cols))) %>%
    dplyr::summarise(
      score_HB_L2 = sum(habitat_score_row, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      logscaledHBscore_L2 = scales::rescale(log(score_HB_L2 + 1), to = c(0, 1))
    )
  
  # ===============================
  # Habitat breadth – Level 1
  # ===============================
  
  habitat_L1 <- df %>%
    dplyr::filter(stringr::str_to_lower(Suitability) != "marginal") %>%
    dplyr::distinct(
      dplyr::across(dplyr::all_of(c(species_cols, "HabitatsLevel1")))
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(species_cols))) %>%
    dplyr::summarise(
      HB_L1 = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      scaledHB_L1 = scales::rescale(HB_L1, to = c(0, 1))
    )
  
  # ===============================
  # Anthropogenic habitat proportion (fixed denominators)
  # ===============================
  
  anthro_lv1_terr <- c(
    "artificial/terrestrial",
    "introduced vegetation" # IUCN Lv1 but included here, altho not present in Australian birds
  )
  
  anthro_lv1_aqua <- c(
    "artificial/aquatic & marine"
  )
  
  DENOM_TERR <- 7
  DENOM_AQUA <- 13 # based on IUCN habitat categories
  
  habitat_anthro <- df %>%
    dplyr::filter(stringr::str_to_lower(Suitability) != "marginal") %>%
    dplyr::mutate(
      lv1_clean = stringr::str_to_lower(HabitatsLevel1),
      lv2_clean = stringr::str_to_lower(HabitatsLevel2)
    ) %>%
    
    # keep only anthropogenic LV1
    dplyr::filter(lv1_clean %in% c(anthro_lv1_terr, anthro_lv1_aqua)) %>%
    
    # each L2 counted only once per species
    dplyr::distinct(
      dplyr::across(dplyr::all_of(c(species_cols, "lv1_clean", "lv2_clean")))
    ) %>%
    
    dplyr::group_by(dplyr::across(dplyr::all_of(species_cols))) %>%
    dplyr::summarise(
      
      n_art_terr_L2 = sum(lv1_clean %in% anthro_lv1_terr),
      n_art_aqua_L2 = sum(lv1_clean %in% anthro_lv1_aqua),
      
      prop_art_terr = n_art_terr_L2 / DENOM_TERR,
      prop_art_aqua = n_art_aqua_L2 / DENOM_AQUA,
      
      # optional combined index
      prop_anthro_total =
        (n_art_terr_L2 + n_art_aqua_L2) / (DENOM_TERR + DENOM_AQUA),
      
      .groups = "drop"
      
    ) %>%
    dplyr::mutate(
      # strongest anthropogenic association
      prop_anthro_max = pmax(prop_art_terr, prop_art_aqua, na.rm = TRUE)
    )
  
  
  # ===============================
  # Combine outputs
  # ===============================
  
  habitat_L2 %>%
    dplyr::left_join(habitat_L1,     by = species_cols) %>%
    dplyr::left_join(habitat_anthro, by = species_cols) %>%
    mutate(
      across(where(is.numeric), ~ tidyr::replace_na(.x, 0)),
      across(where(is.numeric), ~ round(.x, 2))
    )
  
}
