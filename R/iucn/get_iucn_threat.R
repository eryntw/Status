#' Get IUCN assessment data and combine elements
#'
#' @param api IUCN API object
#' @param genus Character, genus name
#' @param species Character, species name
#' @param elements Named list of elements to extract with optional prefixes
#' @return A list with combined dfs: `df_combined` and `syms_combined`
#' 

get_iucn_threat <- function(api, genus, species) {
  
  assess <- tryCatch(
    {
      iucnredlist::assessments_by_name(api, genus, species)
    },
    error = function(e) {
      data.frame()
    }
  )
  
  if (nrow(assess) == 0) {
    message("No IUCN assessment found for ", genus, " ", species)
    return(list(main = NULL, syms = NULL))
  }
  
  assess_id <- assess$assessment_id[1]
  dat <- iucnredlist::assessment_data_many(api, assessment_ids = assess_id)
  
  safe_extract <- \(dat, element, required_cols = NULL) {
    df <- tryCatch(iucnredlist::extract_element(dat, element), error = \(e) data.frame())
    if (nrow(df) == 0) return(data.frame())
    if (!is.null(required_cols) && !all(required_cols %in% names(df))) return(data.frame())
    df
  }
  
  taxon <- safe_extract(dat, "taxon")
  if (nrow(taxon) > 0) taxon <- taxon[, c(2:4)]
  
  common <- safe_extract(dat, "taxon_common_names", required_cols = c("main", "name"))
  if (nrow(common) > 0) {
    common <- common %>%
      dplyr::filter(main == TRUE) %>%
      dplyr::rename(common = name)
  }
  
  synonyms <- safe_extract(dat, "taxon_synonyms")
  if (nrow(synonyms) > 0) {
    synonyms <- synonyms %>%
      dplyr::select(any_of(c("assessment_id", "genus_name", "species_name"))) %>%
      dplyr::distinct()
  }
  
  status <- safe_extract(dat, "red_list_category")
  if (nrow(status) > 0) status <- status[, c(2, 5)]
  
  threats <- safe_extract(dat, "threats")
  if (nrow(threats) > 0) {
    threats <- threats %>% dplyr::rename_with(~ paste0("threat_", .x), -assessment_id)
  }
  
  poptrend <- safe_extract(dat, "population_trend")
  if (nrow(poptrend) > 0) {
    poptrend <- poptrend %>% dplyr::rename_with(~ paste0("poptrend_", .x), -assessment_id)
  }
  
  conservation <- safe_extract(dat, "conservation_actions_in_place")
  if (nrow(conservation) > 0) {
    conservation <- conservation %>% dplyr::rename_with(~ paste0("conservation_", .x), -assessment_id)
  }
  
  dfs <- list(taxon, common, status, threats, poptrend, conservation)
  dfs_nonempty <- dfs[vapply(dfs, function(x) nrow(x) > 0, logical(1))]
  
  combined <- if (length(dfs_nonempty) == 0) {
    NULL
  } else if (length(dfs_nonempty) == 1) {
    dfs_nonempty[[1]]
  } else {
    purrr::reduce(dfs_nonempty, dplyr::left_join, by = "assessment_id")
  }
  
  syms <- list(taxon, common, synonyms)
  syms_nonempty <- syms[vapply(syms, function(x) nrow(x) > 0, logical(1))]
  
  syms_combined <- if (length(syms_nonempty) == 0) {
    NULL
  } else if (length(syms_nonempty) == 1) {
    syms_nonempty[[1]]
  } else {
    purrr::reduce(syms_nonempty, dplyr::left_join, by = "assessment_id")
  }
  
  return(list(main = combined, syms = syms_combined))
}