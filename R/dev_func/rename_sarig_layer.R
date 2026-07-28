#' Rename layer names according to project-specific rules
#'
#' @param x Character vector of layer names (works on the original,
#'   not-yet-shortened `layer_name` column, since it needs to detect
#'   keywords like "application" and "Associated" etc.)
#'
#' @return Character vector of renamed layers
rename_sarig_layer <- \(x) {
  
  # rule 1: detect "application" anywhere (case-insensitive), to append later
  has_app <- stringr::str_detect(x, stringr::regex("application", ignore_case = TRUE))
  
  # rules 2-6: main name lookup based on keyword match
  main_name <- dplyr::case_when(
    stringr::str_detect(x, stringr::regex("associated",  ignore_case = TRUE)) ~ "RenewAssociated",
    stringr::str_detect(x, stringr::regex("mining",      ignore_case = TRUE)) ~ "Mining",
    stringr::str_detect(x, stringr::regex("geothermal",  ignore_case = TRUE)) ~ "Geothermal",
    stringr::str_detect(x, stringr::regex("petroleum",   ignore_case = TRUE)) ~ "Petrol",
    stringr::str_detect(x, stringr::regex("renewable",   ignore_case = TRUE)) ~ "Renew",
    TRUE ~ x  # fallback: leave unchanged if none of the keywords match
  )
  
  # combine main name + suffix
  paste0(main_name, ifelse(has_app, "_app", ""))
}