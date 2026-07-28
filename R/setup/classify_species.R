#' Classify species and split binomial names into Genus/Species
#'
#' Splits a `search_term` column (binomial name, "Genus species") into
#' separate `Genus` and `Species` columns, and queries ALA (via galah) to
#' add taxonomic classification (kingdom, class, family, vernacular name).
#'
#' @param df A data frame containing a `search_term` column with binomial names
#' @param search_term_col Name of the column containing binomial names (default "search_term")
#'
#' @return The input data frame with added columns: Genus, Species,
#'   ala_kingdom, ala_class, ala_family, ala_vernacular_name, ala_match_type
classify_species <- \(df, search_term_col = "search_term") {
  
  # step 1: split binomial into Genus/Species
  df_split <- df |>
    tidyr::separate(
      {{ search_term_col }},
      into = c("Genus", "Species"),
      sep = " ",
      remove = FALSE
    )
  
  # step 2: configure galah/ALA
  potions::brew(.pkg = "galah")
  galah::galah_config(
    atlas = Sys.getenv("ALA_ATLAS"),
    email = Sys.getenv("ALA_EMAIL")
  )
  
  # step 3: unique species list for the batched ALA query
  species_query <- df_split |>
    dplyr::distinct(.data[[search_term_col]]) |>
    dplyr::filter(!is.na(.data[[search_term_col]])) |>
    dplyr::pull(.data[[search_term_col]])
  
  # step 4: batched ALA taxonomic lookup
  ala <- galah::search_taxa(species_query) |>
    dplyr::distinct() |>
    dplyr::select(
      search_term,
      kingdom,
      class,
      family,
      vernacular_name,
      match_type
    ) |>
    dplyr::rename_with(
      ~ paste0("ala_", .x),
      -search_term
    )
  
  # step 5: join classification back onto the split data frame
  df_split |>
    dplyr::left_join(ala, by = setNames("search_term", search_term_col))
}