#' Extract Genus and Species as the first two whitespace-separated words of Taxa
#'
#' @param taxon A single taxon name string.
#' @return A one-row tibble with Genus and Species.
parse_genus_species <- function(taxon) {
  words <- stringr::str_split(taxon, "\\s+")[[1]]
  
  tibble::tibble(
    taxon_original = taxon,
    Genus = words[1],
    Species = words[2],
  )
}

#' Read a region-contribution summary file and extract the top-contributing
#' record per taxon, restricted to a chosen taxonomic rank

summarise_max_regcont <- function(file_path, rank_filter = "Species") {
  
  # Step 1: Read the raw summary file.
  raw <- readr::read_csv(file_path, show_col_types = FALSE)
  
  # Step 2: Restrict to the requested rank(s) before any grouping,
  # so slice_max() only competes within the taxa we actually want.
  filtered <- raw |>
    dplyr::filter(Rank == rank_filter)
  
  # Step 3: For each taxon, keep only its single highest-contribution record.
  top_per_taxon <- filtered |>
    dplyr::group_by(Taxa) |>
    dplyr::slice_max(`Max region contrib (%)`, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  
  # Step 4: Parse Genus/Species from the first two words of Taxa.
  parsed <- top_per_taxon$Taxa |>
    purrr::map_dfr(parse_genus_species) |>
    dplyr::select(Genus, Species)
  
  # Step 5: Row-identity guard before bind_cols() — bind_cols() assumes row
  # alignment silently, so this converts a potential silent misalignment
  # into a loud failure if top_per_taxon and parsed ever diverge in row count.
  stopifnot(nrow(top_per_taxon) == nrow(parsed))
  
  # Step 6: Bind the parsed columns back onto the summary data.
  top_per_taxon |>
    dplyr::bind_cols(parsed)
}