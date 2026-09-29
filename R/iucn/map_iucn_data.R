#' Retrieve IUCN Red List data with synonym retry
#'
#' Queries the IUCN API for a list of search_term and returns the cleaned
#' main search_term records. Species that are not found are automatically
#' re-queried using a synonym lookup table.
#'
#' The function expects a species list containing Genus and Species
#' columns and a column `search_term` with the binomial name
#' ("Genus species").
#'
#' @param splist A data frame containing species names. Must include
#'   columns `Genus`, `Species`
#' @param api An authenticated IUCN API object.
#' @param synonym_path File path to a CSV containing synonym mappings.
#'   The CSV must include columns `id` (original name) and `name_bi`
#'   (accepted binomial name).
#' @param query_fn A function used to query the IUCN API. Must accept
#'   `(api, genus, species)` as arguments and return a list with elements
#'   `main` and `syms`. Defaults to `get_iucn_threat`. Can be swapped
#'   for `get_iucn_habitat` or any compatible function.
#' @param sci_name_col Character, name of the column in `query_fn`'s `main`
#'   output that holds the scientific name to join back to `splist`'s
#'   `search_term`. Defaults to `"scientific_name"`. Set this explicitly
#'   if `query_fn`'s `taxon` element names that column differently.
#' @param pause Numeric, seconds to wait between each API call, to respect
#'   IUCN's rate limit. Default 1.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{iucn_data}{Data frame of all successfully retrieved IUCN records}
#'   \item{splist_iucn}{Original species list joined with IUCN data}
#' }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Queries IUCN for each species using `query_fn`, throttled by `pause`
#'   \item Extracts valid "main" species records
#'   \item Identifies species not found
#'   \item Matches them to a synonym table
#'   \item Re-queries the API using accepted names, also throttled
#' }
#'
#' @examples
#' \dontrun{
#' # Default: uses get_iucn_threat, join on "scientific_name"
#' output <- map_iucn_data(
#'   splist       = species_list,
#'   api          = api,
#'   synonym_path = "data/synonyms.csv"
#' )
#'
#' # Swap to get_iucn_habitat, with a different join column name
#' output <- map_iucn_data(
#'   splist       = species_list,
#'   api          = api,
#'   synonym_path = "data/synonyms.csv",
#'   query_fn     = get_iucn_habitat,
#'   sci_name_col = "taxon_scientific_name",
#'   pause        = 2
#' )
#'
#' iucn_data   <- output$iucn_data
#' splist_iucn <- output$splist_iucn
#' }
#'
#' @export
map_iucn_data <- function(splist,
                          synonym_path = "data/synonyms.csv",
                          query_fn     = get_iucn_threat,
                          sci_name_col = "scientific_name",
                          pause        = 1,
                          max_retries  = 5) {
  
  splist <- splist |>
    dplyr::mutate(search_term = paste(Genus, Species))
  
  query_iucn_safely <- function(api, g, s) {
    attempt <- 1
    repeat {
      Sys.sleep(pause)
      result <- tryCatch(query_fn(api, g, s), error = function(e) e)
      
      is_429 <- inherits(result, "error") && grepl("429", conditionMessage(result))
      if (!is_429) return(result)
      
      if (attempt >= max_retries) {
        warning(sprintf("Giving up on %s %s after %d retries (429)", g, s, attempt))
        return(list(main = NULL, syms = NULL))
      }
      
      backoff <- pause * (2 ^ attempt)
      message(sprintf("429 for %s %s — backing off %.1fs (attempt %d)", g, s, backoff, attempt))
      Sys.sleep(backoff)
      attempt <- attempt + 1
    }
  }
  
  run_query <- function(df) {
    api <- iucnredlist::init_api(Sys.getenv("IUCN_REDLIST_KEY"))
    
    df |>
      dplyr::mutate(
        result = purrr::map2(
          Genus, Species,
          \(g, s) query_iucn_safely(api, g, s),
          .progress = TRUE
        )
      )
  }
  
  extract_mains <- function(results) {
    results |>
      dplyr::mutate(main = purrr::map(result, "main")) |>
      dplyr::pull(main) |>
      purrr::discard(is.null) |>
      dplyr::bind_rows()
  }
  
  ## validate the join column exists before we get to the join, so failures are ##
  ## caught early with a clear message rather than surfacing as a silent all-NA join ##
  check_sci_name_col <- function(mains_df, sci_name_col) {
    if (nrow(mains_df) > 0 && !sci_name_col %in% names(mains_df)) {
      stop(
        sprintf(
          "sci_name_col '%s' not found in query_fn's main output. Available columns: %s",
          sci_name_col, paste(names(mains_df), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  
  results  <- run_query(splist)
  mains_df <- extract_mains(results)
  check_sci_name_col(mains_df, sci_name_col)
  
  ## join key built dynamically from sci_name_col, e.g. c("search_term" = "scientific_name") ##
  join_by <- stats::setNames("search_term", sci_name_col) |> (\(x) c(names(x) <- sci_name_col, "search_term"))()
  
  splist_iucn <- results |>
    dplyr::select(-result) |>
    dplyr::left_join(mains_df, by = stats::setNames(sci_name_col, "search_term"))
  
  rows_null <- results |>
    dplyr::mutate(
      both_null = purrr::map_lgl(result, ~ is.null(.x$main) && is.null(.x$syms))
    ) |>
    dplyr::filter(both_null)
  
  synonyms <- readr::read_csv(synonym_path, col_types = readr::cols())
  
  synmatch <- rows_null |>
    dplyr::select(search_term) |>
    dplyr::left_join(synonyms, by = c("search_term" = "id")) |>
    tidyr::separate(name_bi, into = c("Genus", "Species"), sep = " ")
  
  results2  <- run_query(synmatch)
  mains_df2 <- extract_mains(results2)
  check_sci_name_col(mains_df2, sci_name_col)
  
  iucn_data <- dplyr::bind_rows(mains_df, mains_df2)
  
  return(list(
    iucn_data   = iucn_data,
    splist_iucn = splist_iucn
  ))
}