#' Flag pdi records by industry theme and extract polygons matching selected theme(s)
#'
#' Cleans the natureofdevelopment free-text field, flags records matching
#' renewable energy, mining, and petroleum industry keywords, and returns
#' only the polygons matching the requested theme(s).
#'
#' @param pdi SpatVector with a natureofdevelopment column (development description text)
#' @param flag_type Character vector specifying which theme(s) to filter for.
#'   One or more of "renewable", "mining", "petroleum". Default "renewable".
#'   Rows matching ANY of the specified themes are returned (OR logic).
#'
#' @return SpatVector containing only polygons matching the requested theme(s),
#'   with natureofde_clean, word_count, char_count, and the three flag columns attached
extract_pdi_by_theme <- \(pdi, flag_type = "renewable") {
  
  flag_type <- match.arg(flag_type, choices = c("renewable", "mining", "petroleum"), several.ok = TRUE)
  
  pdi_attr <- as.data.frame(pdi) |> tibble::as_tibble()
  
  pdi_attr <- pdi_attr |>
    dplyr::mutate(
      natureofde_clean = natureofdevelopment |>
        stringr::str_to_lower() |>
        stringr::str_squish(),
      word_count = stringr::str_count(natureofde_clean, "\\S+"),
      char_count = stringr::str_length(natureofde_clean),
      
      renewable_farm_flag = stringr::str_detect(
        natureofde_clean,
        "solar farm|wind farm|wind turbine|ground mounted solar|renewable energy facility"
      ),
      
      mining_flag = stringr::str_detect(
        natureofde_clean,
        "mine|mining|mineral exploration|quarry|extraction|drilling|borefield|tailings"
      ) &
        !stringr::str_detect(natureofde_clean, "exhaust|ventilation|extraction fan|canopy") &
        !stringr::str_detect(natureofde_clean, "heritage|tourist|advertising|reserve"),
      
      petroleum_flag = stringr::str_detect(
        natureofde_clean,
        "oil and gas|gas well|gas pipeline|wellhead|hydrocarbon|petroleum production|petroleum exploration"
      ) &
        !stringr::str_detect(natureofde_clean, "petrol filling|petrol station|fuel bowser|petrol outlet|refuelling")
    )
  
  pdi$natureofde_clean    <- pdi_attr$natureofde_clean
  pdi$word_count          <- pdi_attr$word_count
  pdi$char_count          <- pdi_attr$char_count
  pdi$renewable_farm_flag <- pdi_attr$renewable_farm_flag
  pdi$mining_flag         <- pdi_attr$mining_flag
  pdi$petroleum_flag      <- pdi_attr$petroleum_flag
  
  # build the selection column name(s) corresponding to requested flag_type(s)
  flag_cols <- paste0(flag_type, ifelse(flag_type == "renewable", "_farm_flag", "_flag"))
  
  # OR logic across selected flags — keep rows where ANY requested flag is TRUE
  selected <- Reduce(`|`, as.data.frame(pdi)[, flag_cols, drop = FALSE])
  
  pdi[selected, ]
}