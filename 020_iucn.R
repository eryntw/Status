library(targets)
library(tarchetypes)
library(crew)
library(dplyr)

tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages, 
               controller = crew::crew_controller_local(workers = 1))


# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

# targets -------

thresh_meta_classified <- tar_read(thresh_meta_classified, store = tars$setup$store)
regcontSA_spmax <- tar_read(regcontSA_spmax, store = tars$setup$store)

tar_plan(
  
  ## Get IUCN data for all modeled species -------
  targets::tar_target(name = iucn_data,
                      command = map_iucn_data(
                        regcontSA_spmax, #|> dplyr::slice_sample(n = 50), ## test !!!!
                        query_fn = get_iucn_threat,
                        pause = 1,
                        max_retries = 5)
  ),
  
  ## Extract threats ------
  targets::tar_target(name = iucn_threat,
                      command = iucn_data$iucn_data %>%
                        dplyr::select(scientific_name,
                                      common,
                                      code,
                                      starts_with("threat_")) %>%
                        dplyr::distinct()
  ),
  
  ## Score threats ------
  scored_threat = score_threat(iucn_threat, score_system = "ward_modified"),
  
  
  # Summarise threats ------
  threatsum = summarise_iucn_threat(scored_threat),
  
  # Score trend and status ------
  targets::tar_target(name = scored_trendstatus,
                      command = iucn_data$iucn_data %>%
                        dplyr::select(scientific_name,
                                      common,
                                      code,
                                      poptrend_description) %>%
                        dplyr::distinct() %>%
                        score_trend_status(trend_col = "poptrend_description",
                                           status_col = "code")
  ),
  
  # Join tables -----
  targets::tar_target(name = iucn_summary,
                      command = scored_trendstatus %>%
                        dplyr::left_join(threatsum$species_summary,
                                         by=c("scientific_name","common")) %>%
                        dplyr::mutate(dplyr::across(where(is.numeric),~ round(.x, 2)))
  )
)
