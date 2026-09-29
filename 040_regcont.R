library(targets)
library(dplyr)
library(tarchetypes)
library(crew)

tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages, 
               controller = crew::crew_controller_local(workers = 2))

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

regcontSA_spmax <- tar_read(regcontSA_spmax, store = tars$setup$store)
regcontSA_subspmax <- tar_read(regcontSA_subspmax, store = tars$setup$store)
sdm_summary <- tar_read(sdm_summary, store = tars$sdmstats$store)

# targets -------

tar_plan(

  #### RegContSum ----
  
  ## RegCont bind to sdm_summary ----
  tar_target(
    sdm_reg_summary,
    command = {
      regcontSA_spmax <-  regcontSA_spmax |> ## State RegCont
        dplyr::rename_with(.fn = janitor::make_clean_names) |>
        dplyr::rename_with(.fn = \(x) paste0("sa_", x))
      
      dplyr::left_join(
        sdm_summary,
        regcontSA_spmax,
        by = c("search_term" = "sa_taxa")
      )
    }
  )
)