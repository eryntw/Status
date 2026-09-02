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

# targets -------

tar_plan(

  #### RegContSum ----
  
  ## AOI = state ----

  ## AOI = development ----
  
)