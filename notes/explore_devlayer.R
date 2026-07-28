library(fs)
library(purrr)
library(terra)
library(targets)
library(dplyr)
tar_source("notes/dev_func")

## Read development vectors from SARIG ----

tar_load(combined_sarig, store = tars$setup$store)

combined_sarig %>% as.data.frame() %>% dplyr::count(short_name, sort = TRUE)

## PDI act vector ----

tar_load(pdi, store = tars$setup$store)

## Examine the attributes of PDI

# column names
names(pdi)

# number of features and attributes
nrow(pdi)
ncol(pdi)

# data type of each column
sapply(pdi, class)

# examine the description text for each development entry
pdi_attr <- as.data.frame(pdi) %>% tibble::as_tibble()

pdi_attr <- pdi_attr |>
  mutate(
    natureofde_clean = natureofde |>
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
    )  &
      stringr::str_detect(natureofde_clean, "exhaust|ventilation|extraction fan|canopy") &
      stringr::str_detect(natureofde_clean, "heritage|tourist|advertising|reserve"),
    
    petroleum_flag = stringr::str_detect(
      natureofde_clean,
      "oil and gas|gas well|gas pipeline|wellhead|hydrocarbon|petroleum production|petroleum exploration"
    ) & !stringr::str_detect(natureofde_clean, "petrol filling|petrol station|fuel bowser|petrol outlet|refuelling")
  )

pdi_attr |> count(renewable_farm_flag)
pdi_attr |> count(mining_flag)
pdi_attr |> count(petroleum_flag)

# summary stats
hist(pdi_attr$word_count)

# check for duplicates
pdi_attr %>%
  count(natureofde_clean, sort = TRUE) %>%
  mutate(pct = n / sum(n) * 100)

# description word cloud
library(tidytext)

word_freq <- pdi_attr %>%
  select(natureofde_clean) %>%
  filter(!is.na(natureofde_clean), natureofde_clean != "") %>%
  tibble::rowid_to_column("id") %>%
  tidytext::unnest_tokens(word, natureofde_clean) %>% # splits text into one-word-per-row
  filter(stringr::str_detect(word, "[a-z]")) %>% # drop pure-numeric tokens (e.g. lot numbers)
  anti_join(stop_words, by = "word") %>%  # remove "the", "and", "of", "a", etc.
  count(word, sort = TRUE)

word_freq %>% print(n = 30)

library(wordcloud2)

word_freq %>%
  slice_max(n, n = 100) %>%
  wordcloud2(size = 0.7)

bigram_freq <- pdi_attr %>%
  select(natureofde_clean) %>%
  filter(!is.na(natureofde_clean), natureofde_clean != "") %>%
  tidytext::unnest_tokens(bigram, natureofde_clean, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)

bigram_freq %>% print(n = 30)

## Investigate polygon overlaps ----

# reproject pdi into GCS_GDA2020 (matching combined_sarig)
pdi <- terra::project(pdi1, terra::crs(combined_sarig))

pdi_sub <- pdi[, "globalid"]  # placeholder to carry geometry
pdi_sub$orig_id    <- as.character(pdi$globalid)
pdi_sub$orig_label <- as.character(pdi$applicatio)
pdi_sub$orig_date  <- as.character(pdi$lodgementd)
pdi_sub$short_name <- as.character(pdi$natureofde)
pdi_sub$source     <- rep("pdi", nrow(pdi_sub))
pdi_sub <- pdi_sub[, c("orig_id", "orig_label", "orig_date", "short_name", "source")]

# for each pdi polygon, does it intersect any combined_sarig polygon?
overlap_matrix <- terra::relate(pdi_sub, combined_sarig, relation = "intersects")

# add a flag column: TRUE if this pdi polygon overlaps ANY sarig polygon
pdi_sub$overlaps_sarig <- apply(overlap_matrix, 1, any)

pdi_sub %>% as.data.frame() %>% dplyr::count(overlaps_sarig) # 12 pdi polygons has overlap

# examine the overlapped polygons:
pdi_sub %>% filter(overlaps_sarig == 1) %>% as.data.frame()

## Filter out polygons < 1500 sqm in pdi ----
min_area_m2 <- 1500
pdi_filtered <- pdi[pdi$Shape__Are >= min_area_m2, ]
nrow(pdi)
nrow(pdi_filtered)

## Combine sarig and pdi for spatial filtering ----

combined_vec <- rbind(combined_sarig, pdi_sub)

combined_vec %>% as.data.frame() %>% dplyr::count(source, sort = TRUE)

## HCAS filter ----

hcas <- terra::rast("/mnt/envshare/data/raster/hcas/HCAS33_AHC_VAST_2015_2024.tif")
state <- sfarrow::st_read_parquet("/mnt/envshare/data/vector/sa_br_dissolve.parquet") %>%
  sf::st_transform(crs = terra::crs(hcas)) %>%
  sf::st_make_valid()


# Cumulative pressure: croplands and built-up

hcas6_vec <- raster_to_landscape_vec(
  raster = hcas,
  mask_classes = 6,
  agg_fact = 5, # aggregate to ~ 450m res
  min_area_sqkm = 0.5, # remove polygons < 0.5 sqkm
  crop_extent = state,
  crop_buffer = 0 # using sa_br_dissolve
)

# Future impact
hcas6_vec <- terra::project(hcas6_vec, terra::crs(combined_vec))
overlaps <- terra::relate(combined_vec, hcas6_vec, relation = "intersects") %>% 
  apply(1, any)

futurepolys <- combined_vec[!overlaps,]
pastpolys <- combined_vec[overlaps,]

