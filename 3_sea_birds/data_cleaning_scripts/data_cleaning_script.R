#first things first: libraries, import data
library(tidyverse)
library(readxl)
library(janitor)

ships_raw <- read_excel("raw_data/seabirds.xls", sheet = 1)
birds_raw <- read_excel("raw_data/seabirds.xls", sheet = 2)

#clean ship data: clean names, select only record ID and latitude 
ships_raw <- clean_names(ships_raw)

ships_clean <- ships_raw %>%
  select(record_id, lat)

#clean bird data: clean names, select only bird names and counts
birds_raw <- clean_names(birds_raw)

birds_clean <- birds_raw %>%
  rename("common_name" = 
           "species_common_name_taxon_age_sex_plumage_phase",
         "scientific_name" = 
           "species_scientific_name_taxon_age_sex_plumage_phase",
         "abbreviation" = "species_abbreviation") %>%
  select(record_id, common_name, scientific_name, abbreviation, count)

#join tibbles and export

#note: not terribly worried about NA's, as the brief does not ask for
#  calculations. Just do full join, and filter as necessary for 
#  individual questions. There are bird observations that say [NO BIRDS
#  RECORDED], but again, this can be handled as needed per question.

obs_clean <- ships_clean %>%
  full_join(birds_clean, by = "record_id")

write_csv(obs_clean, "clean_data/obs_clean.csv")