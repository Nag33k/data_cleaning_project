library(tidyverse)
library(janitor)

#read in data and examine

raw <- read_rds("raw_data/decathlon.rds")

view(raw)

str(raw)

dim(raw)

names(raw)

top_n(raw, 10)










#clean data with pipe:
decathlon_tidy <- raw %>%
  # -pull out rownames
  rownames_to_column() %>%
  # -select only relevant columns
  select("rowname", 
         "Long.jump", 
         "100m", 
         "Points", 
         "Shot.put", 
         "Rank",
         "Competition", 
         "400m") %>%
  # -convert names to snake case, both for easier use as columns and to
  #   more easily identify doubles
  mutate("rowname" = str_to_lower(rowname)) %>%
  # -clean column names
  clean_names() %>%
  rename("name" = "rowname") %>%
  # -flip scores to long form
  pivot_longer(cols = c("long_jump", 
                        "x100m", 
                        "shot_put", 
                        "x400m",
                        "points"),
               names_to = "event",
               values_to = "scores")
  

view(decathlon_tidy)

write_csv(decathlon_tidy, "clean_data/decathlon_tidy.csv")
