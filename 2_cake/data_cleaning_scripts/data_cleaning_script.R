library(tidyverse)

cakes_raw <- read_csv("raw_data/cake-ingredients-1961.csv")
code_raw <- read_csv("raw_data/cake_ingredient_code.csv")

view(cakes_raw)
dim(cakes_raw)
names(cakes_raw)

view(code_raw)

spec(cakes_raw)

#what unit measurements do we have to worry about?
unique(code_raw$measure)

#We have an NA! It's in "Sour Cream Cup" - will convert to "one"
# - this will match all the other ingredients that are measured by units
# - It's best to hard-code this. If we just replace NA's and the 
# -- list is changed, it could convert things for which "one" is 
# -- inappropriate.

#Clean code_raw :
code_clean <- code_raw %>%
  mutate(measure = replace(measure, code == "SC", "one"))

#Clean and combine cakes_raw with code:

cakes_clean <- cakes_raw %>%
  #clean Cake name
  rename(cakes = Cake) %>%
  #pivot ingredients vertically
  pivot_longer(cols      = -"cakes",
               names_to  = "ingredient_code",
               values_to = "amount") %>%
  #join ingredients by code
  inner_join(code_clean, by = c("ingredient_code" = "code")) %>%
  drop_na(amount) %>%
  select(cakes, ingredient_code, ingredient, measure, amount)

write_csv(cakes_clean, "clean_data/cakes_clean.csv")
  
