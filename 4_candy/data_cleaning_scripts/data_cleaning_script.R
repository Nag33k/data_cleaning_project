library(tidyverse)
library(janitor)
library(readxl)
library(tidyselect)

#start by loading in data

raw_2015 <- read_xlsx("raw_data/boing-boing-candy-2015.xlsx")
raw_2016 <- read_xlsx("raw_data/boing-boing-candy-2016.xlsx")
raw_2017 <- read_xlsx("raw_data/boing-boing-candy-2017.xlsx")

#clean one year at a time. Start with 2015.

#cannot use clean_names as it removes formatting that allows for quick
#selection of candies for pivoting. Could hard-code by column numbers,
#but this is less flexible.

#extract ratings and candies - no gender or country fields:

clean_2015_candies <- raw_2015 %>%
  select(starts_with("[")) %>%
  clean_names() %>%
  pivot_longer(cols = everything(), 
               names_to       = "candy_type",
               values_to      = "rating",
               #including drop_na as no brief questions need blanks
               values_drop_na = TRUE) %>%
  mutate("year" = 2015) %>%
  select(candy_type, year, rating)
  


#-----------------extracting ages and going/not-going-------------------

#For 2015:

clean_2015_going <- raw_2015 %>%
  clean_names() %>%
  select("how_old_are_you", 
         "are_you_going_actually_going_trick_or_treating_yourself") %>%
  rename("age" = "how_old_are_you",
         "going" = 
           "are_you_going_actually_going_trick_or_treating_yourself") %>%
  mutate(age = as.numeric(age))

#For 2016:

clean_2016_going <- raw_2016 %>%
  clean_names() %>%
  select("how_old_are_you", 
         "are_you_going_actually_going_trick_or_treating_yourself") %>%
  rename("age" = "how_old_are_you",
         "going" = 
           "are_you_going_actually_going_trick_or_treating_yourself") %>%
  mutate(age = as.numeric(age))

#For 2017:

#ASSUMPTION: "q1_going_out" is the same question as "going trick or 
#treating

clean_2017_going <- raw_2017 %>%
  clean_names() %>%
  rename("age" = "q3_age",
         "going" = "q1_going_out") %>%
  select("age", "going") %>%
  mutate(age = as.numeric(age))

#combine and export - will do analysis of outliers and NA's as part of 
#answers in analysis markdown file:

going_total <- clean_2015_going %>%
  bind_rows(clean_2016_going) %>%
  bind_rows(clean_2017_going)

write_csv(going_total, "clean_data/going_total.csv")