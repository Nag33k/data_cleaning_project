library(tidyverse)
library(janitor)
library(readxl)
library(tidyselect)
library(here)

#start by loading in data

raw_2015 <- read_xlsx(here::here("raw_data/boing-boing-candy-2015.xlsx"))
raw_2016 <- read_xlsx(here::here("raw_data/boing-boing-candy-2016.xlsx"))
raw_2017 <- read_xlsx(here::here("raw_data/boing-boing-candy-2017.xlsx"))

#-------------------------------------------------------------------------

#clean one year at a time. Start with 2015.

#extract ratings and candies - no gender or country fields:

clean_2015_candies <- raw_2015 %>%
  select(starts_with("[")) %>%
  #use clean_names now, after taking advantage of bracket for selection
  clean_names() %>%
  pivot_longer(cols           = everything(), 
               names_to       = "candy_type",
               values_to      = "rating",
               #including drop_na as no brief questions need blanks
               values_drop_na = TRUE) %>%
  #add columns for year, country, and gender
  mutate("year"    = 2015,
         "gender"  = "NOT AVAILABLE",
         "country" = "NOT AVAILABLE") %>%
  select(candy_type, year,country, gender, rating)
  
#---------------------------clean 2016----------------------------------

clean_2016_candies <- raw_2016 %>%
  #extract only necesary columns
  select("Your gender:", 
         "Which country do you live in?", 
         starts_with("[")) %>%
  #clean names after utilizing bracket for selection
  clean_names() %>%
  #clean col names for not candies
  rename("gender"  = "your_gender",
         "country" = "which_country_do_you_live_in") %>%
  #pivot ratings
  pivot_longer(cols           = -c("gender", "country"),
               names_to       = "candy_type",
               values_to      = "rating",
               #again, no brief questions want NA ratings - drop them:
               values_drop_na = TRUE) %>%
  #add year
  mutate("year" = 2016) %>%
  select(candy_type, year,country, gender, rating)

#--------------------------clean 2017-----------------------------------

clean_2017_candies <- raw_2017 %>%
  clean_names() %>%
  select("q2_gender",
         "q4_country",
         starts_with("q6")) %>%
  #clean col names for not-candies
  rename("gender"  = "q2_gender",
         "country" = "q4_country") %>%
  #pivot ratings
  pivot_longer(cols           = -c("gender", "country"),
               names_to       = "candy_type",
               values_to      = "rating",
               values_drop_na = TRUE) %>%
  #remove "q6_" from candy names and add year column
  mutate(candy_type = substring(candy_type, 4),
         "year"     = 2017) %>%
  select(candy_type, year,country, gender, rating)

#combine all years

years_combined <- clean_2015_candies %>%
  bind_rows(clean_2016_candies) %>%
  bind_rows(clean_2017_candies)


#-----------------------------------------------------------------------
#cleaning fields

#start with cleaning candy_type. Use the following to check unique types
#alphabetically
candy_names_cleaned <- years_combined %>%
  mutate(candy_type = recode(candy_type, 
                             "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes" =
                               "anonymous_brown_globs_that_come_in_black_and_orange_wrappers",
                             #note: 2015 listed "bonkers", but 2016 and 2017 split between board 
                             #game and candy. Will re-code 2015 as candy, as it is a candy 
                             #questionnaire in the first place.
                             "bonkers"      = "bonkers_the_candy",
                             "boxo_raisins" = "box_o_raisins",
                             #updating 2015's "licorice" to 2016 and 
                             # 2017's "licorice_yes_black'. This is because
                             #2015 also had "licorice_yes_black".
                             "licorice"     = "licorice_yes_black",
                             "sweetums_a_friend_to_diabetes" = "sweetums",
                             "x100_grand_bar" = "100_grand_bar"))

sort(unique(candy_names_cleaned$candy_type))

#clean country:

candy_cleaned <- candy_names_cleaned %>%
  #push all to upper-case to remove case issues
  mutate(country = str_to_upper(country)) %>%
  mutate(country = str_replace(country, ".*USA.*", "USA")) %>%
  mutate(country = str_replace(country, "UNITED S.*", "USA")) %>%
  mutate(country = str_replace(country, "U S.*", "USA")) %>%
  mutate(country = str_replace(country, "UNITED K.*", "UK")) %>%
  mutate(country = str_replace(country, "UNITED  ST.*", "USA")) %>%
  mutate(country = str_replace(country, "U.S.*", "USA")) %>%
  mutate(country = str_replace(country, "THE YOO ESS.*", "USA")) %>%
  mutate(country = str_replace(country, "SUB-CANAD.*", "USA")) %>%
  mutate(country = str_replace(country, "I PRETEND .*", "USA")) %>%
  mutate(country = str_replace(country, "CAN.*", "CANADA")) %>%
  mutate(country = str_replace(country, "A TROPICAL.*", "NOT AVAILABLE")) %>%
  mutate(country = str_replace(country, "[0-9].*", "NOT AVAILABLE")) %>%
  mutate(country = str_replace(country, ".*ENGLAND.*", "UK")) %>%
  mutate(country = recode(country, "USSA" = "USA",
                          "US OF A"       = "USA",
                          "US"            = "USA",
                          "UNITS STATES"  = "USA",
                          "UNITES STATES" = "USA",
                          "TRUMPISTAN"    = "USA",
                          "THIS ONE"      = "NOT AVAILABLE",
                          "THERE ISN'T ONE FOR OLD MEN" = 
                            "NOT AVAILABLE",
                          "THE USA"       = "USA",
                          "THE REPUBLIC OF CASCADIA" = "CASCADIA",
                          "SUSA"          = "USA",
                          "SOMEWHERE"     = "NOT AVAILABLE",
                          "SEE ABOVE"     = "NOT AVAILABLE",
                          "PITTSBURGH"    = "USA",
                          "ONE OF THE BEST ONES" = "NOT AVAILABLE",
                          "NORTH CAROLINA" = "USA",
                          "NEW YORK"      = "USA",
                          "NEW JERSEY"    = "USA",
                          "NEVERLAND"     = "NOT AVAILABLE",
                          "NETHERLANDS"   = "THE NETHERLANDS",
                          "NARNIA"        = "NOT AVAILABLE",
                          "N. AMERICA"    = "NOT AVAILABLE",
                          "MURRIKA"       = "USA",
                          "MURICA"        = "USA",
                          "MERICA"        = "USA",
                          #ASSUMPTION: Moving "KOREA" to "SOUTH KOREA", as it is 
                          #unlikely that citizens of North Korea would 
                          #have been able to participate
                          "KOREA"         = "SOUTH KOREA",
                          "INSANITY LATELY" = "NOT AVAILABLE",
                          "I DON'T KNOW ANYMORE" = "NOT AVAILABLE",
                          "GOD'S COUNTRY" = "NOT AVAILABLE",
                          "FEAR AND LOATHING" = "NOT AVAILABLE",
                          "EUROPE"        = "NOT AVAILABLE",
                          "EUA"           = "USA",
                          "ESPAÑA"        = "SPAIN",
                          "ENDLAND"       = "UK",
                          "EARTH"         = "NOT AVAILABLE",
                          "DENIAL"        = "NOT AVAILABLE",
                          "CALIFORNIA"    = "USA",
                          "ATLANTIS"      = "NOT AVAILABLE",
                          "AMERICA"       = "USA",
                          "ALASKA"        = "USA",
                          "AHEM....AMERCA" = "USA",
                          "A"             = "NOT AVAILABLE",
                          "'MERICA"       = "USA",
                          #merging England and Scotland to UK. Only other
                          #option would be to mark "UK" as "NOT AVAILABLE"
                          "SCOTLAND"      = "UK",
                          "ENGLAND"       = "UK"
                          ))
  
sort(unique(candy_cleaned$country))  

#write to csv

write_csv(candy_cleaned, here::here("clean_data/candy_cleaned.csv"))

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
  rename("age"   = "q3_age",
         "going" = "q1_going_out") %>%
  select("age", "going") %>%
  mutate(age     = as.numeric(age))

#combine and clean:

going_total <- clean_2015_going %>%
  bind_rows(clean_2016_going) %>%
  bind_rows(clean_2017_going) %>%
  #dropping NA's, as imputing them to the mean would artificially skew
  #the results towards the center.
  drop_na(age) %>%
  #removing all ages over 122, as the oldest person whose age has been
  #verified lived to be 122.
  filter(age <= 122)




write_csv(going_total, here::here("clean_data/going_total.csv"))