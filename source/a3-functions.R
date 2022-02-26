# Functions to be used in Assignment 3: Data Visualization (incarceration)------

# Load necessary libraries
library(tidyverse)

# Load incarceration data from Vera Institute
load_incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
                               header = TRUE, stringsAsFactors = FALSE, quote = "")

# Filtered jail population data for BIPOC in current top 5 populous counties in
# WA (https://worldpopulationreview.com/us-counties/states/wa)
filter_jail_pop <- load_incarceration %>%
  filter(state == "WA") %>%
  filter(county_name == "King County" | county_name == "Pierce County" |
         county_name == "Snohomish County" | county_name == "Spokane County" |
         county_name == "Clark County") %>%
  replace_na(list(aapi_jail_pop = 0, black_jail_pop = 0, latinx_jail_pop = 0,
                  native_jail_pop = 0, other_race_jail_pop = 0)) %>%
  summarise(year, county_name, aapi_jail_pop, black_jail_pop, latinx_jail_pop,
            native_jail_pop, other_race_jail_pop)

# Filtered jail population for black inmates
black_pop_state <- load_incarceration %>%
  filter(!is.na(black_jail_pop)) %>%
  summarise(year, state, county_name, black_jail_pop)

# Get WA data
wa_black_pop <- black_pop_state %>%
  filter(state == "WA") %>%
  filter(year >= 1985) %>%
  group_by(year) %>%
  summarise(year = unique(year), wa_black_jail_pop = sum(black_jail_pop))

# Get MS data
ms_black_pop <- black_pop_state %>%
  filter(state == "MS") %>%
  group_by(year) %>%
  summarise(year = unique(year), ms_black_jail_pop = sum(black_jail_pop))

# Join WA and MS df
ms_wa_black_pop <- wa_black_pop %>%
  left_join(ms_black_pop, by = c('year'))




