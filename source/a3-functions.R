# Functions to be used in Assignment 3: Data Visualization (incarceration)------

# Load necessary libraries
library(tidyverse)

# Load incarceration data from Vera Institute
load_incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
                               header = TRUE, stringsAsFactors = FALSE, quote = "")

# Filtered jail population data for BIPOC
filter_jail_pop <- load_incarceration %>%
  filter(state == "WA") %>%
  filter(county_name == "King County" | county_name == "Pierce County" |
         county_name == "Snohomish County" | county_name == "Spokane County" |
         county_name == "Clark County") %>%
  replace_na(list(aapi_jail_pop = 0, black_jail_pop = 0, latinx_jail_pop = 0,
                  native_jail_pop = 0, other_race_jail_pop = 0)) %>%
  summarise(year, county_name, aapi_jail_pop, black_jail_pop, latinx_jail_pop,
            native_jail_pop, other_race_jail_pop)

# Filtered jail population for 
black_pop_state <- load_incarceration %>%
  filter(!is.na(black_jail_pop)) %>%
  summarise(year, state, county_name, black_jail_pop)
