# Functions to be used in Assignment 3: Data Visualization (incarceration)------

# Load necessary libraries
library(tidyverse)
library(maps)
library(mapproj)
library(patchwork)

# Load incarceration data from Vera Institute
load_incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
                               header = TRUE, stringsAsFactors = FALSE, quote = "")

# Function for Chart that Shows Trends Over Time -------------------------------

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

# Function for Chart that Shows Trends Over Time -------------------------------

# Filtered jail population for black inmates
black_pop_state <- load_incarceration %>%
  filter(!is.na(black_jail_pop) | !is.na(white_jail_pop)) %>%
  summarise(year, state, county_name, black_jail_pop, white_jail_pop)

# Get WA data
wa_black_pop <- black_pop_state %>%
  filter(state == "WA") %>%
  filter(year >= 1985) %>%
  group_by(year) %>%
  summarise(year = unique(year), wa_black_jail_pop = sum(black_jail_pop),
            wa_white_jail_pop = sum(white_jail_pop))

# Get MS data
ms_black_pop <- black_pop_state %>%
  filter(state == "MS") %>%
  group_by(year) %>%
  summarise(year = unique(year), ms_black_jail_pop = sum(black_jail_pop),
            ms_white_jail_pop = sum(white_jail_pop))

# Join WA and MS df
ms_wa_black_pop <- wa_black_pop %>%
  left_join(ms_black_pop, by = c("year"))

# Function for map -------------------------------------------------------------

# Filter for current (2018) data
current_data <- load_incarceration %>%
  filter(year == max(year))

# Utilize map function for county information
counties_shape <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

# Map of Washington
wa_map_data <- counties_shape %>%
  left_join(current_data, by = "fips") %>%
  filter(state == "WA")

# Map of Mississippi
ms_map_data <- counties_shape %>%
  left_join(current_data, by = "fips") %>%
  filter(state == "MS")

# Blank graph
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# Summary Information ----------------------------------------------------------

# WA change in incarceration of Black people over the past 20 years
wa_1998 <- ms_wa_black_pop %>%
  filter(year == 1998) %>%
  select(wa_black_jail_pop)

wa_2018 <- ms_wa_black_pop %>%
  filter(year == 2018) %>%
  select(wa_black_jail_pop)

change_wa_20 <- wa_2018 - wa_2008

# # MS change in incarceration of Black people over the past 20 years
ms_1998 <- ms_wa_black_pop %>%
  filter(year == 1998) %>%
  select(ms_black_jail_pop)

ms_2018 <- ms_wa_black_pop %>%
  filter(year == 2018) %>%
  select(ms_black_jail_pop)

change_ms_20 <- ms_2018 - ms_2008

# Most recent highest ratio of WA to MS jail population of Black people
state_highest_ratio_black <- ms_wa_black_pop %>%
  filter(year == max(year)) %>%
  mutate(wa_to_ms = wa_black_jail_pop / ms_black_jail_pop) %>%
  filter(wa_to_ms == max(wa_to_ms)) %>%
  pull(wa_to_ms)

# # Most recent highest ratio of WA to MS jail population of White people
state_highest_ratio_white <- ms_wa_black_pop %>%
  filter(year == max(year)) %>%
  mutate(wa_to_ms = wa_white_jail_pop / ms_white_jail_pop) %>%
  filter(wa_to_ms == max(wa_to_ms)) %>%
  pull(wa_to_ms)

# Which Washington state county had current highest jail population of Black
# people?
max_pop_WA <- current_data %>%
  filter(state == "WA") %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  pull(county_name)

# Which Mississippi county had current highest jail population of Black people?
max_pop_MS <- current_data %>%
  filter(state == "MS") %>%
  filter(!is.na(black_jail_pop)) %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  pull(county_name)

