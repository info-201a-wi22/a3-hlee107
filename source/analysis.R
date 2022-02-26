# Assignment 3: Data Visualization (incarceration) -----------------------------
#
# Use your data analysis and visualization skills to expose patterns of
# inequality using incarceration data collected by the Vera Institute.

# Summary Information ----------------------------------------------------------

# 
summary_list <- function(data) {
  info_list <- list()
  
}

# Function for Chart that Shows Trends Over Time -------------------------------
# Start at year 1984 because there is no data before that year for the top 5
# populous counties in WA.
# Sum each row to get the total of BIPOC community
population_data <- function(data) {
  race_county_data <- data %>%
  filter(year >= 1984) %>%
  mutate(bipoc_jail_pop = rowSums(select(., "aapi_jail_pop", "black_jail_pop",
                                        "latinx_jail_pop", "native_jail_pop",
                                        "other_race_jail_pop"))) %>%
  summarise(year, county_name, bipoc_jail_pop)
}

# Line plot for Chart that Shows Trends Over Time ------------------------------
bipoc_pop_line <- function(data) {
  ggplot(data = data, aes(x = year, y = bipoc_jail_pop)) +

    scale_x_continuous(breaks = seq(1984, 2018, by = 2)) +
    scale_y_continuous(breaks = seq(0, 1600, by = 100)) +

    geom_line(aes(color = county_name), size = 1) +

    labs(title = "The Relationship Between County and BIPOC* Jail Population",
         caption = "* For this chart, indigenous people were categorized under
         native population in the incareration csv file, and people of color
         were categorized as AAPI, Latinx, and other race in the csv file.",
         x = "Year",
         y = "Population",
         color = "Top 5 Populous County \n (not in order)") +

    theme(axis.text.x = element_text(angle = 90),
          panel.grid.minor = element_blank())
}

# Function for Chart that Compares Two Variables -------------------------------
# Compare WA state and the most conservation state in the country, Mississippi
# for their jail population of black people over the years. Start at year 1984
# because there is no data before that year.
# (https://worldpopulationreview.com/state-rankings/most-conservative-states)
state_black_scatter <- function(data_2) {
  ggplot(data_2) +

    scale_x_continuous(breaks = seq(1984, 2018, by = 2)) +
    scale_y_continuous(breaks = seq(0, 8500, by = 500)) +

    geom_point(aes(x = year, y = wa_black_jail_pop, color = "blue")) +
    geom_point(aes(x = year, y = ms_black_jail_pop, color = "red")) +

    labs(title = "Comparison between Washington and Mississippi Jail Population
         of Black People",
         x = "Year",
         y = "Population",
         color = "State") +

    scale_color_manual(labels = c("Washington", "Mississippi"),
                       values = c("blue", "red")) +

    theme(axis.text.x = element_text(angle = 90))
}

# Map Function -----------------------------------------------------------------

# WA jail population of Black people
wa_black_map <- function(data_3) {
  ggplot(data_3) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "gray", size = 0.3
  ) +

  coord_map() +

  scale_fill_continuous(limits = c(0, max(wa_map_data$black_jail_pop)),
                        na.value = "white", low = "yellow", high = "red") +

  labs(fill = "Jailed Population \n of Black People") +

  blank_theme +

  ggtitle("Washington State Jail Population of Black People")
}

# WA jail population of White people
wa_white_map <- function(data_3) {
  ggplot(data_3) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = white_jail_pop),
    color = "gray", size = 0.3
  ) +

  coord_map() +

  scale_fill_continuous(limits = c(0, max(wa_map_data$white_jail_pop)),
                        na.value = "white", low = "yellow", high = "red") +

  labs(fill = "Jailed Population \n of White People") +

  blank_theme +

  ggtitle("Washington State Jail Population of White People")
}

# MS jail population of Black people
ms_black_map <- function(data_4) {
  ggplot(data_4) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "gray", size = 0.3
  ) +

  coord_map() +

  scale_fill_continuous(limits = c(0, max(ms_map_data$black_jail_pop)),
                        na.value = "white", low = "yellow", high = "red") +

  labs(fill = "Jailed Population \n of Black People") +

  blank_theme +

  ggtitle("Mississippi State Jail Population of Black People")
}

# MS jail population of White people
ms_white_map <- function(data_4) {
  ggplot(ms_map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = white_jail_pop),
    color = "gray", size = 0.3
  ) +

  coord_map() +

  scale_fill_continuous(limits = c(0, max(ms_map_data$white_jail_pop)),
                        na.value = "white", low = "yellow", high = "red") +

  labs(fill = "Jailed Population \n of White People") +

  blank_theme +

  ggtitle("Mississippi State Jail Population of White People")
}
