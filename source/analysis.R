# Assignment 3: Data Visualization (incarceration) -----------------------------
# 
# Use your data analysis and visualization skills to expose patterns of 
# inequality using incarceration data collected by the Vera Institute.

# Function for Chart that Shows Trends Over Time
population_data <- function(data) {
  race_county_data <- data %>%
  filter(year >= 1984) %>%
  mutate(bipoc_jail_pop = rowSums(select(., "aapi_jail_pop", "black_jail_pop",
                                        "latinx_jail_pop", "native_jail_pop",
                                        "other_race_jail_pop"))) %>%
  summarise(year, county_name, bipoc_jail_pop)
}

# Line plot for Chart that Shows Trends Over Time
bipoc_pop_scatter <- function(data) {
  ggplot(data = data, aes(x = year, y = bipoc_jail_pop)) +
    
    scale_x_continuous(breaks = seq(1984, 2018, by = 2)) +
    scale_y_continuous(breaks = seq(0, 1600, by = 100)) +
    
    geom_line(aes(color = county_name), size = 1) +

    labs(title = "The Relationship Between County and People of Color Jail
                  Population",
         x = "Year",
         y = "Population",
         color = "Top 5 Populous County \n (not in order)") +
    
    theme(axis.text.x = element_text(angle = 90),
          panel.grid.minor = element_blank())
}
