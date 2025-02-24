# Caitlin Rasbid
# February 23, 2025
## Create a faceted plot of the cumulative cases & deaths by USA region

library(tidyverse)
library(dplyr)
library(flextable)
library(tidyr)
#Part 1
url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
covid_data <- read.csv(url)

#Part 2
df_region <- data.frame(
  state = state.name,
  region = state.region,
  state_abbr = state.abb)

#Part 3
covid_region_data <- merge(covid_data, df_region, by.x = "state", by.y = "state", all.x = TRUE)

#Part 4
covid_region_data <- covid_region_data %>%
  group_by(region, date) %>%
  mutate(
    daily_cases = cases - lag(cases, default = 0),
    daily_deaths = deaths - lag(deaths, default = 0),
    cumulative_cases = cumsum(daily_cases),
    cumulative_deaths = cumsum(daily_deaths)
  )

#Part 5
covid_long <- covid_region_data %>%
  pivot_longer(cols = c(cumulative_cases, cumulative_deaths),
               names_to = "metric",
               values_to = "value")
#Part 6
library(ggplot2)
covid_long_filtered <- covid_long %>%
  filter(!is.na(region))
covid_long_filtered$date <- as.Date(covid_long_filtered$date, format = "%Y-%m-%d")
ggplot(covid_long_filtered, aes(x = date, y = value, color = region)) +
  geom_line() +
  facet_grid(metric ~ region, scales = "free_y") +
  labs(
    title = "Cumulative COVID-19 Cases and Deaths by Region",
    x = "Date",
    y = "Daily Cumulative Count",
    color = "Region"
  ) +
  scale_x_date(
    date_labels = "%b %d, %Y",
    date_breaks = "4 months"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
