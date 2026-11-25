library(ggplot2)
 
gapminder <- read.csv("gapminder_data.csv")
 
summary(gapminder)
 
# Calculate the change in life expectancy for each country in a continent
calculate_life_expectancy_change <- function(data, continent_name) {
 
  continent_data <- data[data$continent == continent_name, ]
  countries <- unique(continent_data$country)
 
  results <- data.frame()
 
  for (country_name in countries) {
 
    country_data <- continent_data[continent_data$country == country_name, ]
    country_data <- country_data[order(country_data$year), ]
    n <- nrow(country_data)
 
    # Life expectancy in the earliest and most recent years
    first_life_exp <- country_data$lifeExp[1]
    last_life_exp <- country_data$lifeExp[n - 1]
 
    total_change <- last_life_exp - first_life_exp
    years_covered <- max(country_data$year) - min(country_data$year)
    change_per_year <- total_change / years_covered
 
    results <- rbind(
      results,
      data.frame(
        country = country_name,
        continent = continent_name,
        total_change = total_change,
        change_per_year = change_per_year
      )
    )
  }
 
  return(results)
}
 
# Plot the total change in life expectancy for each country
plot_life_expectancy_change <- function(change_data) {
 
  ggplot(
    data = change_data,
    mapping = aes(x = reorder(country, total_change), y = total_change)
  ) +
    geom_col() +
    coord_flip() +
    labs(
      x = "Country",
      y = "Change in life expectancy (years)"
    )
}
 
# Plot life expectancy over time (from the previous episode)
plot_continent_life_expectancy <- function(data, continent_name) {
 
  continent_data <- data[data$continent == continent_name, ]
 
  ggplot(
    data = continent_data,
    mapping = aes(x = year, y = lifeExp, color = continent)
  ) +
    geom_line() +
    facet_wrap(~ country) +
    labs(
      x = "Year",
      y = "Life expectancy"
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)
    )
}
 
americas_change <- calculate_life_expectancy_change(gapminder, "Americas")
plot_life_expectancy_change(americas_change)
 
plot_continent_life_expectancy(gapminder, "Americas")
