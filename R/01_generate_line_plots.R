library(ggplot2)

plot_life_expectancy <- function(df, country, outdir="results") {
  lifeExp_plot <- ggplot(data = df[df$country == country,], mapping = aes(x = year, y = lifeExp)) +
    geom_line()
  ggsave(filename = paste0(outdir, "/", country, "_lifeExp.png"), plot = lifeExp_plot, width = 12, height = 10, dpi = 300, units = "cm")
}

gapminder <- read.csv('gapminder_data.csv')
americas <- gapminder[gapminder$continent == "Americas",]

for (country in unique(americas$country)) {
  plot_life_expectancy(americas, country)
}
