
library(plotly)    
library(gapminder)
data("gapminder")

plot1 <- gapminder %>%
  filter(year == 2007) %>%
  plot_ly() %>%
  add_trace(
    
    x = ~gdpPercap, 
    y = ~lifeExp,
    
    type = 'scatter', 
    mode = 'markers+text',
    
    color = ~continent,
    colors = "Accent",
    
    symbol = ~continent,
    symbols = c("triangle-up", "diamond", "circle","square","triangle-down"),
    
    marker = list(
      size = ~pop,
      sizeref = 10000000,
      opacity = 0.5,
      line = list(color = "black", width = 2)
    ),
    
    text = ~country,
    textposition = "top",
    hovertemplate = paste(
      "<b>%{text}</b><br><br>",
      "%{yaxis.title.text}: %{y}<br>",
      "%{xaxis.title.text}: %{x}<br>",
      "Population: %{marker.size:,}",
      "<extra></extra>"
    )
    
  ) %>%
  layout(
    title = "Gapminder",
    xaxis = list(title = "GDP per capita"),
    yaxis = list(title = "Life expectancy")
  )


plot2 <- gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  mutate(strain = lifeExp/gdpPercap) %>%
  #summarise(strain = mean(strain)) %>%
  plot_ly() %>%
  add_markers(x = ~strain, y = ~forcats::fct_reorder(country, strain), hoverinfo = "strain") %>%
  layout(
    xaxis = list(title = "Years per dollar"),
    yaxis = list(title = "")
  ) 

chart <- subplot(plot2, plot1, widths = c(0.3, 0.7), titleX = TRUE) %>%
  layout(showlegend = FALSE)


chart
