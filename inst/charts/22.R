library(tidyverse)
library(ggmap)
library(maps)


# https://www.latlong.net/

locations <- tibble::tibble(
  city = c(
    "Amsterdam",
    "Tokyo",
    "Chicago",
    "Vancouver",
    "Madrid"),
  lat = c(
    52.3702163,
    35.689487,
    41.878113,
    49.282730,
    40.416775
  ),
  lon = c(
    4.895168,
    139.691711,
    -87.629799,
    -123.120735,
    -3.703790
  )
)

travels <- tibble::tibble(
  origin = c("Amsterdam","Amsterdam","Amsterdam","Amsterdam"),
  destination = c("Tokyo","Chicago","Vancouver","Madrid")
) %>%
  left_join(
    select(
      locations,
      origin = city,
      lato = lat,
      lono = lon
    ),
    by = "origin"
  ) %>%
  left_join(
    select(
      locations,
      destination = city,
      latd = lat,
      lond = lon
    ),
    by = "destination"
  ) %>%
  mutate(curve = case_when(
    lono > lond ~ -0.5,
    lono > lond ~ 0.5,
    TRUE ~ 0.1
  ))

world <- borders("world", colour="gray71", fill="gray71")

chart <- ggplot() +
  world +
  geom_point(data = locations, aes(x=lon, y=lat), color="black", size=5) +
  geom_curve(data = travels, aes(x = lono, xend = lond, y = lato, yend = latd),
             color = "black", curvature = 0.5, lwd = 1.2, angle = 60) +
  geom_text(data = locations, aes(x=lon, y=lat, label=city), color = "black", nudge_x = -30) +
  ggthemes::theme_map()
