library(dplyr)
library(tibble)
library(ggplot2)

p <- 0.05

chart <- tibble::tibble(
  value = c(1:100)
) %>%
  mutate(
    estimate = (value - 50)/25,
    std.error = 0.1 + (abs(value - 50)/100)^2
  ) %>%
  mutate(
    lower_bound = estimate - std.error * qnorm(1-p/2),
    upper_bound = estimate + std.error * qnorm(1-p/2)
  ) %>%
  ggplot() +
  geom_smooth(aes(x = value, y = estimate), lty = 1, color = 'black', lwd = 1, method = 'loess') +
  geom_smooth(aes(x = value, y = lower_bound), lty = 1, color = 'black', lwd = 0.5, method = 'loess') +
  geom_smooth(aes(x = value, y = upper_bound), lty = 1, color = 'black', lwd = 0.5, method = 'loess') +
  geom_hline(yintercept = 0, lty = 2) +
  writR::graph_theme()
