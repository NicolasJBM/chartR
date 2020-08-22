library(dplyr)
library(tibble)
library(ggplot2)

p <- 0.05

base <- tibble::tibble(
  category = c('A','B','C','D','E','F','G','H'),
  estimate = c(10,12,20,50,30,40,70,50),
  std.error = c(10,2,6,9,2,8,12,22)
) %>%
  mutate(category = as.factor(category)) %>%
  mutate(category = forcats::fct_reorder(category, estimate))

chart <- base %>%
  ggplot(aes(x = category, y = estimate, group = 1, label = round(estimate,2))) +
  geom_line(lty = 2, size = 1) +
  geom_point(size = 5) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_errorbar(aes(ymin= estimate - std.error * qnorm(1-p/2),
                    ymax = estimate + std.error * qnorm(1-p/2)),
                width=.6,
                position=position_dodge(.9)) +
  geom_text(color = "grey51", nudge_x = 0.35) +
  geom_text(aes(x = category, y = estimate, group = 1, label = paste0('(',round(std.error,2), ')')), color = "grey51", nudge_x = -0.35) +
  coord_flip() +
  writR::graph_theme()
