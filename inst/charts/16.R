library(dplyr)
library(ggplot2)

chart <- data.frame(
  Variances = c('Budgeted\nprofit','Market\nsize\nvariance','Market\nshare\nvariance','Selling\nprice\nvariance',
                'Material\nusage\nvariance','Material\nprice\nvariance','Labor\nusage\nvariance','Labor\nprice\nvariance',
                'Fixed\ncosts\nvariance', 'Actual\nprofit'),
  Amounts = c(1000, 500, -200, -150, 100, -300, -400, 300, -100, (1000+500-200-150+100-300-400+300-100))
) %>%
  mutate(
    Variances = factor(Variances, levels = Variances),
    id = seq_along(Amounts),
    type = case_when(
      Amounts >= 0 ~ 'Favorable',
      Amounts < 0  ~ 'Unfavorable'
    )
  ) %>%
  mutate(type = case_when(
    Variances == 'Budgeted\nprofit' ~ 'Budget',
    Variances == 'Actual\nprofit' ~ 'Actual',
    TRUE ~ type
  )) %>%
  mutate(
    type = factor(type, levels = c('Budget','Favorable','Unfavorable','Actual')),
    end = cumsum(Amounts)
  ) %>%
  mutate(
    end = c(head(end, -1), 0),
    start = c(0, head(end, -1)),
    label = case_when(
      type == 'Favorable' ~ paste(abs(Amounts),' (F)'),
      type == 'Unfavorable' ~ paste(abs(Amounts), '(U)'),
      TRUE ~ as.character(Amounts)
    )
  ) %>%
  mutate(position = purrr::map2_dbl(start, end, max)  + 50) %>%
  ggplot(aes(Variances, fill = type)) +
  geom_rect(aes(x = Variances, xmin = id - 0.45, xmax = id + 0.45, ymin = end, ymax = start)) +
  scale_fill_manual(values=c('darkblue','palegreen','pink','darkred')) +
  geom_text(aes(x = Variances, y = position, label = label)) +
  ylab('Amounts') +
  writR::graph_theme()
