library(ggplot2)
library(dplyr)

base <- ggplot2::diamonds %>%
  dplyr::select(cut, color) %>%
  dplyr::mutate(cut = as.character(cut), color = as.character(color)) %>%
  dplyr::group_by(cut, color) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(cut) %>%
  tidyr::nest() %>%
  dplyr::mutate(data = purrr::map(
    data,
    function(x) dplyr::bind_rows(arrange(x, count), tibble::tibble(color = rep('',3), count = rep(0,3)))
  )) %>%
  tidyr::unnest() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(position = as.numeric(rownames(.))) %>%
  dplyr::mutate(angle = 90 - 360 * (position-0.5) / max(position)) %>%
  dplyr::mutate(angle2 = dplyr::case_when(angle > -90 ~ angle, TRUE ~ angle + 180))

base_groups <- base %>%
  filter(color != '') %>%
  select(cut, position, angle) %>%
  group_by(cut) %>%
  summarise(start = min(position), end = max(position), angle = mean(angle)) %>%
  mutate(
    angle2 = dplyr::case_when(angle > -90 ~ angle, TRUE ~ angle + 180),
    title = (start+end) / 2
  )

chart <- ggplot2::ggplot(base) +
  ggplot2::geom_segment(aes(x = min(base$position), y = 1000, xend = max(base$position), yend = 1000), color = 'grey71', lty = 2) +
  ggplot2::geom_segment(aes(x = min(base$position), y = 2000, xend = max(base$position), yend = 2000), color = 'grey71', lty = 2) +
  ggplot2::geom_segment(aes(x = min(base$position), y = 3000, xend = max(base$position), yend = 3000), color = 'grey71', lty = 2) +
  ggplot2::geom_segment(aes(x = min(base$position), y = 4000, xend = max(base$position), yend = 4000), color = 'grey71', lty = 2) +
  ggplot2::geom_segment(aes(x = min(base$position), y = 5000, xend = max(base$position), yend = 5000), color = 'grey71', lty = 2) +
  ggplot2::annotate(
    'text',
    x = rep(min(base$position),5),
    y = c(1000,2000,3000,4000,5000),
    label = c('1000','2000','3000','4000','5000'),
    color='grey41',
    size=3,
    angle=0,
    fontface='bold',
    hjust=1) +
  ggplot2::geom_bar(
    data = base,
    aes(x = position, y = count, group = color, fill = color),
    position = 'dodge',
    stat = 'identity',
    alpha = 0.75) +
  ggplot2::ylim(-2500,5500) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = 'none',
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), 'cm') 
  ) +
  ggplot2::coord_polar() +
  ggplot2::geom_text(aes(x = position, y = count+500, label = color, angle = angle2)) +
  ggplot2::geom_segment(
    data=base_groups,
    aes(x = start, y = -250, xend = end, yend = -250),
    colour = 'black',
    alpha=0.8,
    size=0.6 ,
    inherit.aes = FALSE )  +
  ggplot2::geom_text(
    data=base_groups,
    aes(x = title, y = -500, label=cut, angle =angle2),
    hjust=c(1,1,1,0,0),
    colour = 'black',
    alpha=0.8,
    size=3,
    fontface='bold',
    inherit.aes = FALSE)
