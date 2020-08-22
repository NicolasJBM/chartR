#' Apply a common theme to graphs generated with ggplot2
#' @param base_size       Numeric. Adjust size parameters.
#' @param base_family     Character. Font.
#' @param legend.position Character. "none", "bottom", "top", or vector of coordinates.
#' @param legend.box      Character. "horizontal" or "vertical".
#' @param axis            Logical. Whether axis should be dranw on the side.
#' @param base_line_size  Numeric. Adjust size parameters.
#' @param base_rect_size  Numeric. Adjust size parameters.
#' @return A formated graph.
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(chartR)
#' g <- iris %>%
#'   mutate_if(is.numeric, scale) %>%
#'   ggplot(aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
#'   geom_point() +
#'   geom_smooth(method = "lm") +
#'   scale_colour_grey() +
#'   scale_x_continuous(breaks = seq(-2, 10, 0.5)) + xlab("Setal") +
#'   scale_y_continuous(breaks = seq(-2, 10, 0.5)) + ylab("Pepal") +
#'   graph_theme(axis = FALSE, legend.position = "bottom")
#'   graph_place_axis(g, x = 0, y = 0, xunit = 0.5, yunit = 0.5)
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 rel
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 margin
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 element_blank
#' @importFrom grDevices rgb
#' @export


graph_theme <- function(base_size = 11,
                        base_family = "",
                        legend.position = "bottom",
                        legend.box = NULL,
                        axis = TRUE,
                        base_line_size = base_size / 22,
                        base_rect_size = base_size / 22) {
  
  half_line <- base_size / 2
  if (axis) axiscolour <- "grey50" else axiscolour = rgb(0, 0, 0, alpha = 0)
  
  theme(
    line = element_line(
      colour = "black", size = base_line_size,
      linetype = 1, lineend = "butt"
    ),
    rect = element_rect(
      fill = "white", colour = "black",
      size = base_rect_size, linetype = 1
    ),
    text = element_text(
      family = base_family, face = "plain",
      colour = "black", size = base_size,
      lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
      margin = margin(), debug = FALSE
    ),
    
    axis.line =          element_line(colour = axiscolour, size = rel(1)),
    axis.line.x =        element_line(colour = axiscolour, size = rel(1)),
    axis.line.y =        element_line(colour = axiscolour, size = rel(1)),
    axis.text =          element_text(size = rel(0.8), colour = "grey30"),
    axis.text.x =        element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top =    element_text(margin = margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y =        element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right =  element_text(margin = margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks =         element_line(colour = "grey50"),
    axis.ticks.length =  unit(half_line / 2, "pt"),
    axis.title.x =       element_text(
      margin = margin(t = half_line / 2),
      vjust = 1
    ),
    axis.title.x.top =   element_text(
      margin = margin(b = half_line / 2),
      vjust = 0
    ),
    axis.title.y =       element_text(
      angle = 90,
      margin = margin(r = half_line / 2),
      vjust = 1
    ),
    axis.title.y.right = element_text(
      angle = -90,
      margin = margin(l = half_line / 2),
      vjust = 0
    ),
    
    legend.background =  element_rect(colour = NA),
    legend.spacing =     unit(2 * half_line, "pt"),
    legend.spacing.x =    NULL,
    legend.spacing.y =    NULL,
    legend.margin =      margin(half_line, half_line, half_line, half_line),
    legend.key =         element_rect(fill = "white", colour = "white"),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.8)),
    legend.text.align =  NULL,
    legend.title =       element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position =    legend.position,
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         legend.box,
    legend.box.margin =  margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(2 * half_line, "pt"),
    
    panel.background =   element_rect(fill = "white", colour = NA),
    panel.border =       element_blank(),
    panel.grid =         element_line(colour = "grey80", linetype = 2),
    panel.grid.minor =   element_line(size = rel(1), linetype = 3),
    panel.spacing =      unit(half_line, "pt"),
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,
    
    strip.background =   element_rect(fill = "grey85", colour = NA),
    strip.text =         element_text(
      colour = "grey10",
      size = rel(0.8),
      margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
    ),
    strip.text.x =       NULL,
    strip.text.y =       element_text(angle = -90),
    strip.placement =    "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = unit(half_line / 2, "pt"),
    
    plot.background =    element_rect(colour = "white"),
    plot.title =         element_text( # font size "large"
      size = rel(1),
      hjust = 0, vjust = 1,
      margin = margin(b = half_line)
    ),
    plot.subtitle =      element_text( # font size "regular"
      hjust = 0, vjust = 1,
      margin = margin(b = half_line)
    ),
    plot.caption =       element_text( # font size "small"
      size = rel(1.2),
      hjust = 0, vjust = 1,
      margin = margin(t = half_line)
    ),
    plot.tag =           element_text(
      size = rel(1.2),
      hjust = 0.5, vjust = 0.5
    ),
    plot.tag.position =  'topleft',
    plot.margin =        margin(half_line, half_line, half_line, half_line),
    
    complete = TRUE
  )
}
