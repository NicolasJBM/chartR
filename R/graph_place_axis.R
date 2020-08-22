#' Add axis at the specified coordinate on a ggplot2 graph
#' @param graph ggplot2 graph.
#' @param x     Numeric. Where the x axis should appear.
#' @param y     Numeric. Where the y axis should appear.
#' @param xunit Numeric. Units of the x axis (for breaks) 
#' @param yunit Numeric. Units of the y axis (for breaks) 
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
#' @importFrom ggplot2 ggplotGrob
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggplot_build
#' @importFrom dplyr filter
#' @importFrom plyr round_any
#' @export


graph_place_axis <- function(graph, x = 0, y = 0, xunit = 0.5, yunit = 0.5){
  
  # Bind variables
  xt <- NULL
  yt <- NULL
  lab <- NULL
  
  xrange <- plyr::round_any(ggplot_build(graph)$layout$panel_scales_x[[1]]$range$range, xunit)
  xticks <- seq(xrange[1], xrange[2], xunit)
  yrange <- plyr::round_any(ggplot_build(graph)$layout$panel_scales_y[[1]]$range$range, yunit)
  yticks <- seq(yrange[1], yrange[2], yunit)
  
  xy <- data.frame(
    xt = c(xticks, rep(-xunit/4, length(yticks))),
    yt = c(rep(-yunit/4, length(xticks)), yticks),
    lab = c(xticks, yticks)
  ) %>%
    dplyr::filter(xt != x, yt != y)
  
  new_graph <- graph +
    geom_vline(aes(xintercept=x), colour = "grey50") +
    geom_hline(aes(yintercept=y), colour = "grey50") +
    annotate(geom = "text", x = xy$xt, y = xy$yt, label = xy$lab, colour = "grey50")
  
  return(new_graph)
}
