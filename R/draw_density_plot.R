#' @name draw_density_plot
#' @title Edit diagrams
#' @author Nicolas Mangin
#' @description Module facilitating the quick exploration of bivariate relationships.
#' @param scores Tibble.
#' @param slctx Character. 
#' @param slcty Character. 
#' @param slctbkgalpha Double. 
#' @param slctpntalpha Double.
#' @param slctpntsize Double.
#' @return A set of graphs.
#' @importFrom shinyWidgets radioGroupButtons
#' @export



draw_density_plot <- function(scores, slctx, slcty, slctbkgalpha, slctpntalpha, slctpntsize){
  
  x <- NULL
  y <- NULL
  
  scores <- scores[,c(slctx, slcty)]
  base::names(scores) <- c("x","y")
  scores |>
    dplyr::mutate_all(base::as.numeric) |>
    tidyr::replace_na(base::list(x = 0, y = 0)) |>
    dplyr::filter(base::is.finite(x), is.finite(y)) |>
    ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_density2d_filled(show.legend = FALSE, alpha = slctbkgalpha) +
    ggplot2::geom_point(color = "black", alpha = slctpntalpha, size = slctpntsize) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "red", linewidth = 1) +
    ggplot2::xlab(slctx) +
    ggplot2::ylab(slcty) +
    ggplot2::theme_minimal()
}

