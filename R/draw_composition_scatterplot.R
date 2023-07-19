#' @name draw_composition_scatterplot
#' @title Composition scatterplot
#' @author Nicolas Mangin
#' @description Function creating scatterplot showing the composition of a test.
#' @param composition Tibble. Table showing the composition of a test.
#' @return A ggplot object ready for rendering.
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_alpha
#' @importFrom ggplot2 scale_size
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom ggrepel geom_label_repel
#' @export



draw_composition_scatterplot <- function(composition){
  
  discrimination <- NULL
  points <- NULL
  guess <- NULL
  question <- NULL
  difficulty <- NULL
  
  composition |>
    ggplot2::ggplot(ggplot2::aes(x = difficulty, y = discrimination, size = points, alpha = 1-guess, label = question)) +
    ggrepel::geom_label_repel(nudge_y = 0.1) +
    ggplot2::xlim(0,10) +
    ggplot2::ylim(base::min(composition$discrimination-0.1,0), base::max(composition$discrimination+0.1)) +
    ggplot2::geom_vline(xintercept = 1, color = "red", lty = 2, size = 1.5) +
    ggplot2::geom_vline(xintercept = 9, color = "red", lty = 2, size = 1.5) +
    ggplot2::geom_hline(yintercept = 1, color = "red", lty = 2, size = 1.5) +
    ggplot2::scale_size(range = c(3,5)) +
    ggplot2::scale_alpha(range = c(0.5,1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}


