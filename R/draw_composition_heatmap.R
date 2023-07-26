#' @name draw_composition_heatmap
#' @title Composition heatmap
#' @author Nicolas Mangin
#' @description Function creating heatmap showing the composition of a test.
#' @param composition Tibble. Table showing the composition of a test.
#' @param cat1 Character. First dimension selected.
#' @param cat2 Character. Second dimension selected.
#' @param categorical_values1 Tibble. Categories associated to the first dimension selected.
#' @param categorical_values2 Tibble. Categories associated to the second dimension selected.
#' @return A ggplot object ready for rendering.
#' @importFrom dplyr full_join
#' @importFrom dplyr group_by
#' @importFrom dplyr right_join
#' @importFrom dplyr summarise
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom tidyr replace_na
#' @export


draw_composition_heatmap <- function(composition, cat1, cat2, categorical_values1, categorical_values2){
  
  category1 <- NULL
  category2 <- NULL
  value <- NULL
  success <- NULL
  fact1 <- NULL
  fact2 <- NULL
  
  categorical_values1 <- categorical_values1 |>
    dplyr::mutate(fact1 = base::factor(category1, levels = category1))
  categorical_values2 <- categorical_values2 |>
    dplyr::mutate(fact2 = base::factor(category2, levels = category2))
  
  composition |> 
    dplyr::group_by(category1, category2) |>
    dplyr::summarise(value = base::sum(value), .groups = "keep") |>
    dplyr::right_join(categorical_values1, by = "category1") |>
    dplyr::full_join(categorical_values2, by = "category2") |>
    tidyr::replace_na(base::list(category1 = "", category2 = "", value = 0)) |>
    ggplot2::ggplot(ggplot2::aes(x=fact1, y=fact2, fill=value)) + 
    ggplot2::geom_tile() +
    ggplot2::xlab(cat1) +
    ggplot2::ylab(cat2) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text = ggplot2::element_text(size = 14),
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust=1)
    )
}




