#' @name draw_composition_barchart
#' @title Composition barchart
#' @author Nicolas Mangin
#' @description Function creating barchart showing the composition of a test.
#' @param composition Tibble. Table showing the composition of a test.
#' @param cat1 Character. First dimension selected.
#' @param categorical_values1 Tibble. Categories associated to the first dimension selected.
#' @return A ggplot object ready for rendering.
#' @importFrom dplyr full_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 element_text
#' @importFrom tidyr replace_na
#' @export


draw_composition_barchart <- function(composition, cat1, categorical_values1){
  
  category1 <- NULL
  value <- NULL
  cat2 <- NULL
  fact1 <- NULL
  
  categorical_values1 <- categorical_values1 |>
    dplyr::mutate(fact1 = base::factor(category1, levels = category1))
  
  composition |>
    dplyr::group_by(category1) |>
    dplyr::summarise(value = base::sum(value)) |>
    dplyr::full_join(categorical_values1, by = "category1") |>
    tidyr::replace_na(base::list(category1 = "", value = 0)) |>
    ggplot2::ggplot(ggplot2::aes(x = fact1, y = value, fill = value)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab(cat1) +
    ggplot2::ylab(cat2) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal()+
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 14),
      legend.position = "none"
    )
}

