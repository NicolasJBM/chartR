#' @name draw_score_differences
#' @title Edit diagrams
#' @author Nicolas Mangin
#' @description Module facilitating the quick exploration of bivariate relationships.
#' @param scores Tibble.
#' @param selection Character vector
#' @return A set of graphs.
#' @importFrom forcats fct_reorder
#' @importFrom shinyWidgets radioGroupButtons
#' @export



draw_score_differences <- function(scores, selection){
  
  average <- NULL
  group <- NULL
  lower <- NULL
  score <- NULL
  student <- NULL
  upper <- NULL
  value <- NULL
  
  scores <- scores |>
    dplyr::select_if(base::is.numeric) |>
    tibble::rownames_to_column("student") |>
    dplyr::mutate_if(base::is.numeric, function(x) base::as.numeric(base::scale(x)))
  
  col <- base::setdiff(base::names(scores), "student")
  
  scores <- scores |>
    tidyr::pivot_longer(dplyr::all_of(col), names_to = "score", values_to = "value")
  
  scores |>
    dplyr::filter(student %in% selection) |>
    dplyr::mutate(score = base::as.factor(score)) |>
    dplyr::group_by(score) |>
    dplyr::summarise(
      average = base::mean(value, na.rm = TRUE),
      sd = stats::sd(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::replace_na(base::list(sd = 0)) |>
    dplyr::mutate(
      lower = average - sd,
      upper = average + sd,
      group = "selected"
    ) |>
    dplyr::mutate(score = forcats::fct_reorder(score, average)) |>
    ggplot2::ggplot(ggplot2::aes(x = score, y = average)) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = lower, ymax = upper), size = 1) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper)) +
    ggplot2::geom_line(ggplot2::aes(group = group)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_hline(yintercept = 1, lty = 2) +
    ggplot2::geom_hline(yintercept = -1, lty = 2) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal()
}


