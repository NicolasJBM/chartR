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
  question <- NULL
  
  scores <- scores |>
    dplyr::select_if(base::is.numeric) |>
    tibble::rownames_to_column("student") |>
    dplyr::mutate_if(base::is.numeric, function(x) base::as.numeric(base::scale(x)))
  
  columns <- base::setdiff(base::names(scores), "student")
  
  scores <- scores |>
    tidyr::pivot_longer(dplyr::all_of(columns), names_to = "question", values_to = "value")
  
  scores |>
    dplyr::filter(student %in% selection) |>
    dplyr::mutate(question = base::as.factor(question)) |>
    dplyr::group_by(question) |>
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
    dplyr::mutate(col = dplyr::case_when(
      average >= 1 ~ "4",
      average >= 0 ~ "3",
      average >= -1 ~ "2",
      TRUE ~ "1"
    )) |>
    dplyr::mutate(question = forcats::fct_reorder(question, average)) |>
    ggplot2::ggplot(ggplot2::aes(x = question, y = average)) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = lower, ymax = upper, color = col), size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper)) +
    ggplot2::geom_line(ggplot2::aes(group = group)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_hline(yintercept = 1, lty = 2) +
    ggplot2::geom_hline(yintercept = -1, lty = 2) +
    ggplot2::scale_color_manual(values = c("firebrick4", "orange", "green", "forestgreen")) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}


