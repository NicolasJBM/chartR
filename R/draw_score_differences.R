#' @name draw_score_differences
#' @title Edit diagrams
#' @author Nicolas Mangin
#' @description function showing how a set of selected observations differ from the mean of the whole group on a set of questions.
#' @param scores Tibble.
#' @param selection Character vector
#' @return A ggplot object ready for rendering.
#' @importFrom dplyr all_of
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr select_if
#' @importFrom dplyr summarise
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_pointrange
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr replace_na
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
  stddev <- NULL
  
  scores <- scores |>
    dplyr::select_if(base::is.numeric) |>
    tibble::rownames_to_column("student") |>
    dplyr::mutate_if(base::is.numeric, function(x) base::as.numeric(base::scale(x)))
  
  columns <- base::setdiff(base::names(scores), "student")
  
  scores <- scores |>
    tidyr::pivot_longer(dplyr::all_of(columns), names_to = "question", values_to = "value")
  
  scores <- scores |>
    dplyr::filter(student %in% selection) |>
    dplyr::mutate(question = base::as.factor(question)) |>
    dplyr::group_by(question) |>
    dplyr::summarise(
      average = base::mean(value, na.rm = TRUE),
      stddev = stats::sd(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::replace_na(base::list(stddev = 0)) |>
    dplyr::mutate(
      lower = average - stddev,
      upper = average + stddev,
      group = "selected"
    ) |>
    dplyr::mutate(col = dplyr::case_when(
      average >= 1 ~ "forestgreen",
      average >= 0 ~ "green",
      average >= -1 ~ "orange",
      TRUE ~ "firebrick4"
    )) |>
    dplyr::mutate(question = forcats::fct_reorder(question, average))
  
  scores |>
    ggplot2::ggplot(ggplot2::aes(x = question, y = average)) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = lower, ymax = upper), size = 2, color = scores$col) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper)) +
    ggplot2::geom_line(ggplot2::aes(group = group)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_hline(yintercept = 1, lty = 2) +
    ggplot2::geom_hline(yintercept = -1, lty = 2) +
    ggplot2::scale_color_manual(values = c("", "", "", "")) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}


