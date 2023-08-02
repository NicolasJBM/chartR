#' @name display_curve
#' @title Display IRT curve
#' @author Nicolas Mangin
#' @description Function drawing the IRT curve associated to a selected question.
#' @param selected_model Tibble. Data produced by the function statistics_compute and stored in statistics$models for each question.
#' @return Plot a curve.
#' @import patchwork
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 scale_y_reverse
#' @export

display_curve <- function(selected_model){
  
  correct  <- NULL
  ability <- NULL
  prediction <- NULL
  rank <- NULL
  count <- NULL
  outcome <- NULL
  probability <- NULL
  difficulty <- NULL
  discrimination <- NULL
  count_rank <- NULL
  
  selected_model <- selected_model |>
    dplyr::mutate(rank = dplyr::ntile(ability, 20)) |>
    dplyr::mutate(
      ability = ability / 10,
      rank = rank / 2
    ) |>
    dplyr::group_by(rank, correct) |>
    dplyr::mutate(count_rank = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(outcome = dplyr::case_when(
      correct == 1 ~ "Success",
      TRUE ~ "Failure"
    )) |>
    dplyr::mutate(outcome = base::factor(outcome, levels = c("Success","Failure")))
  
  observations <- selected_model |>
    dplyr::group_by(ability) |>
    dplyr::summarise(
      probability = base::mean(probability, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::select(difficulty = ability, probability) |>
    dplyr::arrange(difficulty) 
  
  smooth <- stats::loess.smooth(x = observations$difficulty, y = observations$probability)
  
  parameters <- tibble::tibble(
    difficulty = smooth$x, probability = smooth$y,
    discrimination = c(0, base::diff(smooth$y)/base::diff(smooth$x))
  ) |>
    dplyr::filter(probability >= 0.45, probability <= 0.55) |>
    dplyr::summarise_all(base::mean, na.rm = TRUE) |>
    dplyr::mutate(
      difficulty = base::round(difficulty, 1),
      probability = base::round(probability, 2),
      discrimination = base::round(discrimination, 2)
    ) |>
    base::unique() |>
    dplyr::mutate(intercept = 0.5 - discrimination * difficulty)
  
  top <- selected_model |>
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = rank, y = correct, size = count_rank, color = outcome), alpha = 0.1) +
    ggplot2::scale_color_manual(values = c("forestgreen","firebrick4")) +
    ggplot2::geom_smooth(
      ggplot2::aes(x = ability, y = correct),
      method = "glm",
      method.args = base::list(family = "binomial"),
      formula = y ~ x,
      na.rm = TRUE,
      linewidth = 1.25
    ) +
    ggplot2::geom_hline(yintercept = parameters$probability, lty = 3, color = "purple", linewidth = 1.25) +
    ggplot2::geom_vline(xintercept = parameters$difficulty, lty = 3, color = "purple", linewidth = 1.25) +
    ggplot2::geom_abline(slope = parameters$discrimination, intercept = parameters$intercept, lty = 2, color = "purple", linewidth = 1.25) +
    ggplot2::scale_x_continuous(limits = c(0,11), breaks = base::seq(1,10,0.5)) +
    ggplot2::labs(
      x = "",
      y = "Probability of success"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
  
  down <- selected_model |>
    dplyr::group_by(rank, outcome) |>
    dplyr::summarise(count = dplyr::n()) |>
    ggplot2::ggplot(ggplot2::aes(x = rank, y = count, fill = outcome)) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::scale_x_continuous(limits = c(0,11), breaks = base::seq(1,10,0.5)) +
    ggplot2::scale_fill_manual(values = c("forestgreen","firebrick4")) +
    ggplot2::labs(x = "Student's ability", y = "Count") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
  
  top + down +
    patchwork::plot_layout(widths = 1, heights = c(2,1))
}
