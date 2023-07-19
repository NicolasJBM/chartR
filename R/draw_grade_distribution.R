#' @name draw_grade_distribution
#' @title Display grade distribution
#' @author Nicolas Mangin
#' @description Function drawing the distribution of grades
#' @param student_grades Tibble. Table with each "student" id, the maximum number of "points", and the "grade".
#' @param pass Numeric. Value above which students validate.
#' @param increment Numeric. Size of breaks.
#' @return Plot a distribution.
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 xlim
#' @importFrom stats median
#' @importFrom stats sd
#' @export



draw_grade_distribution <- function(student_grades, pass, increment){
  
  grade <- NULL
  passing <- NULL
  points <- NULL
  
  student_grades <- student_grades |>
    dplyr::filter(points > 0) |>
    tidyr::replace_na(base::list(grade = 0)) |>
    stats::na.omit()
  
  student_grades |>
    dplyr::mutate(passing = grade >= pass) |>
    ggplot2::ggplot(ggplot2::aes(x = grade, fill = passing)) +
    ggplot2::geom_histogram(
      alpha = 0.5, color = "black",
      breaks=base::seq(base::min(0, student_grades$grade), base::max(student_grades$points), by = increment)
    ) +
    ggplot2::geom_vline(xintercept = base::mean(student_grades$grade), color = "red", size = 2) +
    ggplot2::geom_vline(xintercept = base::mean(student_grades$grade)-stats::sd(student_grades$grade), color = "red", size = 1, lty = 2) +
    ggplot2::geom_vline(xintercept = base::mean(student_grades$grade)+stats::sd(student_grades$grade), color = "red", size = 1, lty = 2) +
    ggplot2::geom_vline(xintercept = stats::median(student_grades$grade), color = "blue", size = 2) +
    ggplot2::scale_x_continuous(
      breaks = base::seq(0, base::max(student_grades$points), by = increment),
      limits = c(base::min(0, student_grades$grade), base::max(student_grades$points, student_grades$grade))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}



