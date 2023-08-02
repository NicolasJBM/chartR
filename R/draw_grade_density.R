#' @name draw_grade_density
#' @title Grade density
#' @author Nicolas Mangin
#' @description Function drawing grade density and positioning the student relative to the mean and median and showing the status areas.
#' @param grades Tibble. List of grades of all students. Three necessary variables: student, points, grade.
#' @param studentid Character. ID of the student.
#' @param structure Tibble. Variables: threshold, status, color, and comment. Comment is not necessary.
#' @param labels List. Names for axes and legends (for translations purposes)
#' @return A ggplot object ready for rendering.
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 geom_area
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 ylim
#' @export


draw_grade_density <- function(
    grades, studentid, structure,
    labels = base::list(
      x = "Grade",
      y = "Proportion",
      avg = "Mean",
      med = "Median",
      stu = "You"
    )
){
  
  x <- NULL
  y <- NULL
  grade <- NULL
  status <- NULL
  
  avg <- base::mean(grades$grade)
  med <- stats::median(grades$grade)
  sg <- grades$grade[base::match(studentid, grades$student)]
  maxx <- base::max(grades$points)*1.1
  maxy <- stats::density(stats::na.omit(grades$grade))
  maxy <- base::max(maxy$y)*1.1
  
  dens = base::data.frame(stats::density(grades$grade, n=2^10, adjust=1)[c("x","y")]) |>
    dplyr::mutate(status = base::cut(x, breaks=c(
      -Inf,
      structure$threshold[1],
      structure$threshold[2],
      structure$threshold[3],
      structure$threshold[4],
      Inf
    ))) |>
    dplyr::group_by(status) |>
    dplyr::mutate(prob = base::paste0(base::round(base::sum(y)*base::mean(base::diff(x))*100),"%"))
  
  # Assign colors to each level of section
  col = stats::setNames(structure$color, base::levels(dens$status))
  
  ggplot2::ggplot(dens, ggplot2::aes(x, y, fill=status)) +
    ggplot2::geom_area(alpha = 0.5) +
    ggplot2::geom_line(linewidth=1.5) +
    ggplot2::scale_fill_manual(labels=structure$status, values = col) +
    ggplot2::geom_vline(xintercept = avg, color = "blue", size = 1.5, lty = 2) +
    ggplot2::geom_vline(xintercept = med, color = "skyblue3", size = 1.5, lty = 2) +
    ggplot2::geom_vline(xintercept = sg, color = "purple", size = 1.5) +
    ggplot2::annotate("text", label = labels$med, x = 1, y = maxy*0.95, color = "blue") +
    ggplot2::annotate("text", label = labels$avg, x = 1, y = maxy*0.90, color = "skyblue3") +
    ggplot2::annotate("text", label = labels$stu, x = 1, y = maxy*0.85, color = "purple") +
    ggplot2::scale_x_continuous(breaks = base::seq(0,maxx,1)) +
    ggplot2::xlab(labels$x) +
    ggplot2::ylab(labels$y) +
    ggplot2::xlim(0, maxx) +
    ggplot2::ylim(0, maxy) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
  
}
