% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_grade_density.R
\name{draw_grade_density}
\alias{draw_grade_density}
\title{Grade density}
\usage{
draw_grade_density(
  grades,
  studentid,
  structure,
  labels = base::list(x = "Grade", y = "Proportion", avg = "Mean", med = "Median", stu =
    "You")
)
}
\arguments{
\item{grades}{Tibble. List of grades of all students. Three necessary variables: student, points, grade.}

\item{studentid}{Character. ID of the student.}

\item{structure}{Tibble. Variables: threshold, status, color, and comment. Comment is not necessary.}

\item{labels}{List. Names for axes and legends (for translations purposes)}
}
\value{
A ggplot object ready for rendering.
}
\description{
Function drawing grade density and positioning the student relative to the mean and median and showing the status areas.
}
\author{
Nicolas Mangin
}
