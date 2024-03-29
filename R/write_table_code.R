#' @name write_table_code
#' @title Write a relation table as diagram code
#' @author Nicolas Mangin
#' @description Take a table and transforms it into lines of code which can be written and inserted.
#' @param table Data.frame or tibble. Table which should be written as code.
#' @param name Character. Name of the resulting table.
#' @return A vector of character in which entries are lines to be written.
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom purrr map2_chr
#' @importFrom tibble tibble
#' @export


write_table_code <- function(table, name){
  columns <- NULL
  classes <- NULL
  
  table <- table |>
    base::as.data.frame() |>
    dplyr::mutate_if(base::is.factor, base::as.character)
  
  lines <- tibble::tibble(
    columns = base::names(table),
    classes = base::vapply(table, class, base::character(1))
  ) |>
    dplyr::mutate(
      lines = purrr::map2_chr(columns, classes, prepare_lines, table)
    )
  lines <- lines$lines
  lines[(base::seq_len(base::length(lines)-1))] <- base::paste0(
    lines[(base::seq_len(base::length(lines)-1))], ","
  )
  
  code <- c(
    paste0(name, " <- data.frame("),
    lines,
    ")"
  )
  return(code)
}

prepare_lines <- function(column, class, table) {
  values <- table[,column]
  if (class == "numeric"){
    values <- base::paste(values, collapse = ', ')
    values <- base::paste0('  ', column, ' = c(', values, ')')
  } else {
    values <- base::paste(values, collapse = '", "')
    values <- base::paste0('  ', column, ' = c("', values, '")')
  }
  return(values)
}
