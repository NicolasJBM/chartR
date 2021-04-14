#' @name write_table_code
#' @title Write a table as code
#' @author Nicolas Mangin
#' @description Take a table and transforms it into lines of code which can be written and inserted.
#' @param table Data.frame or tibble. Table which should be written as code.
#' @param name Character. Name of the resulting table.
#' @return A vector of character in which entries are lines to be written.
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom purrr map_chr
#' @importFrom stringr str_remove


write_table_code <- function(table, name){
  
  columns <- NULL
  
  lines <- tibble::tibble(
    columns = names(table)
  ) %>%
    dplyr::mutate(
      lines = purrr::map_chr(columns, prepare_lines, table)
    )
  
  lines <- lines$lines
  lines[length(lines)] <- stringr::str_remove(lines[length(lines)], ",$")
  
  firstline <- paste0(name, " <- data.frame(")
  result <- c(firstline, lines, ")")
  return(result)
}


prepare_lines <- function(column, table) {
  if (nrow(table) > 1){
    paste0("  ",column, " = c(",paste(prepchr(table[,column]), collapse = ','), "),")
  } else {
    paste0("  ",column, " = ",prepchr(table[1,column]), ",")
  }
}

prepchr <- function(x) {
  if (is.na(suppressWarnings(as.numeric(x)))[1]) paste0('"',x,'"') else x
} 
