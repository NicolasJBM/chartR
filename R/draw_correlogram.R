#' @name draw_correlogram
#' @title Correlogram
#' @author Nicolas Mangin
#' @description Function creating a correlogram.
#' @param scores Tibble. Table showing the different scores associated to various observations.
#' @return A ggplot object ready for rendering.
#' @importFrom ggcorrplot cor_pmat
#' @importFrom ggcorrplot ggcorrplot
#' @export

draw_correlogram <- function(scores){
  scores <- dplyr::select_if(scores, base::is.numeric)
  scores <- scores[,(base::apply(scores, 2, stats::sd) != 0)]
  corr <- base::round(stats::cor(scores, method = "kendall"), 2)
  p.mat <- ggcorrplot::cor_pmat(scores)
  ggcorrplot::ggcorrplot(
    corr,
    p.mat = p.mat,
    show.diag = TRUE,
    show.legend = FALSE,
    hc.order = TRUE,
    sig.level = 0.10,
    type = "full",
    insig = "blank",
    colors = c("red", "white", "forestgreen"),
    outline.col = "white"
  )
}


