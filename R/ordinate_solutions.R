#' Ordinate solutions
#'
#' @export


ordinate_solutions = function(solutions, distance, dim = 2) {
  solutions %>%
    dplyr::select(-class, -variableType) %>%
    tibble::column_to_rownames("RC") %>%
    t() %>%
    as.matrix() %>%
    vegan::metaMDS(distance = distance, k = dim)
}
