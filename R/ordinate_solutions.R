#' Ordinate solutions
#'
#' @export


ordinate_solutions <- function(solutions,
                               distance,
                               dim = 2,
                               quiet = F) {
  if ("ensemble" %in% class(solutions)) {
    # If given an ensemble, then save the ordination data to a new slot and return a new ensemble

    if (isTRUE(quiet)) {
      trace <- 0
    } else {
      trace <- 1
    }

    s <- matrify_solutions(solutions@solutions, scale = solutions@scale) |>
      dplyr::select(-class, -variableType) |>
      tibble::column_to_rownames("RC") |>
      t() |>
      as.matrix() |>
      vegan::metaMDS(
        distance = distance,
        k = dim,
        trace = trace
      )

    solutions@distance <- distance
    solutions@ordination$nmds <- s
    solutions@ordination$scores <- vegan::scores(solutions@ordination$nmds) |>
      as.data.frame() |>
      tibble::rownames_to_column("model")

    return(solutions)
  } else if ("data.frame" %in% class(solutions)) {
    matrify_solutions(solutions) |>
      dplyr::select(-class, -variableType) |>
      tibble::column_to_rownames("RC") |>
      t() |>
      as.matrix() |>
      vegan::metaMDS(distance = distance, k = dim)
  } else {
    stop(glue::glue("solutions must be class <ensemble> or a <data.frame>, not <{class(solutions)[1]}>."), call. = FALSE)
  }
}
