#' Turned parsed solutions into a matrix of models (columns) by fluxes (rows)
#'
#'
#' @param solution A dataframe of ensemble solutions imported from `parse_solutions()`
#' @param scale A boolean value to scale the fluxes to the maximum value
#' @param types A character vector of the types of solutions to include
#'
#' @export

matrify_solutions <- function(solution,
                              scale = FALSE,
                              types = c("compounds", "reactions", "biomass")) {
  a <- solution |>
    dplyr::filter(type %in% types) |>
    dplyr::select(RC, class, variableType, other_values) |>
    dplyr::group_by(RC) |> # need to group here to get the right row_number next
    dplyr::mutate(R = paste0("model", dplyr::row_number())) |>
    dplyr::ungroup()

  if (isTRUE(scale)) {
    a <- a |>
      dplyr::group_by(RC) |>
      dplyr::mutate(other_values = dplyr::case_when(
        max(abs(other_values)) == 0 ~ 0,
        TRUE ~ other_values / max(abs(other_values))
      )) |>
      dplyr::ungroup()
  }

  a |>
    tidyr::pivot_wider(names_from = R, values_from = other_values)
}
