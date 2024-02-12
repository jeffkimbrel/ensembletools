#' Ensemble data class
#'
#' @export

ensemble <- S7::new_class(
  "ensemble",
  properties = list(
    path = S7::class_character,
    solutions = S7::class_data.frame,
    nmds = S7::class_list,
    clusters = S7::class_list
  ),
  constructor = function(path, scale) {

    solutions = parse_solutions(path)


    S7::new_object(S7::S7_object(),
      path = path,
      solutions = solutions,
      nmds = list(),
      clusters = list()
    )
  }
)


#' Get information about an ensemble
#'
#' @export
#'

info <- S7::new_generic("info", "x")
S7::method(info, ensemble) <- function(x) {
  e@solutions |> dplyr::select(RC, type) |>
    unique() |>
    dplyr::count(type)
}
